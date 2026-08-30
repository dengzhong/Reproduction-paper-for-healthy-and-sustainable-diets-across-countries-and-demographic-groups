suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(sf)
  library(patchwork)
  library(grid)
})


get_script_dir <- function() {
  script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)

  if (is.null(script_file)) {
    file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(file_arg) > 0) {
      script_file <- sub("^--file=", "", file_arg[1])
    }
  }

  if (is.null(script_file)) {
    return(normalizePath(getwd()))
  }

  dirname(normalizePath(script_file))
}

fig_dir <- get_script_dir()
input_file <- file.path(fig_dir, "health_food_deaths_step3.xlsx")
income_file <- file.path(fig_dir, "income group.xlsx")
world_file <- file.path(fig_dir, "world.geojson")

deaths_detail <- read_excel(
  input_file,
  sheet = "delta_deaths_detail",
  .name_repair = "minimal"
)

income_lookup <- read_excel(
  income_file,
  .name_repair = "minimal"
)

short_number <- label_number(
  scale_cut = cut_short_scale(),
  accuracy = 0.1,
  big.mark = ","
)

income_axis_breaks <- function(limits) {
  upper <- max(limits, na.rm = TRUE)
  if (upper < 1e5) c(0, 2e4, 4e4) else c(0, 3e5, 6e5)
}

# ---- Shared styling ---------------------------------------------------------

text_colour <- "black"
secondary_text <- "black"
grid_colour <- "#E3E7EA"

disease_levels <- c(
  "Ischemic heart disease",
  "Total cancers",
  "Diabetes mellitus type 2",
  "Stroke",
  "Colon and rectum cancer"
)

disease_labels <- c(
  "Ischemic heart disease" = "Ischaemic heart disease",
  "Total cancers" = "Total cancers",
  "Diabetes mellitus type 2" = "Type 2 diabetes",
  "Stroke" = "Stroke",
  "Colon and rectum cancer" = "Colorectal cancer"
)

disease_palette <- c(
  "Ischemic heart disease" = "#B84A48",
  "Total cancers" = "#7867A5",
  "Diabetes mellitus type 2" = "#3E8E96",
  "Stroke" = "#5B8DB8",
  "Colon and rectum cancer" = "#D59035"
)

food_levels <- c(
  "Fruits",
  "Vegetables",
  "Legumes",
  "Nuts",
  "Whole grains",
  "Processed meat",
  "Red meat"
)

food_palette <- c(
  "Fruits"         = "#E1A93C",
  "Vegetables"     = "#4F8F5B",
  "Legumes"        = "#2F8C7F",
  "Nuts"           = "#477DA5",
  "Whole grains"   = "#8873A9",
  "Processed meat" = "#D27A55",
  "Red meat"       = "#9F3F4D"
)

# ---- Panel a: geographic distribution --------------------------------------

country_total <- deaths_detail %>%
  group_by(ISO3) %>%
  summarise(
    avoidable_deaths = sum(delta_deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    deaths_group = cut(
      avoidable_deaths,
      breaks = c(-Inf, 0, 5000, 20000, 50000, 150000, Inf),
      labels = c(
        "<5K",
        "<5K",
        "5–20K",
        "20–50K",
        "50–150K",
        "≥150K"
      ),
      right = FALSE
    )
  )

global_total <- sum(country_total$avoidable_deaths, na.rm = TRUE)

world_map <- sf::st_read(
  world_file,
  quiet = TRUE
) %>%
  filter(
    !grepl("ATA", SOC, ignore.case = TRUE)
  )

world <- world_map %>%
  mutate(
    join_iso3 = recode(
      SOC,
      "ROM" = "ROU",
      "TMP" = "TLS"
    )
  ) %>%
  left_join(country_total, by = c("join_iso3" = "ISO3"))

unmatched_iso3 <- setdiff(country_total$ISO3, world$join_iso3)
if (length(unmatched_iso3) > 0) {
  warning(
    "Country totals not matched to the map: ",
    paste(unmatched_iso3, collapse = ", ")
  )
}

map_palette <- c(
  "Increase" = "#DDEBE8",
  "<5K" = "#DDEBE8",
  "5–20K" = "#B7D8D2",
  "20–50K" = "#78B7AE",
  "50–150K" = "#3E8F88",
  "≥150K" = "#175E61"
)

panel_a <- ggplot(world) +
  geom_sf(
    aes(fill = deaths_group),
    linewidth = 0.13,
    colour = "black"
  ) +
  coord_sf(
    crs = "+proj=robin",
    datum = NA,
    expand = FALSE
  ) +
  scale_fill_manual(
    values = map_palette,
    breaks = names(map_palette),
    drop = FALSE,
    na.value = "#E7E7E7",
    name = "Avoided deaths"
  ) +
  guides(
    fill = guide_legend(
      title.position = "left",
      title.vjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      keywidth = unit(5, "mm"),
      keyheight = unit(3.5, "mm")
    )
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(
      size = 14,
      face = "plain",
      colour = text_colour,
      margin = margin(b = 2)
    ),
    plot.subtitle = element_text(
      size = 9,
      colour = secondary_text,
      margin = margin(b = 4)
    ),
    legend.position = "bottom",
    legend.justification = "left",
    legend.box.just = "left",
    legend.title = element_text(
      size = 12,
      face = "plain",
      colour = text_colour,
      margin = margin(r = 4)
    ),
    legend.text = element_text(size = 12, face = "plain", colour = secondary_text),
    legend.spacing.x = unit(0.8, "mm"),
    legend.margin = margin(0),
    plot.margin = margin(1, 2, 0, 2)
  )

# A compact income-group summary is kept inside panel a so that geography and
# development level are read together without multiplying the age panels.
income_levels <- c("High", "Upper-middle", "Lower-middle", "Low")
income_abbreviations <- c(
  "High" = "H",
  "Upper-middle" = "UM",
  "Lower-middle" = "LM",
  "Low" = "L"
)

deaths_income <- deaths_detail %>%
  left_join(income_lookup, by = "ISO3") %>%
  mutate(
    income_group = case_when(
      `Income-level` == "High income" ~ "High",
      `Income-level` == "Upper-middle income" ~ "Upper-middle",
      `Income-level` == "Lower middle income" ~ "Lower-middle",
      `Income-level` == "Low income" ~ "Low",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(income_group))

income_total <- deaths_income %>%
  group_by(income_group) %>%
  summarise(total = sum(delta_deaths, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    income_group = factor(income_group, levels = rev(income_levels)),
    share = total / global_total,
    total_label = paste0(
      short_number(total),
      "\n",
      percent(share, accuracy = 0.1)
    ),
    label_x = total + 0.10e6,
    label_hjust = 0,
    label_colour = text_colour
  )

income_disease <- deaths_income %>%
  filter(Endpoint %in% disease_levels) %>%
  group_by(income_group, Endpoint) %>%
  summarise(
    avoidable_deaths = sum(delta_deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    income_group = factor(income_group, levels = rev(income_levels)),
    Endpoint = factor(Endpoint, levels = disease_levels)
  ) %>%
  group_by(income_group) %>%
  mutate(
    disease_share = avoidable_deaths / sum(avoidable_deaths),
    disease_label = ifelse(
      disease_share >= 0.05,
      percent(disease_share, accuracy = 1),
      ""
    )
  ) %>%
  ungroup()

panel_income <- ggplot(
  income_disease,
  aes(
    x = avoidable_deaths,
    y = income_group,
    fill = Endpoint
  )
) +
  geom_col(
    width = 0.62,
    colour = "black",
    linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  geom_vline(
    xintercept = 0,
    colour = "#C83E3E",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_text(
    data = income_total,
    aes(
      x = label_x,
      y = income_group,
      label = total_label,
      hjust = label_hjust,
      colour = label_colour
    ),
    inherit.aes = FALSE,
    size = 10 / .pt,
    fontface = "bold",
    lineheight = 0.88,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = disease_palette,
    breaks = disease_levels,
    drop = FALSE,
    guide = "none"
  ) +
  scale_colour_identity() +
  scale_x_continuous(
    limits = c(0, 5.6e6),
    breaks = c(0, 2e6, 4e6),
    labels = label_number(
      scale_cut = cut_short_scale(),
      accuracy = 0.1
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(
    drop = FALSE,
    labels = income_abbreviations
  ) +
  labs(
    x = "Net deaths avoided",
    y = NULL
  ) +
  theme_minimal(base_family = "sans", base_size = 8.5) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(
      size = 11,
      face = "plain",
      colour = text_colour,
      margin = margin(b = 2)
    ),
    plot.subtitle = element_text(
      size = 7.8,
      colour = secondary_text,
      margin = margin(b = 4)
    ),
    axis.title.x = element_text(
      size = 12,
      face = "plain",
      colour = text_colour,
      margin = margin(t = 4)
    ),
    axis.text = element_text(size = 12, face = "plain", colour = text_colour),
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "black", linewidth = 0.32),
    axis.ticks.x = element_line(colour = "black", linewidth = 0.32),
    axis.ticks.length.x = unit(1.5, "mm"),
    plot.margin = margin(1, 1, 5, 1)
  )

make_disease_donut <- function(group_name) {
  donut_data <- income_disease %>%
    filter(as.character(income_group) == group_name) %>%
    arrange(Endpoint) %>%
    mutate(
      cumulative_start = lag(cumsum(disease_share), default = 0),
      cumulative_end = cumsum(disease_share),
      angle_start = pi / 2 - 2 * pi * cumulative_start,
      angle_end = pi / 2 - 2 * pi * cumulative_end,
      angle_mid = (angle_start + angle_end) / 2
    )

  sector_polygons <- bind_rows(lapply(
    seq_len(nrow(donut_data)),
    function(i) {
      outer_angle <- seq(
        donut_data$angle_start[i],
        donut_data$angle_end[i],
        length.out = 80
      )
      inner_angle <- rev(outer_angle)

      tibble(
        x = c(cos(outer_angle), 0.50 * cos(inner_angle)),
        y = c(sin(outer_angle), 0.50 * sin(inner_angle)),
        polygon_id = i,
        Endpoint = donut_data$Endpoint[i]
      )
    }
  ))

  curved_labels <- donut_data %>%
    filter(disease_label != "") %>%
    mutate(
      x = 0.79 * cos(angle_mid),
      y = 0.79 * sin(angle_mid),
      tangent_angle = (angle_mid * 180 / pi + 90) %% 360,
      angle = ifelse(
        tangent_angle > 90 & tangent_angle < 270,
        tangent_angle + 180,
        tangent_angle
      )
    )

  ggplot() +
    geom_polygon(
      data = sector_polygons,
      aes(
        x = x,
        y = y,
        group = polygon_id,
        fill = Endpoint
      ),
      colour = NA
    ) +
    geom_text(
      data = curved_labels,
      aes(
        x = x,
        y = y,
        label = disease_label,
        angle = angle
      ),
      size = 8 / .pt,
      fontface = "bold",
      colour = "white"
    ) +
    annotate(
      "text",
      x = 0,
      y = 0,
      label = unname(income_abbreviations[group_name]),
      size = 8 / .pt,
      fontface = "bold",
      colour = "black"
    ) +
    coord_equal(
      xlim = c(-1.08, 1.08),
      ylim = c(-1.08, 1.08),
      expand = FALSE,
      clip = "off"
    ) +
    scale_fill_manual(
      values = disease_palette,
      breaks = disease_levels,
      drop = FALSE,
      guide = "none"
    ) +
    theme_void(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = NA, colour = NA),
      plot.margin = margin(0)
    )
}

panel_a <- panel_a +
  labs(tag = "a") +
  theme(
    plot.tag = element_text(
      family = "sans",
      size = 10,
      face = "bold",
      colour = text_colour,
      hjust = 0,
      vjust = 1
    ),
    plot.tag.position = c(-0.09, 1)
  ) +
  inset_element(
    make_disease_donut("High"),
    left = 0.11,
    bottom = 0.45,
    right = 0.33,
    top = 1.00,
    align_to = "panel"
  ) +
  inset_element(
    make_disease_donut("Upper-middle"),
    left = 0.27,
    bottom = 0.00,
    right = 0.49,
    top = 0.55,
    align_to = "panel"
  ) +
  inset_element(
    make_disease_donut("Lower-middle"),
    left = 0.51,
    bottom = 0.055,
    right = 0.73,
    top = 0.605,
    align_to = "panel"
  ) +
  inset_element(
    make_disease_donut("Low"),
    left = 0.71,
    bottom = 0.395,
    right = 0.93,
    top = 0.945,
    align_to = "panel"
  )

# ---- Panel b: global age and disease profile --------------------------------

age_levels <- c("20–44", "45–59", "60–69", "70–79", "80+")
sex_levels <- c("Male", "Female")

income_total_named <- setNames(
  income_total$total,
  as.character(income_total$income_group)
)

income_facet_labels <- setNames(
  paste0(
    unname(income_abbreviations[income_levels]),
    "  ·  ",
    short_number(income_total_named[income_levels])
  ),
  income_levels
)

age_disease <- deaths_detail %>%
  left_join(income_lookup, by = "ISO3") %>%
  mutate(
    Sex = recode(Sex, "MLE" = "Male", "FML" = "Female"),
    income_group = case_when(
      `Income-level` == "High income" ~ "High",
      `Income-level` == "Upper-middle income" ~ "Upper-middle",
      `Income-level` == "Lower middle income" ~ "Lower-middle",
      `Income-level` == "Low income" ~ "Low",
      TRUE ~ NA_character_
    ),
    age_band = case_when(
      age_group %in% c("20-24", "25-29", "30-34", "35-39", "40-44") ~ "20–44",
      age_group %in% c("45-49", "50-54", "55-59") ~ "45–59",
      age_group %in% c("60-64", "65-69") ~ "60–69",
      age_group %in% c("70-74", "75-79") ~ "70–79",
      age_group == "80plus" ~ "80+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(age_band),
    !is.na(income_group),
    Sex %in% sex_levels,
    Endpoint %in% disease_levels
  ) %>%
  group_by(income_group, Sex, age_band, Endpoint) %>%
  summarise(
    avoidable_deaths = sum(delta_deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    income_group = factor(income_group, levels = income_levels),
    Sex = factor(Sex, levels = sex_levels),
    age_band = factor(age_band, levels = rev(age_levels)),
    Endpoint = factor(Endpoint, levels = disease_levels)
  )

age_disease_bounds <- age_disease %>%
  group_by(income_group, Sex, age_band) %>%
  summarise(
    positive_total = sum(pmax(avoidable_deaths, 0)),
    negative_total = sum(pmin(avoidable_deaths, 0)),
    net_change = sum(avoidable_deaths),
    .groups = "drop"
  ) %>%
  mutate(
    net_share = net_change /
      income_total_named[as.character(income_group)],
    net_label = percent(net_share, accuracy = 1)
  )

panel_b <- ggplot(
  age_disease,
  aes(x = avoidable_deaths, y = age_band, fill = Endpoint)
) +
  geom_col(
    width = 0.74,
    colour = "black",
    linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  geom_col(
    data = age_disease_bounds,
    aes(x = positive_total, y = age_band),
    inherit.aes = FALSE,
    width = 0.74,
    fill = NA,
    colour = "black",
    linewidth = 0
  ) +
  geom_col(
    data = filter(age_disease_bounds, negative_total < 0),
    aes(x = negative_total, y = age_band),
    inherit.aes = FALSE,
    width = 0.74,
    fill = NA,
    colour = "black",
    linewidth = 0
  ) +
  geom_vline(
    xintercept = 0,
    colour = "#C83E3E",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_point(
    data = age_disease_bounds,
    aes(x = net_change, y = age_band, shape = "Net change"),
    inherit.aes = FALSE,
    size = 2.6,
    stroke = 0.45,
    fill = "white",
    colour = "black"
  ) +
  geom_text(
    data = age_disease_bounds,
    aes(x = net_change, y = age_band, label = net_label),
    inherit.aes = FALSE,
    hjust = -0.12,
    size = 10 / .pt,
    fontface = "bold",
    colour = "black"
  ) +
  facet_grid(
    rows = vars(Sex),
    cols = vars(income_group),
    scales = "free_x",
    labeller = labeller(income_group = income_facet_labels)
  ) +
  scale_fill_manual(
    values = disease_palette,
    breaks = disease_levels,
    labels = disease_labels,
    drop = FALSE,
    name = NULL
  ) +
  scale_shape_manual(
    values = c("Net change" = 21),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = income_axis_breaks,
    labels = label_number(
      scale_cut = cut_short_scale(),
      accuracy = 1,
      big.mark = ","
    ),
    expand = expansion(mult = c(0.06, 0.24))
  ) +
  scale_y_discrete(drop = FALSE) +
  guides(
    fill = guide_legend(
      order = 1,
      nrow = 2,
      byrow = TRUE,
      keywidth = unit(7, "mm"),
      keyheight = unit(3.5, "mm")
    ),
    shape = guide_legend(
      order = 2,
      override.aes = list(
        fill = "white",
        colour = "black",
        size = 2.6
      )
    )
  ) +
  labs(
    x = "Net deaths avoided",
    y = "Age group"
  ) +
  theme_minimal(base_family = "sans", base_size = 9) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(
      size = 14,
      face = "plain",
      colour = text_colour,
      margin = margin(b = 2)
    ),
    plot.subtitle = element_text(
      size = 8.5,
      colour = secondary_text,
      margin = margin(b = 3)
    ),
    axis.title = element_text(
      size = 12,
      face = "plain",
      colour = text_colour
    ),
    axis.title.y = element_text(margin = margin(r = 4)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 12, face = "plain", colour = text_colour),
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "black", linewidth = 0.32),
    axis.ticks.x = element_line(colour = "black", linewidth = 0.32),
    axis.ticks.length.x = unit(1.5, "mm"),
    panel.spacing.x = unit(2.5, "mm"),
    panel.spacing.y = unit(0.4, "mm"),
    strip.background = element_blank(),
    strip.text = element_text(
      size = 10,
      face = "bold",
      colour = text_colour,
      hjust = 0
    ),
    strip.text.y.right = element_text(
      size = 10,
      face = "bold",
      colour = text_colour,
      angle = 270,
      hjust = 0.5,
      vjust = 0.5
    ),
    legend.position = "bottom",
    legend.justification = "left",
    legend.box.just = "left",
    legend.box = "horizontal",
    legend.title = element_text(size = 12, face = "plain", colour = text_colour),
    legend.text = element_text(size = 12, face = "plain", colour = secondary_text),
    legend.spacing.x = unit(0.8, "mm"),
    legend.margin = margin(1, 0, 0, 0),
    plot.margin = margin(0, 2, 1, 2)
  )

# ---- Panel c: global age and food profile -----------------------------------

age_food <- deaths_detail %>%
  left_join(income_lookup, by = "ISO3") %>%
  mutate(
    Sex = recode(Sex, "MLE" = "Male", "FML" = "Female"),
    income_group = case_when(
      `Income-level` == "High income" ~ "High",
      `Income-level` == "Upper-middle income" ~ "Upper-middle",
      `Income-level` == "Lower middle income" ~ "Lower-middle",
      `Income-level` == "Low income" ~ "Low",
      TRUE ~ NA_character_
    ),
    age_band = case_when(
      age_group %in% c("20-24", "25-29", "30-34", "35-39", "40-44") ~ "20–44",
      age_group %in% c("45-49", "50-54", "55-59") ~ "45–59",
      age_group %in% c("60-64", "65-69") ~ "60–69",
      age_group %in% c("70-74", "75-79") ~ "70–79",
      age_group == "80plus" ~ "80+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(age_band),
    !is.na(income_group),
    Sex %in% sex_levels,
    `Food group` %in% food_levels
  ) %>%
  group_by(income_group, Sex, age_band, `Food group`) %>%
  summarise(
    avoidable_deaths = sum(delta_deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    income_group = factor(income_group, levels = income_levels),
    Sex = factor(Sex, levels = sex_levels),
    age_band = factor(age_band, levels = rev(age_levels)),
    `Food group` = factor(`Food group`, levels = food_levels)
  )

age_food_totals <- age_food %>%
  group_by(income_group, Sex, age_band) %>%
  summarise(
    positive_total = sum(pmax(avoidable_deaths, 0)),
    negative_total = sum(pmin(avoidable_deaths, 0)),
    net_change = sum(avoidable_deaths),
    .groups = "drop"
  ) %>%
  mutate(
    net_share = net_change /
      income_total_named[as.character(income_group)],
    net_label = percent(net_share, accuracy = 1)
  )

panel_c <- ggplot(
  age_food,
  aes(x = avoidable_deaths, y = age_band, fill = `Food group`)
) +
  geom_col(
    width = 0.74,
    colour = "black",
    linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  geom_col(
    data = age_food_totals,
    aes(x = positive_total, y = age_band),
    inherit.aes = FALSE,
    width = 0.74,
    fill = NA,
    colour = "black",
    linewidth = 0
  ) +
  geom_col(
    data = filter(age_food_totals, negative_total < 0),
    aes(x = negative_total, y = age_band),
    inherit.aes = FALSE,
    width = 0.74,
    fill = NA,
    colour = "black",
    linewidth = 0
  ) +
  geom_vline(
    xintercept = 0,
    colour = "#C83E3E",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_point(
    data = age_food_totals,
    aes(x = net_change, y = age_band, shape = "Net change"),
    inherit.aes = FALSE,
    size = 2.6,
    stroke = 0.45,
    fill = "white",
    colour = "black"
  ) +
  geom_text(
    data = age_food_totals,
    aes(x = net_change, y = age_band, label = net_label),
    inherit.aes = FALSE,
    hjust = -0.12,
    size = 10 / .pt,
    fontface = "bold",
    colour = "black"
  ) +
  facet_grid(
    rows = vars(Sex),
    cols = vars(income_group),
    scales = "free_x",
    labeller = labeller(income_group = income_facet_labels)
  ) +
  scale_fill_manual(
    values = food_palette,
    breaks = food_levels,
    drop = FALSE,
    name = NULL
  ) +
  scale_shape_manual(
    values = c("Net change" = 21),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = income_axis_breaks,
    labels = label_number(
      scale_cut = cut_short_scale(),
      accuracy = 1,
      big.mark = ","
    ),
    expand = expansion(mult = c(0.06, 0.24))
  ) +
  scale_y_discrete(drop = FALSE) +
  guides(
    fill = guide_legend(
      order = 1,
      nrow = 2,
      byrow = TRUE,
      keywidth = unit(6, "mm"),
      keyheight = unit(3.5, "mm")
    ),
    shape = guide_legend(
      order = 2,
      override.aes = list(
        fill = "white",
        colour = "black",
        size = 2.6
      )
    )
  ) +
  labs(
    x = "Net deaths avoided",
    y = "Age group"
  ) +
  theme_minimal(base_family = "sans", base_size = 9) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(
      size = 14,
      face = "plain",
      colour = text_colour,
      margin = margin(b = 2)
    ),
    plot.subtitle = element_text(
      size = 8.5,
      colour = secondary_text,
      margin = margin(b = 3)
    ),
    axis.title = element_text(
      size = 12,
      face = "plain",
      colour = text_colour
    ),
    axis.title.y = element_text(margin = margin(r = 4)),
    axis.title.x = element_text(margin = margin(t = 3)),
    axis.text = element_text(size = 12, face = "plain", colour = text_colour),
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "black", linewidth = 0.32),
    axis.ticks.x = element_line(colour = "black", linewidth = 0.32),
    axis.ticks.length.x = unit(1.5, "mm"),
    panel.spacing.x = unit(2.5, "mm"),
    panel.spacing.y = unit(0.4, "mm"),
    strip.background = element_blank(),
    strip.text = element_text(
      size = 10,
      face = "bold",
      colour = text_colour,
      hjust = 0
    ),
    strip.text.y.right = element_text(
      size = 10,
      face = "bold",
      colour = text_colour,
      angle = 270,
      hjust = 0.5,
      vjust = 0.5
    ),
    legend.position = "bottom",
    legend.justification = "left",
    legend.box.just = "left",
    legend.box = "horizontal",
    legend.title = element_text(size = 12, face = "plain", colour = text_colour),
    legend.text = element_text(size = 12, face = "plain", colour = secondary_text),
    legend.spacing.x = unit(0.7, "mm"),
    legend.margin = margin(1, 0, 0, 0),
    plot.margin = margin(0, 2, 1, 2)
  )

# ---- Assemble and export ----------------------------------------------------

tag_theme <- theme(
  plot.tag = element_text(
    family = "sans",
    size = 10,
    face = "bold",
    colour = text_colour
  )
)

panel_b <- panel_b + labs(tag = "b") + tag_theme
panel_c <- panel_c + labs(tag = "c") + tag_theme


shared_legend_theme <- theme(
  legend.position = "bottom",
  legend.justification = "left",
  legend.box.just = "left"
)

panel_a_with_income <- (panel_a | panel_income) +
  plot_layout(
    widths = c(2.8, 1.15),
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    legend.box.just = "left"
  )

fig5 <- panel_a_with_income / panel_b / panel_c +
  plot_layout(heights = c(0.78, 0.83, 0.83)) +
  plot_annotation()

print(fig5)

ggsave(
  filename = file.path(fig_dir, "Fig5.svg"),
  plot = fig5,
  width = 210,
  height = 227,
  units = "mm",
  device = svglite::svglite,
  bg = "white"
)

ggsave(
  filename = file.path(fig_dir, "Fig5.png"),
  plot = fig5,
  width = 210,
  height = 227,
  units = "mm",
  dpi = 300,
  bg = "white"
)

message(
  "Fig. 5 created: ",
  nrow(country_total),
  " countries; global net total = ",
  number(global_total, accuracy = 1, big.mark = ","),
  " deaths."
)
