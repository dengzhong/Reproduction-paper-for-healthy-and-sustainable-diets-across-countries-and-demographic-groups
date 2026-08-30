# Figure 4: nutrient and food drivers of nutritional adequacy gains
# =================================================================
#
# Figure contract
# Core conclusion:
#   The nutrients and foods responsible for improved nutritional adequacy
#   vary systematically across income, sex and age strata.
#
# Archetype:
#   Quantitative grid with one hero panel and one explanatory panel.
#
# Panel map:
#   a, Dominant nutrient contributor and its percentage contribution.
#   b, Food sources underlying four prespecified nutrient-gain pathways.
#
# Statistical unit:
#   Country × sex × age stratum. Nutrient adequacy is calculated within each
#   country before population-weighted aggregation.
#
# Required input files (placed beside this script):
#   00Nutrient_Ori.xlsx
#   00Nutrient_New.xlsx
#   RDA.xlsx
#   population_long.xlsx
#   Income-level.xlsx

required_packages <- c(
  "readxl", "readr", "dplyr", "tidyr", "purrr",
  "ggplot2", "patchwork", "scales", "svglite", "ragg"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly=TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following R packages before running this script: ",
    paste(missing_packages, collapse=", ")
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value=TRUE)
base_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  getwd()
}

output_dir <- file.path(base_dir, "Figure4_bc_submission_output")
dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)

# ---------------------------------------------------------------------------
# 1. Constants and labels
# ---------------------------------------------------------------------------

ages <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65-69", "70-74", "75-79", "80+"
)

regions <- c("High", "Upper-middle", "Lower-middle", "Low")
plot_regions <- c("World", regions)

region_labels <- c(
  World="World",
  High="High income",
  `Upper-middle`="Upper-middle income",
  `Lower-middle`="Lower-middle income",
  Low="Low income"
)

income_codes <- c(
  World="W",
  High="H",
  `Upper-middle`="UM",
  `Lower-middle`="LM",
  Low="L"
)

sex_labels <- c(
  FML="Female",
  MLE="Male"
)

nutrient_labels <- c(
  calcium="Calcium",
  vitamina="Vitamin A",
  iron="Iron",
  potassium="Potassium"
)

nutrient_colours <- c(
  calcium="#3F7FAF",
  vitamina="#D69035",
  iron="#B85A58",
  potassium="#739C6C"
)

food_labels <- c(
  milk="Milk",
  seed_sesame="Sesame",
  cassava="Cassava",
  othr_vegetables="Vegetables",
  seed_rape="Rapeseed",
  soyabeans="Soybeans",
  beans="Beans",
  othr_pulse="Other pulses",
  othr_grains="Other grains",
  oil_palm="Palm oil",
  sweet_potato="Sweet potato",
  oil_fish_liver="Fish-liver oil",
  offals="Offal",
  eggs="Eggs",
  orange="Orange",
  plantains="Plantains"
)

input_files <- c(
  original=file.path(base_dir, "00Nutrient_Ori.xlsx"),
  optimized=file.path(base_dir, "00Nutrient_New.xlsx"),
  rda=file.path(base_dir, "RDA.xlsx"),
  population=file.path(base_dir, "population_long.xlsx"),
  income=file.path(base_dir, "Income-level.xlsx")
)

if (!all(file.exists(input_files))) {
  stop(
    "Missing input file(s): ",
    paste(names(input_files)[!file.exists(input_files)], collapse=", ")
  )
}

nutrient_key <- function(x) {
  gsub("[^a-z0-9]", "", tolower(x))
}

# ---------------------------------------------------------------------------
# 2. Read lookup tables
# ---------------------------------------------------------------------------

population <- read_excel(input_files["population"]) |>
  pivot_longer(
    -c(ISO3, Sex),
    names_to="Age_group",
    values_to="Population"
  ) |>
  filter(Age_group %in% ages) |>
  mutate(Population=as.numeric(Population))

income <- read_excel(input_files["income"]) |>
  transmute(
    ISO3,
    Region=recode(
      `Income-level`,
      "High income"="High",
      "Upper-middle income"="Upper-middle",
      "Lower middle income"="Lower-middle",
      "Low income"="Low"
    )
  )

rda <- read_excel(input_files["rda"]) |>
  pivot_longer(
    -c(Nutrient, Sex),
    names_to="Age_group",
    values_to="RDA"
  ) |>
  filter(Age_group %in% ages) |>
  mutate(
    Nutrient_key=nutrient_key(Nutrient),
    RDA=as.numeric(RDA)
  ) |>
  select(Nutrient_key, Sex, Age_group, RDA)

stopifnot(
  !anyDuplicated(population[c("ISO3", "Sex", "Age_group")]),
  !anyDuplicated(income["ISO3"]),
  !anyDuplicated(rda[c("Nutrient_key", "Sex", "Age_group")]),
  !anyNA(population$Population),
  !anyNA(income$Region),
  !anyNA(rda$RDA),
  all(population$Population > 0),
  all(rda$RDA > 0)
)

rda_keys <- unique(rda$Nutrient_key)
nutrients_evaluated <- length(rda_keys)

# ---------------------------------------------------------------------------
# 3. Country-first nutrient adequacy and food attribution
# ---------------------------------------------------------------------------
#
# For country c, sex s, age a and nutrient n:
#
#   A_ori  = 100 × min(I_ori / RDA, 1)
#   A_new  = 100 × min(I_new / RDA, 1)
#   DeltaA = A_new - A_ori
#
# Income-group values are then calculated as:
#
#   weighted DeltaA = sum_c(Pop_c × DeltaA_c) / sum_c(Pop_c)
#
# This order preserves the nonlinear 100% cap at the country level.

process_age_sheet <- function(age) {
  original <- read_excel(
    input_files["original"],
    sheet=paste0("Ori_", age)
  )

  optimized <- read_excel(
    input_files["optimized"],
    sheet=paste0("New_", age)
  )

  stopifnot(
    identical(names(original), names(optimized)),
    identical(
      original[c("ISO3", "Sex", "Nutrient")],
      optimized[c("ISO3", "Sex", "Nutrient")]
    )
  )

  keep <- nutrient_key(original$Nutrient) %in% rda_keys
  original <- original[keep, ]
  optimized <- optimized[keep, ]

  food_columns <- setdiff(
    names(original),
    c("ISO3", "Sex", "Nutrient")
  )

  original_matrix <- as.matrix(original[food_columns])
  optimized_matrix <- as.matrix(optimized[food_columns])

  storage.mode(original_matrix) <- "double"
  storage.mode(optimized_matrix) <- "double"

  positive_delta <- pmax(
    optimized_matrix - original_matrix,
    0
  )

  negative_delta <- pmax(
    original_matrix - optimized_matrix,
    0
  )

  meta <- tibble(
    ISO3=original$ISO3,
    Sex=original$Sex,
    Age_group=age,
    Nutrient=original$Nutrient,
    Nutrient_key=nutrient_key(original$Nutrient)
  ) |>
    left_join(
      population |> filter(Age_group == age),
      by=c("ISO3", "Sex", "Age_group")
    ) |>
    left_join(income, by="ISO3") |>
    left_join(
      rda |> filter(Age_group == age),
      by=c("Nutrient_key", "Sex", "Age_group")
    )

  stopifnot(
    nrow(meta) == nrow(original_matrix),
    !anyNA(meta$Population),
    !anyNA(meta$Region),
    !anyNA(meta$RDA),
    all(is.finite(original_matrix)),
    all(is.finite(optimized_matrix)),
    all(original_matrix >= 0),
    all(optimized_matrix >= 0)
  )

  country_nutrient <- meta |>
    mutate(
      Original_intake=rowSums(original_matrix),
      Optimized_intake=rowSums(optimized_matrix),
      Original_adequacy_pct=
        100 * pmin(Original_intake / RDA, 1),
      Optimized_adequacy_pct=
        100 * pmin(Optimized_intake / RDA, 1),
      Adequacy_gain_pp=
        Optimized_adequacy_pct - Original_adequacy_pct
    )

  # Attribute each nutrient adequacy gain to foods in proportion to positive
  # food-specific increases in that nutrient. Food decreases are retained in
  # the source table for audit but are not used to allocate a positive gain.
  positive_total <- rowSums(positive_delta)

  stopifnot(
    !any(
      positive_total <= 0 &
        country_nutrient$Adequacy_gain_pp > 1e-10
    )
  )

  allocation_scale <- ifelse(
    positive_total > 0,
    country_nutrient$Adequacy_gain_pp /
      nutrients_evaluated /
      positive_total,
    0
  )

  food_contribution_matrix <-
    positive_delta * allocation_scale

  food_values <- cbind(
    setNames(
      as.data.frame(positive_delta),
      paste0("Positive__", food_columns)
    ),
    setNames(
      as.data.frame(negative_delta),
      paste0("Negative__", food_columns)
    ),
    setNames(
      as.data.frame(food_contribution_matrix),
      paste0("Contribution__", food_columns)
    )
  )

  country_food <- bind_cols(
    meta |>
      select(
        ISO3, Region, Sex, Age_group,
        Nutrient, Nutrient_key, Population
      ),
    as_tibble(food_values)
  ) |>
    pivot_longer(
      cols=matches("^(Positive|Negative|Contribution)__"),
      names_to=c(".value", "Food"),
      names_sep="__"
    ) |>
    rename(
      Positive_nutrient_increase=Positive,
      Negative_nutrient_decrease=Negative,
      Food_contribution_to_mean_pp=Contribution
    )

  list(
    country_nutrient=country_nutrient,
    country_food=country_food
  )
}

age_results <- map(ages, process_age_sheet)

country_nutrient <- bind_rows(
  map(age_results, "country_nutrient")
)

country_food <- bind_rows(
  map(age_results, "country_food")
)

# ---------------------------------------------------------------------------
# 4. Population-weighted aggregation
# ---------------------------------------------------------------------------

regional_nutrient <- bind_rows(
  country_nutrient,
  country_nutrient |> mutate(Region="World")
) |>
  group_by(
    Region, Sex, Age_group,
    Nutrient, Nutrient_key
  ) |>
  summarise(
    Original_intake=
      weighted.mean(Original_intake, Population),
    Optimized_intake=
      weighted.mean(Optimized_intake, Population),
    RDA=first(RDA),
    Original_adequacy_pct=
      weighted.mean(Original_adequacy_pct, Population),
    Optimized_adequacy_pct=
      weighted.mean(Optimized_adequacy_pct, Population),
    Adequacy_gain_pp=
      weighted.mean(Adequacy_gain_pp, Population),
    Population=sum(Population),
    .groups="drop"
  )

regional_mean <- regional_nutrient |>
  group_by(Region, Sex, Age_group) |>
  summarise(
    Mean_original_adequacy_pct=
      mean(Original_adequacy_pct),
    Mean_optimized_adequacy_pct=
      mean(Optimized_adequacy_pct),
    Mean_adequacy_gain_pp=
      mean(Adequacy_gain_pp),
    Nutrients_evaluated=n(),
    .groups="drop"
  )

regional_nutrient <- regional_nutrient |>
  left_join(
    regional_mean,
    by=c("Region", "Sex", "Age_group")
  ) |>
  group_by(Region, Sex, Age_group) |>
  mutate(
    Total_nutrient_gain_pp=sum(Adequacy_gain_pp),
    Contribution_to_mean_pp=
      Adequacy_gain_pp / Nutrients_evaluated,
    Contribution_share_pct=if_else(
      Total_nutrient_gain_pp > 1e-10,
      100 * Adequacy_gain_pp / Total_nutrient_gain_pp,
      0
    ),
    Nutrient_rank=min_rank(desc(Adequacy_gain_pp))
  ) |>
  ungroup()

regional_food <- bind_rows(
  country_food,
  country_food |> mutate(Region="World")
) |>
  group_by(
    Region, Sex, Age_group,
    Nutrient, Nutrient_key, Food
  ) |>
  summarise(
    Positive_nutrient_increase=
      weighted.mean(Positive_nutrient_increase, Population),
    Negative_nutrient_decrease=
      weighted.mean(Negative_nutrient_decrease, Population),
    Food_contribution_to_mean_pp=
      weighted.mean(Food_contribution_to_mean_pp, Population),
    Population=sum(Population),
    .groups="drop"
  )

# ---------------------------------------------------------------------------
# 5. Country-level dominance statistic
# ---------------------------------------------------------------------------

country_stratum_total <- country_nutrient |>
  group_by(
    ISO3, Region, Sex,
    Age_group, Population
  ) |>
  summarise(
    Total_adequacy_gain_pp=sum(Adequacy_gain_pp),
    .groups="drop"
  ) |>
  mutate(
    Eligible=Total_adequacy_gain_pp > 1e-8
  )

country_dominant <- country_nutrient |>
  inner_join(
    country_stratum_total |> filter(Eligible),
    by=c(
      "ISO3", "Region", "Sex",
      "Age_group", "Population"
    )
  ) |>
  group_by(
    ISO3, Region, Sex, Age_group,
    Population, Total_adequacy_gain_pp
  ) |>
  slice_max(
    Adequacy_gain_pp,
    n=1,
    with_ties=FALSE
  ) |>
  ungroup() |>
  transmute(
    ISO3, Region, Sex, Age_group, Population,
    Total_adequacy_gain_pp,
    Dominant_nutrient=Nutrient_key,
    Dominant_gain_pp=Adequacy_gain_pp,
    Dominant_share_pct=
      100 * Dominant_gain_pp /
      Total_adequacy_gain_pp
  )

calcium_country_n <- sum(
  country_dominant$Dominant_nutrient == "calcium"
)

eligible_country_n <- nrow(country_dominant)

calcium_country_pct <-
  100 * calcium_country_n / eligible_country_n

zero_gain_n <- sum(!country_stratum_total$Eligible)

# ---------------------------------------------------------------------------
# 6. Figure source data
# ---------------------------------------------------------------------------

panel_a <- regional_mean |>
  filter(Region %in% plot_regions) |>
  mutate(
    Age_group=factor(Age_group, levels=ages),
    Income_code=factor(
      unname(income_codes[Region]),
      levels=rev(unname(income_codes[plot_regions]))
    ),
    Sex_label=factor(
      unname(sex_labels[Sex]),
      levels=c("Female", "Male")
    ),
    Value_label=sprintf("%.1f", Mean_adequacy_gain_pp),
    Text_colour=if_else(
      Mean_adequacy_gain_pp >= 12,
      "#FFFFFF",
      "#21302B"
    )
  )

panel_b <- regional_nutrient |>
  filter(Region %in% plot_regions) |>
  group_by(Region, Sex, Age_group) |>
  slice_max(
    Contribution_share_pct,
    n=1,
    with_ties=FALSE
  ) |>
  ungroup() |>
  mutate(
    Age_group=factor(Age_group, levels=ages),
    Income_code=factor(
      unname(income_codes[Region]),
      levels=rev(unname(income_codes[plot_regions]))
    ),
    Sex_label=factor(
      unname(sex_labels[Sex]),
      levels=c("Female", "Male")
    ),
    Dominant_nutrient=factor(
      Nutrient_key,
      levels=names(nutrient_labels)
    )
  )

case_levels <- c(
  "W \u00b7 all ages and sexes\nCalcium",
  "H \u00b7 Female \u00b7 age 20\u201334\nIron",
  "H \u00b7 Male \u00b7 age 15+\nVitamin A",
  "LM \u00b7 Male \u00b7 age 65+\nVitamin A"
)

panel_c_all <- bind_rows(
  regional_food |>
    filter(
      Region == "World",
      Nutrient_key == "calcium"
    ) |>
    mutate(
      Case=case_levels[1],
      Target_nutrient="calcium"
    ),
  regional_food |>
    filter(
      Region == "High",
      Sex == "FML",
      Age_group %in% c("20-24", "25-29", "30-34"),
      Nutrient_key == "iron"
    ) |>
    mutate(
      Case=case_levels[2],
      Target_nutrient="iron"
    ),
  regional_food |>
    filter(
      Region == "High",
      Sex == "MLE",
      Age_group %in% ages[4:17],
      Nutrient_key == "vitamina"
    ) |>
    mutate(
      Case=case_levels[3],
      Target_nutrient="vitamina"
    ),
  regional_food |>
    filter(
      Region == "Lower-middle",
      Sex == "MLE",
      Age_group %in% ages[14:17],
      Nutrient_key == "vitamina"
    ) |>
    mutate(
      Case=case_levels[4],
      Target_nutrient="vitamina"
    )
) |>
  group_by(Case, Target_nutrient, Food) |>
  summarise(
    Food_contribution_to_mean_pp=
      weighted.mean(
        Food_contribution_to_mean_pp,
        Population
      ),
    .groups="drop"
  ) |>
  group_by(Case, Target_nutrient) |>
  mutate(
    Target_nutrient_gain_pp=
      sum(Food_contribution_to_mean_pp),
    Food_share_pct=
      100 * Food_contribution_to_mean_pp /
      Target_nutrient_gain_pp
  ) |>
  ungroup()

panel_c <- panel_c_all |>
  group_by(Case, Target_nutrient) |>
  slice_max(
    Food_share_pct,
    n=5,
    with_ties=FALSE
  ) |>
  ungroup() |>
  mutate(
    Case=factor(Case, levels=case_levels),
    Food_label=if_else(
      Food %in% names(food_labels),
      unname(food_labels[Food]),
      tools::toTitleCase(gsub("_", " ", Food))
    ),
    Share_label=paste0(round(Food_share_pct), "%"),
    Bar_colour=unname(
      nutrient_colours[Target_nutrient]
    )
  ) |>
  arrange(Case, Food_share_pct) |>
  mutate(
    Food_case=factor(
      paste(Case, Food_label, sep="__"),
      levels=unique(
        paste(Case, Food_label, sep="__")
      )
    )
  )

# ---------------------------------------------------------------------------
# 7. Data-integrity checks
# ---------------------------------------------------------------------------

nutrient_reconciliation <- regional_nutrient |>
  group_by(Region, Sex, Age_group) |>
  summarise(
    Contribution_sum_pp=
      sum(Contribution_to_mean_pp),
    Mean_gain_pp=
      first(Mean_adequacy_gain_pp),
    Share_sum_pct=
      sum(Contribution_share_pct),
    .groups="drop"
  )

food_reconciliation <- regional_food |>
  group_by(Region, Sex, Age_group) |>
  summarise(
    Food_contribution_sum_pp=
      sum(Food_contribution_to_mean_pp),
    .groups="drop"
  ) |>
  left_join(
    regional_mean |>
      select(
        Region, Sex, Age_group,
        Mean_adequacy_gain_pp
      ),
    by=c("Region", "Sex", "Age_group")
  )

stopifnot(
  n_distinct(country_nutrient$ISO3) == 163,
  nrow(country_stratum_total) ==
    163 * 2 * length(ages),
  nrow(panel_a) ==
    length(plot_regions) * 2 * length(ages),
  nrow(panel_b) ==
    length(plot_regions) * 2 * length(ages),
  all(country_nutrient$Adequacy_gain_pp >= -1e-9),
  max(abs(
    nutrient_reconciliation$Contribution_sum_pp -
      nutrient_reconciliation$Mean_gain_pp
  )) < 1e-8,
  max(abs(
    nutrient_reconciliation$Share_sum_pct - 100
  )) < 1e-7,
  max(abs(
    food_reconciliation$Food_contribution_sum_pp -
      food_reconciliation$Mean_adequacy_gain_pp
  )) < 1e-8
)

write_csv(
  panel_b |>
    transmute(
      Region, Sex, Age_group,
      Dominant_nutrient=Nutrient_key,
      Contribution_to_mean_pp,
      Contribution_share_pct
    ),
  file.path(output_dir, "Figure4a_source_data.csv")
)

write_csv(
  panel_c |>
    transmute(
      Case,
      Target_nutrient,
      Food,
      Food_label,
      Food_contribution_to_mean_pp,
      Food_share_pct
    ),
  file.path(output_dir, "Figure4b_source_data.csv")
)

write_csv(
  country_dominant,
  file.path(
    output_dir,
    "Figure4_country_level_dominant_nutrient.csv"
  )
)

# ---------------------------------------------------------------------------
# 8. Publication theme
# ---------------------------------------------------------------------------

theme_publication <- function(base_size=10) {
  theme_classic(
    base_size=base_size,
    base_family="Arial"
  ) +
    theme(
      axis.line=element_line(
        linewidth=.32,
        colour="#27312D"
      ),
      axis.ticks=element_line(
        linewidth=.30,
        colour="#27312D"
      ),
      axis.text=element_text(
        size=12,
        colour="#37423D"
      ),
      axis.title=element_text(
        size=12,
        colour="#25302B"
      ),
      legend.background=element_blank(),
      legend.key=element_blank(),
      legend.title=element_text(
        size=12,
        face="bold"
      ),
      legend.text=element_text(
        size=12
      ),
      strip.background=element_rect(
        fill="#EFF2F1",
        colour=NA
      ),
      strip.text=element_text(
        size=12,
        face="bold",
        colour="#25302B"
      ),
      plot.title=element_text(
        size=12,
        face="bold",
        colour="#18231F",
        margin=margin(b=1.5)
      ),
      plot.subtitle=element_text(
        size=10,
        colour="#5F6964",
        margin=margin(b=3.5)
      ),
      plot.tag=element_text(
        size=12,
        face="bold",
        colour="#111A17"
      ),
      plot.tag.position=c(0, 1),
      plot.margin=margin(3, 3, 3, 3),
      panel.grid=element_blank()
    )
}

# ---------------------------------------------------------------------------
# 9. Panel a: magnitude of improvement
# ---------------------------------------------------------------------------

p_a <- ggplot(
  panel_a,
  aes(
    x=Age_group,
    y=Income_code,
    fill=Mean_adequacy_gain_pp
  )
) +
  geom_tile(
    colour="#FAFBFA",
    linewidth=.18,
    width=.96,
    height=.88
  ) +
  geom_text(
    aes(
      label=Value_label,
      colour=Text_colour
    ),
    family="Arial",
    fontface="plain",
    size=3.52,
    show.legend=FALSE
  ) +
  facet_grid(
    Sex_label ~ .
  ) +
  scale_colour_identity() +
  scale_fill_gradientn(
    colours=c(
      "#F4F7F6",
      "#DCE9E7",
      "#B5D1CE",
      "#73AAA8",
      "#2A7277"
    ),
    limits=c(0, 25),
    oob=scales::squish,
    guide="none"
  ) +
  scale_x_discrete(
    drop=FALSE,
    expand=expansion(add=.02),
    guide="none"
  ) +
  scale_y_discrete(
    position="left",
    drop=FALSE,
    expand=expansion(add=.05)
  ) +
  labs(
    title=NULL,
    subtitle=NULL,
    x=NULL,
    y=NULL
  ) +
  theme_publication() +
  theme(
    panel.border=element_rect(
      fill=NA,
      colour="#7F8984",
      linewidth=.38
    ),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    axis.text.x=element_text(
      size=12,
      hjust=0
    ),
    axis.text.y=element_text(
      size=12,
      margin=margin(l=3)
    ),
    strip.background=element_blank(),
    strip.placement="outside",
    strip.text.y.right=element_text(
      size=12,
      face="bold",
      angle=90,
      hjust=.5,
      vjust=.5,
      margin=margin(l=2)
    ),
    panel.spacing.y=unit(1.2, "mm")
  )

# ---------------------------------------------------------------------------
# 10. Panel b: identity and contribution of the dominant nutrient
# ---------------------------------------------------------------------------

p_b <- ggplot(
  panel_b,
  aes(
    x=Age_group,
    y=Income_code
  )
) +
  geom_tile(
    width=.96,
    height=.88,
    fill="#F7F8F7",
    colour="#FBFCFB",
    linewidth=.18
  ) +
  geom_point(
    aes(
      size=Contribution_share_pct,
      fill=Dominant_nutrient
    ),
    shape=21,
    stroke=.30,
    colour="#FFFFFF"
  ) +
  facet_grid(
    Sex_label ~ .
  ) +
  scale_fill_manual(
    values=nutrient_colours,
    breaks=names(nutrient_labels),
    labels=unname(nutrient_labels),
    name="Nutrient",
    drop=FALSE
  ) +
  scale_size_continuous(
    range=c(2.0, 5.2),
    limits=c(15, 70),
    breaks=c(20, 40, 60),
    labels=function(x) paste0(x, "%"),
    name="Share"
  ) +
  scale_x_discrete(
    drop=FALSE,
    expand=expansion(add=.02),
    guide=guide_axis(angle=45)
  ) +
  scale_y_discrete(
    position="left",
    drop=FALSE,
    expand=expansion(add=.05)
  ) +
  guides(
    fill=guide_legend(
      order=1,
      nrow=1,
      title.position="left",
      title.hjust=.5,
      override.aes=list(size=4.2)
    ),
    size=guide_legend(
      order=2,
      nrow=1,
      title.position="left",
      title.hjust=.5,
      override.aes=list(
        shape=21,
        fill="#FFFFFF",
        colour="#27312D",
        stroke=.45
      )
    )
  ) +
  labs(
    title=NULL,
    subtitle=NULL,
    x="Age group (years)",
    y=NULL
  ) +
  theme_publication() +
  theme(
    panel.border=element_rect(
      fill=NA,
      colour="#7F8984",
      linewidth=.38
    ),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    axis.text.x=element_text(
      size=12,
      hjust=1
    ),
    axis.text.y=element_text(
      size=12,
      margin=margin(l=3)
    ),
    axis.title.x=element_text(
      size=12,
      margin=margin(t=3)
    ),
    strip.background=element_blank(),
    strip.placement="outside",
    strip.text.y.right=element_text(
      size=12,
      face="bold",
      angle=90,
      hjust=.5,
      vjust=.5,
      margin=margin(l=2)
    ),
    panel.spacing.y=unit(1.2, "mm"),
    legend.position="bottom",
    legend.box="horizontal",
    legend.box.just="center",
    legend.spacing.x=unit(4.0, "mm"),
    legend.key.width=unit(5.0, "mm"),
    legend.key.height=unit(4.8, "mm"),
    legend.margin=margin(t=1.0),
    legend.box.spacing=unit(.7, "mm")
  )

# ---------------------------------------------------------------------------
# 11. Panel c: food pathways
# ---------------------------------------------------------------------------

p_c <- ggplot(
  panel_c,
  aes(
    x=Food_share_pct,
    y=Food_case,
    fill=Bar_colour
  )
) +
  geom_col(
    width=.58,
    colour="#FFFFFF",
    linewidth=.18
  ) +
  geom_text(
    aes(
      x=Food_share_pct + .65,
      label=Share_label
    ),
    hjust=0,
    family="Arial",
    fontface="bold",
    size=3.52,
    colour="#26312C"
  ) +
  facet_wrap(
    ~Case,
    ncol=2,
    scales="free_y"
  ) +
  scale_fill_identity() +
  scale_x_continuous(
    breaks=seq(0, 40, 10),
    labels=function(x) paste0(x, "%"),
    limits=c(0, 48),
    expand=expansion(mult=c(0, 0))
  ) +
  scale_y_discrete(
    labels=function(x) sub("^.*__", "", x),
    expand=expansion(add=.40)
  ) +
  labs(
    title=NULL,
    subtitle=NULL,
    x="Share of target nutrient gain",
    y=NULL
  ) +
  theme_publication() +
  theme(
    panel.grid.major.x=element_line(
      colour="#E7EBE9",
      linewidth=.24
    ),
    panel.border=element_rect(
      fill=NA,
      colour="#7F8984",
      linewidth=.38
    ),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    axis.text.x=element_text(
      size=12
    ),
    axis.text.y=element_text(
      size=12
    ),
    axis.title.x=element_text(
      size=12,
      margin=margin(t=3)
    ),
    strip.text=element_text(
      size=10,
      face="bold",
      hjust=0,
      lineheight=.95,
      colour="#2D3833",
      margin=margin(2.4, 2, 2.4, 0)
    ),
    strip.background=element_blank(),
    panel.spacing.x=unit(5, "mm"),
    panel.spacing.y=unit(3.5, "mm")
  )

# ---------------------------------------------------------------------------
# 12. Assemble and export
# ---------------------------------------------------------------------------

figure4 <- p_b / patchwork::free(p_c) +
  plot_layout(
    heights=c(1.05, 1.00)
  ) +
  plot_annotation(
    tag_levels="a"
  )

output_prefix <- file.path(
  output_dir,
  "Figure4_bc_submission"
)

width_mm = 183
height_mm = 190
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

svglite::svglite(
  paste0(output_prefix, ".svg"),
  width=width_in,
  height=height_in
)
print(figure4)
dev.off()

grDevices::cairo_pdf(
  paste0(output_prefix, ".pdf"),
  width=width_in,
  height=height_in,
  family="Arial"
)
print(figure4)
dev.off()

ragg::agg_tiff(
  paste0(output_prefix, ".tiff"),
  width=width_in,
  height=height_in,
  units="in",
  res=600,
  compression="lzw"
)
print(figure4)
dev.off()

ragg::agg_png(
  paste0(output_prefix, ".png"),
  width=width_in,
  height=height_in,
  units="in",
  res=300
)
print(figure4)
dev.off()

# ---------------------------------------------------------------------------
# 13. Machine-readable QA summary
# ---------------------------------------------------------------------------

qa_lines <- c(
  "Figure 4 submission QA",
  "======================",
  "",
  paste0("Countries: ", n_distinct(country_nutrient$ISO3)),
  paste0("Nutrients evaluated: ", nutrients_evaluated),
  paste0(
    "Country-sex-age strata: ",
    nrow(country_stratum_total)
  ),
  paste0(
    "Positive-gain strata used for dominant-nutrient frequency: ",
    eligible_country_n
  ),
  paste0(
    "Zero-gain strata excluded only from dominant-nutrient frequency: ",
    zero_gain_n
  ),
  paste0(
    "Calcium-dominant positive-gain strata: ",
    calcium_country_n,
    " (",
    sprintf("%.2f", calcium_country_pct),
    "%)"
  ),
  "",
  paste0(
    "Maximum nutrient reconciliation error: ",
    format(
      max(abs(
        nutrient_reconciliation$Contribution_sum_pp -
          nutrient_reconciliation$Mean_gain_pp
      )),
      scientific=TRUE
    )
  ),
  paste0(
    "Maximum food reconciliation error: ",
    format(
      max(abs(
        food_reconciliation$Food_contribution_sum_pp -
          food_reconciliation$Mean_adequacy_gain_pp
      )),
      scientific=TRUE
    )
  ),
  "",
  "Aggregation rule:",
  paste(
    "Adequacy was capped at 100% within each country-sex-age-nutrient",
    "record before population-weighted aggregation."
  ),
  "",
  "Food attribution rule:",
  paste(
    "Each positive nutrient adequacy gain was allocated across foods",
    "in proportion to their positive nutrient-intake increases."
  ),
  "",
  paste0("Final size: ", width_mm, " x ", height_mm, " mm"),
  "Typography: 12 pt axes and legends; all remaining text is 10-12 pt",
  "Income codes: W, world; H, high; UM, upper-middle; LM, lower-middle; L, low",
  "Vector outputs: SVG and PDF",
  "Raster outputs: 600-dpi TIFF and 300-dpi PNG"
)

writeLines(
  qa_lines,
  file.path(output_dir, "Figure4_bc_submission_QA.txt")
)

legend_text <- sprintf(
  paste0(
    "Fig. 4 | Demographic variation in the nutrient and food drivers of ",
    "improved nutritional adequacy. ",
    "a, Nutrient contributing the largest share of the total adequacy gain ",
    "in each displayed world/income-group\u2013sex\u2013age stratum; point ",
    "size represents its percentage contribution. W denotes world; H, high ",
    "income; UM, upper-middle income; LM, lower-middle income; and L, low ",
    "income. Among %s country\u2013sex\u2013age strata with ",
    "positive gains, calcium is dominant in %s (%.1f%%); %s zero-gain ",
    "strata are excluded only from the dominant-nutrient classification. ",
    "b, Top five food sources contributing to four prespecified nutrient-",
    "gain pathways. Food contributions are allocated in proportion to ",
    "positive food-specific increases in nutrient intake. All aggregated ",
    "estimates are population weighted."
  ),
  format(eligible_country_n, big.mark=","),
  format(calcium_country_n, big.mark=","),
  calcium_country_pct,
  format(zero_gain_n, big.mark=",")
)

writeLines(
  legend_text,
  file.path(output_dir, "Figure4_bc_submission_legend.txt")
)

message("Figure 4 b/c redesign completed.")
message("Output directory: ", output_dir)
message(
  sprintf(
    "Calcium dominant in %d/%d positive-gain country strata (%.2f%%).",
    calcium_country_n,
    eligible_country_n,
    calcium_country_pct
  )
)
