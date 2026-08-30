required_packages <- c("readxl", "openxlsx", "dplyr", "tidyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0) sub("^--file=", "", script_arg[[1]]) else "calculate_fig4.R"
base_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
data_dir <- if (basename(base_dir) == "output new intensity") {
  base_dir
} else {
  file.path(base_dir, "output new intensity")
}
project_dir <- dirname(data_dir)
intake_file <- file.path(data_dir, "Nutrient intake.xlsx")
rda_file <- file.path(data_dir, "RDA.xlsx")
income_file <- file.path(project_dir, "output", "Income-level.xlsx")
output_file <- file.path(data_dir, "Fig4.xlsx")

nutrient_key <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("\\([^)]*\\)", "", x)
  x <- gsub("[^a-z0-9]+", "", x)
  x
}

age_key <- function(x) {
  trimws(gsub("years", "", as.character(x)))
}

write_sheet <- function(wb, sheet_name, data) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data, withFilter = TRUE)
  freezePane(wb, sheet_name, firstActiveRow = 2)

  header_style <- createStyle(
    fgFill = "#1F4E78",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE
  )

  addStyle(
    wb,
    sheet = sheet_name,
    style = header_style,
    rows = 1,
    cols = seq_along(data),
    gridExpand = TRUE
  )

  numeric_cols <- which(vapply(data, is.numeric, logical(1)))
  if (length(numeric_cols) > 0 && nrow(data) > 0) {
    number_style <- createStyle(numFmt = "0.000000")
    addStyle(
      wb,
      sheet = sheet_name,
      style = number_style,
      rows = 2:(nrow(data) + 1),
      cols = numeric_cols,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  setColWidths(wb, sheet = sheet_name, cols = seq_along(data), widths = "auto")
}

intake <- read_excel(intake_file) %>%
  mutate(
    `Age band` = age_key(`Age group`),
    `Nutrient key` = nutrient_key(Nutrient)
  )

rda_long <- read_excel(rda_file) %>%
  pivot_longer(
    cols = -Nutrient,
    names_to = "Age band",
    values_to = "RDA"
  ) %>%
  mutate(`Nutrient key` = nutrient_key(Nutrient)) %>%
  rename(`RDA nutrient label` = Nutrient)

income_level <- read_excel(income_file) %>%
  rename(
    Country = ISO3,
    `Income group` = `Income-level`
  ) %>%
  distinct(Country, `Income group`)

merged <- intake %>%
  left_join(rda_long, by = c("Nutrient key", "Age band"))

missing_rda <- merged %>%
  filter(is.na(RDA)) %>%
  distinct(Nutrient, `Age group`)

if (nrow(missing_rda) > 0) {
  print(missing_rda)
  stop("Some intake rows did not match RDA values.", call. = FALSE)
}

merged <- merged %>%
  mutate(
    `Adequacy ratio` = value / RDA,
    `Capped adequacy ratio` = pmin(`Adequacy ratio`, 1),
    `Meets RDA` = `Adequacy ratio` >= 1
  )

detail <- merged %>%
  transmute(
    Country,
    Scenario,
    `Age group`,
    Nutrient,
    Intake = value,
    RDA,
    `Adequacy ratio`,
    `Capped adequacy ratio`,
    `Meets RDA`,
    `RDA nutrient label`
  ) %>%
  arrange(Country, Scenario, `Age group`, Nutrient)

fig4a <- merged %>%
  group_by(Country, Scenario, `Age group`) %>%
  summarise(
    `Nutritional adequacy` = mean(`Capped adequacy ratio`, na.rm = TRUE),
    `Nutrients evaluated` = n(),
    .groups = "drop"
  ) %>%
  arrange(Country, Scenario, `Age group`)

fig4a_wide <- fig4a %>%
  select(Country, `Age group`, Scenario, `Nutritional adequacy`) %>%
  pivot_wider(names_from = Scenario, values_from = `Nutritional adequacy`) %>%
  mutate(
    `Nutritional adequacy change (Opt-Ori)` = Opt - Ori
  ) %>%
  arrange(Country, `Age group`)

fig4b <- merged %>%
  select(Country, Scenario, `Age group`, Nutrient, `Capped adequacy ratio`) %>%
  pivot_wider(names_from = Scenario, values_from = `Capped adequacy ratio`) %>%
  left_join(income_level, by = "Country") %>%
  group_by(Country, `Age group`) %>%
  mutate(
    `Nutrients evaluated` = n(),
    `Nutrient adequacy change (Opt-Ori)` = Opt - Ori,
    `Total nutrient adequacy change` =
      sum(`Nutrient adequacy change (Opt-Ori)`, na.rm = TRUE),
    `Contribution share of adequacy change` =
      if_else(
        `Total nutrient adequacy change` == 0,
        NA_real_,
        `Nutrient adequacy change (Opt-Ori)` / `Total nutrient adequacy change`
      )
  ) %>%
  ungroup() %>%
  relocate(`Income group`, .after = Country) %>%
  arrange(Country, `Age group`, Nutrient)

missing_income <- fig4b %>%
  filter(is.na(`Income group`)) %>%
  distinct(Country)

if (nrow(missing_income) > 0) {
  print(missing_income)
  stop("Some Fig4b countries did not match income groups.", call. = FALSE)
}

fig4b_summary_by_age_nutrient <- fig4b %>%
  group_by(`Age group`, Nutrient) %>%
  summarise(
    `Countries evaluated` = n_distinct(Country),
    `Mean nutrient adequacy change (Opt-Ori)` =
      mean(`Nutrient adequacy change (Opt-Ori)`, na.rm = TRUE),
    `Mean contribution share of adequacy change` =
      mean(`Contribution share of adequacy change`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(`Age group`, desc(`Mean contribution share of adequacy change`))

fig4b_summary_global <- fig4b %>%
  group_by(Nutrient) %>%
  summarise(
    `Country-age groups evaluated` = n(),
    `Countries evaluated` = n_distinct(Country),
    `Age groups evaluated` = n_distinct(`Age group`),
    `Mean nutrient adequacy change (Opt-Ori)` =
      mean(`Nutrient adequacy change (Opt-Ori)`, na.rm = TRUE),
    `Mean contribution share of adequacy change` =
      mean(`Contribution share of adequacy change`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(`Mean contribution share of adequacy change`))

fig4b_summary_by_income_age <- fig4b %>%
  group_by(`Income group`, `Age group`, Nutrient) %>%
  summarise(
    `Countries evaluated` = n_distinct(Country),
    `Mean nutrient adequacy change (Opt-Ori)` =
      mean(`Nutrient adequacy change (Opt-Ori)`, na.rm = TRUE),
    `Mean contribution share of adequacy change` =
      mean(`Contribution share of adequacy change`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(`Income group`, `Age group`, desc(`Mean contribution share of adequacy change`))

fig4b_summary_by_income_global <- fig4b %>%
  group_by(`Income group`, Nutrient) %>%
  summarise(
    `Country-age groups evaluated` = n(),
    `Countries evaluated` = n_distinct(Country),
    `Age groups evaluated` = n_distinct(`Age group`),
    `Mean nutrient adequacy change (Opt-Ori)` =
      mean(`Nutrient adequacy change (Opt-Ori)`, na.rm = TRUE),
    `Mean contribution share of adequacy change` =
      mean(`Contribution share of adequacy change`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(`Income group`, desc(`Mean contribution share of adequacy change`))

readme <- data.frame(
  Item = c(
    "Intake source",
    "RDA source",
    "Adequacy ratio",
    "Capped adequacy ratio",
    "Fig4a",
    "Fig4b nutrient change",
    "Fig4b contribution share",
    "Fig4b_summary_by_age",
    "Fig4b_summary_global",
    "Fig4b_summary_by_income_age",
    "Fig4b_summary_by_income_global",
    "Rows in intake",
    "Countries",
    "Scenarios",
    "Age groups",
    "Nutrients",
    "RDA match status"
  ),
  Value = c(
    intake_file,
    rda_file,
    "Intake / RDA",
    "min(Adequacy ratio, 1), used for summary scores",
    "Mean capped adequacy ratio across nutrients for each Country-Scenario-Age group",
    "Opt capped nutrient adequacy ratio - Ori capped nutrient adequacy ratio",
    "Nutrient adequacy change divided by total nutrient adequacy change within the same Country-Age group: (Opt - Ori) / sum(Opt - Ori)",
    "Summary by Age group and Nutrient",
    "Summary by Nutrient across all countries and age groups",
    "Summary by Income group, Age group, and Nutrient",
    "Summary by Income group and Nutrient across all age groups",
    nrow(intake),
    n_distinct(intake$Country),
    paste(sort(unique(intake$Scenario)), collapse = ", "),
    n_distinct(intake$`Age group`),
    n_distinct(intake$Nutrient),
    "All intake rows matched to RDA"
  )
)

round_numeric <- function(data) {
  data %>%
    mutate(across(where(is.numeric), ~ round(.x, 6)))
}

wb <- createWorkbook()
write_sheet(wb, "README", readme)
write_sheet(wb, "Fig4a", round_numeric(fig4a))
write_sheet(wb, "Fig4a_wide", round_numeric(fig4a_wide))
write_sheet(wb, "Fig4b", round_numeric(fig4b))
write_sheet(wb, "Fig4b_summary_by_age", round_numeric(fig4b_summary_by_age_nutrient))
write_sheet(wb, "Fig4b_summary_global", round_numeric(fig4b_summary_global))
write_sheet(wb, "Fig4b_summary_by_income_age", round_numeric(fig4b_summary_by_income_age))
write_sheet(wb, "Fig4b_summary_by_income_global", round_numeric(fig4b_summary_by_income_global))
write_sheet(wb, "Adequacy_detail", round_numeric(detail))

saveWorkbook(wb, output_file, overwrite = TRUE)
message("Saved: ", output_file)
