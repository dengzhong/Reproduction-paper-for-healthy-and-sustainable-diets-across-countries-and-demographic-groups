library(gdxrrw)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)

project_dir <- path.expand(
  "~/Desktop/dengzc/2026/paper/national age specific/Code"
)
gams_sysdir <- "/Library/Frameworks/GAMS.framework/Versions/49/Resources"
gdx_file <- file.path(
  project_dir,
  "food_proxy_code_data_reps",
  "output_reps_full",
  "FBS_proxy_2020.gdx"
)
out_dir <- file.path(project_dir, "output data")
country_file <- file.path(project_dir, "Input data", "Country.xlsx")
food_file <- file.path(project_dir, "Input data", "Food.xlsx")

igdx(gams_sysdir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_param <- function(symbol, dimensions) {
  rgdx.param(
    gdxName = gdx_file,
    symName = symbol,
    names = dimensions,
    compress = TRUE
  )
}

pop <- read_param(
  "pop",
  c("region", "age", "sex", "residence", "year")
)

fbs_intake <- read_param(
  "FBS_intake_age_sex_agg",
  c("type", "unit", "food_group", "region", "age", "sex", "year", "stats")
)

food_intake <- fbs_intake |>
  filter(
    sex != "BTH",
    age != "all-a",
    stats == "mean",
    unit == "g/d_w"
  )

age_order <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65-69", "70-74", "75-79", "80+"
)
age_order <- age_order[age_order %in% as.character(food_intake$age)]

country_order <- read_excel(country_file) |>
  pull(1) |>
  as.character()
country_order <- country_order[!is.na(country_order) & country_order != ""]

food_order <- read_excel(food_file) |>
  pull(1) |>
  as.character()
food_order <- food_order[!is.na(food_order) & food_order != ""]

food_intake <- food_intake |>
  mutate(
    across(c(type, unit, food_group, region, age, sex, stats), as.character),
    year = as.integer(year)
  ) |>
  filter(
    region %in% country_order,
    food_group %in% food_order
  )

country_order <- country_order[country_order %in% unique(food_intake$region)]
food_order <- food_order[food_order %in% unique(food_intake$food_group)]

population <- pop |>
  filter(
    region %in% country_order,
    sex != "BTH",
    age != "all-a",
    residence == "all-u"
  ) |>
  transmute(
    ISO3 = as.character(region),
    Age = factor(as.character(age), levels = age_order),
    Sex = as.character(sex),
    Value = round(value * 1000)
  ) |>
  arrange(ISO3, Age, Sex) |>
  mutate(Age = as.character(Age))

write.xlsx(
  population,
  file.path(out_dir, "population.xlsx"),
  overwrite = TRUE
)

write_sex_workbook <- function(data, sex_name) {
  sex_data <- data |>
    filter(sex == sex_name)

  available_ages <- age_order[age_order %in% unique(sex_data$age)]
  workbook <- createWorkbook()

  for (age_name in available_ages) {
    age_data <- sex_data |>
      filter(age == age_name) |>
      group_by(region, food_group) |>
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
      complete(
        region = country_order,
        food_group = food_order,
        fill = list(value = 0)
      ) |>
      mutate(
        region = factor(region, levels = country_order),
        food_group = factor(food_group, levels = food_order)
      ) |>
      arrange(region, food_group) |>
      pivot_wider(
        names_from = food_group,
        values_from = value,
        values_fill = 0,
        names_expand = TRUE
      ) |>
      rename(ISO3 = region) |>
      mutate(ISO3 = as.character(ISO3)) |>
      select(ISO3, all_of(food_order))

    addWorksheet(workbook, substr(age_name, 1, 31))
    writeData(workbook, age_name, age_data)
  }

  saveWorkbook(
    workbook,
    file.path(out_dir, paste0(sex_name, "_age_groups.xlsx")),
    overwrite = TRUE
  )
}

write_sex_workbook(food_intake, "FML")
write_sex_workbook(food_intake, "MLE")
