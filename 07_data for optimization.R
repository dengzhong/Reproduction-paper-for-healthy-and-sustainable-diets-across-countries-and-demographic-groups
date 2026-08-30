library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(gdxrrw)

# Paths -------------------------------------------------------------------

project_dir <- path.expand(
  "~/Desktop/dengzc/2026/paper/national age specific/Code"
)
input_dir <- file.path(project_dir, "Input data")
output_dir <- file.path(project_dir, "output data")
gams_dir <- "/Library/Frameworks/GAMS.framework/Versions/49/Resources/"

igdx(gams_dir)

# Sets --------------------------------------------------------------------

age_set <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44", "45-49",
  "50-54", "55-59", "60-64", "65-69", "70-74",
  "75-79", "80+"
)
sex_set <- c("FML", "MLE")

# Helpers -----------------------------------------------------------------

wide_to_long <- function(data) {
  names(data)[1] <- "iso"

  data |>
    pivot_longer(
      cols = -iso,
      names_to = "Food",
      values_to = "Value"
    ) |>
    transmute(
      iso = as.character(iso),
      Food = as.character(Food),
      Value = as.numeric(Value)
    ) |>
    filter(!is.na(Value))
}

read_age_file <- function(file, sex) {
  missing_sheets <- setdiff(age_set, excel_sheets(file))

  if (length(missing_sheets) > 0) {
    stop(
      basename(file),
      " missing sheets: ",
      paste(missing_sheets, collapse = ", ")
    )
  }

  bind_rows(lapply(age_set, function(age) {
    read_excel(file, sheet = age) |>
      wide_to_long() |>
      mutate(age = age, sex = sex)
  }))
}

read_age_pair <- function(female_file, male_file) {
  bind_rows(
    read_age_file(female_file, "FML"),
    read_age_file(male_file, "MLE")
  )
}

make_set <- function(name, values) {
  value_matrix <- matrix(seq_along(values), ncol = 1)
  storage.mode(value_matrix) <- "double"

  list(
    name = name,
    type = "set",
    dim = 1,
    form = "sparse",
    uels = list(values),
    val = value_matrix
  )
}

make_parameter <- function(name, data, keys, sets, domains) {
  grouped_data <- data |>
    select(all_of(c(keys, "Value"))) |>
    group_by(across(all_of(keys))) |>
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

  index_columns <- Map(
    function(column, set_values) match(column, set_values),
    grouped_data[keys],
    sets
  )

  sparse_data <- as.data.frame(index_columns, check.names = FALSE)
  sparse_data$Value <- grouped_data$Value
  sparse_data <- sparse_data[complete.cases(sparse_data), , drop = FALSE]

  value_matrix <- as.matrix(sparse_data)
  storage.mode(value_matrix) <- "double"

  list(
    name = name,
    type = "parameter",
    dim = length(keys),
    form = "sparse",
    uels = sets,
    domains = domains,
    val = value_matrix
  )
}

find_column <- function(data, candidates, label) {
  column <- intersect(candidates, names(data))[1]

  if (is.na(column)) {
    stop("Missing ", label, " column")
  }

  column
}

parameter_name <- function(file) {
  name <- tools::file_path_sans_ext(basename(file)) |>
    str_replace_all("[^A-Za-z0-9_]", "_")

  str_replace(name, "^[0-9]", "p_\\0")
}

# Nutrient parameters -----------------------------------------------------

data_list <- list()

csv_files <- list.files(
  input_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

for (file in csv_files) {
  data_list[[parameter_name(file)]] <- read_csv(
    file,
    show_col_types = FALSE
  ) |>
    wide_to_long()
}

# Demand parameters -------------------------------------------------------

demand_original <- read_age_pair(
  file.path(output_dir, "FML_age_groups.xlsx"),
  file.path(output_dir, "MLE_age_groups.xlsx")
)

demand_lower <- read_age_pair(
  file.path(output_dir, "food_intake_lower_by_age_FML.xlsx"),
  file.path(output_dir, "food_intake_lower_by_age_MLE.xlsx")
)

demand_upper <- read_age_pair(
  file.path(output_dir, "food_intake_upper_by_age_FML.xlsx"),
  file.path(output_dir, "food_intake_upper_by_age_MLE.xlsx")
)

# Waste and price parameters ---------------------------------------------

data_list$waste <- read_excel(
  file.path(output_dir, "waste_factor.xlsx")
) |>
  wide_to_long()

data_list$Price <- read_excel(
  file.path(output_dir, "Price_FAO_fake.xlsx")
) |>
  wide_to_long()

# Intensity parameter -----------------------------------------------------

intensity_data <- read_excel(
  file.path(output_dir, "intensity data.xlsx"),
  sheet = 1
)
names(intensity_data)[1] <- "emi"

intensity_long <- intensity_data |>
  pivot_longer(
    cols = -emi,
    names_to = "Food",
    values_to = "Value"
  ) |>
  transmute(
    emi = as.character(emi),
    Food = as.character(Food),
    Value = as.numeric(Value)
  ) |>
  filter(!is.na(Value))

# Population parameter ----------------------------------------------------

population_data <- read_excel(
  file.path(output_dir, "population.xlsx"),
  sheet = 1
)
names(population_data) <- tolower(names(population_data))

iso_column <- find_column(population_data, c("iso3", "region"), "ISO3")
age_column <- find_column(population_data, "age", "age")
sex_column <- find_column(population_data, "sex", "sex")
value_column <- find_column(population_data, "value", "value")

population_long <- population_data |>
  transmute(
    iso = as.character(.data[[iso_column]]),
    age = as.character(.data[[age_column]]),
    sex = as.character(.data[[sex_column]]),
    Value = as.numeric(.data[[value_column]])
  ) |>
  filter(!is.na(Value))

# GDX objects -------------------------------------------------------------

i_set <- unique(population_long$iso)
j_set <- unique(intensity_long$Food)
emi_set <- unique(intensity_long$emi)

gdx_objects <- list(
  make_set("i", i_set),
  make_set("j", j_set),
  make_set("emi", emi_set),
  make_set("age", age_set),
  make_set("sex", sex_set)
)

for (name in names(data_list)) {
  gdx_objects <- append(
    gdx_objects,
    list(make_parameter(
      name,
      data_list[[name]],
      keys = c("iso", "Food"),
      sets = list(i_set, j_set),
      domains = c("i", "j")
    ))
  )
}

gdx_objects <- append(gdx_objects, list(
  make_parameter(
    "Demand_ori",
    demand_original,
    keys = c("iso", "Food", "age", "sex"),
    sets = list(i_set, j_set, age_set, sex_set),
    domains = c("i", "j", "age", "sex")
  ),
  make_parameter(
    "DemandLB",
    demand_lower,
    keys = c("iso", "Food", "age", "sex"),
    sets = list(i_set, j_set, age_set, sex_set),
    domains = c("i", "j", "age", "sex")
  ),
  make_parameter(
    "DemandUB",
    demand_upper,
    keys = c("iso", "Food", "age", "sex"),
    sets = list(i_set, j_set, age_set, sex_set),
    domains = c("i", "j", "age", "sex")
  ),
  make_parameter(
    "Intensity",
    intensity_long,
    keys = c("emi", "Food"),
    sets = list(emi_set, j_set),
    domains = c("emi", "j")
  ),
  make_parameter(
    "pop",
    population_long,
    keys = c("iso", "age", "sex"),
    sets = list(i_set, age_set, sex_set),
    domains = c("i", "age", "sex")
  )
))

# Output ------------------------------------------------------------------

gdx_file <- file.path(output_dir, "all_raw_data_newIntensity.gdx")

do.call(wgdx, c(list(gdx_file), gdx_objects))

message("GDX file has been created: ", gdx_file)
