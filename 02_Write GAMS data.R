library(readxl)
library(dplyr)
library(stringr)
library(gdxrrw)

# Paths -------------------------------------------------------------------

igdx("/Library/Frameworks/GAMS.framework/Versions/49/Resources/")

project_dir <- path.expand(
  "~/Desktop/dengzc/2026/paper/national age specific/Code"
)
out_dir <- file.path(project_dir, "food_proxy_code_data_reps", "input_reps_full")
out_gdx <- file.path(out_dir, "all_raw_data.gdx")


# Helper functions --------------------------------------------------------

make_gams_name <- function(x, max_length = NULL) {
  name <- x |>
    as.character() |>
    str_replace_all("[^A-Za-z0-9_]", "_") |>
    str_replace("^[0-9]", "p_\\0")

  if (!is.null(max_length)) str_sub(name, 1, max_length) else name
}

get_value_col <- function(data) {
  candidates <- names(data)[
    tolower(names(data)) %in% c("value", "val", "values")
  ]

  if (length(candidates) > 0) candidates[1] else names(data)[ncol(data)]
}

make_set <- function(name, values) {
  values <- unique(as.character(values))
  values <- values[!is.na(values) & values != ""]

  val <- matrix(seq_along(values), ncol = 1)
  storage.mode(val) <- "double"

  list(
    name = name,
    type = "set",
    dim = 1,
    form = "sparse",
    uels = list(values),
    val = val
  )
}

clean_parameter <- function(data, name) {
  if (ncol(data) < 2) {
    warning("Skip ", name, ": less than 2 columns.")
    return(NULL)
  }

  value_col <- get_value_col(data)
  dimension_cols <- setdiff(names(data), value_col)

  data |>
    rename(Value = all_of(value_col)) |>
    mutate(
      across(all_of(dimension_cols), ~ str_trim(as.character(.x))),
      Value = as.numeric(Value)
    ) |>
    filter(
      !is.na(Value),
      if_all(all_of(dimension_cols), ~ !is.na(.x) & .x != "")
    )
}

make_parameter <- function(name, data) {
  dimension_cols <- setdiff(names(data), "Value")

  if (length(dimension_cols) == 0) {
    warning("Skip ", name, ": no dimension columns.")
    return(NULL)
  }

  data <- data |>
    group_by(across(all_of(dimension_cols))) |>
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

  uels <- setNames(
    lapply(dimension_cols, function(column) {
      unique(as.character(data[[column]]))
    }),
    dimension_cols
  )

  indexed_data <- data
  for (column in dimension_cols) {
    indexed_data[[column]] <- match(
      as.character(indexed_data[[column]]),
      uels[[column]]
    )
  }

  val <- indexed_data |>
    select(all_of(dimension_cols), Value) |>
    as.matrix()
  storage.mode(val) <- "double"

  list(
    name = make_gams_name(name, max_length = 60),
    type = "parameter",
    dim = length(dimension_cols),
    form = "sparse",
    uels = uels,
    domains = rep("*", length(dimension_cols)),
    val = val
  )
}


# Read Excel files --------------------------------------------------------

xlsx_files <- list.files(
  out_dir,
  pattern = "\\.xlsx$",
  full.names = TRUE
)
xlsx_files <- xlsx_files[
  !str_starts(basename(xlsx_files), fixed("~$"))
]

if (length(xlsx_files) == 0) {
  stop("No .xlsx files found in: ", out_dir)
}

data_list <- list()
for (file in xlsx_files) {
  parameter_name <- tools::file_path_sans_ext(basename(file)) |>
    make_gams_name()
  data_list[[parameter_name]] <- read_excel(file)
}

message("Read xlsx files:")
print(names(data_list))


# fg_SUA is the only set written to the GDX file -------------------------

if ("fg_SUA" %in% names(data_list)) {
  fg_SUA_values <- data_list[["fg_SUA"]][[1]] |>
    as.character() |>
    str_trim()
  data_list[["fg_SUA"]] <- NULL
} else {
  fg_SUA_values <- c("yoghurt", "cheese")
}


# Write every other Excel file as a parameter ----------------------------

clean_data <- Map(clean_parameter, data_list, names(data_list))
clean_data <- clean_data[!vapply(clean_data, is.null, logical(1))]

message("Cleaned data:")
print(lapply(clean_data, dim))

parameters <- Map(make_parameter, names(clean_data), clean_data)
parameters <- parameters[!vapply(parameters, is.null, logical(1))]

gdx_objects <- c(
  list(make_set("fg_SUA", fg_SUA_values)),
  unname(parameters)
)


# Write GDX ---------------------------------------------------------------

do.call(wgdx, c(list(out_gdx), gdx_objects))
message("GDX file has been created: ", out_gdx)
