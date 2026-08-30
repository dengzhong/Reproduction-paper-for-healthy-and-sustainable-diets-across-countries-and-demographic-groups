library(dplyr)
library(openxlsx)
library(purrr)

input_dir <- "/Users/zhongcideng/Desktop/FoodBalance2013-/optimized/optmized/output/Health model"
output_dir <- file.path(input_dir, "merged_age_groups")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Ori combines the original female/male workbooks; Opt combines the New workbooks.
merge_jobs <- tibble::tribble(
  ~scenario, ~sex_code, ~prim_file,                 ~prcd_file,
  "Ori",     "FML",     "00Ori_FML_age_groups.xlsx", "00Ori_FML_age_groups_prcd.xlsx",
  "Ori",     "MLE",     "00Ori_MLE_age_groups.xlsx", "00Ori_MLE_age_groups_prcd.xlsx",
  "Opt",     "FML",     "00New_FML_age_groups.xlsx", "00New_FML_age_groups_prcd.xlsx",
  "Opt",     "MLE",     "00New_MLE_age_groups.xlsx", "00New_MLE_age_groups_prcd.xlsx"
)

output_names <- c(
  Ori = "00Intake_Ori.xlsx",
  Opt = "00Intake_Opt.xlsx"
)

standardize_sex_column <- function(data, sex_code) {
  # Replace an existing sex/Sex column with one consistent column named Sex.
  data <- data |> select(-any_of(c("sex", "Sex")))
  sex_position <- match("age", names(data)) + 1
  tibble::add_column(data, Sex = sex_code, .before = sex_position)
}

read_one_sheet <- function(prim_path, prcd_path, sheet_name, sex_code) {
  prim_data <- read.xlsx(prim_path, sheet = sheet_name, check.names = FALSE) |>
    standardize_sex_column(sex_code)
  prcd_data <- read.xlsx(prcd_path, sheet = sheet_name, check.names = FALSE) |>
    standardize_sex_column(sex_code)

  if (!identical(names(prim_data), names(prcd_data))) {
    stop("Column names do not match on sheet ", sheet_name, " for ", sex_code)
  }

  bind_rows(prim_data, prcd_data)
}

merge_one_scenario <- function(scenario_name, scenario_jobs, output_path) {
  paths <- scenario_jobs |>
    mutate(
      prim_path = file.path(input_dir, prim_file),
      prcd_path = file.path(input_dir, prcd_file)
    )

  missing_files <- c(paths$prim_path, paths$prcd_path)[
    !file.exists(c(paths$prim_path, paths$prcd_path))
  ]
  if (length(missing_files) > 0) {
    stop("Missing workbook(s): ", paste(missing_files, collapse = ", "))
  }

  sheet_lists <- map(c(paths$prim_path, paths$prcd_path), getSheetNames)
  if (!all(map_lgl(sheet_lists, identical, y = sheet_lists[[1]]))) {
    stop("Sheet names/order do not match for scenario ", scenario_name)
  }
  sheet_names <- sheet_lists[[1]]

  wb <- createWorkbook()
  header_style <- createStyle(
    fontColour = "#FFFFFF",
    fgFill = "#1F4E78",
    textDecoration = "bold",
    halign = "center",
    border = "Bottom",
    borderColour = "#A6A6A6"
  )

  checks <- map_dfr(sheet_names, function(sheet_name) {
    sex_data <- pmap(
      paths |> select(prim_path, prcd_path, sex_code),
      function(prim_path, prcd_path, sex_code) {
        read_one_sheet(prim_path, prcd_path, sheet_name, sex_code)
      }
    )
    # Row order: FML prim, FML prcd, MLE prim, MLE prcd.
    merged_data <- bind_rows(sex_data)

    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, merged_data, headerStyle = header_style, withFilter = TRUE)
    freezePane(wb, sheet_name, firstRow = TRUE)
    setColWidths(wb, sheet_name, cols = 1:ncol(merged_data), widths = "auto")
    setColWidths(wb, sheet_name, cols = which(names(merged_data) == "food_group"), widths = 18)

    expected_rows <- sum(map_int(sex_data, nrow))
    tibble(
      scenario = scenario_name,
      sheet = sheet_name,
      FML_rows = sum(merged_data$Sex == "FML"),
      MLE_rows = sum(merged_data$Sex == "MLE"),
      prim_rows = sum(merged_data$type == "prim"),
      prcd_rows = sum(merged_data$type == "prcd"),
      merged_rows = nrow(merged_data),
      check_ok = nrow(merged_data) == expected_rows &&
        all(c("FML", "MLE") %in% merged_data$Sex) &&
        all(c("prim", "prcd") %in% merged_data$type)
    )
  })

  if (!all(checks$check_ok)) stop("Validation failed for ", output_path)
  saveWorkbook(wb, output_path, overwrite = TRUE)
  checks
}

merge_report <- imap_dfr(output_names, function(output_file, scenario_name) {
  message("Creating ", output_file)
  merge_one_scenario(
    scenario_name,
    filter(merge_jobs, scenario == scenario_name),
    file.path(output_dir, output_file)
  )
})

write.csv(
  merge_report,
  file.path(output_dir, "merge_age_groups_check.csv"),
  row.names = FALSE
)

print(merge_report)
cat("Combined workbooks saved to:", output_dir, "\n")
