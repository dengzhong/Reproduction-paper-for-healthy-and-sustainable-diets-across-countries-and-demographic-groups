library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# Paths -------------------------------------------------------------------

project_dir <- path.expand(
  "~/Desktop/dengzc/2026/paper/national age specific/Code"
)
input_dir <- file.path(
  project_dir,
  "output data",
  "optimized model output"
)
output_dir <- file.path(project_dir, "Health model")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Age groups --------------------------------------------------------------

age_groups <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44", "45-49",
  "50-54", "55-59", "60-64", "65-69", "70-74",
  "75-79", "80+"
)

output_sheets <- ifelse(age_groups == "80+", "80plus", age_groups)

# Reconstruction ---------------------------------------------------------

reconstruct_sheet <- function(file, sheet, age, sex) {
  data <- read_excel(file, sheet = sheet)
  names(data)[1] <- "region"

  data |>
    pivot_longer(
      cols = -region,
      names_to = "food_group",
      values_to = "value"
    ) |>
    transmute(
      type = "prim",
      unit = "g/d_w",
      food_group = as.character(food_group),
      region = as.character(region),
      age = age,
      sex = sex,
      year = 1,
      stats = "mean",
      value = replace_na(as.numeric(value), 0)
    )
}

reconstruct_workbook <- function(file, prefix, sex) {
  source_sheets <- paste0(prefix, "_", age_groups)
  missing_sheets <- setdiff(source_sheets, excel_sheets(file))

  if (length(missing_sheets) > 0) {
    stop(
      basename(file),
      " missing sheets: ",
      paste(missing_sheets, collapse = ", ")
    )
  }

  result <- Map(
    function(sheet, age) reconstruct_sheet(file, sheet, age, sex),
    source_sheets,
    age_groups
  )
  names(result) <- output_sheets
  result
}

write_reconstructed_files <- function(sex) {
  input_file <- file.path(input_dir, paste0("00Intake_", sex, ".xlsx"))

  for (prefix in c("New", "Ori")) {
    output_file <- file.path(
      output_dir,
      paste0("00", prefix, "_", sex, "_age_groups.xlsx")
    )

    write_xlsx(
      reconstruct_workbook(input_file, prefix, sex),
      output_file,
      format_headers = TRUE
    )
  }
}

invisible(lapply(c("FML", "MLE"), write_reconstructed_files))
