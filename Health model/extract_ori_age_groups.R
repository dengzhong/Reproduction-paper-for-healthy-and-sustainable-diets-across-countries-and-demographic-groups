library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

data_dir <- "/Users/zhongcideng/Desktop/FoodBalance2013-/optimized/optmized/output/Health model"

fml_path <- file.path(data_dir, "00Intake_FML.xlsx")
mle_path <- file.path(data_dir, "00Intake_MLE.xlsx")

fml_output <- file.path(data_dir, "00Ori_FML_age_groups.xlsx")
mle_output <- file.path(data_dir, "00Ori_MLE_age_groups.xlsx")

age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65-69", "70-74", "75-79", "80+"
)

# ============================================================
# Female: extract Ori sheets and convert to frame.xlsx format
# ============================================================

fml_result <- vector("list", length(age_levels))
names(fml_result) <- ifelse(age_levels == "80+", "80plus", age_levels)

for (i in seq_along(age_levels)) {
  age <- age_levels[i]
  sheet <- paste0("Ori_", age)

  x <- read_excel(
    fml_path,
    sheet = sheet,
    .name_repair = "minimal"
  )

  names(x)[1] <- "region"

  fml_result[[i]] <- x %>%
    pivot_longer(
      cols = -region,
      names_to = "food_group",
      values_to = "value"
    ) %>%
    mutate(
      value = replace_na(as.numeric(value), 0),
      type = "prim",
      unit = "g/d_w",
      age = age,
      sex = "FML",
      year = 1,
      stats = "mean"
    ) %>%
    select(
      type,
      unit,
      food_group,
      region,
      age,
      sex,
      year,
      stats,
      value
    )
}

write_xlsx(fml_result, fml_output)

# ============================================================
# Male: extract Ori sheets and convert to frame.xlsx format
# ============================================================

mle_result <- vector("list", length(age_levels))
names(mle_result) <- ifelse(age_levels == "80+", "80plus", age_levels)

for (i in seq_along(age_levels)) {
  age <- age_levels[i]
  sheet <- paste0("Ori_", age)

  x <- read_excel(
    mle_path,
    sheet = sheet,
    .name_repair = "minimal"
  )

  names(x)[1] <- "region"

  mle_result[[i]] <- x %>%
    pivot_longer(
      cols = -region,
      names_to = "food_group",
      values_to = "value"
    ) %>%
    mutate(
      value = replace_na(as.numeric(value), 0),
      type = "prim",
      unit = "g/d_w",
      age = age,
      sex = "MLE",
      year = 1,
      stats = "mean"
    ) %>%
    select(
      type,
      unit,
      food_group,
      region,
      age,
      sex,
      year,
      stats,
      value
    )
}

write_xlsx(mle_result, mle_output)
