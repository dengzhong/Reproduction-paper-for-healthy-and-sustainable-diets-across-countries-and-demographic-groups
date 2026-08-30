library(dplyr)
library(gdxrrw)
library(openxlsx)
library(purrr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

analysis_year <- 2020
gams_sysdir <- "/Library/Frameworks/GAMS.framework/Versions/49/Resources"
sets_file <- "~/Desktop/dengzc/2026/paper/national age specific/Code/food_proxy_code_data_reps/code_reps_full/prxy_sets_full.gms"

input_books <- c(
  FML = "~/Desktop/dengzc/2026/paper/national age specific/Code/food_proxy_code_data_reps/output_reps_full/FML_age_groups.xlsx",
  MLE = "~/Desktop/dengzc/2026/paper/national age specific/Code/food_proxy_code_data_reps/output_reps_full/MLE_age_groups.xlsx"
)

raw_input_gdx <- "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/food_proxy_code_data_reps/input_reps_full/input_reps_full.gdx"
raw_gdd_gdx <- "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/food_proxy_code_data_reps/input_reps_full/GDD_data_2020.gdx"
raw_iom_gdx <- "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/food_proxy_code_data_reps/input_reps_full/IOM_data_2020.gdx"
actual_output_gdx <- "/Users/zhongcideng/Desktop/dengzc/2026/paper/national age specific/Code/food_proxy_code_data_reps/output_reps_full/FBS_proxy_2020.gdx"

output_dir <- "~/Desktop/dengzc/2026/paper/national age specific/Code/Health model/"
dist_file <- file.path(output_dir, paste0("prcd_raw_distributions_", analysis_year, ".csv"))
ratio_file <- file.path(output_dir, paste0("prcd_raw_country_ratios_", analysis_year, ".csv"))
recon_file <- file.path(output_dir, paste0("prcd_reconstructed_from_raw_", analysis_year, ".csv"))
check_file <- file.path(output_dir, paste0("prcd_reconstruction_from_raw_check_", analysis_year, ".csv"))
summary_file <- file.path(output_dir, paste0("prcd_reconstruction_from_raw_summary_", analysis_year, ".csv"))

grain_foods <- c("wheat", "rice", "maize", "barley", "rye", "oats", "millet", "sorghum", "othr_grains")
red_meat_foods <- c("beef", "lamb", "pork")

age_map <- tibble::tribble(
  ~age, ~age_5y,
  "<1", "0-4",
  "1", "0-4",
  "2-5", "0-4",
  "6-10", "5-9",
  "11-14", "10-14",
  "15-19", "15-19",
  "20-24", "20-24",
  "25-29", "25-29",
  "30-34", "30-34",
  "35-39", "35-39",
  "40-44", "40-44",
  "45-49", "45-49",
  "50-54", "50-54",
  "55-59", "55-59",
  "60-64", "60-64",
  "65-69", "65-69",
  "70-74", "70-74",
  "75-79", "75-79",
  "80-84", "80+",
  "85-89", "80+",
  "90-94", "80+",
  "95+", "80+"
)

reconstruction_rules <- tibble::tribble(
  ~food_group,      ~source_group,    ~source_gdd_group, ~target_gdd_group,
  "whole_grains",   "total_grains",   "grains",          "whole_grains",
  "prc_grains",     "total_grains",   "grains",          "prc_grains",
  "red_meat",       "total_red_meat", "total_red_meat",  "red_meat",
  "prc_meat",       "total_red_meat", "total_red_meat",  "prc_meat",
  "yoghurt",        "milk",           "dairy",           "yoghurt",
  "cheese",         "milk",           "dairy",           "cheese",
  "milk_actl",      "milk",           "dairy",           "milk"
)

safe_weighted_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  sum(x[keep] * w[keep]) / sum(w[keep])
}

clean_label <- function(x) {
  str_replace_all(x, '^"|"$', "")
}

parse_map_fg_gdd <- function(path) {
  lines <- readLines(path, warn = FALSE)
  start_idx <- grep("^set\\s+map_fg_GDD\\(\\*,\\*\\)", lines)
  if (length(start_idx) == 0) {
    stop("Could not find map_fg_GDD in ", path)
  }

  block <- lines[start_idx:length(lines)]
  end_rel <- grep("^\\s*/;", block)
  if (length(end_rel) == 0) {
    stop("Could not find end of map_fg_GDD block in ", path)
  }

  block <- block[2:(end_rel[1] - 1)]
  block <- block[!grepl("^\\s*\\*", block)]
  block <- trimws(block)
  block <- block[nzchar(block)]

  parsed <- str_match(block, '^(".*?"|[A-Za-z0-9_]+)\\s+\\.\\s*(".*?"|[A-Za-z0-9_]+)$')
  parsed <- parsed[!is.na(parsed[, 1]), , drop = FALSE]

  tibble(
    food_group = clean_label(parsed[, 2]),
    gdd_group = clean_label(parsed[, 3])
  )
}

read_age_book <- function(path, sex_name) {
  map_dfr(excel_sheets(path), function(sheet_name) {
    read_excel(path, sheet = sheet_name) |>
      mutate(sheet_age = sheet_name)
  }) |>
    mutate(
      type = as.character(type),
      unit = as.character(unit),
      food_group = as.character(food_group),
      region = as.character(region),
      age = as.character(age),
      sex = sex_name,
      stats = as.character(stats),
      year = analysis_year,
      value = as.numeric(value)
    )
}

read_prim_input <- function(book_map) {
  imap_dfr(book_map, read_age_book) |>
    filter(
      type == "prim",
      unit == "g/d_w",
      stats == "mean",
      sex != "BTH",
      age != "all-a"
    ) |>
    select(type, unit, food_group, region, age, sex, year, stats, value)
}

read_actual_prcd <- function() {
  if (!file.exists(actual_output_gdx)) {
    return(NULL)
  }

  rgdx.param(
    gdxName = actual_output_gdx,
    symName = "FBS_intake_age_sex_agg",
    names = c("type", "unit", "food_group", "region", "age", "sex", "year", "stats"),
    compress = TRUE
  ) |>
    mutate(
      across(c(type, unit, food_group, region, age, sex, stats), as.character),
      year = as.integer(as.character(year))
    ) |>
    filter(
      type == "prcd",
      unit == "g/d_w",
      stats == "mean",
      sex %in% c("FML", "MLE"),
      age != "all-a",
      year == analysis_year,
      food_group %in% reconstruction_rules$food_group
    ) |>
    select(type, unit, food_group, region, age, sex, year, stats, value)
}

read_raw_inputs <- function() {
  igdx(gams_sysdir)

  list(
    fbs_intake = rgdx.param(
      raw_input_gdx,
      "FBS_intake_data",
      names = c("region", "food_group", "unit", "year"),
      compress = TRUE
    ) |>
      mutate(
        across(c(region, food_group, unit), as.character),
        year = as.integer(as.character(year))
      ),
    cns_sua = rgdx.param(
      raw_input_gdx,
      "cns_SUA",
      names = c("region", "food_group", "unit", "year"),
      compress = TRUE
    ) |>
      mutate(
        across(c(region, food_group, unit), as.character),
        year = as.integer(as.character(year))
      ),
    gdd_iom = rgdx.param(
      raw_gdd_gdx,
      "GDD_intake_IOM",
      names = c("region", "age", "sex", "urban", "edu", "year", "stats", "food_group"),
      compress = TRUE
    ) |>
      mutate(
        across(c(region, age, sex, urban, edu, stats, food_group), as.character),
        year = as.integer(as.character(year))
      ),
    gdd_ratio = rgdx.param(
      raw_gdd_gdx,
      "GDD_ratio_age_sex_urban_edu",
      names = c("region", "age", "sex", "urban", "edu", "year", "food_group"),
      compress = TRUE
    ) |>
      mutate(
        across(c(region, age, sex, urban, edu, food_group), as.character),
        year = as.integer(as.character(year))
      ),
    gdd_pop = rgdx.param(
      raw_gdd_gdx,
      "GDD_pop",
      names = c("region", "age", "sex", "urban", "edu", "year"),
      compress = TRUE
    ) |>
      mutate(
        across(c(region, age, sex, urban, edu), as.character),
        year = as.integer(as.character(year))
      ),
    eer = rgdx.param(
      raw_iom_gdx,
      "EER_GDD",
      names = c("region", "sex", "age", "urban", "year", "stats"),
      compress = TRUE
    ) |>
      mutate(
        across(c(region, sex, age, urban, stats), as.character),
        year = as.integer(as.character(year))
      )
  )
}

build_source_values <- function(prim_input) {
  bind_rows(
    prim_input |>
      filter(food_group %in% grain_foods) |>
      group_by(region, age, sex, year) |>
      summarise(source_value = sum(value), .groups = "drop") |>
      mutate(source_group = "total_grains"),
    prim_input |>
      filter(food_group %in% red_meat_foods) |>
      group_by(region, age, sex, year) |>
      summarise(source_value = sum(value), .groups = "drop") |>
      mutate(source_group = "total_red_meat"),
    prim_input |>
      filter(food_group == "milk") |>
      transmute(region, age, sex, year, source_value = value, source_group = "milk")
  )
}

build_country_ratios <- function(raw_inputs) {
  country_intake <- raw_inputs$fbs_intake |>
    filter(year == analysis_year, unit == "g/d_w")

  country_intake_kcal <- raw_inputs$fbs_intake |>
    filter(year == analysis_year, unit == "kcal/d_w")

  grain_source <- country_intake |>
    filter(food_group %in% grain_foods) |>
    group_by(region) |>
    summarise(source_total = sum(value), .groups = "drop")

  meat_source <- country_intake |>
    filter(food_group %in% red_meat_foods) |>
    group_by(region) |>
    summarise(source_total = sum(value), .groups = "drop")

  milk_source <- country_intake |>
    filter(food_group == "milk") |>
    transmute(region, source_total = value)

  gdd_ref <- raw_inputs$gdd_iom |>
    filter(
      year == analysis_year,
      age == "all-a",
      sex == "BTH",
      urban == "all-u",
      edu == "all-e",
      stats == "mean",
      food_group %in% c("grains", "whole_grains", "prc_grains", "total_red_meat", "red_meat", "prc_meat")
    ) |>
    select(region, food_group, value) |>
    pivot_wider(names_from = food_group, values_from = value)

  grain_ref <- grain_source |>
    left_join(gdd_ref, by = "region")

  grain_ratios <- bind_rows(
    grain_ref |>
      transmute(
        region,
        food_group = "whole_grains",
        source_group = "total_grains",
        country_ratio = if_else(grains > 0, whole_grains / grains, 0)
      ),
    grain_ref |>
      transmute(
        region,
        food_group = "prc_grains",
        source_group = "total_grains",
        country_ratio = if_else(grains > 0, prc_grains / grains, 0)
      )
  )

  meat_ref <- meat_source |>
    left_join(gdd_ref, by = "region")

  meat_ratios <- bind_rows(
    meat_ref |>
      transmute(
        region,
        food_group = "red_meat",
        source_group = "total_red_meat",
        country_ratio = if_else(total_red_meat > 0, red_meat / total_red_meat, 0)
      ),
    meat_ref |>
      transmute(
        region,
        food_group = "prc_meat",
        source_group = "total_red_meat",
        country_ratio = if_else(total_red_meat > 0, prc_meat / total_red_meat, 0)
      )
  )

  dairy_components <- milk_source |>
    left_join(
      raw_inputs$cns_sua |>
        filter(year == analysis_year, unit == "g/d_w", food_group %in% c("yoghurt", "cheese")) |>
        select(region, food_group, value) |>
        pivot_wider(names_from = food_group, values_from = value),
      by = "region"
    ) |>
    left_join(
      raw_inputs$cns_sua |>
        filter(year == analysis_year, unit == "kcal/d_w", food_group %in% c("yoghurt", "cheese")) |>
        select(region, food_group, value) |>
        pivot_wider(names_from = food_group, values_from = value, names_prefix = "kcal_"),
      by = "region"
    ) |>
    left_join(
      country_intake_kcal |>
        filter(food_group == "milk") |>
        transmute(region, milk_kcal = value),
      by = "region"
    ) |>
    mutate(
      yoghurt = coalesce(yoghurt, 0),
      cheese = coalesce(cheese, 0),
      kcal_yoghurt = coalesce(kcal_yoghurt, 0),
      kcal_cheese = coalesce(kcal_cheese, 0),
      milk_actl = if_else(
        milk_kcal > 0,
        (milk_kcal - kcal_yoghurt - kcal_cheese) * source_total / milk_kcal,
        0
      )
    )

  dairy_ratios <- bind_rows(
    dairy_components |>
      transmute(region, food_group = "yoghurt", source_group = "milk", country_ratio = if_else(source_total > 0, yoghurt / source_total, 0)),
    dairy_components |>
      transmute(region, food_group = "cheese", source_group = "milk", country_ratio = if_else(source_total > 0, cheese / source_total, 0)),
    dairy_components |>
      transmute(region, food_group = "milk_actl", source_group = "milk", country_ratio = if_else(source_total > 0, milk_actl / source_total, 0))
  )

  bind_rows(grain_ratios, meat_ratios, dairy_ratios) |>
    arrange(food_group, region)
}

build_exact_distributions <- function(raw_inputs, map_fg_gdd) {
  pop_detail <- raw_inputs$gdd_pop |>
    filter(
      year == analysis_year,
      age != "all-a",
      sex %in% c("FML", "MLE"),
      urban %in% c("rural", "urban"),
      edu %in% c("low", "medium", "high")
    ) |>
    transmute(region, age, sex, urban, edu, year, pop = value)

  eer_detail <- raw_inputs$eer |>
    filter(
      year == analysis_year,
      age != "all-a",
      sex %in% c("FML", "MLE"),
      urban %in% c("rural", "urban"),
      stats == "mean"
    ) |>
    transmute(region, age, sex, urban, year, eer_kcal = value)

  ratio_detail <- raw_inputs$gdd_ratio |>
    filter(
      year == analysis_year,
      age != "all-a",
      sex %in% c("FML", "MLE"),
      urban %in% c("rural", "urban"),
      edu %in% c("low", "medium", "high")
    ) |>
    transmute(region, age, sex, urban, edu, year, gdd_group = food_group, raw_ratio = value)

  country_kcal_base <- raw_inputs$fbs_intake |>
    filter(
      year == analysis_year,
      unit == "kcal/d_w",
      food_group != "all-fg"
    ) |>
    select(region, year, food_group, country_kcal = value) |>
    inner_join(map_fg_gdd, by = "food_group")

  all_fg_kcal_base <- country_kcal_base |>
    inner_join(
      ratio_detail,
      by = c("region", "year", "gdd_group"),
      relationship = "many-to-many"
    ) |>
    mutate(contribution = country_kcal * raw_ratio) |>
    group_by(region, age, sex, urban, edu, year) |>
    summarise(all_fg_kcal_base = sum(contribution), .groups = "drop")

  detail_base <- pop_detail |>
    left_join(eer_detail, by = c("region", "age", "sex", "urban", "year")) |>
    left_join(all_fg_kcal_base, by = c("region", "age", "sex", "urban", "edu", "year")) |>
    mutate(
      energy_factor = if_else(all_fg_kcal_base > 0, eer_kcal / all_fg_kcal_base, NA_real_)
    )

  relevant_gdd_groups <- unique(c(reconstruction_rules$source_gdd_group, reconstruction_rules$target_gdd_group))

  dist_age5 <- ratio_detail |>
    filter(gdd_group %in% relevant_gdd_groups) |>
    inner_join(detail_base, by = c("region", "age", "sex", "urban", "edu", "year")) |>
    mutate(signal = raw_ratio * energy_factor) |>
    group_by(region, gdd_group) |>
    mutate(country_signal = safe_weighted_mean(signal, pop)) |>
    ungroup() |>
    left_join(age_map, by = "age") |>
    mutate(dist_value = if_else(country_signal > 0, signal / country_signal, 0)) |>
    group_by(region, age_5y, sex, year, gdd_group) |>
    summarise(
      dist_value = safe_weighted_mean(dist_value, pop),
      .groups = "drop"
    ) |>
    rename(age = age_5y)

  pop_age5 <- detail_base |>
    left_join(age_map, by = "age") |>
    group_by(region, age_5y, sex, year) |>
    summarise(pop = sum(pop), .groups = "drop") |>
    rename(age = age_5y)

  list(dist_age5 = dist_age5, pop_age5 = pop_age5)
}

reconstruct_prcd_from_raw <- function(source_values, country_ratios, distributions) {
  source_dist <- distributions$dist_age5 |>
    rename(source_gdd_group = gdd_group, source_dist = dist_value)

  target_dist <- distributions$dist_age5 |>
    rename(target_gdd_group = gdd_group, target_dist = dist_value)

  prelim <- source_values |>
    inner_join(reconstruction_rules, by = "source_group", relationship = "many-to-many") |>
    left_join(country_ratios, by = c("region", "food_group", "source_group")) |>
    left_join(source_dist, by = c("region", "age", "sex", "year", "source_gdd_group")) |>
    left_join(target_dist, by = c("region", "age", "sex", "year", "target_gdd_group")) |>
    left_join(distributions$pop_age5, by = c("region", "age", "sex", "year")) |>
    mutate(
      source_dist = coalesce(source_dist, 0),
      target_dist = coalesce(target_dist, 0),
      country_ratio = coalesce(country_ratio, 0),
      pop = coalesce(pop, 0),
      prelim_value = if_else(source_dist > 0, source_value * country_ratio * target_dist / source_dist, 0)
    )

  scales <- prelim |>
    group_by(region, food_group) |>
    summarise(
      desired_mean = safe_weighted_mean(source_value * country_ratio, pop),
      prelim_mean = safe_weighted_mean(prelim_value, pop),
      scale = case_when(
        is.na(desired_mean) ~ 0,
        is.na(prelim_mean) ~ 0,
        prelim_mean == 0 & desired_mean == 0 ~ 0,
        prelim_mean == 0 ~ 0,
        TRUE ~ desired_mean / prelim_mean
      ),
      .groups = "drop"
    )

  prelim |>
    left_join(scales, by = c("region", "food_group")) |>
    mutate(
      value = prelim_value * scale,
      type = "prcd",
      unit = "g/d_w",
      stats = "mean"
    ) |>
    select(type, unit, food_group, region, age, sex, year, stats, value) |>
    arrange(sex, age, region, food_group)
}

compare_prcd <- function(reconstructed, actual_prcd) {
  actual_prcd |>
    rename(actual_value = value) |>
    full_join(
      reconstructed |> rename(reconstructed_value = value),
      by = c("type", "unit", "food_group", "region", "age", "sex", "year", "stats")
    ) |>
    mutate(
      reconstructed_value = coalesce(reconstructed_value, 0),
      actual_value = coalesce(actual_value, 0),
      diff = reconstructed_value - actual_value,
      abs_diff = abs(diff),
      rel_diff = if_else(actual_value != 0, diff / actual_value, NA_real_)
    )
}

write_age_workbooks <- function(data, prefix) {
  split(data, data$sex) |>
    iwalk(function(sex_data, sex_name) {
      wb <- createWorkbook()

      age_levels <- c(
        "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
        "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"
      )

      for (age_i in age_levels) {
        age_data <- sex_data |>
          filter(age == age_i) |>
          arrange(region, food_group)

        if (nrow(age_data) == 0) {
          next
        }

        sheet_name <- if (age_i == "80+") "80plus" else age_i
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet = sheet_name, age_data)
      }

      saveWorkbook(
        wb,
        file = file.path(output_dir, paste0(sex_name, "_", prefix, ".xlsx")),
        overwrite = TRUE
      )
    })
}

map_fg_gdd <- parse_map_fg_gdd(sets_file)
prim_input <- read_prim_input(input_books)
raw_inputs <- read_raw_inputs()
source_values <- build_source_values(prim_input)
country_ratios <- build_country_ratios(raw_inputs)
distributions <- build_exact_distributions(raw_inputs, map_fg_gdd)
reconstructed_prcd <- reconstruct_prcd_from_raw(source_values, country_ratios, distributions)

write_csv(distributions$dist_age5, dist_file)
write_csv(country_ratios, ratio_file)
write_csv(reconstructed_prcd, recon_file)
write_age_workbooks(reconstructed_prcd, "age_groups_prcd_from_raw")

cat("Saved age-sex distributions to:", dist_file, "\n")
cat("Saved country ratios to:", ratio_file, "\n")
cat("Saved reconstructed prcd data to:", recon_file, "\n")

actual_prcd <- read_actual_prcd()

if (!is.null(actual_prcd)) {
  comparison <- compare_prcd(reconstructed_prcd, actual_prcd)

  summary_tbl <- comparison |>
    group_by(food_group) |>
    summarise(
      n = n(),
      mae = mean(abs_diff, na.rm = TRUE),
      rmse = sqrt(mean(diff^2, na.rm = TRUE)),
      max_abs_diff = max(abs_diff, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(food_group)

  write_csv(comparison, check_file)
  write_csv(summary_tbl, summary_file)

  cat("Saved reconstruction check to:", check_file, "\n")
  cat("Saved reconstruction summary to:", summary_file, "\n")
  print(summary_tbl)
}
