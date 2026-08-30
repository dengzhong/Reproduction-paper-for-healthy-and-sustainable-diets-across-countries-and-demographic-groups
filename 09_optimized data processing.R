suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(writexl)
})

# ============================================================
# 1. Paths and constants
# ============================================================

data_dir <- "~/Desktop/dengzc/2026/paper/national age specific/Code/output data/optimized model output/"

coefficient_path <- file.path(data_dir, "00Coefficient.xlsx")
intake_fml_path <- file.path(data_dir, "00Intake_FML.xlsx")
intake_mle_path <- file.path(data_dir, "00Intake_MLE.xlsx")

age_groups <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65-69", "70-74", "75-79", "80+"
)

food_groups <- list(
  Seafood = c(
    "fish_freshw", "fish_demrs", "fish_pelag", "fish_marine",
    "crustaceans", "cephalopods", "othr_molluscs", "fish_aquatic",
    "fish_aquaticplant"
  ),
  `Dairy & eggs` = c("milk", "eggs"),
  Meat = c("beef", "lamb", "pork", "othr_meat", "offals", "poultry"),
  `Sugar & oil` = c(
    "sugar_cane", "sugar_non", "raw_sugar", "sweeteners", "honey",
    "oil_soyabeans", "oil_groundnut", "oil_sunflower", "oil_rape",
    "oil_cotton", "oil_palmkernel", "oil_palm", "oil_coconut",
    "oil_sesame", "oil_olive", "oil_ricebran", "oil_maize",
    "oil_oilcrop", "butter", "cream", "fat_ani", "oil_fish_body",
    "oil_fish_liver"
  ),
  `Staple foods` = c(
    "wheat", "rice", "barley", "maize", "rye", "oats", "millet",
    "sorghum", "othr_grains", "cassava", "potato", "sweet_potato",
    "yams", "othr_roots"
  ),
  `Legumes & nuts` = c(
    "beans", "peas", "othr_pulse", "soyabeans", "nuts", "groundnut",
    "seed_sunflower", "seed_rape", "seed_cotton", "seed_sesame",
    "seed_oilcrop"
  ),
  Fruits = c(
    "coconuts", "preserved_olives", "orange", "lemon", "grapefruit",
    "citrus", "banana", "plantains", "apple", "pineapple", "dates",
    "grapes", "othr_fruits"
  ),
  Vegetable = c("tomato", "onion", "othr_vegetables")
)

foodgroup_cols <- names(food_groups)
foodgroup_total_cols <- c(foodgroup_cols, "Total")

# ============================================================
# 2. Helpers
# ============================================================

replace_numeric_na <- function(data, id_cols) {
  data %>%
    mutate(
      across(all_of(id_cols), as.character),
      across(-all_of(id_cols), ~ replace_na(as.numeric(.x), 0))
    )
}

weighted_or_zero <- function(x, weight) {
  total_weight <- sum(weight, na.rm = TRUE)
  if (total_weight == 0) 0 else sum(x * weight, na.rm = TRUE) / total_weight
}

calculate_price <- function(intake_list, waste, price) {
  result_list <- intake_list

  for (sheet in names(intake_list)) {
    intake_data <- intake_list[[sheet]]
    food_cols <- Reduce(
      intersect,
      list(names(intake_data)[-c(1, 2)], names(waste)[-1], names(price)[-1])
    )

    intake_matrix <- as.matrix(intake_data[food_cols])
    waste_matrix <- as.matrix(
      waste[match(intake_data$ISO3, waste$ISO3), food_cols]
    )
    price_matrix <- as.matrix(
      price[match(intake_data$ISO3, price$ISO3), food_cols]
    )

    calculated <- ifelse(
      is.na(waste_matrix) | waste_matrix == 0 | is.na(price_matrix),
      0,
      intake_matrix / waste_matrix * price_matrix
    )
    colnames(calculated) <- food_cols
    result_list[[sheet]][food_cols] <- as.data.frame(calculated)
  }

  result_list
}

calculate_environment <- function(intake_list, waste, intensity) {
  result_list <- setNames(vector("list", length(intake_list)), names(intake_list))

  for (sheet in names(intake_list)) {
    intake_data <- intake_list[[sheet]]
    food_cols <- Reduce(
      intersect,
      list(
        names(intake_data)[-c(1, 2)],
        names(waste)[-1],
        names(intensity)[-1]
      )
    )

    intake_matrix <- as.matrix(intake_data[food_cols])
    waste_matrix <- as.matrix(
      waste[match(intake_data$ISO3, waste$ISO3), food_cols]
    )
    sheet_result <- vector("list", nrow(intensity))

    for (i in seq_len(nrow(intensity))) {
      intensity_matrix <- matrix(
        as.numeric(unlist(intensity[i, food_cols], use.names = FALSE)),
        nrow = nrow(intake_matrix),
        ncol = length(food_cols),
        byrow = TRUE
      )

      calculated <- ifelse(
        is.na(waste_matrix) | waste_matrix == 0 | is.na(intensity_matrix),
        0,
        intake_matrix / waste_matrix * intensity_matrix
      )
      colnames(calculated) <- food_cols

      item <- intake_data %>% select(ISO3, Sex)
      item[food_cols] <- as.data.frame(calculated)
      sheet_result[[i]] <- item %>%
        mutate(Environment = intensity$Environment[i], .after = Sex)
    }

    result_list[[sheet]] <- bind_rows(sheet_result)
  }

  result_list
}

calculate_total_environment <- function(environment_list, population_data, prefix) {
  result_list <- setNames(
    vector("list", length(environment_list)),
    names(environment_list)
  )

  for (sheet in names(environment_list)) {
    age_name <- sub(paste0("^", prefix, "_"), "", sheet)
    item <- environment_list[[sheet]] %>%
      mutate(Age = age_name, .after = ISO3) %>%
      left_join(population_data, by = c("ISO3", "Age", "Sex")) %>%
      mutate(Population = replace_na(Population, 0))

    food_cols <- setdiff(
      names(item),
      c("ISO3", "Age", "Sex", "Environment", "Population")
    )
    calculated <- sweep(
      as.matrix(item[food_cols]), 1, item$Population, `*`
    )
    colnames(calculated) <- food_cols
    item[food_cols] <- as.data.frame(calculated)

    result_list[[sheet]] <- item %>%
      select(ISO3, Age, Sex, Environment, Population, all_of(food_cols))
  }

  result_list
}

calculate_nutrients <- function(intake_list, nutrient_coefficients) {
  result_list <- setNames(vector("list", length(intake_list)), names(intake_list))

  for (sheet in names(intake_list)) {
    intake_data <- intake_list[[sheet]]
    sheet_result <- vector("list", length(nutrient_coefficients))

    for (i in seq_along(nutrient_coefficients)) {
      nutrient_name <- names(nutrient_coefficients)[i]
      nutrient_data <- nutrient_coefficients[[i]]
      food_cols <- intersect(names(intake_data)[-c(1, 2)], names(nutrient_data)[-1])

      nutrient_matrix <- as.matrix(
        nutrient_data[
          match(intake_data$ISO3, nutrient_data$ISO3),
          food_cols
        ]
      )
      calculated <- ifelse(
        is.na(nutrient_matrix),
        0,
        as.matrix(intake_data[food_cols]) * nutrient_matrix
      )
      colnames(calculated) <- food_cols

      item <- intake_data %>% select(ISO3, Sex)
      item[food_cols] <- as.data.frame(calculated)
      sheet_result[[i]] <- item %>%
        mutate(Nutrient = nutrient_name, .after = Sex)
    }

    result_list[[sheet]] <- bind_rows(sheet_result)
  }

  result_list
}

aggregate_food_groups <- function(data_list, id_cols) {
  result_list <- setNames(vector("list", length(data_list)), names(data_list))

  for (sheet in names(data_list)) {
    source <- data_list[[sheet]]
    item <- source %>% select(all_of(id_cols))

    for (group in foodgroup_cols) {
      food_cols <- intersect(food_groups[[group]], names(source))
      item[[group]] <- rowSums(source[food_cols], na.rm = TRUE)
    }

    item$Total <- rowSums(item[foodgroup_cols], na.rm = TRUE)
    result_list[[sheet]] <- item
  }

  result_list
}

add_price_per_capita <- function(price_list, population_data, prefix) {
  age_sheets <- intersect(paste0(prefix, "_", age_groups), names(price_list))
  all_ages <- setNames(vector("list", length(age_sheets)), age_sheets)

  for (sheet in age_sheets) {
    age_name <- sub(paste0("^", prefix, "_"), "", sheet)
    all_ages[[sheet]] <- price_list[[sheet]] %>%
      mutate(Age = age_name, .after = ISO3)
  }

  all_ages <- bind_rows(all_ages) %>%
    left_join(population_data, by = c("ISO3", "Age", "Sex")) %>%
    mutate(Population = replace_na(Population, 0))

  per_capita <- all_ages %>%
    group_by(ISO3) %>%
    summarise(
      across(
        all_of(foodgroup_total_cols),
        ~ weighted_or_zero(.x, Population)
      ),
      .groups = "drop"
    )

  price_list[[paste0(prefix, "_PerCapita")]] <- per_capita
  price_list
}

add_environment_total <- function(environment_list, prefix) {
  age_sheets <- intersect(paste0(prefix, "_", age_groups), names(environment_list))

  country_total <- bind_rows(environment_list[age_sheets]) %>%
    group_by(ISO3, Environment) %>%
    summarise(
      Population = sum(Population, na.rm = TRUE),
      across(all_of(foodgroup_total_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(Age = "All", Sex = "All", .after = ISO3) %>%
    select(
      ISO3, Age, Sex, Environment, Population,
      all_of(foodgroup_total_cols)
    )

  environment_list[[paste0(prefix, "_Total")]] <- country_total
  environment_list
}

add_nutrient_per_capita <- function(nutrient_list, population_data, prefix) {
  age_sheets <- intersect(paste0(prefix, "_", age_groups), names(nutrient_list))
  all_ages <- setNames(vector("list", length(age_sheets)), age_sheets)

  for (sheet in age_sheets) {
    age_name <- sub(paste0("^", prefix, "_"), "", sheet)
    all_ages[[sheet]] <- nutrient_list[[sheet]] %>%
      mutate(Age = age_name, .after = ISO3) %>%
      left_join(population_data, by = c("ISO3", "Age", "Sex")) %>%
      mutate(Population = replace_na(Population, 0))
  }

  per_capita <- bind_rows(all_ages) %>%
    group_by(ISO3, Sex, Nutrient) %>%
    summarise(
      across(
        all_of(foodgroup_total_cols),
        ~ weighted_or_zero(.x, Population)
      ),
      .groups = "drop"
    )

  nutrient_list[[paste0(prefix, "_PerCapita")]] <- per_capita
  nutrient_list
}

aggregate_price_region <- function(
  price_list,
  population_age_sex,
  country_population,
  iso_groups_long,
  prefix
) {
  age_sheets <- intersect(paste0(prefix, "_", age_groups), names(price_list))
  result_list <- setNames(
    vector("list", length(age_sheets) + 1),
    c(age_sheets, paste0(prefix, "_PerCapita"))
  )

  for (sheet in age_sheets) {
    age_name <- sub(paste0("^", prefix, "_"), "", sheet)
    region_data <- price_list[[sheet]] %>%
      mutate(Age = age_name, .after = ISO3) %>%
      left_join(population_age_sex, by = c("ISO3", "Age", "Sex")) %>%
      mutate(Population = replace_na(Population, 0)) %>%
      left_join(iso_groups_long, by = "ISO3", relationship = "many-to-many")

    result_list[[sheet]] <- region_data %>%
      group_by(Group, Sex) %>%
      summarise(
        across(
          all_of(foodgroup_total_cols),
          ~ weighted_or_zero(.x, Population)
        ),
        Population = sum(Population, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Age = age_name, .after = Group) %>%
      select(Group, Age, Sex, Population, all_of(foodgroup_total_cols))
  }

  result_list[[paste0(prefix, "_PerCapita")]] <-
    price_list[[paste0(prefix, "_PerCapita")]] %>%
    select(-any_of(c("Population", "Country_Population"))) %>%
    left_join(country_population, by = "ISO3") %>%
    mutate(Country_Population = replace_na(Country_Population, 0)) %>%
    left_join(iso_groups_long, by = "ISO3", relationship = "many-to-many") %>%
    group_by(Group) %>%
    summarise(
      across(
        all_of(foodgroup_total_cols),
        ~ weighted_or_zero(.x, Country_Population)
      ),
      Population = sum(Country_Population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(Group, Population, all_of(foodgroup_total_cols))

  result_list
}

aggregate_environment_region <- function(environment_list, iso_groups_long, prefix) {
  age_sheets <- intersect(paste0(prefix, "_", age_groups), names(environment_list))
  result_list <- setNames(
    vector("list", length(age_sheets) + 1),
    c(age_sheets, paste0(prefix, "_Total"))
  )

  for (sheet in age_sheets) {
    result_list[[sheet]] <- environment_list[[sheet]] %>%
      left_join(iso_groups_long, by = "ISO3", relationship = "many-to-many") %>%
      group_by(Group, Age, Sex, Environment) %>%
      summarise(
        across(all_of(foodgroup_total_cols), ~ sum(.x, na.rm = TRUE)),
        Population = sum(Population, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      select(
        Group, Age, Sex, Environment, Population,
        all_of(foodgroup_total_cols)
      )
  }

  result_list[[paste0(prefix, "_Total")]] <-
    environment_list[[paste0(prefix, "_Total")]] %>%
    left_join(iso_groups_long, by = "ISO3", relationship = "many-to-many") %>%
    group_by(Group, Environment) %>%
    summarise(
      across(all_of(foodgroup_total_cols), ~ sum(.x, na.rm = TRUE)),
      Population = sum(Population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Age = "All", Sex = "All", .after = Group) %>%
    select(
      Group, Age, Sex, Environment, Population,
      all_of(foodgroup_total_cols)
    )

  result_list
}

aggregate_nutrient_region <- function(
  nutrient_list,
  population_age_sex,
  iso_groups_long,
  prefix
) {
  age_sheets <- intersect(paste0(prefix, "_", age_groups), names(nutrient_list))
  result_list <- setNames(
    vector("list", length(age_sheets) + 1),
    c(age_sheets, paste0(prefix, "_PerCapita"))
  )

  for (sheet in age_sheets) {
    age_name <- sub(paste0("^", prefix, "_"), "", sheet)
    region_data <- nutrient_list[[sheet]] %>%
      mutate(Age = age_name, .after = ISO3) %>%
      left_join(population_age_sex, by = c("ISO3", "Age", "Sex")) %>%
      mutate(Population = replace_na(Population, 0)) %>%
      left_join(iso_groups_long, by = "ISO3", relationship = "many-to-many")

    result_list[[sheet]] <- region_data %>%
      group_by(Group, Sex, Nutrient) %>%
      summarise(
        across(
          all_of(foodgroup_total_cols),
          ~ weighted_or_zero(.x, Population)
        ),
        Population = sum(Population, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Age = age_name, .after = Group) %>%
      select(
        Group, Age, Sex, Nutrient, Population,
        all_of(foodgroup_total_cols)
      )
  }

  country_population_sex <- population_age_sex %>%
    group_by(ISO3, Sex) %>%
    summarise(
      Population = sum(Population, na.rm = TRUE),
      .groups = "drop"
    )

  result_list[[paste0(prefix, "_PerCapita")]] <-
    nutrient_list[[paste0(prefix, "_PerCapita")]] %>%
    left_join(country_population_sex, by = c("ISO3", "Sex")) %>%
    mutate(Population = replace_na(Population, 0)) %>%
    left_join(iso_groups_long, by = "ISO3", relationship = "many-to-many") %>%
    group_by(Group, Sex, Nutrient) %>%
    summarise(
      across(
        all_of(foodgroup_total_cols),
        ~ weighted_or_zero(.x, Population)
      ),
      Population = sum(Population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Age = "All", .after = Group) %>%
    select(
      Group, Age, Sex, Nutrient, Population,
      all_of(foodgroup_total_cols)
    )

  result_list
}

# ============================================================
# 3. Read and clean source data
# ============================================================

Waste_Coefficient <- read_excel(
  coefficient_path, sheet = "Waste_Coefficient"
) %>%
  filter(ISO3 != "MUS")

Intensity_Coefficient <- read_excel(
  coefficient_path, sheet = "Intensity_Coefficient"
)

Price <- read_excel(coefficient_path, sheet = "Price") %>%
  filter(ISO3 != "MUS")

population <- read_excel(coefficient_path, sheet = "population") %>%
  filter(ISO3 != "MUS")


names(Waste_Coefficient)[1] <- "ISO3"
names(Price)[1] <- "ISO3"
names(Intensity_Coefficient)[1] <- "Environment"

Waste_Coefficient <- replace_numeric_na(Waste_Coefficient, "ISO3")
Price <- replace_numeric_na(Price, "ISO3")
Intensity_Coefficient <- replace_numeric_na(
  Intensity_Coefficient, "Environment"
)

population <- population %>%
  transmute(
    ISO3 = as.character(ISO3),
    Age = as.character(Age),
    Sex = as.character(sex),
    Population = replace_na(as.numeric(value), 0)
  )

# ============================================================
# 4. Intake
# ============================================================

sheet_names <- excel_sheets(intake_fml_path)

Intake <- setNames(
  lapply(sheet_names, function(sheet) {
    fml <- read_excel(intake_fml_path, sheet = sheet, .name_repair = "minimal")
    mle <- read_excel(intake_mle_path, sheet = sheet, .name_repair = "minimal")
    names(fml)[1] <- "ISO3"
    names(mle)[1] <- "ISO3"

    bind_rows(
      fml %>% mutate(Sex = "FML", .after = ISO3),
      mle %>% mutate(Sex = "MLE", .after = ISO3)
    ) %>%
      filter(ISO3 != "MUS") %>%
      replace_numeric_na(c("ISO3", "Sex"))
  }),
  sheet_names
)

Intake_New <- Intake[grepl("^New_", names(Intake))]
Intake_Ori <- Intake[grepl("^Ori_", names(Intake))]

# ============================================================
# 5. Price and environment
# ============================================================

Price_New <- calculate_price(Intake_New, Waste_Coefficient, Price)
Price_Ori <- calculate_price(Intake_Ori, Waste_Coefficient, Price)

Envir_New <- calculate_environment(
  Intake_New, Waste_Coefficient, Intensity_Coefficient
)
Envir_Ori <- calculate_environment(
  Intake_Ori, Waste_Coefficient, Intensity_Coefficient
)

Envir_Total_New <- calculate_total_environment(
  Envir_New, population, "New"
)
Envir_Total_Ori <- calculate_total_environment(
  Envir_Ori, population, "Ori"
)

# ============================================================
# 6. Nutrients
# ============================================================

nutrient_files <- list.files('~/Desktop/dengzc/2026/paper/national age specific/Code/Input data/', pattern = "\\.csv$", full.names = TRUE)
nutrient_names <- tools::file_path_sans_ext(basename(nutrient_files))

Nutrient_Coefficient <- setNames(
  lapply(nutrient_files, function(file) {
    nutrient_data <- read.csv(
      file, check.names = FALSE, stringsAsFactors = FALSE
    )
    names(nutrient_data)[1] <- "ISO3"

    nutrient_data %>%
      filter(ISO3 != "MUS") %>%
      replace_numeric_na("ISO3")
  }),
  nutrient_names
)

Nutrient_New <- calculate_nutrients(Intake_New, Nutrient_Coefficient)
Nutrient_Ori <- calculate_nutrients(Intake_Ori, Nutrient_Coefficient)

# ============================================================
# 7. Food-group aggregation
# ============================================================

Price_New_FoodGroup <- aggregate_food_groups(Price_New, c("ISO3", "Sex"))
Price_Ori_FoodGroup <- aggregate_food_groups(Price_Ori, c("ISO3", "Sex"))

Price_New_FoodGroup <- add_price_per_capita(
  Price_New_FoodGroup, population, "New"
)
Price_Ori_FoodGroup <- add_price_per_capita(
  Price_Ori_FoodGroup, population, "Ori"
)

Envir_Total_New_FoodGroup <- aggregate_food_groups(
  Envir_Total_New,
  c("ISO3", "Age", "Sex", "Environment", "Population")
)
Envir_Total_Ori_FoodGroup <- aggregate_food_groups(
  Envir_Total_Ori,
  c("ISO3", "Age", "Sex", "Environment", "Population")
)

Envir_Total_New_FoodGroup <- add_environment_total(
  Envir_Total_New_FoodGroup, "New"
)
Envir_Total_Ori_FoodGroup <- add_environment_total(
  Envir_Total_Ori_FoodGroup, "Ori"
)

Nutrient_New_FoodGroup <- aggregate_food_groups(
  Nutrient_New, c("ISO3", "Sex", "Nutrient")
)
Nutrient_Ori_FoodGroup <- aggregate_food_groups(
  Nutrient_Ori, c("ISO3", "Sex", "Nutrient")
)

Nutrient_New_FoodGroup <- add_nutrient_per_capita(
  Nutrient_New_FoodGroup, population, "New"
)
Nutrient_Ori_FoodGroup <- add_nutrient_per_capita(
  Nutrient_Ori_FoodGroup, population, "Ori"
)

# ============================================================
# 8. ISO groups and regional aggregation
# ============================================================

iso <- unique(population$ISO3)

ISO_Groups <- read_excel(file.path('~/Desktop/dengzc/2026/paper/national age specific/Code/Input data/', "ISO groups.xlsx")) %>%
  filter(ISO3 %in% iso)

ISO_Groups_Long <- ISO_Groups %>%
  pivot_longer(cols = -ISO3, names_to = "Group", values_to = "Member") %>%
  mutate(
    ISO3 = as.character(ISO3),
    Member = toupper(as.character(Member))
  ) %>%
  filter(Member %in% c("Y", "YES", "TRUE", "1")) %>%
  select(ISO3, Group)

Population_AgeSex <- population %>%
  transmute(
    ISO3 = as.character(ISO3),
    Age = as.character(Age),
    Sex = as.character(Sex),
    Population = replace_na(as.numeric(Population), 0)
  )

Country_Population <- Population_AgeSex %>%
  group_by(ISO3) %>%
  summarise(
    Country_Population = sum(Population, na.rm = TRUE),
    .groups = "drop"
  )

Price_New_FoodGroup_region <- aggregate_price_region(
  Price_New_FoodGroup,
  Population_AgeSex,
  Country_Population,
  ISO_Groups_Long,
  "New"
)
Price_Ori_FoodGroup_region <- aggregate_price_region(
  Price_Ori_FoodGroup,
  Population_AgeSex,
  Country_Population,
  ISO_Groups_Long,
  "Ori"
)

Envir_Total_New_FoodGroup_region <- aggregate_environment_region(
  Envir_Total_New_FoodGroup, ISO_Groups_Long, "New"
)
Envir_Total_Ori_FoodGroup_region <- aggregate_environment_region(
  Envir_Total_Ori_FoodGroup, ISO_Groups_Long, "Ori"
)

Nutrient_New_FoodGroup_region <- aggregate_nutrient_region(
  Nutrient_New_FoodGroup, Population_AgeSex, ISO_Groups_Long, "New"
)
Nutrient_Ori_FoodGroup_region <- aggregate_nutrient_region(
  Nutrient_Ori_FoodGroup, Population_AgeSex, ISO_Groups_Long, "Ori"
)

#-----------fig1-----------
fig1_ori <- bind_rows(
  Nutrient_Ori_FoodGroup_region
) %>%
  filter(
    tolower(Nutrient) %in% c(
      "energy",
      "calorie",
      "calories"
    )
  ) %>%
  mutate(
    Scenario = "Current diets"
  )

fig1_new <- bind_rows(
  Nutrient_New_FoodGroup_region
) %>%
  filter(
    tolower(Nutrient) %in% c(
      "energy",
      "calorie",
      "calories"
    )
  ) %>%
  mutate(
    Scenario = "Optimized diets"
  )

fig1 <- bind_rows(
  fig1_ori,
  fig1_new
) %>%
  pivot_longer(
    cols = all_of(
      c(
        names(food_groups),
        "Total"
      )
    ),
    names_to = "FoodGroup",
    values_to = "Intake"
  ) %>%
  rename(
    region = Group
  ) %>%
  select(
    Scenario,
    Age,
    Sex,
    FoodGroup,
    region,
    Intake
  )


write_xlsx(fig1,file.path('~/Desktop/dengzc/2026/paper/national age specific/Code/output data/fig/fig1/', "00fig1.xlsx"))


#-----------fig2-----------
fig2_ori <- bind_rows(
  Price_Ori_FoodGroup_region
) %>%
  mutate(
    Scenario = "Current diets"
  )

fig2_new <- bind_rows(
  Price_New_FoodGroup_region
) %>%
  mutate(
    Scenario = "Optimized diets"
  )

fig2 <- bind_rows(
  fig2_ori,
  fig2_new
) %>%
  pivot_longer(
    cols = all_of(
      c(
        names(food_groups),
        "Total"
      )
    ),
    names_to = "FoodGroup",
    values_to = "Intake"
  ) %>%
  rename(
    region = Group
  ) %>%
  select(
    Scenario,
    Age,
    Sex,
    FoodGroup,
    region,
    Intake
  )


write_xlsx(fig2,file.path('~/Desktop/dengzc/2026/paper/national age specific/Code/output data/fig/fig2/', "00fig2.xlsx"))
write_xlsx(Price_New_FoodGroup,file.path('~/Desktop/dengzc/2026/paper/national age specific/Code/output data/fig/fig2/', "Price_New.xlsx"))
write_xlsx(Price_Ori_FoodGroup,file.path('~/Desktop/dengzc/2026/paper/national age specific/Code/output data/fig/fig2/', "Price_Ori.xlsx"))
#-----------fig3-----------
fig3_ori <- bind_rows(
  Envir_Total_Ori_FoodGroup_region
) %>%
  mutate(
    Scenario = "Current diets"
  )

fig3_new <- bind_rows(
  Envir_Total_New_FoodGroup_region
) %>%
  mutate(
    Scenario = "Optimized diets"
  )

fig3 <- bind_rows(
  fig3_ori,
  fig3_new
) %>%
  pivot_longer(
    cols = all_of(
      c(
        names(food_groups),
        "Total"
      )
    ),
    names_to = "FoodGroup",
    values_to = "Intake"
  ) %>%
  rename(
    region = Group
  ) %>%
  select(
    Scenario,
    Age,
    Sex,
    Environment,
    FoodGroup,
    region,
    Intake
  )


write_xlsx(fig3,file.path('~/Desktop/dengzc/2026/paper/national age specific/Code/output data/fig/fig3/', "00fig3.xlsx"))
write_xlsx(Envir_Total_New_FoodGroup,file.path('~/Desktop/dengzc/2026/paper/national age specific/Code/output data/fig/fig3/', "Envir_New.xlsx"))
write_xlsx(Envir_Total_Ori_FoodGroup,file.path('~/Desktop/dengzc/2026/paper/national age specific/Code/output data/fig/fig3/', "Envir_Ori.xlsx"))

#-----------fig4a-----------
fig4a_ori <- bind_rows(
  Nutrient_Ori,
  .id = "list_name"
) %>%
  mutate(
    Age = sub("^Ori_", "", list_name),
    Age = factor(
      Age,
      levels = sub("^Ori_", "", names(Nutrient_Ori))
    )
  ) %>%
  select(-list_name) %>%
  mutate(
    Value = rowSums(
      pick(-all_of(c("ISO3", "Sex", "Age", "Nutrient"))),
      na.rm = TRUE
    ),
    Scenario = "Current diets"
  ) %>%
  select(
    Scenario,
    ISO3,
    Sex,
    Age,
    Nutrient,
    Value
  )


fig4a_new <- bind_rows(
  Nutrient_New,
  .id = "list_name"
) %>%
  mutate(
    Age = sub("^New_", "", list_name),
    Age = factor(
      Age,
      levels = sub("^New_", "", names(Nutrient_New))
    )
  ) %>%
  select(-list_name) %>%
  mutate(
    Value = rowSums(
      pick(-all_of(c("ISO3", "Sex", "Age", "Nutrient"))),
      na.rm = TRUE
    ),
    Scenario = "Optimized diets"
  ) %>%
  select(
    Scenario,
    ISO3,
    Sex,
    Age,
    Nutrient,
    Value
  )

fig4a <- bind_rows(
  fig4a_ori,
  fig4a_new
)


write_xlsx(fig4a,file.path('~/Desktop/dengzc/2026/paper/national age specific/Code/output data/fig/fig4/', "00fig4.xlsx"))
write_xlsx(Nutrient_New,file.path('~/Desktop/dengzc/2026/paper/national age specific/Code/output data/fig/fig4/', "00Nutrient_New.xlsx"))
write_xlsx(Nutrient_Ori,file.path('~/Desktop/dengzc/2026/paper/national age specific/Code/output data/fig/fig4/', "00Nutrient_Ori.xlsx"))
#--------------fig S1--------------
figS1_ori <- bind_rows(
  Nutrient_Ori_FoodGroup,
  .id = "Age"
) %>%
  mutate(
    Age = sub("^[^_]+_", "", Age)
  ) %>%
  filter(
    tolower(Nutrient) %in% c(
      "energy",
      "calorie",
      "calories"
    )
  ) %>%
  mutate(
    Scenario = "Current diets"
  )

figS1_new <- bind_rows(
  Nutrient_New_FoodGroup,
  .id = "Age"
) %>%
  mutate(
    Age = sub("^[^_]+_", "", Age)
  ) %>%
  filter(
    tolower(Nutrient) %in% c(
      "energy",
      "calorie",
      "calories"
    )
  ) %>%
  mutate(
    Scenario = "Optimized diets"
  )


figS1 <- bind_rows(
  figS1_ori,
  figS1_new
) %>%
  pivot_longer(
    cols = all_of(
      c(
        names(food_groups),
        "Total"
      )
    ),
    names_to = "FoodGroup",
    values_to = "Intake"
  )  %>%
  select(
    Scenario,
    ISO3,
    Age,
    Sex,
    FoodGroup,
    Intake
  )

write_xlsx(figS1,'fig/figs/figs1/Intake_groups_all_demographic.xlsx')


#-------------------figs2-----------------
