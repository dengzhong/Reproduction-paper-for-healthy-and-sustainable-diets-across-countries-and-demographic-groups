library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

script_arg <- grep("^--file=", commandArgs(FALSE), value=TRUE)
base_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  getwd()
}

ages <- c(
  "0-4","5-9","10-14","15-19","20-24","25-29","30-34",
  "35-39","40-44","45-49","50-54","55-59","60-64",
  "65-69","70-74","75-79","80+"
)
region_order <- c("World","High","Upper-middle","Lower-middle","Low")

files <- c(
  ori=file.path(base_dir,"00Nutrient_Ori.xlsx"),
  new=file.path(base_dir,"00Nutrient_New.xlsx"),
  rda=file.path(base_dir,"RDA.xlsx"),
  population=file.path(base_dir,"population_long.xlsx"),
  income=file.path(base_dir,"Income-level.xlsx")
)

nutrient_key <- function(x) gsub("[^a-z0-9]","",tolower(x))

population <- read_excel(files["population"]) |>
  pivot_longer(
    -c(ISO3,Sex),
    names_to="Age_group",
    values_to="Population"
  )

income <- read_excel(files["income"]) |>
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

rda <- read_excel(files["rda"]) |>
  pivot_longer(
    -c(Nutrient,Sex),
    names_to="Age_group",
    values_to="RDA"
  ) |>
  mutate(Nutrient_key=nutrient_key(Nutrient)) |>
  select(Nutrient_key,Sex,Age_group,RDA)

stopifnot(
  !anyDuplicated(population[c("ISO3","Sex","Age_group")]),
  !anyDuplicated(income["ISO3"]),
  !anyDuplicated(rda[c("Nutrient_key","Sex","Age_group")]),
  !anyNA(population$Population),
  !anyNA(income$Region),
  !anyNA(rda$RDA),
  all(population$Population>0),
  all(rda$RDA>0)
)

rda_keys <- unique(rda$Nutrient_key)
nutrients_evaluated <- length(rda_keys)

process_age <- function(age) {
  ori <- read_excel(files["ori"],sheet=paste0("Ori_",age))
  new <- read_excel(files["new"],sheet=paste0("New_",age))

  stopifnot(
    identical(names(ori),names(new)),
    identical(
      ori[c("ISO3","Sex","Nutrient")],
      new[c("ISO3","Sex","Nutrient")]
    )
  )

  keep <- nutrient_key(ori$Nutrient) %in% rda_keys
  ori <- ori[keep,]
  new <- new[keep,]
  foods <- setdiff(names(ori),c("ISO3","Sex","Nutrient"))

  ori_matrix <- as.matrix(ori[foods])
  new_matrix <- as.matrix(new[foods])
  delta_matrix <- new_matrix-ori_matrix
  positive_matrix <- pmax(delta_matrix,0)
  negative_matrix <- pmax(-delta_matrix,0)

  meta <- tibble(
    ISO3=ori$ISO3,
    Sex=ori$Sex,
    Nutrient=ori$Nutrient,
    Nutrient_key=nutrient_key(ori$Nutrient),
    Age_group=age
  ) |>
    left_join(
      population |> filter(Age_group==age),
      by=c("ISO3","Sex","Age_group")
    ) |>
    left_join(income,by="ISO3") |>
    left_join(
      rda |> filter(Age_group==age),
      by=c("Nutrient_key","Sex","Age_group")
    )

  stopifnot(
    nrow(meta)==nrow(ori_matrix),
    !anyNA(meta$Population),
    !anyNA(meta$Region),
    !anyNA(meta$RDA),
    all(is.finite(ori_matrix)),
    all(is.finite(new_matrix)),
    all(ori_matrix>=0),
    all(new_matrix>=0)
  )

  country_nutrient <- meta |>
    mutate(
      Ori_intake=rowSums(ori_matrix),
      New_intake=rowSums(new_matrix),
      Ori_adequacy_pct=100*pmin(Ori_intake/RDA,1),
      New_adequacy_pct=100*pmin(New_intake/RDA,1),
      Adequacy_change_pp=New_adequacy_pct-Ori_adequacy_pct
    )

  positive_total <- rowSums(positive_matrix)
  unallocated <- positive_total<=0 &
    country_nutrient$Adequacy_change_pp>1e-10
  stopifnot(!any(unallocated))

  contribution_scale <- ifelse(
    positive_total>0,
    country_nutrient$Adequacy_change_pp /
      nutrients_evaluated / positive_total,
    0
  )
  contribution_matrix <- positive_matrix*contribution_scale

  regional_nutrient <- bind_rows(
    country_nutrient,
    country_nutrient |> mutate(Region="World")
  ) |>
    group_by(Region,Sex,Age_group,Nutrient,Nutrient_key) |>
    summarise(
      Ori_intake=weighted.mean(Ori_intake,Population),
      New_intake=weighted.mean(New_intake,Population),
      RDA=first(RDA),
      Ori_adequacy_pct=weighted.mean(Ori_adequacy_pct,Population),
      New_adequacy_pct=weighted.mean(New_adequacy_pct,Population),
      Adequacy_change_pp=weighted.mean(Adequacy_change_pp,Population),
      Population=sum(Population),
      .groups="drop"
    )

  food_value_matrix <- cbind(
    setNames(
      as.data.frame(positive_matrix),
      paste0("Positive__",foods)
    ),
    setNames(
      as.data.frame(negative_matrix),
      paste0("Negative__",foods)
    ),
    setNames(
      as.data.frame(contribution_matrix),
      paste0("Contribution__",foods)
    )
  )
  food_value_columns <- names(food_value_matrix)

  country_food <- bind_cols(
    meta |>
      select(
        ISO3,Sex,Age_group,Nutrient,Nutrient_key,
        Population,Region
      ),
    as_tibble(food_value_matrix)
  )

  regional_food <- bind_rows(
    country_food,
    country_food |> mutate(Region="World")
  ) |>
    group_by(Region,Sex,Age_group,Nutrient,Nutrient_key) |>
    summarise(
      across(
        all_of(food_value_columns),
        ~weighted.mean(.x,Population)
      ),
      Population=sum(Population),
      .groups="drop"
    ) |>
    pivot_longer(
      cols=all_of(food_value_columns),
      names_to=c(".value","Food"),
      names_sep="__"
    ) |>
    rename(
      Positive_intake_increase=Positive,
      Negative_intake_decrease=Negative,
      Food_contribution_to_mean_pp=Contribution
    )

  list(nutrient=regional_nutrient,food=regional_food)
}

results <- map(ages,process_age)

nutrient_base <- bind_rows(map(results,"nutrient"))

mean_adequacy <- nutrient_base |>
  group_by(Region,Sex,Age_group) |>
  summarise(
    Mean_Ori_adequacy_pct=mean(Ori_adequacy_pct),
    Mean_New_adequacy_pct=mean(New_adequacy_pct),
    Mean_adequacy_change_pp=
      Mean_New_adequacy_pct-Mean_Ori_adequacy_pct,
    Nutrients_evaluated=n(),
    .groups="drop"
  )

nutrient_output <- nutrient_base |>
  left_join(
    mean_adequacy,
    by=c("Region","Sex","Age_group")
  ) |>
  group_by(Region,Sex,Age_group) |>
  mutate(
    Total_adequacy_change_pp=sum(Adequacy_change_pp),
    Nutrient_contribution_to_mean_pp=
      Adequacy_change_pp/Nutrients_evaluated,
    Nutrient_contribution_share_pct=if_else(
      Total_adequacy_change_pp>0,
      100*Adequacy_change_pp/Total_adequacy_change_pp,
      0
    ),
    Nutrient_rank=min_rank(desc(Adequacy_change_pp))
  ) |>
  ungroup() |>
  select(
    Region,Sex,Age_group,Nutrient,
    Ori_intake,New_intake,RDA,
    Ori_adequacy_pct,New_adequacy_pct,Adequacy_change_pp,
    Mean_Ori_adequacy_pct,Mean_New_adequacy_pct,
    Mean_adequacy_change_pp,Nutrients_evaluated,
    Nutrient_contribution_to_mean_pp,
    Nutrient_contribution_share_pct,Nutrient_rank,Population
  ) |>
  arrange(
    match(Region,region_order),
    Sex,match(Age_group,ages),
    Nutrient_rank,Nutrient
  )

food_output <- bind_rows(map(results,"food")) |>
  select(
    Region,Sex,Age_group,Nutrient,Food,
    Positive_intake_increase,Negative_intake_decrease,
    Food_contribution_to_mean_pp,Population
  ) |>
  arrange(
    match(Region,region_order),
    Sex,match(Age_group,ages),Nutrient,
    desc(Food_contribution_to_mean_pp),Food
  )

nutrient_check <- nutrient_output |>
  group_by(Region,Sex,Age_group) |>
  summarise(
    contribution_sum=sum(Nutrient_contribution_to_mean_pp),
    mean_gain=first(Mean_adequacy_change_pp),
    share_sum=sum(Nutrient_contribution_share_pct),
    .groups="drop"
  )

food_check <- food_output |>
  group_by(Region,Sex,Age_group) |>
  summarise(
    food_sum=sum(Food_contribution_to_mean_pp),
    .groups="drop"
  ) |>
  left_join(
    nutrient_output |>
      distinct(
        Region,Sex,Age_group,
        Mean_adequacy_change_pp
      ),
    by=c("Region","Sex","Age_group")
  )

stopifnot(
  nrow(nutrient_output)==
    length(region_order)*2*length(ages)*nutrients_evaluated,
  max(abs(
    nutrient_check$contribution_sum-
      nutrient_check$mean_gain
  ))<1e-9,
  max(abs(nutrient_check$share_sum-100))<1e-8,
  max(abs(
    food_check$food_sum-
      food_check$Mean_adequacy_change_pp
  ))<1e-8,
  all(nutrient_output$Adequacy_change_pp>=-1e-10),
  all(food_output$Food_contribution_to_mean_pp>=-1e-12)
)

nutrient_file <- file.path(
  base_dir,
  "country_first_nutrient_adequacy.csv"
)
food_file <- file.path(
  base_dir,
  "country_first_food_contributions.csv"
)

write_csv(nutrient_output,nutrient_file,na="")
write_csv(food_output,food_file,na="")

message("Saved: ",nutrient_file)
message("Saved: ",food_file)
message(
  "Validation passed: nutrient and food contributions both reconcile ",
  "to the mean adequacy gain."
)
