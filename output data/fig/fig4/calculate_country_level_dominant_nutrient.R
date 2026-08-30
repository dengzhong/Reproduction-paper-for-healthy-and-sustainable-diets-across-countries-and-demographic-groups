library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)

script_arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)
base_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=","",script_arg[1])))
} else {
  getwd()
}

ages <- c(
  "0-4","5-9","10-14","15-19","20-24","25-29","30-34",
  "35-39","40-44","45-49","50-54","55-59","60-64",
  "65-69","70-74","75-79","80+"
)

nutrient_key <- function(x) {
  gsub("[^a-z0-9]","",tolower(x))
}

population <- read_excel(
  file.path(base_dir,"population_long.xlsx")
) |>
  pivot_longer(
    -c(ISO3,Sex),
    names_to="Age_group",
    values_to="Population"
  )

income <- read_excel(
  file.path(base_dir,"Income-level.xlsx")
) |>
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

rda <- read_excel(
  file.path(base_dir,"RDA.xlsx")
) |>
  pivot_longer(
    -c(Nutrient,Sex),
    names_to="Age_group",
    values_to="RDA"
  ) |>
  mutate(Nutrient_key=nutrient_key(Nutrient)) |>
  select(Nutrient_key,Sex,Age_group,RDA)

rda_keys <- unique(rda$Nutrient_key)

process_age <- function(age) {
  ori <- read_excel(
    file.path(base_dir,"00Nutrient_Ori.xlsx"),
    sheet=paste0("Ori_",age)
  )
  new <- read_excel(
    file.path(base_dir,"00Nutrient_New.xlsx"),
    sheet=paste0("New_",age)
  )

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
  foods <- setdiff(
    names(ori),
    c("ISO3","Sex","Nutrient")
  )

  tibble(
    ISO3=ori$ISO3,
    Sex=ori$Sex,
    Age_group=age,
    Nutrient=ori$Nutrient,
    Nutrient_key=nutrient_key(ori$Nutrient),
    Ori_intake=rowSums(ori[foods]),
    New_intake=rowSums(new[foods])
  ) |>
    left_join(
      population,
      by=c("ISO3","Sex","Age_group")
    ) |>
    left_join(income,by="ISO3") |>
    left_join(
      rda,
      by=c("Nutrient_key","Sex","Age_group")
    ) |>
    mutate(
      Ori_adequacy_pct=
        100*pmin(Ori_intake/RDA,1),
      New_adequacy_pct=
        100*pmin(New_intake/RDA,1),
      Adequacy_gain_pp=
        New_adequacy_pct-Ori_adequacy_pct
    )
}

country_nutrient <- map_dfr(
  ages,
  process_age
)

stratum_totals <- country_nutrient |>
  group_by(
    ISO3,Region,Sex,Age_group,Population
  ) |>
  summarise(
    Total_adequacy_gain_pp=
      sum(Adequacy_gain_pp,na.rm=TRUE),
    .groups="drop"
  ) |>
  mutate(
    Eligible=
      Total_adequacy_gain_pp>1e-8
  )

dominant_valid <- country_nutrient |>
  inner_join(
    stratum_totals |>
      filter(Eligible),
    by=c(
      "ISO3","Region","Sex",
      "Age_group","Population"
    )
  ) |>
  group_by(
    ISO3,Region,Sex,Age_group,
    Population,Total_adequacy_gain_pp,
    Eligible
  ) |>
  slice_max(
    Adequacy_gain_pp,
    n=1,
    with_ties=FALSE
  ) |>
  ungroup() |>
  transmute(
    ISO3,Region,Sex,Age_group,Population,
    Total_adequacy_gain_pp,Eligible,
    Dominant_nutrient=Nutrient_key,
    Dominant_gain_pp=Adequacy_gain_pp,
    Dominant_share_pct=
      100*Dominant_gain_pp/
      Total_adequacy_gain_pp,
    Calcium_dominant=
      Dominant_nutrient=="calcium"
  )

dominant_zero <- stratum_totals |>
  filter(!Eligible) |>
  transmute(
    ISO3,Region,Sex,Age_group,Population,
    Total_adequacy_gain_pp,Eligible,
    Dominant_nutrient=NA_character_,
    Dominant_gain_pp=NA_real_,
    Dominant_share_pct=NA_real_,
    Calcium_dominant=NA
  )

dominant_output <- bind_rows(
  dominant_valid,
  dominant_zero
) |>
  arrange(
    Region,ISO3,Sex,
    match(Age_group,ages)
  )

summary_rows <- bind_rows(
  dominant_valid |>
    summarise(
      Scope="Overall",
      Countries=n_distinct(ISO3),
      Eligible_strata=n(),
      Calcium_dominant_strata=
        sum(Calcium_dominant),
      Calcium_dominant_pct=
        100*mean(Calcium_dominant),
      Population_weighted_pct=
        100*weighted.mean(
          Calcium_dominant,
          Population
        )
    ),
  dominant_valid |>
    group_by(Region) |>
    summarise(
      Scope=first(Region),
      Countries=n_distinct(ISO3),
      Eligible_strata=n(),
      Calcium_dominant_strata=
        sum(Calcium_dominant),
      Calcium_dominant_pct=
        100*mean(Calcium_dominant),
      Population_weighted_pct=
        100*weighted.mean(
          Calcium_dominant,
          Population
        ),
      .groups="drop"
    ) |>
    select(-Region)
)

stopifnot(
  n_distinct(dominant_output$ISO3)==163,
  nrow(dominant_output)==
    163*2*length(ages),
  nrow(dominant_valid)==5395,
  nrow(dominant_zero)==147,
  sum(dominant_valid$Calcium_dominant)==3375,
  abs(
    mean(dominant_valid$Calcium_dominant)-
      3375/5395
  )<1e-12
)

detail_file <- file.path(
  base_dir,
  "country_level_dominant_nutrient.csv"
)
summary_file <- file.path(
  base_dir,
  "country_level_dominant_summary.csv"
)

write_csv(
  dominant_output,
  detail_file,
  na=""
)
write_csv(
  summary_rows,
  summary_file,
  na=""
)

message("Saved: ",detail_file)
message("Saved: ",summary_file)
message(
  "Country-level validation passed: ",
  "3375 of 5395 eligible country-sex-age ",
  "strata (62.56%) are calcium-dominant; ",
  "147 zero-gain strata are excluded."
)
