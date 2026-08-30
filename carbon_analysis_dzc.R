rm(list = ls())
gc()
library(Matrix)

data_dir <- "D:/fabio_v2_beta/fabio_v2_beta"
output_dir <- data_dir
years <- 2014:2023
x_base_year <- 2010
tolerance <- 1e-8

Z <- readRDS(file.path(data_dir, "Z_mass.rds"))
Y <- readRDS(file.path(data_dir, "Y.rds"))
X <- as.numeric(readRDS(file.path(data_dir, "X.rds")))
E <- readRDS(file.path(data_dir, "E.rds"))
items <- read.csv(
  file.path(data_dir, "items.csv")
)

node_sets <- vector("list", length(years))

for (i in seq_along(years)) {
  year <- years[i]
  key <- as.character(year)

  if (is.null(Z[[key]]) || is.null(Y[[key]]) || is.null(E[[key]])) {
    stop("Missing data for year ", year)
  }
  if (!("gwp_total" %in% rownames(E[[key]]))) {
    stop("Missing gwp_total for year ", year)
  }

  node_sets[[i]] <- Reduce(
    intersect,
    list(
      rownames(Z[[key]]),
      colnames(Z[[key]]),
      rownames(Y[[key]]),
      colnames(E[[key]])
    )
  )
}

common_nodes <- Reduce(intersect, node_sets)
first_order <- rownames(Z[[as.character(years[1])]])
common_nodes <- first_order[first_order %in% common_nodes]

if (length(common_nodes) == 0) {
  stop("No common nodes across years.")
}

# Extract X and keep one fixed positive-output node set
n_all <- nrow(Z[[as.character(years[1])]])
x_by_year <- vector("list", length(years))
names(x_by_year) <- as.character(years)
valid_nodes <- rep(TRUE, length(common_nodes))

for (year in years) {
  key <- as.character(year)

  if (nrow(Z[[key]]) != n_all || ncol(Z[[key]]) != n_all) {
    stop("Z dimensions differ in year ", year)
  }

  block <- year - x_base_year
  first <- block * n_all + 1
  last <- (block + 1) * n_all

  if (block < 0 || last > length(X)) {
    stop("Invalid X index for year ", year)
  }

  x_year <- X[first:last]
  names(x_year) <- rownames(Z[[key]])
  x_by_year[[key]] <- x_year

  x_common <- x_year[common_nodes]
  valid_nodes <- valid_nodes & is.finite(x_common) & x_common > 0
}

nodes <- common_nodes[valid_nodes]

if (length(nodes) == 0) {
  stop("No positive-output nodes across all years.")
}
if (sum(!valid_nodes) > 0) {
  warning(sum(!valid_nodes), " non-positive-output nodes removed.")
}

# Group labels; aggregation is done after MRIO calculations
if (!all(c("comm_code", "group") %in% colnames(items))) {
  stop("items.csv must contain comm_code and group.")
}
if (anyDuplicated(items$comm_code)) {
  stop("Duplicated comm_code in items.csv.")
}

group_map <- setNames(as.character(items$group), as.character(items$comm_code))
comm_code <- sub(".*_", "", nodes)
node_group <- unname(group_map[comm_code])
node_group[is.na(node_group) | node_group == ""] <- "Unmapped"

cat("Nodes:", length(nodes), " | Groups:", length(unique(node_group)), "\n")

# Carbon intensity and household final demand
c_by_year <- vector("list", length(years))
y_by_year <- vector("list", length(years))
names(c_by_year) <- names(y_by_year) <- as.character(years)

for (year in years) {
  key <- as.character(year)
  x_year <- x_by_year[[key]][nodes]
  e_year <- as.numeric(E[[key]]["gwp_total", nodes])

  if (any(!is.finite(e_year))) {
    stop("Invalid emissions in year ", year)
  }

  c_by_year[[key]] <- e_year / x_year

  food_cols <- grep("_food$", colnames(Y[[key]]), value = TRUE)
  other_cols <- grep("_other$", colnames(Y[[key]]), value = TRUE)
  food_keys <- sub("_food$", "", food_cols)
  other_keys <- sub("_other$", "", other_cols)

  if (anyDuplicated(food_keys) || anyDuplicated(other_keys)) {
    stop("Duplicated household columns in year ", year)
  }

  food_map <- setNames(food_cols, food_keys)
  other_map <- setNames(other_cols, other_keys)
  household_keys <- intersect(food_keys, other_keys)

  if (length(household_keys) == 0) {
    stop("No matched food/other columns in year ", year)
  }
  if (!setequal(food_keys, other_keys)) {
    warning("Unmatched food/other columns in year ", year)
  }

  food <- Y[[key]][nodes, unname(food_map[household_keys]), drop = FALSE]
  other <- Y[[key]][nodes, unname(other_map[household_keys]), drop = FALSE]
  y_by_year[[key]] <- as.numeric(rowSums(food + other))

  if (any(!is.finite(y_by_year[[key]]))) {
    stop("Invalid final demand in year ", year)
  }
}

# Solve L_t y_s without forming the full inverse
Ly <- vector("list", length(years))
names(Ly) <- as.character(years)
I <- Diagonal(length(nodes))

for (i in seq_along(years)) {
  year <- years[i]
  key <- as.character(year)
  cat("Solving", year, "...\n")

  Z_year <- Z[[key]][nodes, nodes, drop = FALSE]
  x_year <- x_by_year[[key]][nodes]

  # Column-normalized technical coefficients
  A <- Z_year %*% Diagonal(x = 1 / as.numeric(x_year))

  adjacent <- unique(
    pmax(1, pmin(length(years), c(i - 1, i, i + 1)))
  )
  demand_keys <- as.character(years[adjacent])
  rhs <- do.call(cbind, y_by_year[demand_keys])
  colnames(rhs) <- demand_keys

  solution <- solve(I - A, rhs)

  Ly[[key]] <- as.matrix(solution)
  rownames(Ly[[key]]) <- nodes
  colnames(Ly[[key]]) <- demand_keys

  rm(Z_year, A, rhs, solution)
  invisible(gc())
}

# Three-factor Shapley SDA
summary_results <- vector("list", length(years) - 1)
group_results <- vector("list", length(years) - 1)

for (i in seq_len(length(years) - 1)) {
  year1 <- years[i]
  year2 <- years[i + 1]
  key1 <- as.character(year1)
  key2 <- as.character(year2)
  period <- paste0(year1, "-", year2)

  c1 <- as.numeric(c_by_year[[key1]])
  c2 <- as.numeric(c_by_year[[key2]])
  dc <- c2 - c1

  # q_ab = L_a y_b
  q11 <- as.numeric(Ly[[key1]][, key1])
  q12 <- as.numeric(Ly[[key1]][, key2])
  q21 <- as.numeric(Ly[[key2]][, key1])
  q22 <- as.numeric(Ly[[key2]][, key2])

  total1_node <- c1 * q11
  total2_node <- c2 * q22
  change_node <- total2_node - total1_node

  effect_c_node <- dc * (
    q11 / 3 + q21 / 6 + q12 / 6 + q22 / 3
  )

  dL_y1 <- q21 - q11
  dL_y2 <- q22 - q12
  effect_L_node <-
    c1 * dL_y1 / 3 + c2 * dL_y1 / 6 +
    c1 * dL_y2 / 6 + c2 * dL_y2 / 3

  L1_dy <- q12 - q11
  L2_dy <- q22 - q21
  effect_y_node <-
    c1 * L1_dy / 3 + c2 * L1_dy / 6 +
    c1 * L2_dy / 6 + c2 * L2_dy / 3

  residual_node <-
    change_node - effect_c_node - effect_L_node - effect_y_node

  total1 <- sum(total1_node)
  total2 <- sum(total2_node)
  change <- sum(change_node)
  effect_c <- sum(effect_c_node)
  effect_L <- sum(effect_L_node)
  effect_y <- sum(effect_y_node)
  residual <- change - effect_c - effect_L - effect_y

  if (abs(residual) > tolerance * max(1, abs(change))) {
    warning(period, " closure residual: ", format(residual, scientific = TRUE))
  }

  summary_results[[i]] <- data.frame(
    period = period,
    total_year1 = total1,
    total_year2 = total2,
    total_change = change,
    intensity_effect = effect_c,
    production_structure_effect = effect_L,
    final_demand_effect = effect_y,
    residual = residual
  )

  node_results <- data.frame(
    group = node_group,
    total_year1 = total1_node,
    total_year2 = total2_node,
    total_change = change_node,
    intensity_effect = effect_c_node,
    production_structure_effect = effect_L_node,
    final_demand_effect = effect_y_node,
    residual = residual_node
  )

  group_result <- aggregate(
    . ~ group,
    data = node_results,
    FUN = sum,
    na.rm = TRUE
  )
  group_result$period <- period
  group_result <- group_result[
    , c("period", "group", setdiff(colnames(group_result), c("period", "group")))
  ]
  group_results[[i]] <- group_result

  print(summary_results[[i]])
}

summary_results <- do.call(rbind, summary_results)
group_results <- do.call(rbind, group_results)
rownames(summary_results) <- NULL
rownames(group_results) <- NULL

# Export
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

summary_path <- file.path(output_dir, "SDA_results_2014_2023_corrected.csv")
group_path <- file.path(output_dir, "SDA_results_by_group_2014_2023_corrected.csv")

write.csv(summary_results, summary_path, row.names = FALSE)
write.csv(group_results, group_path, row.names = FALSE)

cat("Saved:", summary_path, "\n")
cat("Saved:", group_path, "\n")
