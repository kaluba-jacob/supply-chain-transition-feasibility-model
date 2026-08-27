#===============================================================================
# Project: Supply-Chain Transition Feasibility Scoring Model
# Script: 02_scoring_model.R
# Objective: compute 5 core dimensions, min-max normalize to 0-100,
#            calculate total score under two weighting schemes, classify tiers
# Input:  output/data_clean.rds
# Output: output/scoring_results_main.rds ; output/scoring_results_main.csv
#===============================================================================

# ---- 0. Load packages ----
library(tidyverse)

# ---- 1. Load cleaned panel data ----
data_clean <- readRDS("output/data_clean.rds")

# ---- 2. Build analysis samples ----
# Main sample: 2021-2023 firm-level averages
data_main <- data_clean %>%
  filter(year %in% 2021:2023) %>%
  group_by(id, industry, ownership, province, is_high_tech, is_heavy_pollute) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

# 2023 single-year cross-section (saved for Stage 4 robustness)
data_2023 <- data_clean %>%
  filter(year == 2023)

cat("Main sample (3-year avg):", nrow(data_main), "firms\n")
cat("2023 cross-section sample:", nrow(data_2023), "firms\n")

# ---- 3. Missing value imputation: industry median ----
impute_industry_median <- function(df) {
  df %>%
    group_by(industry) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.) | is.infinite(.), 
                                             median(., na.rm = TRUE), .))) %>%
    ungroup()
}

data_main <- impute_industry_median(data_main)
data_2023 <- impute_industry_median(data_2023)

# ---- 4. Compute 5 base dimensions (all positive-oriented: higher = better) ----
compute_dimensions <- function(df) {
  df %>%
    mutate(
      # 1. Supplier stability: reverse supplier concentration
      dim_supplier_stability = 100 - supp_conc,
      
      # 2. Operational efficiency: reverse capex-to-revenue pressure
      dim_operational_efficiency = -(capex / revenue),
      
      # 3. Digitalization level
      dim_digitalization = digital_index,
      
      # 4. Upstream-downstream bargaining power: reverse avg concentration
      dim_bargaining_power = 100 - (supp_conc + cust_conc) / 2,
      
      # 5. Cash-flow health: operating cash flow margin
      dim_cashflow_health = op_cashflow / revenue
    )
}

data_main <- compute_dimensions(data_main)

# ---- 5. Min-Max normalization: scale every dimension to 0-100 points ----
normalize_minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  ((x - rng[1]) / (rng[2] - rng[1])) * 100
}

dim_cols <- c("dim_supplier_stability", "dim_operational_efficiency",
              "dim_digitalization", "dim_bargaining_power", "dim_cashflow_health")

data_main <- data_main %>%
  mutate(across(all_of(dim_cols), normalize_minmax, .names = "score_{.col}"))

# ---- 6. Total feasibility score under two weighting schemes ----
# Scheme A: business-driven weights
weight_scheme_a <- c(
  score_dim_digitalization        = 0.30,
  score_dim_supplier_stability    = 0.25,
  score_dim_cashflow_health       = 0.15,
  score_dim_operational_efficiency = 0.15,
  score_dim_bargaining_power      = 0.15
)

# Scheme B: equal weights
weight_scheme_b <- rep(0.20, 5)
names(weight_scheme_b) <- paste0("score_", dim_cols)

data_main <- data_main %>%
  rowwise() %>%
  mutate(
    total_score_schemeA = sum(c_across(names(weight_scheme_a)) * weight_scheme_a),
    total_score_schemeB = sum(c_across(names(weight_scheme_b)) * weight_scheme_b)
  ) %>%
  ungroup()

# ---- 7. Classify feasibility tiers (based on Scheme A, 25% / 75% quantiles) ----
q25 <- quantile(data_main$total_score_schemeA, 0.25)
q75 <- quantile(data_main$total_score_schemeA, 0.75)

data_main <- data_main %>%
  mutate(
    feasibility_tier = case_when(
      total_score_schemeA >= q75 ~ "High feasibility",
      total_score_schemeA <= q25 ~ "Low feasibility",
      TRUE ~ "Medium feasibility"
    )
  )

cat("Feasibility tier distribution:\n")
print(table(data_main$feasibility_tier))

# ---- 8. Export results ----
saveRDS(data_main, "output/scoring_results_main.rds")
write_csv(data_main, "output/scoring_results_main.csv")

# Save 2023 sample for later robustness stage
saveRDS(data_2023, "output/data_2023_clean.rds")

cat(" Scoring model finished. Results saved to output/ folder.\n")
