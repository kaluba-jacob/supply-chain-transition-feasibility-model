#===============================================================================
# Project: Supply-Chain Transition Feasibility Scoring Model
# Script: 04_robustness_checks.R
# Objective: robustness checks:
#            (1) alternative equal-weight scoring scheme
#            (2) 2023 single-year cross-section
#            Compare both against baseline Model 4
# Input:  output/scoring_results_main.rds ; output/data_2023_clean.rds
# Output: output/robustness_comparison.csv
#===============================================================================

# ---- 0. Load packages ----
library(tidyverse)
library(lmtest)
library(sandwich)
library(broom)

# ---- 1. Load data ----
data_main  <- readRDS("output/scoring_results_main.rds")
data_2023  <- readRDS("output/data_2023_clean.rds")

# ---- 2. Recompute scoring dimensions for 2023 sample ----
compute_dimensions <- function(df) {
  df %>%
    mutate(
      dim_supplier_stability   = 100 - supp_conc,
      dim_operational_efficiency = -(capex / revenue),
      dim_digitalization       = digital_index,
      dim_bargaining_power     = 100 - (supp_conc + cust_conc) / 2,
      dim_cashflow_health      = op_cashflow / revenue
    )
}

normalize_minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  ((x - rng[1]) / (rng[2] - rng[1])) * 100
}

impute_industry_median <- function(df) {
  df %>%
    group_by(industry) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.) | is.infinite(.), 
                                             median(., na.rm = TRUE), .))) %>%
    ungroup()
}

# Build 2023 scoring sample
data_2023_scored <- data_2023 %>%
  impute_industry_median() %>%
  compute_dimensions()

dim_cols <- c("dim_supplier_stability", "dim_operational_efficiency",
              "dim_digitalization", "dim_bargaining_power", "dim_cashflow_health")

data_2023_scored <- data_2023_scored %>%
  mutate(across(all_of(dim_cols), normalize_minmax, .names = "score_{.col}"))

# Scheme A weights for 2023
weight_scheme_a <- c(
  score_dim_digitalization        = 0.30,
  score_dim_supplier_stability    = 0.25,
  score_dim_cashflow_health       = 0.15,
  score_dim_operational_efficiency = 0.15,
  score_dim_bargaining_power      = 0.15
)

data_2023_scored <- data_2023_scored %>%
  rowwise() %>%
  mutate(total_score_schemeA = sum(c_across(names(weight_scheme_a)) * weight_scheme_a)) %>%
  ungroup()

# ---- 3. Preferred specification formula (full model) ----
full_formula <- as.formula(
  "total_score_schemeA ~ revenue + roaa + debt_ratio +
   is_high_tech + is_heavy_pollute + digital_economy +
   factor(ownership) + factor(industry) + factor(province)"
)

# ---- 4. Run three regressions ----
# Baseline: main sample, Scheme A
baseline <- lm(full_formula, data = data_main)

# Robustness 1: main sample, Scheme B (equal weights)
robust1_formula <- update(full_formula, total_score_schemeB ~ .)
robust1 <- lm(robust1_formula, data = data_main)

# Robustness 2: 2023 sample, Scheme A
robust2 <- lm(full_formula, data = data_2023_scored)

# ---- 5. HC1 robust standard errors ----
get_robust_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC1"))
}

baseline_rob <- get_robust_se(baseline)
robust1_rob  <- get_robust_se(robust1)
robust2_rob  <- get_robust_se(robust2)

# ---- 6. Extract key coefficients comparison table ----
extract_key_coef <- function(robust_obj, spec_name) {
  tidy(robust_obj) %>%
    filter(!str_detect(term, "factor\\(")) %>%
    select(term, estimate, std.error, p.value) %>%
    mutate(specification = spec_name, .before = 1)
}

comparison_table <- bind_rows(
  extract_key_coef(baseline_rob, "Baseline: 3-yr avg, Scheme A"),
  extract_key_coef(robust1_rob,  "Robustness: 3-yr avg, Scheme B (equal weights)"),
  extract_key_coef(robust2_rob,  "Robustness: 2023 cross-section, Scheme A")
)

# ---- 7. Model stats comparison ----
model_stats_comp <- tibble(
  specification = c("Baseline: 3-yr avg, Scheme A",
                    "Robustness: 3-yr avg, Scheme B",
                    "Robustness: 2023 cross-section, Scheme A"),
  adj_r2 = c(summary(baseline)$adj.r.squared,
             summary(robust1)$adj.r.squared,
             summary(robust2)$adj.r.squared),
  n_obs  = c(nobs(baseline), nobs(robust1), nobs(robust2))
)

cat("=== Robustness check — model fit comparison ===\n")
print(model_stats_comp)

# ---- 8. Export ----
write_csv(comparison_table, "output/robustness_coefficient_comparison.csv")
write_csv(model_stats_comp,   "output/robustness_model_stats.csv")

saveRDS(list(baseline = baseline, robust1 = robust1, robust2 = robust2,
             baseline_rob = baseline_rob, robust1_rob = robust1_rob,
             robust2_rob = robust2_rob, comparison_table = comparison_table,
             model_stats_comp = model_stats_comp),
        "output/robustness_results.rds")

cat(" Robustness checks finished. Comparison tables saved to output/ folder.\n")
