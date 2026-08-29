#===============================================================================
# Project: Supply-Chain Transition Feasibility Scoring Model
# Script: 03_baseline_regression.R
# Objective: explain feasibility score by firm characteristics,
#            robust standard errors, industry + province fixed effects
# Dependent var: total_score_schemeA (composite feasibility index)
# Independent vars: size, profitability, leverage, ownership, tech, pollution, digital economy
# Input:  output/scoring_results_main.rds
# Output: output/regression_baseline.rds ; output/regression_table.csv
#===============================================================================

# ---- 0. Load packages ----
library(tidyverse)
library(lmtest)
library(sandwich)
library(broom)

# ---- 1. Load scoring results ----
data_main <- readRDS("output/scoring_results_main.rds")

# ---- 2. Nested regression models ----
# Model 1: basic firm characteristics
model1 <- lm(total_score_schemeA ~ 
               revenue + roaa + debt_ratio +
               is_high_tech + is_heavy_pollute + digital_economy,
             data = data_main)

# Model 2: + ownership type
model2 <- lm(total_score_schemeA ~ 
               revenue + roaa + debt_ratio +
               is_high_tech + is_heavy_pollute + digital_economy +
               factor(ownership),
             data = data_main)

# Model 3: + industry fixed effects
model3 <- lm(total_score_schemeA ~ 
               revenue + roaa + debt_ratio +
               is_high_tech + is_heavy_pollute + digital_economy +
               factor(ownership) + factor(industry),
             data = data_main)

# Model 4: full model + province fixed effects (preferred specification)
model4 <- lm(total_score_schemeA ~ 
               revenue + roaa + debt_ratio +
               is_high_tech + is_heavy_pollute + digital_economy +
               factor(ownership) + factor(industry) + factor(province),
             data = data_main)

# ---- 3. Heteroskedasticity-robust standard errors (HC1) ----
get_robust_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC1"))
}

robust1 <- get_robust_se(model1)
robust2 <- get_robust_se(model2)
robust3 <- get_robust_se(model3)
robust4 <- get_robust_se(model4)

cat("=== Preferred Model (4) — Robust SE ===\n")
print(robust4)

# ---- 4. Model summary statistics ----
model_stats <- tibble(
  model = c("Model 1", "Model 2", "Model 3", "Model 4 (preferred)"),
  adj_r2 = c(summary(model1)$adj.r.squared,
             summary(model2)$adj.r.squared,
             summary(model3)$adj.r.squared,
             summary(model4)$adj.r.squared),
  n_obs = c(nobs(model1), nobs(model2), nobs(model3), nobs(model4)),
  ownership_controls = c("No", "Yes", "Yes", "Yes"),
  industry_FE = c("No", "No", "Yes", "Yes"),
  province_FE = c("No", "No", "No", "Yes")
)

print(model_stats)

# ---- 5. Extract key coefficients table ----
extract_key_coef <- function(robust_obj, model_name) {
  tidy(robust_obj) %>%
    filter(!str_detect(term, "factor\\(")) %>%  # drop FE dummies
    select(term, estimate, std.error, statistic, p.value) %>%
    mutate(model = model_name, .before = 1)
}

reg_table <- bind_rows(
  extract_key_coef(robust1, "Model 1"),
  extract_key_coef(robust2, "Model 2"),
  extract_key_coef(robust3, "Model 3"),
  extract_key_coef(robust4, "Model 4 (preferred)")
)

# ---- 6. Export results ----
saveRDS(list(model1 = model1, model2 = model2, model3 = model3, model4 = model4,
             robust1 = robust1, robust2 = robust2, robust3 = robust3, robust4 = robust4,
             reg_table = reg_table, model_stats = model_stats),
        "output/regression_baseline.rds")

write_csv(reg_table, "output/regression_coefficients.csv")
write_csv(model_stats, "output/regression_model_stats.csv")

cat(" Baseline regression finished. Results saved to output/ folder.\n")
