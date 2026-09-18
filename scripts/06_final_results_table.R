#===============================================================================
# Project: Supply-Chain Transition Feasibility Scoring Model
# Script: 06_final_results_table.R
# Objective: publication-style combined regression table
#            Baseline Models 1–4 + 2 robustness specifications
#            HC1 robust SE in parentheses, significance stars
# Input:  output/regression_baseline.rds ; output/robustness_results.rds
# Output: output/final_regression_table.csv
#===============================================================================

# ---- 0. Load packages ----
library(tidyverse)
library(lmtest)
library(sandwich)
library(broom)

# ---- 1. Load all regression results ----
reg_baseline <- readRDS("output/regression_baseline.rds")
reg_robust   <- readRDS("output/robustness_results.rds")

# Coefficient + SE objects (HC1 robust)
all_robust <- list(
  "Model 1"              = reg_baseline$robust1,
  "Model 2"              = reg_baseline$robust2,
  "Model 3"              = reg_baseline$robust3,
  "Model 4\n(Baseline)"  = reg_baseline$robust4,
  "Robust:\nEqual weights" = reg_robust$robust1_rob,
  "Robust:\n2023 only"   = reg_robust$robust2_rob
)

# Raw lm objects (for diagnostics: nobs, R²)
all_lm <- list(
  "Model 1"              = reg_baseline$model1,
  "Model 2"              = reg_baseline$model2,
  "Model 3"              = reg_baseline$model3,
  "Model 4\n(Baseline)"  = reg_baseline$model4,
  "Robust:\nEqual weights" = reg_robust$robust1,
  "Robust:\n2023 only"   = reg_robust$robust2
)

# ---- 2. Helper: format coefficient + SE + stars ----
format_coef <- function(robust_obj, var_name) {
  res <- tidy(robust_obj) %>% filter(term == var_name)
  if (nrow(res) == 0) return("")
  
  b  <- round(res$estimate, 3)
  se <- round(res$std.error, 3)
  p  <- res$p.value
  
  star <- case_when(
    p < 0.01  ~ "***",
    p < 0.05  ~ "**",
    p < 0.10  ~ "*",
    TRUE      ~ ""
  )
  
  paste0(b, star, "\n(", se, ")")
}

# ---- 3. Variable order and labels ----
var_labels <- c(
  "revenue"           = "Firm size (revenue)",
  "roaa"              = "Profitability (ROAA)",
  "debt_ratio"        = "Leverage (debt ratio)",
  "is_high_tech"      = "High-tech industry",
  "is_heavy_pollute"  = "Heavy-polluting industry",
  "digital_economy"   = "Regional digital economy index"
)

# ---- 4. Build coefficient panel ----
final_table <- tibble(Variable = unname(var_labels))

for (col_name in names(all_robust)) {
  final_table[[col_name]] <- map_chr(names(var_labels),
                                     ~format_coef(all_robust[[col_name]], .x))
}

# ---- 5. Add model diagnostics rows ----
diag_rows <- tibble(Variable = c(
  "Observations", "Adjusted R²",
  "Firm controls", "Ownership controls",
  "Industry FE", "Province FE"
))

for (col_name in names(all_lm)) {
  m <- all_lm[[col_name]]
  
  diag_rows[[col_name]] <- c(
    format(nobs(m), big.mark = ","),
    round(summary(m)$adj.r.squared, 3),
    ifelse(str_detect(col_name, "Model 1"), "No", "Yes"),
    ifelse(str_detect(col_name, "Model [12]"), "No", "Yes"),
    ifelse(str_detect(col_name, "Model [12]|Robust:"), "No", "Yes"),
    ifelse(str_detect(col_name, "Model 4|Baseline"), "Yes", "No")
  )
}

final_table <- bind_rows(final_table, diag_rows)

# ---- 6. Export ----
write_csv(final_table, "output/final_regression_table.csv")

cat("✅ Final publication-style regression table saved.\n")
cat("   File: output/final_regression_table.csv\n\n")
cat("   Significance: *** p<0.01, ** p<0.05, * p<0.10\n")
cat("   Robust standard errors (HC1) in parentheses.\n")
