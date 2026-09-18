#===============================================================================
# Project: Supply‑Chain Transition Feasibility Scoring Model
# Script: 01_data_preparation.R
# Objective: import raw data, select relevant variables, clean, 5% two‑sided
#            winsorization, descriptive stats, export cleaned dataset
# Input:  data/data2.xlsx
# Output: output/data_clean.rds ; output/data_clean.csv ; output/descriptive_statistics.csv
#===============================================================================

# ---- 0. Install & load required packages ----
install.packages(c("tidyverse","readxl","psych","janitor"))

library(tidyverse)
library(readxl)
library(psych)
library(janitor)

# ---- 1. Import raw dataset ----
raw_data <- read_excel(path = "data/data2.xlsx")

cat("Raw dataset dimensions:", dim(raw_data), "\n")

# ---- 2. Select only variables needed for this project ----
data_clean <- raw_data %>%
  select(
    id              = 证券代码,
    year            = year,
    industry        = 所属行业,
    ownership       = 产权性质,
    province        = 所在省份,
    supp_conc       = 供应商集中度,
    cust_conc       = 客户集中度,
    capex           = 资本支出,
    revenue         = 营业收入,
    digital_index   = 数字化转型指数,
    op_cashflow     = 经营活动产生的现金流量净额,
    debt_ratio      = 资产负债率,
    roaa            = 总资产净利润率ROAA,
    is_high_tech    = 是否高技术制造业,
    is_heavy_pollute = 是否重污染,
    digital_economy = 数字经济指数
  )

cat("After selecting project variables:", dim(data_clean), "\n")

# ---- 3. Drop rows where ALL five core scoring variables are missing ----
# (keeps rows with partial data; full imputation happens in Stage 2)
data_clean <- data_clean %>%
  filter(!(is.na(supp_conc) & is.na(cust_conc) & is.na(capex) &
             is.na(digital_index) & is.na(op_cashflow)))

cat("After dropping fully‑empty core rows:", dim(data_clean), "\n")

# ---- 4. Two‑sided 5% winsorization for numeric variables ----
winsor_5 <- function(x) {
  if (!is.numeric(x)) return(x)
  q_low  <- quantile(x, probs = 0.05, na.rm = TRUE)
  q_high <- quantile(x, probs = 0.95, na.rm = TRUE)
  x <- ifelse(x < q_low,  q_low,  x)
  x <- ifelse(x > q_high, q_high, x)
  return(x)
}

data_clean <- data_clean %>%
  mutate(across(where(is.numeric), winsor_5))

# ---- 5. Descriptive statistics table ----
desc_stats <- describe(data_clean) %>%
  as_tibble(rownames = "variable")

print(desc_stats)

# ---- 6. Export outputs to output folder ----
saveRDS(data_clean, file = "output/data_clean.rds")
write_csv(data_clean, file = "output/data_clean.csv")
write_csv(desc_stats, file = "output/descriptive_statistics.csv")

cat("Data preparation finished. Files saved into output/ folder.\n")