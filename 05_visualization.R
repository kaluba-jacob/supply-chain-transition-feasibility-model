#===============================================================================
# Project: Supply-Chain Transition Feasibility Scoring Model
# Script: 05_visualization.R
# Objective: publication-quality figures
#            Fig1: feasibility score distribution by tier
#            Fig2: 5-dimension radar by major industry
#            Fig3: average feasibility by ownership × tech status
# Input:  output/scoring_results_main.rds
# Output: figures/fig1_score_distribution.png
#         figures/fig2_dimensions_by_industry.png
#         figures/fig3_ownership_tech.png
#===============================================================================

# ---- 0. Load packages & set theme ----
library(tidyverse)

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40")
    )
)

# ---- 1. Load scoring results ----
data_main <- readRDS("output/scoring_results_main.rds")

# ---- 2. Figure 1: Feasibility score distribution by tier ----
fig1 <- ggplot(data_main, aes(x = total_score_schemeA, fill = feasibility_tier)) +
  geom_histogram(bins = 50, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("High feasibility" = "#2E8B57",
                               "Medium feasibility" = "#DAA520",
                               "Low feasibility" = "#B22222")) +
  labs(
    title = "Distribution of Supply-Chain Transition Feasibility Scores",
    subtitle = "Firm-level sample, 2021–2023 average, business-driven weighting scheme",
    x = "Total feasibility score (0–100)",
    y = "Number of firms",
    fill = "Feasibility tier"
  ) +
  geom_vline(xintercept = median(data_main$total_score_schemeA),
             linetype = "dashed", color = "gray30") +
  annotate("text", x = median(data_main$total_score_schemeA) + 2, y = 180,
           label = paste0("Median = ", round(median(data_main$total_score_schemeA), 1)),
           hjust = 0, size = 3.5)

ggsave("figures/fig1_score_distribution.png", fig1,
       width = 9, height = 5.5, dpi = 300)

cat("✅ Figure 1 saved.\n")

# ---- 3. Figure 2: 5 dimensions by major industry (top 8 industries) ----
top_industries <- data_main %>%
  count(industry, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(industry)

dim_long <- data_main %>%
  filter(industry %in% top_industries) %>%
  group_by(industry) %>%
  summarise(across(starts_with("score_dim_"), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = starts_with("score_dim_"),
               names_to = "dimension", values_to = "score") %>%
  mutate(dimension = str_remove(dimension, "score_dim_") %>%
           str_replace_all("_", " ") %>%
           str_to_title())

fig2 <- ggplot(dim_long, aes(x = dimension, y = score, fill = dimension)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  facet_wrap(~ industry, ncol = 4) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_viridis_d(option = "D", end = 0.85) +
  labs(
    title = "Average Dimension Scores by Industry",
    subtitle = "Top 8 industries by sample size, 0–100 normalized scale",
    x = NULL, y = "Average score"
  )

ggsave("figures/fig2_dimensions_by_industry.png", fig2,
       width = 11, height = 7, dpi = 300)

cat("✅ Figure 2 saved.\n")

# ---- 4. Figure 3: Feasibility by ownership × high-tech status ----
ownership_summary <- data_main %>%
  group_by(ownership, is_high_tech) %>%
  summarise(
    mean_score = mean(total_score_schemeA, na.rm = TRUE),
    se = sd(total_score_schemeA, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    is_high_tech = ifelse(is_high_tech == 1, "High-tech", "Non high-tech"),
    ownership = factor(ownership)
  )

fig3 <- ggplot(ownership_summary,
               aes(x = ownership, y = mean_score, fill = is_high_tech)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_score - 1.96*se, ymax = mean_score + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_manual(values = c("High-tech" = "#1F4E79", "Non high-tech" = "#9CB6D1")) +
  labs(
    title = "Average Feasibility Score by Ownership Type and Tech Status",
    subtitle = "Mean ± 95% confidence interval",
    x = "Ownership type",
    y = "Average total feasibility score",
    fill = NULL
  )

ggsave("figures/fig3_ownership_tech.png", fig3,
       width = 9, height = 5.5, dpi = 300)

cat("✅ Figure 3 saved.\n")

cat("\n✅ All figures saved to figures/ folder (300 dpi PNG).\n")
