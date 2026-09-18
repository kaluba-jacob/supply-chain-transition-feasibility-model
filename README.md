# Supply-Chain Transition Feasibility Scoring Model

> Portfolio project: A multi-criteria scoring model to evaluate firm-level supply chain low-carbon transition feasibility, with bottleneck diagnosis and econometric validation. Built with R / RStudio.

[![R](https://img.shields.io/badge/R-4.5%2B-276DC3?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg?style=flat)](https://opensource.org/licenses/MIT)
[![Release](https://img.shields.io/github/v/release/kaluba-jacob/supply-chain-transition-feasibility-model?label=Release&color=brightgreen&style=flat)](https://github.com/kaluba-jacob/supply-chain-transition-feasibility-model/releases)


## ✨ Features
- **5-dimension evaluation system**: digital capability, operational efficiency, financial health, environmental pressure, and policy adaptability
- **Dual weighting schemes**: business-driven weighted scheme + equal-weight robustness check
- **Nested econometric validation**: 4 OLS specifications with industry and province fixed effects
- **HC1 heteroskedasticity-robust standard errors** for all regression results
- **Two robustness tests**: alternative weighting scheme + single-year cross-section validation
- **Publication-quality visualizations**: 3 high-resolution figures ready for academic or report use
- **Fully reproducible pipeline**: 6 sequentially numbered scripts, end-to-end runnable

## 🚀 Quick Start

### Prerequisites
- R 4.5.0 or higher
- Required packages:
```r
install.packages(c("tidyverse", "lmtest", "sandwich", "broom"))
```

### Run the full pipeline
Execute scripts in numerical order:
1. `01_data_preparation.R` – clean raw data, sample filtering, 5% winsorization
2. `02_scoring_model.R` – compute 5-dimension scores and total feasibility index
3. `03_baseline_regression.R` – baseline OLS models with fixed effects
4. `04_robustness_checks.R` – alternative weights + 2023 single-year cross-section check
5. `05_visualization.R` – generate 3 publication-quality figures
6. `06_final_results_table.R` – export combined formatted regression table

### Data input
Place your raw firm-level dataset in the `data/` folder. The pipeline expects standard panel data with firm financials, industry codes, and regional indicators. Raw data is excluded from Git via `.gitignore` for data privacy.

## 📁 Project Structure
```
supply-chain-transition-feasibility-model/
├── 01_data_preparation.R      # Data cleaning & winsorization
├── 02_scoring_model.R         # 5-dimension scoring engine
├── 03_baseline_regression.R   # Baseline OLS + fixed effects
├── 04_robustness_checks.R     # Robustness tests
├── 05_visualization.R         # Figure generation
├── 06_final_results_table.R   # Publication table export
├── data/                      # Raw input data (not tracked)
├── output/                    # Generated result tables (not tracked)
├── figures/                   # Generated plots (not tracked)
├── .gitignore
├── LICENSE
└── README.md
```

## 📊 Key Findings
- Regional digital economy development is the strongest positive predictor of transition feasibility
- High-tech firms show systematically higher feasibility scores than traditional industries
- Firm size and profitability have significant but smaller effects compared to digital infrastructure
- Results are stable across both weighting schemes and time windows

## 📄 License
MIT License – feel free to use and adapt for your own projects.

## 👤 Author
Jacob Kaluba Luboya