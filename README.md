# HERD Tax Modelling

Microsimulation framework for estimating health and economic impacts of sugar-sweetened beverage (SSB) taxation in Nepal.

## Project Structure

```
HERD_TAX_MODELLING/
├── 1_data/           # Input data (GBD estimates, consumption data, parameters)
├── 2_prep/           # R scripts for analysis
├── 3_results/        # Output files (Excel workbooks)
└── _archive/         # Previous versions and reference files
```

## Scripts (`2_prep/`)

| Script | Description |
|--------|-------------|
| `baseline.R` | Baseline population characteristics and descriptive statistics |
| `calculate_outcomes.R` | Health outcome calculations (DALYs, cases averted) |
| `disease_catalog.R` | Disease parameters and risk relationships |
| `detailed_mcmc.R` | Bayesian MCMC estimation for uncertainty quantification |
| `everything_from_scratch.R` | Full model pipeline |
| `extended_longitudinal_MCsim.R` | Monte Carlo simulation with longitudinal projections |
| `long_claude.R` | Extended longitudinal analysis |
| `Sensitivity_longitudinal.R` | Sensitivity analysis across parameter ranges |

## Key Outputs (`3_results/`)

- **Baseline_Descriptives.xlsx** — Population baseline characteristics
- **Longitudinal_MC_Scenario_*.xlsx** — Monte Carlo simulation results under tax scenarios
- **Sensitivity_Longitudinal_Results.xlsx** — Sensitivity analysis outputs
- **Detailed_MCMC_Results.xlsx** — MCMC posterior estimates

## Methods

The model simulates:
1. Price elasticity effects of SSB taxes on consumption
2. Reduced sugar intake and associated caloric reduction
3. Long-term disease burden changes (diabetes, CVD, obesity-related conditions)
4. DALYs averted and healthcare cost savings

## Requirements

- R ≥ 4.0
- Packages: `tidyverse`, `openxlsx`, `mc2d` (or similar Monte Carlo libraries)

## Data Sources

- Global Burden of Disease (GBD) 2023
- Nepal-specific consumption and demographic data


---
*Health Research and Social Development Forum (HERD International)*
