# ==============================================================================
# PHACES MODEL - Longitudinal Monte Carlo (Uncertainty Analysis)
# File: longitudinal_extended.R
#
# Simulates uncertainty in the 10-year longitudinal results by drawing
# population growth and GDP growth rates from realistic distributions.
#
# Rationale: The elasticity matrix drives Year 1 behaviour change only.
# Over a 10-year horizon, the dominant sources of uncertainty in cumulative
# outcomes are demographic growth (how many people benefit) and economic
# growth (how much each DALY is worth). This script quantifies that.
#
# Depends on: calculate_health_impact(), calculate_outcomes(),
#             elasticity_matrix, year_by_year.R (run_longitudinal)
# ==============================================================================

run_longitudinal_mc <- function(
    df_market,
    df_consumption,
    df_population,
    disease_catalog,
    elasticity_matrix,

    # --- Fixed parameters ---
    TAX_RATE              = 0.20,
    N_YEARS               = 10,
    SSB_GROWTH_RATE       = 0.05,
    INCIDENCE_GROWTH_RATE = 0.03,   
    DISCOUNT_RATE         = 0.03,
    GDP_Per_Capita        = 1447.3,
    

    # --- Monte Carlo settings ---
    n_runs          = 100,
    seed            = 123,

  pop_growth_mean = 0.0092,
  pop_growth_sd   = 0.002,
  gdp_growth_mean = 0.05,
  gdp_growth_sd   = 0.015,

  scenario_name   = "Scenario"
) {

  message(paste0("\n========== LONGITUDINAL MONTE CARLO: ", scenario_name,
                 " | Tax = ", TAX_RATE * 100, "% | Runs = ", n_runs, " =========="))
  message(">> Uncertainty parameters:")
  message(paste0("   Population growth: Normal(mean=", pop_growth_mean*100, "%, sd=", pop_growth_sd*100, "%)"))
  message(paste0("   GDP growth:        Normal(mean=", gdp_growth_mean*100, "%, sd=", gdp_growth_sd*100, "%)"))
  set.seed(seed)

  # ============================================================================
  # STORAGE — one value per run for each key cumulative outcome
  # ============================================================================

  sim_results <- data.frame(
    run                      = 1:n_runs,
    pop_growth_rate          = numeric(n_runs),
    gdp_growth_rate          = numeric(n_runs),
    cum_revenue_gain         = numeric(n_runs),
    cum_npv_revenue_gain     = numeric(n_runs),
    cum_dalys_saved          = numeric(n_runs),
    cum_deaths_averted       = numeric(n_runs),
    cum_cases_avoided        = numeric(n_runs),
    cum_econ_value_npv       = numeric(n_runs),
    stringsAsFactors         = FALSE
  )

  # ============================================================================
  # MONTE CARLO LOOP
  # ============================================================================

  pb <- txtProgressBar(min = 0, max = n_runs, style = 3)

  for (i in 1:n_runs) {

    # --- Draw parameters from uniform distributions ---
    run_pop_growth <- max(rnorm(1, mean = pop_growth_mean, sd = pop_growth_sd), 0.001)
    run_gdp_growth <- max(rnorm(1, mean = gdp_growth_mean, sd = gdp_growth_sd), 0.005)

    # --- Run the full longitudinal model silently ---
    suppressMessages({
      run_result <- run_longitudinal(
        df_market             = df_market,
        df_consumption        = df_consumption,
        df_population         = df_population,
        disease_catalog       = disease_catalog,
        elasticity_matrix     = elasticity_matrix,
        TAX_RATE              = TAX_RATE,
        N_YEARS               = N_YEARS,
        SSB_GROWTH_RATE       = SSB_GROWTH_RATE,
        INCIDENCE_GROWTH_RATE = INCIDENCE_GROWTH_RATE, 
        GDP_GROWTH_RATE       = run_gdp_growth,
        POP_GROWTH_RATE       = run_pop_growth,
        DISCOUNT_RATE         = DISCOUNT_RATE,
        GDP_Per_Capita        = GDP_Per_Capita,
        scenario_name         = paste0("MC_run_", i),
        export = FALSE
      )
    })

    yr <- run_result$yearly_results

    # --- Store cumulative outcomes ---
    sim_results[i, "pop_growth_rate"]      <- run_pop_growth
    sim_results[i, "gdp_growth_rate"]      <- run_gdp_growth
    sim_results[i, "cum_revenue_gain"]     <- sum(yr$Revenue_Gain)
    sim_results[i, "cum_npv_revenue_gain"] <- sum(yr$NPV_Revenue_Gain)
    sim_results[i, "cum_dalys_saved"]      <- sum(yr$Total_DALYs_Saved)
    sim_results[i, "cum_deaths_averted"]   <- sum(yr$Total_Deaths_Averted)
    sim_results[i, "cum_cases_avoided"]    <- sum(yr$Total_Cases_Avoided)
    sim_results[i, "cum_econ_value_npv"]   <- sum(yr$NPV_Econ_Value)

    setTxtProgressBar(pb, i)
  }

  close(pb)

  # ============================================================================
  # SUMMARISE: 95% Confidence Intervals
  # ============================================================================

  get_ci <- function(x, decimals = 0) {
    qs <- quantile(x, probs = c(0.025, 0.50, 0.975))
    fmt <- function(v) format(round(v, decimals), big.mark = ",")
    paste0(fmt(qs[2]), " (", fmt(qs[1]), " to ", fmt(qs[3]), ")")
  }

  mc_summary <- data.frame(
    Metric = c(
      "Cumulative Revenue Gain — Nominal (USD)",
      "Cumulative Revenue Gain — NPV (USD)",
      "Total DALYs Saved (10-year)",
      "Total Deaths Averted (10-year)",
      "Total Cases Avoided (10-year)",
      "Total Economic Value of DALYs — NPV (USD)"
    ),
    `Median (95% CI)` = c(
      get_ci(sim_results$cum_revenue_gain,     decimals = 0),
      get_ci(sim_results$cum_npv_revenue_gain, decimals = 0),
      get_ci(sim_results$cum_dalys_saved,      decimals = 1),
      get_ci(sim_results$cum_deaths_averted,   decimals = 1),
      get_ci(sim_results$cum_cases_avoided,    decimals = 1),
      get_ci(sim_results$cum_econ_value_npv,   decimals = 0)
    ),
    check.names = FALSE
  )

  # Also store raw quantiles for programmatic use
  mc_quantiles <- data.frame(
    Metric     = c("cum_revenue_gain", "cum_npv_revenue_gain", "cum_dalys_saved",
                   "cum_deaths_averted", "cum_cases_avoided", "cum_econ_value_npv"),
    Lower_2.5  = c(quantile(sim_results$cum_revenue_gain,     0.025),
                   quantile(sim_results$cum_npv_revenue_gain, 0.025),
                   quantile(sim_results$cum_dalys_saved,      0.025),
                   quantile(sim_results$cum_deaths_averted,   0.025),
                   quantile(sim_results$cum_cases_avoided,    0.025),
                   quantile(sim_results$cum_econ_value_npv,   0.025)),
    Median     = c(quantile(sim_results$cum_revenue_gain,     0.50),
                   quantile(sim_results$cum_npv_revenue_gain, 0.50),
                   quantile(sim_results$cum_dalys_saved,      0.50),
                   quantile(sim_results$cum_deaths_averted,   0.50),
                   quantile(sim_results$cum_cases_avoided,    0.50),
                   quantile(sim_results$cum_econ_value_npv,   0.50)),
    Upper_97.5 = c(quantile(sim_results$cum_revenue_gain,     0.975),
                   quantile(sim_results$cum_npv_revenue_gain, 0.975),
                   quantile(sim_results$cum_dalys_saved,      0.975),
                   quantile(sim_results$cum_deaths_averted,   0.975),
                   quantile(sim_results$cum_cases_avoided,    0.975),
                   quantile(sim_results$cum_econ_value_npv,   0.975))
  )

  # ============================================================================
  # PRINT SUMMARY
  # ============================================================================

  cat("\n\n")
  cat("======================================================\n")
  cat(paste0("  MONTE CARLO SUMMARY: ", scenario_name, " | Tax = ", TAX_RATE * 100, "%\n"))
  cat(paste0("  Runs = ", n_runs, "\n"))
  cat(paste0("  Pop growth: Normal(mean=", pop_growth_mean*100, "%, sd=", pop_growth_sd*100, "%)\n"))
  cat(paste0("  GDP growth: Normal(mean=", gdp_growth_mean*100, "%, sd=", gdp_growth_sd*100, "%)\n"))
  cat("======================================================\n")
  print(mc_summary, row.names = FALSE)
  cat("======================================================\n")

  # ============================================================================
  # EXPORT TO EXCEL
  # ============================================================================

  library(openxlsx)
  fname <- paste0("Longitudinal_MC_", gsub(" ", "_", scenario_name),
                  "_Tax", TAX_RATE * 100, "pct_", n_runs, "runs.xlsx")

  wb <- createWorkbook()

  # Sheet 1: Summary with CIs (the main result)
  addWorksheet(wb, "MC_Summary")
  writeData(wb, "MC_Summary", mc_summary)
  setColWidths(wb, "MC_Summary", cols = 1:2, widths = c(45, 35))

  # Sheet 2: Raw quantiles (for further analysis)
  addWorksheet(wb, "MC_Quantiles")
  writeData(wb, "MC_Quantiles", mc_quantiles)
  setColWidths(wb, "MC_Quantiles", cols = 1:4, widths = c(30, 15, 15, 15))

  # Sheet 3: All raw simulation runs (for diagnostics / custom analysis)
  addWorksheet(wb, "Raw_Runs")
  writeData(wb, "Raw_Runs", sim_results)
  setColWidths(wb, "Raw_Runs", cols = 1:ncol(sim_results), widths = "auto")


  saveWorkbook(wb, fname, overwrite = TRUE)
  message(paste0(">> Exported: ", fname))

  # ============================================================================
  # RETURN
  # ============================================================================

  return(list(
    scenario_name = scenario_name,
    tax_rate      = TAX_RATE,
    mc_summary    = mc_summary,
    mc_quantiles  = mc_quantiles,
    raw_runs      = sim_results
  ))
}

mc_results <- run_longitudinal_mc(
  df_market         = df_market,
  df_consumption    = df_consumption,
  df_population     = df_population,
  disease_catalog   = disease_catalog,
  elasticity_matrix = elasticity_matrix,
  TAX_RATE          = 0.20,
  n_runs            = 100,
  scenario_name     = "Scenario 20pct"
)