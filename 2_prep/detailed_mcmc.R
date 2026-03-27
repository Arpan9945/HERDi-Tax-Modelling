# ==============================================================================
# detailed_mcmc.R — Expanded Monte Carlo Simulation
# Depends on: calculate_impact(), calculate_health_impact(), calculate_outcomes()
#             elasticity_matrix, df_market, df_consumption, df_population,
#             disease_catalog — all passed as parameters
# ==============================================================================

# ==============================================================================
# 1. THE EXPANDED SIMULATION FUNCTION
# ==============================================================================



run_monte_carlo_expanded <- function(
    df_market,
    df_consumption,
    df_population,
    disease_catalog,
    elasticity_matrix,
    n_runs           = 1000,
    tax_scenario     = 0.20,
    gdp_per_capita   = 1447.3,
    uncertainty_ranges = list(
      elasticity_sd  = 0.10,
      pass_through_sd = 0.05,
      rr_sd          = 0.02
    ),
    scenario_name    = "Scenario"
) {

  message(paste0("\n>> Running Expanded Monte Carlo: ", scenario_name,
                 " (", n_runs, " runs)..."))

  # --- STORAGE: AGGREGATE ---
  res_revenue <- numeric(n_runs)
  res_dalys   <- numeric(n_runs)
  res_cases   <- numeric(n_runs)

  # --- STORAGE: DISAGGREGATED ---
  mat_weight_change <- NULL
  mat_bmi_change    <- NULL
  mat_new_bmi       <- NULL

  pb <- txtProgressBar(min = 0, max = n_runs, style = 3)

  for (i in 1:n_runs) {

    # 1. RANDOMIZE PARAMETERS ------------------------------------------------

    # Elasticity uncertainty
    noise_matrix   <- matrix(rnorm(16, mean = 1, sd = uncertainty_ranges$elasticity_sd),
                             nrow = 4, ncol = 4)
    sim_elasticity <- elasticity_matrix * noise_matrix

    # Pass-through uncertainty
    sim_market_data <- df_market
    random_pt       <- rnorm(nrow(sim_market_data),
                             mean = 0.9,
                             sd   = uncertainty_ranges$pass_through_sd)
    sim_market_data$Pass_Through_Rate <- pmax(0.5, pmin(1.1, random_pt))

    # Relative risk uncertainty
    sim_diseases            <- disease_catalog
    sim_diseases$RR_per_BMI <- rnorm(nrow(sim_diseases),
                                     mean = disease_catalog$RR_per_BMI,
                                     sd   = uncertainty_ranges$rr_sd)

    # 2. RUN MODEL CHAIN ------------------------------------------------------

    # Market Impact
    sim_market_res               <- calculate_impact(sim_market_data, tax_scenario,
                                                     sim_elasticity, verbose = FALSE)
    sim_market_res               <- as.data.frame(sim_market_res)
    rownames(sim_market_res)     <- sim_market_res$Category

    # Health Impact
    sim_health_res <- calculate_health_impact(sim_market_res, df_consumption, df_population)

    # Outcomes
    sim_outcomes <- calculate_outcomes(
      sim_health_res,
      sim_diseases,
      df_market,
      sim_market_res,
      tax_inc_rate   = tax_scenario,
      gdp_per_capita = gdp_per_capita,
      verbose        = FALSE
    )

    # 3. STORE AGGREGATE RESULTS ----------------------------------------------
    res_revenue[i] <- sim_outcomes$Revenue_Gain
    res_dalys[i]   <- sum(sim_outcomes$Total_DALYs_Saved)
    res_cases[i]   <- sum(sim_outcomes$Disease_Breakdown$Cases_Avoided)

    # 4. STORE DISAGGREGATED RESULTS ------------------------------------------
    if (is.null(mat_weight_change)) {
      num_groups   <- nrow(sim_health_res)
      group_names  <- paste(sim_health_res$`Age-Group`, sim_health_res$Sex, sep = " ")
      mat_weight_change <- matrix(NA, nrow = n_runs, ncol = num_groups,
                                  dimnames = list(NULL, group_names))
      mat_bmi_change    <- matrix(NA, nrow = n_runs, ncol = num_groups,
                                  dimnames = list(NULL, group_names))
      mat_new_bmi       <- matrix(NA, nrow = n_runs, ncol = num_groups,
                                  dimnames = list(NULL, group_names))
    }

    mat_weight_change[i, ] <- sim_health_res$Weight_Loss_kg * -1
    mat_bmi_change[i, ]    <- sim_health_res$BMI_Change     * -1
    mat_new_bmi[i, ]       <- sim_health_res$New_Mean_BMI

    setTxtProgressBar(pb, i)
  }
  close(pb)

  return(list(
    scenario_name  = scenario_name,
    Aggregate      = data.frame(Revenue = res_revenue,
                                DALYs   = res_dalys,
                                Cases   = res_cases),
    Disaggregated  = list(
      Weight_Change_kg = mat_weight_change,
      BMI_Change_Units = mat_bmi_change,
      New_BMI_Absolute = mat_new_bmi
    )
  ))
}

# ==============================================================================
# 2. REPORTING FUNCTION — Generates Table C1
# ==============================================================================

generate_expanded_report <- function(mc_results) {

  format_ci <- function(vec, decimals = 3) {
    qs <- quantile(vec, probs = c(0.025, 0.975), na.rm = TRUE)
    mn <- mean(vec, na.rm = TRUE)
    paste0(sprintf(paste0("%.", decimals, "f"), mn),
           " (", sprintf(paste0("%.", decimals, "f"), qs[1]),
           " to ", sprintf(paste0("%.", decimals, "f"), qs[2]), ")")
  }

  message("\n=======================================================")
  message(paste0(" POLICY SCORECARD: ", mc_results$scenario_name))
  message("=======================================================")

  agg <- mc_results$Aggregate
  agg_table <- data.frame(
    Metric = c("Govt Revenue (USD)", "Total DALYs Saved", "Total Cases Avoided"),
    Result = c(
      paste0(format(round(mean(agg$Revenue), 0), big.mark = ","), " (95% CI)"),
      paste0(format(round(mean(agg$DALYs),   1), big.mark = ","), " (95% CI)"),
      paste0(format(round(mean(agg$Cases),   0), big.mark = ","), " (95% CI)")
    )
  )
  print(agg_table)

  message("\n-------------------------------------------------------")
  message(" DETAILED HEALTH OUTCOMES BY GROUP (TABLE C1)")
  message("-------------------------------------------------------")

  mat_w  <- mc_results$Disaggregated$Weight_Change_kg
  mat_b  <- mc_results$Disaggregated$BMI_Change_Units
  mat_n  <- mc_results$Disaggregated$New_BMI_Absolute
  groups <- colnames(mat_w)

  summary_table <- data.frame(
    Group                = groups,
    `Weight Change (kg)` = apply(mat_w, 2, format_ci),
    `BMI Change (units)` = apply(mat_b, 2, format_ci),
    `New Mean BMI`       = apply(mat_n, 2, format_ci),
    check.names          = FALSE
  )
  print(summary_table)

  return(summary_table)
}

# ==============================================================================
# 3. SCORECARD HELPER
# ==============================================================================

get_summary_stats <- function(vec, decimals = 0) {
  qs <- quantile(vec, probs = c(0.025, 0.975), na.rm = TRUE)
  mn <- mean(vec, na.rm = TRUE)
  paste0(
    format(round(mn,     decimals), big.mark = ","), " (",
    format(round(qs[1],  decimals), big.mark = ","), " to ",
    format(round(qs[2],  decimals), big.mark = ","), ")"
  )
}

# ==============================================================================
# 4. EXPORT FUNCTION
# ==============================================================================

export_mcmc_results <- function(full_results, final_table_c1, final_scorecard,
                                filename = "Detailed_MCMC_Results.xlsx") {

  library(openxlsx)
  message(paste0(">> Exporting MCMC results to '", filename, "'..."))

  wb_mcmc <- createWorkbook()

  # Sheet 1: Executive Summary
  addWorksheet(wb_mcmc, "Executive_Summary")
  writeData(wb_mcmc, "Executive_Summary", final_scorecard)
  setColWidths(wb_mcmc, "Executive_Summary", cols = 1:2, widths = c(25, 40))

  # Sheet 2: Group Analysis (Table C1)
  addWorksheet(wb_mcmc, "Group_Analysis_Age_Sex")
  writeData(wb_mcmc, "Group_Analysis_Age_Sex", final_table_c1)
  setColWidths(wb_mcmc, "Group_Analysis_Age_Sex", cols = 1:4, widths = c(20, 25, 25, 25))

  # Sheet 3: Raw Simulation Data
  addWorksheet(wb_mcmc, "Raw_Simulation_Data")
  writeData(wb_mcmc, "Raw_Simulation_Data", full_results$Aggregate)

  saveWorkbook(wb_mcmc, filename, overwrite = TRUE)
  message(paste0(">> Export complete: '", filename, "'"))
}

# ==============================================================================
# 5. EXECUTE THE SIMULATION AND EXPORT
# ==============================================================================

# 1. Run the simulation (This turns the machine on!)
# Note: Ensure your dataframes (df_market, df_consumption, etc.) are loaded in your environment
my_mcmc_results <- run_monte_carlo_expanded(
  df_market         = df_market,
  df_consumption    = df_consumption,
  df_population     = df_population,
  disease_catalog   = disease_catalog,
  elasticity_matrix = elasticity_matrix,
  n_runs            = 1000, 
  tax_scenario      = 0.20,
  scenario_name     = "20% SSB Tax Scenario"
)

# 2. Generate the detailed Table C1 report (Prints to console)
my_table_c1 <- generate_expanded_report(my_mcmc_results)

# 3. Create the final Executive Scorecard using the helper function
my_scorecard <- data.frame(
  Metric = c("Govt Revenue (USD)", "Total DALYs Saved", "Total Cases Avoided"),
  Estimated_Impact = c(
    get_summary_stats(my_mcmc_results$Aggregate$Revenue, decimals = 0),
    get_summary_stats(my_mcmc_results$Aggregate$DALYs, decimals = 1),
    get_summary_stats(my_mcmc_results$Aggregate$Cases, decimals = 0)
  )
)

# 4. Export everything to Excel
export_mcmc_results(
  full_results    = my_mcmc_results, 
  final_table_c1  = my_table_c1, 
  final_scorecard = my_scorecard,
  filename        = "Detailed_MCMC_Results.xlsx"
)