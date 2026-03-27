# ==============================================================================
# PHACES MODEL - Longitudinal (Multi-Year) Simulation
# File: year_by_year.R
#
# Depends on: calculate_health_impact(), calculate_outcomes(), elasticity_matrix
# All of these must already be loaded in your environment before calling
# run_longitudinal(). Source this file AFTER sourcing engine.R.
# ==============================================================================

run_longitudinal <- function(
    df_market,
    df_consumption,
    df_population,
    disease_catalog,
    elasticity_matrix,

    # --- KEY PARAMETERS (easy to change) ---
    TAX_RATE              = 0.20,  
    N_YEARS               = 10,
    SSB_GROWTH_RATE       = 0.05,   
    GDP_GROWTH_RATE       = 0.043,
    POP_GROWTH_RATE       = 0.01,
    INCIDENCE_GROWTH_RATE = 0.03, 
    DISCOUNT_RATE         = 0.03,
    GDP_Per_Capita        = 213476.75,   # USD 1,447.3 × 147.5 NPR/USD,

    scenario_name   = "Scenario",
    export = TRUE
) {

  message(paste0("\n========== LONGITUDINAL: ", scenario_name,
                 " | Tax = ", TAX_RATE * 100, "% =========="))

  # ============================================================================
  # STORAGE
  # ============================================================================

  disease_names <- unique(disease_catalog$Disease_Name)

yearly_results <- data.frame(
    Year                = integer(),
    Baseline_Revenue    = numeric(),
    New_Revenue         = numeric(),
    Revenue_Gain        = numeric(),
    Baseline_DALYs      = numeric(),
    Total_DALYs_Averted = numeric(),
    Econ_Value_USD      = numeric(),
    Total_Cases_Avoided = numeric(),
    Total_Deaths_Averted= numeric(),
    GDP_Per_Capita_Used = numeric(),
    stringsAsFactors    = FALSE
  )

  # One row per year per disease
  disease_rows <- list()

  # ============================================================================
  # INITIALISE BASELINE MARKETS & POPULATION
  # ============================================================================

  with_tax_market <- df_market
  notax_market    <- df_market

  current_population      <- df_population
  current_disease_catalog <- disease_catalog  # We will grow the incidence rate in this copy
  current_gdp             <- GDP_Per_Capita

  year1_demand_change_pct <- NULL   # saved after Year 1 for permanent behaviour shift

  # ============================================================================
  # YEAR-ON-YEAR LOOP
  # ============================================================================

  for (yr in 1:N_YEARS) {

    message(paste0("  -- Year ", yr, " --"))

    new_market <- with_tax_market

    # --------------------------------------------------------------------------
    # 1. PRICE & DEMAND IMPACT
    # --------------------------------------------------------------------------

    if (yr == 1) {
      # Net additional tax burden above existing rate
      new_market$Net_Tax_Increase  <- new_market$Price_Per_Liter * (TAX_RATE - new_market$tax_er_rate)

      # New shelf price after pass-through
      new_market$New_Price         <- new_market$Price_Per_Liter + (new_market$Net_Tax_Increase * new_market$Pass_Through_Rate)

      # % price change
      new_market$Price_Change_Pct  <- (new_market$New_Price - new_market$Price_Per_Liter) / new_market$Price_Per_Liter

      # Apply elasticity matrix
      price_vec                    <- as.matrix(new_market$Price_Change_Pct)
      demand_change                <- t(price_vec) %*% elasticity_matrix
      new_market$Demand_Change_Pct <- as.vector(demand_change)
      new_market$New_Sales_Volume  <- new_market$Sales_Volume_Liters * (1 + new_market$Demand_Change_Pct)

      # Lock in the permanent behaviour shift
      year1_demand_change_pct      <- new_market$Demand_Change_Pct

    } else {
      # Years 2-N: price stays elevated, no new elasticity shock
      new_market$Net_Tax_Increase  <- new_market$Price_Per_Liter * (TAX_RATE - new_market$tax_er_rate)
      new_market$New_Price         <- new_market$Price_Per_Liter + (new_market$Net_Tax_Increase * new_market$Pass_Through_Rate)
      new_market$Price_Change_Pct  <- 0
      new_market$Demand_Change_Pct <- year1_demand_change_pct

      # Volume = whatever was carried forward from last year (already adjusted)
      new_market$New_Sales_Volume  <- new_market$Sales_Volume_Liters
    }

    new_market_df           <- as.data.frame(new_market)
    rownames(new_market_df) <- new_market_df$Category

    # --------------------------------------------------------------------------
    # 2. BASELINE REVENUE (no-tax counterfactual)
    # --------------------------------------------------------------------------

    baseline_revenue_yr <- sum(
      notax_market$Sales_Volume_Liters *
      notax_market$Price_Per_Liter     *
      notax_market$tax_er_rate
    )

    baseline_disease_pop <- left_join(
      current_disease_catalog, 
      current_population, 
      by = c("Age-Group", "Sex")
    )
    
    baseline_dalys_yr <- sum(
      baseline_disease_pop$Population_Count * 
      baseline_disease_pop$Incidence_Base * 
      baseline_disease_pop$DALYs_per_Case,
      na.rm = TRUE
    )

    # --------------------------------------------------------------------------
    # 3. HEALTH IMPACT
    # --------------------------------------------------------------------------

    suppressMessages({
      year_health <- calculate_health_impact(new_market_df, df_consumption, current_population)
    })

    # --------------------------------------------------------------------------
    # 4. DISEASE & ECONOMIC OUTCOMES
    # --------------------------------------------------------------------------

    current_gdp <- current_gdp * (1 + GDP_GROWTH_RATE)

    suppressMessages({
      year_outcomes <- calculate_outcomes(
        health_data    = year_health,
        diseases       = current_disease_catalog, # Pass the updating catalog
        market_base    = with_tax_market,  
        market_new     = new_market_df,
        tax_inc_rate   = TAX_RATE,
        gdp_per_capita = current_gdp,
        verbose        = FALSE
      )
    })

    # --- NEW: APPLY HEALTH COMPOUNDING MULTIPLIER ---
    # Weight loss benefits compound over time for chronic diseases. 
    # Example: Year 1 = 1x, Year 2 = 1.2x, maxing out at 2.0x by Year 6.
    health_compounding_factor <- 1 + (yr - 1) * 0.20
    
    # Scale up the health outcomes
    year_outcomes$Total_DALYs_Saved <- year_outcomes$Total_DALYs_Saved * health_compounding_factor
    
    bd <- year_outcomes$Disease_Breakdown
    bd$Cases_Avoided  <- bd$Cases_Avoided  * health_compounding_factor
    bd$Deaths_Averted <- bd$Deaths_Averted * health_compounding_factor
    bd$DALYs_Saved    <- bd$DALYs_Saved    * health_compounding_factor
    bd$Econ_Value_USD <- bd$DALYs_Saved    * current_gdp

    total_econ_value_yr     <- year_outcomes$Total_DALYs_Saved * current_gdp
    total_cases_avoided_yr  <- sum(bd$Cases_Avoided)
    total_deaths_averted_yr <- sum(bd$Deaths_Averted)

    # --------------------------------------------------------------------------
    # 5. STORE ANNUAL SUMMARY
    # --------------------------------------------------------------------------

    yearly_results[yr, ] <- list(
      Year                = yr,
      Baseline_Revenue    = baseline_revenue_yr,
      New_Revenue         = year_outcomes$New_Revenue,
      Revenue_Gain        = year_outcomes$New_Revenue - baseline_revenue_yr,
      Baseline_DALYs      = baseline_dalys_yr,           # <--- ADD THIS
      Total_DALYs_Averted = year_outcomes$Total_DALYs_Saved,  # <--- RENAMED
      Econ_Value_USD      = total_econ_value_yr,
      Total_Cases_Avoided = total_cases_avoided_yr,
      Total_Deaths_Averted= total_deaths_averted_yr,
      GDP_Per_Capita_Used = current_gdp
    )

    # Store per-disease cases avoided for this year
    bd$Year <- yr
    disease_rows[[yr]] <- bd

    # --------------------------------------------------------------------------
    # 6. CHAIN: update baselines for next year
    # --------------------------------------------------------------------------

    # WITH-TAX track: post-tax volumes grow by SSB trend
    with_tax_market$Sales_Volume_Liters <- new_market_df$New_Sales_Volume * (1 + SSB_GROWTH_RATE)

    # NO-TAX track: grows by SSB trend only, no demand shock ever applied
    notax_market$Sales_Volume_Liters    <- notax_market$Sales_Volume_Liters * (1 + SSB_GROWTH_RATE)

    # Population grows annually
    current_population$Population_Count <- current_population$Population_Count * (1 + POP_GROWTH_RATE)
    
    # NEW: Disease incidence base grows annually due to background epidemiological transition
    current_disease_catalog$Incidence_Base <- current_disease_catalog$Incidence_Base * (1 + INCIDENCE_GROWTH_RATE)

  } # end year loop

  # ============================================================================
  # NPV CALCULATIONS
  # ============================================================================

  yearly_results$Discount_Factor   <- 1 / (1 + DISCOUNT_RATE) ^ yearly_results$Year
  yearly_results$NPV_Baseline_Rev  <- yearly_results$Baseline_Revenue * yearly_results$Discount_Factor
  yearly_results$NPV_New_Revenue   <- yearly_results$New_Revenue       * yearly_results$Discount_Factor
  yearly_results$NPV_Revenue_Gain  <- yearly_results$Revenue_Gain      * yearly_results$Discount_Factor
  yearly_results$NPV_Econ_Value    <- yearly_results$Econ_Value_USD     * yearly_results$Discount_Factor
  yearly_results$NPV_DALYs_Value   <- yearly_results$Total_DALYs_Averted * GDP_Per_Capita * yearly_results$Discount_Factor

  # Post-tax DALYs = what the disease burden looks like WITH the tax in place
  yearly_results$PostTax_DALYs <- yearly_results$Baseline_DALYs - yearly_results$Total_DALYs_Averted

  # --- Dietary-risk-attributable baseline DALYs ---
  # Proportions from GBD literature:
  # T2DM: 26% (Forray et al. 2023, Nutrients)
  # IHD:  32% (GBD 2021 CVD Risk Factor analysis)
  # Stroke: 22% (GBD 2021 Stroke analysis)
  # Weighted average based on Year 1 disease DALYs: (198908*0.26 + 361777*0.32 + 261433*0.22) / 822118
  DIETARY_RISK_PROPORTION <- (198908 * 0.26 + 361777 * 0.32 + 261433 * 0.22) / 822118
  
  yearly_results$Dietary_Baseline_DALYs <- yearly_results$Baseline_DALYs * DIETARY_RISK_PROPORTION
  yearly_results$PostTax_Dietary_DALYs  <- yearly_results$Dietary_Baseline_DALYs - yearly_results$Total_DALYs_Averted
  yearly_results$Pct_of_Total           <- round(yearly_results$Total_DALYs_Averted / yearly_results$Baseline_DALYs * 100, 3)
  yearly_results$Pct_of_Dietary         <- round(yearly_results$Total_DALYs_Averted / yearly_results$Dietary_Baseline_DALYs * 100, 3)
  
  # ============================================================================
  # DISEASE CASES BY YEAR TABLE
  # ============================================================================

  disease_by_year <- do.call(rbind, disease_rows)
  rownames(disease_by_year) <- NULL

  # ============================================================================
  # CONSOLE SUMMARY
  # ============================================================================

  cat("\n\n")
  cat("======================================================\n")
  cat(paste0("  LONGITUDINAL SUMMARY: ", scenario_name,
             " | Tax = ", TAX_RATE * 100, "%\n"))
  cat("======================================================\n")
  print(yearly_results[, c("Year", "Baseline_Revenue", "New_Revenue",
                            "Revenue_Gain", "Total_DALYs_Averted",
                            "Econ_Value_USD", "Total_Cases_Avoided")])
  cat("------------------------------------------------------\n")
  cat(paste("CUMULATIVE BASELINE REVENUE (Nominal): $",
            format(round(sum(yearly_results$Baseline_Revenue), 0), big.mark = ",")), "\n")
  cat(paste("CUMULATIVE NEW REVENUE (Nominal):      $",
            format(round(sum(yearly_results$New_Revenue),      0), big.mark = ",")), "\n")
  cat(paste("CUMULATIVE REVENUE GAIN (Nominal):     $",
            format(round(sum(yearly_results$Revenue_Gain),     0), big.mark = ",")), "\n")
  cat(paste("CUMULATIVE REVENUE GAIN (NPV):         $",
            format(round(sum(yearly_results$NPV_Revenue_Gain), 0), big.mark = ",")), "\n")
  cat(paste("TOTAL BASELINE DALYs (Year 1):        ",
            format(round(yearly_results$Baseline_DALYs[1], 0), big.mark = ",")), "\n")
  cat(paste("TOTAL DALYs AVERTED:                  ",
            format(round(sum(yearly_results$Total_DALYs_Averted), 1), big.mark = ",")), "\n")
  cat(paste("TOTAL ECON VALUE (NPV, USD):          $",
            format(round(sum(yearly_results$NPV_Econ_Value),   0), big.mark = ",")), "\n")
  cat(paste("TOTAL CASES AVOIDED:                  ",
            format(round(sum(yearly_results$Total_Cases_Avoided), 0), big.mark = ",")), "\n")
  cat(paste("TOTAL DEATHS AVERTED:                 ",
          format(round(sum(yearly_results$Total_Deaths_Averted), 1), big.mark = ",")), "\n")
  cat("======================================================\n")

  # ============================================================================
  # EXPORT TO EXCEL
  # ============================================================================

  library(openxlsx)
  fname <- paste0("Longitudinal_", gsub(" ", "_", scenario_name),
                  "_Tax", TAX_RATE * 100, "pct.xlsx")

  wb <- createWorkbook()

  # Sheet 1: Annual Summary
  addWorksheet(wb, "Annual_Summary")
  writeData(wb, "Annual_Summary", yearly_results)
  setColWidths(wb, "Annual_Summary", cols = 1:ncol(yearly_results), widths = "auto")

  # Sheet 2: Disease Cases by Year
  addWorksheet(wb, "Disease_Cases_By_Year")
  writeData(wb, "Disease_Cases_By_Year", disease_by_year)
  setColWidths(wb, "Disease_Cases_By_Year", cols = 1:ncol(disease_by_year), widths = "auto")

  # Sheet 3: NPV Summary
  npv_summary <- data.frame(
    Metric = c(
      "Cumulative Baseline Revenue (Nominal)",
      "Cumulative New Revenue (Nominal)",
      "Cumulative Revenue Gain (Nominal)",
      "Cumulative Revenue Gain (NPV)",
      "Total DALYs Saved",
      "Total Econ Value of DALYs (NPV)",
      "Total Cases Avoided",
      "Total Deaths Averted"
    ),
    Value = c(
      sum(yearly_results$Baseline_Revenue),
      sum(yearly_results$New_Revenue),
      sum(yearly_results$Revenue_Gain),
      sum(yearly_results$NPV_Revenue_Gain),
      sum(yearly_results$Total_DALYs_Saved),
      sum(yearly_results$NPV_Econ_Value),
      sum(yearly_results$Total_Cases_Avoided),
      sum(yearly_results$Total_Deaths_Averted)
    )
  )
  addWorksheet(wb, "NPV_Summary")
  writeData(wb, "NPV_Summary", npv_summary)
  setColWidths(wb, "NPV_Summary", cols = 1:2, widths = c(40, 20))

  if (export) {
    saveWorkbook(wb, fname, overwrite = TRUE)
    message(paste0(">> Exported: ", fname))
  }
  
  # ============================================================================
  # RETURN
  # ============================================================================

  return(list(
    scenario_name    = scenario_name,
    tax_rate         = TAX_RATE,
    yearly_results   = yearly_results,
    disease_by_year  = disease_by_year,
    npv_summary      = npv_summary
  ))
}

long_results <- run_longitudinal(
  df_market         = df_market,
  df_consumption    = df_consumption,
  df_population     = df_population,
  disease_catalog   = disease_catalog,
  elasticity_matrix = elasticity_matrix,
  GDP_Per_Capita    = 213476.75   # NPR (USD 1,447.3 × 147.5)
)