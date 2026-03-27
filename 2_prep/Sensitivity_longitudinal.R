# ==============================================================================
# SENSITIVITY LONGITUDINAL ANALYSIS
# Runs the 10-year longitudinal model across 4 tax rates × 4 pass-through rates
# Produces cumulative and average annual outcomes + heatmaps
#
# Run AFTER: everything_from_scratch.R and long_claude.R are sourced
# ==============================================================================

library(openxlsx)

# Define scenarios
tax_rates <- c(0.15, 0.20, 0.25, 0.30)
pass_throughs <- c(0.80, 0.85, 0.90, 1.00)

# Storage matrices — CUMULATIVE 10-year
cum_revenue   <- matrix(NA, nrow = 4, ncol = 4,
                        dimnames = list(paste0(tax_rates*100, "% Tax"),
                                        paste0(pass_throughs*100, "% PT")))
cum_dalys     <- cum_revenue
cum_cases     <- cum_revenue
cum_deaths    <- cum_revenue
cum_hc_savings <- cum_revenue   # healthcare cost savings

# Storage matrices — AVERAGE ANNUAL
avg_revenue   <- cum_revenue
avg_dalys     <- cum_revenue
avg_cases     <- cum_revenue
avg_deaths    <- cum_revenue

# Treatment costs (NPR)
cost_diabetes <- 660 * 147.5   # NPR 97,350
cost_ihd      <- 1500 * 147.5  # NPR 221,250
cost_stroke   <- 1200 * 147.5  # NPR 177,000

message(">> Running 16 longitudinal scenarios (4 tax × 4 pass-through)...")

for (i in 1:length(tax_rates)) {
  for (j in 1:length(pass_throughs)) {
    
    target_tax <- tax_rates[i]
    current_pt <- pass_throughs[j]
    
    message(paste0("  Scenario: Tax=", target_tax*100, "%, PT=", current_pt*100, "%"))
    
    # Set up market with this pass-through rate
    temp_market <- df_market
    temp_market$Pass_Through_Rate <- current_pt
    
    # Run the full 10-year longitudinal model
    suppressMessages({
      result <- run_longitudinal(
        df_market         = temp_market,
        df_consumption    = df_consumption,
        df_population     = df_population,
        disease_catalog   = disease_catalog,
        elasticity_matrix = elasticity_matrix,
        TAX_RATE          = target_tax,
        N_YEARS           = 10,
        SSB_GROWTH_RATE   = 0.05,
        GDP_GROWTH_RATE   = 0.043,
        POP_GROWTH_RATE   = 0.01,
        INCIDENCE_GROWTH_RATE = 0.03,
        DISCOUNT_RATE     = 0.03,
        GDP_Per_Capita    = 213476.75,
        scenario_name     = paste0("Tax", target_tax*100, "_PT", current_pt*100),
        export            = FALSE
      )
    })
    
    yr <- result$yearly_results
    
    # Calculate healthcare cost savings from disease breakdown
    dby <- result$disease_by_year
    hc_total <- 0
    for (y in 1:10) {
      yr_diseases <- dby[dby$Year == y, ]
      for (r in 1:nrow(yr_diseases)) {
        d_name <- yr_diseases$Disease[r]
        cases  <- yr_diseases$Cases_Avoided[r]
        if (grepl("Diabetes", d_name))  hc_total <- hc_total + cases * cost_diabetes
        if (grepl("Ischemic", d_name))  hc_total <- hc_total + cases * cost_ihd
        if (grepl("Stroke", d_name))    hc_total <- hc_total + cases * cost_stroke
      }
    }
    
    # Store CUMULATIVE
    cum_revenue[i, j]    <- sum(yr$Revenue_Gain)
    cum_dalys[i, j]      <- sum(yr$Total_DALYs_Averted)
    cum_cases[i, j]      <- sum(yr$Total_Cases_Avoided)
    cum_deaths[i, j]     <- sum(yr$Total_Deaths_Averted)
    cum_hc_savings[i, j] <- hc_total
    
    # Store AVERAGE ANNUAL
    avg_revenue[i, j]    <- mean(yr$Revenue_Gain)
    avg_dalys[i, j]      <- mean(yr$Total_DALYs_Averted)
    avg_cases[i, j]      <- mean(yr$Total_Cases_Avoided)
    avg_deaths[i, j]     <- mean(yr$Total_Deaths_Averted)
  }
}

# ==============================================================================
# PRINT RESULTS
# ==============================================================================

print_mat <- function(mat, title, fmt = ",.0f") {
  cat("\n===================================================\n")
  cat(title, "\n")
  cat("===================================================\n")
  print(format(round(mat, 0), big.mark = ","), quote = FALSE)
}

cat("\n\n========== CUMULATIVE 10-YEAR RESULTS ==========\n")
print_mat(cum_revenue / 1e9, "Cumulative Revenue Gain (NPR Billion)")
print_mat(cum_dalys, "Cumulative DALYs Averted")
print_mat(cum_cases, "Cumulative Cases Avoided")
print_mat(cum_deaths, "Cumulative Deaths Averted")
print_mat(cum_hc_savings / 1e6, "Cumulative HC Savings (NPR Million)")

cat("\n\n========== AVERAGE ANNUAL RESULTS ==========\n")
print_mat(avg_revenue / 1e9, "Average Annual Revenue Gain (NPR Billion)")
print_mat(avg_dalys, "Average Annual DALYs Averted")
print_mat(avg_cases, "Average Annual Cases Avoided")
print_mat(avg_deaths, "Average Annual Deaths Averted")

# ==============================================================================
# EXPORT TO EXCEL
# ==============================================================================

wb <- createWorkbook()

addWorksheet(wb, "Cumulative_Revenue_B")
writeData(wb, "Cumulative_Revenue_B", cum_revenue / 1e9, rowNames = TRUE)

addWorksheet(wb, "Cumulative_DALYs")
writeData(wb, "Cumulative_DALYs", cum_dalys, rowNames = TRUE)

addWorksheet(wb, "Cumulative_Cases")
writeData(wb, "Cumulative_Cases", cum_cases, rowNames = TRUE)

addWorksheet(wb, "Cumulative_Deaths")
writeData(wb, "Cumulative_Deaths", cum_deaths, rowNames = TRUE)

addWorksheet(wb, "Cumulative_HC_Savings_M")
writeData(wb, "Cumulative_HC_Savings_M", cum_hc_savings / 1e6, rowNames = TRUE)

addWorksheet(wb, "Avg_Annual_Revenue_B")
writeData(wb, "Avg_Annual_Revenue_B", avg_revenue / 1e9, rowNames = TRUE)

addWorksheet(wb, "Avg_Annual_DALYs")
writeData(wb, "Avg_Annual_DALYs", avg_dalys, rowNames = TRUE)

addWorksheet(wb, "Avg_Annual_Cases")
writeData(wb, "Avg_Annual_Cases", avg_cases, rowNames = TRUE)

saveWorkbook(wb, "Sensitivity_Longitudinal_Results.xlsx", overwrite = TRUE)
message(">> Exported: Sensitivity_Longitudinal_Results.xlsx")