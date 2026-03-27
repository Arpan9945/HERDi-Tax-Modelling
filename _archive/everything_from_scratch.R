rm(list = ls())

library(dplyr)
library(readxl)


source("disease_catalog.R")
source("calculate_outcomes.R")

data_file = "C:/Users/Arpan Acharya/OneDrive - HERD/Documents/HERD-tasks/tax_research/data.xlsx"


df_market      <- read_xlsx(data_file)
df_consumption <- read_xlsx(data_file, sheet = "cons")
df_population  <- read_xlsx(data_file, sheet = "pop")

  
#Step 1:


elasticity_matrix <- matrix(
  c(-1.2,  0.21, 0.09, 1.19,     # Column 1: Effect of Price Changes on Soda Demand
    -0.93, -0.45, -0.13, -0.29,  # Column 2: Effect of Price Changes on Juice Demand
    -1.30,  0.47, -1.12, -1.00,  # Column 3: Effect of Price Changes on Milk Demand
    -0.60, 0.39, -0.28, -1.19),  
  nrow = 4, ncol = 4, byrow = TRUE
)
dimnames(elasticity_matrix) <- list( 
  c("Carbonated_drinks", "Juice", "Milk-Sweetened", "Energy_drinks"),
  c("Carbonated_drinks", "Juice", "Milk-Sweetened", "Energy_drinks")
)


#Function 1

calculate_impact <- function(market_data = df_market, tax_scenario_increase, elasticity_data = elasticity_matrix, verbose = TRUE) {
  
  if(verbose) message(paste("\n>> Running Scenario: +", tax_scenario_increase*100, "% Tax Increase <<"))

  df_results <- market_data 

  df_results$Net_Tax_Increase <- df_results$Price_Per_Liter *(tax_scenario_increase - df_results$tax_er_rate)
  #Apply pass through
  df_results$New_Price <- df_results$Price_Per_Liter + (df_results$Net_Tax_Increase * df_results$Pass_Through_Rate)

  #Calculate Percentage change
  df_results$Price_Change_Pct <- (df_results$New_Price - df_results$Price_Per_Liter) / df_results$Price_Per_Liter

  if(verbose){
  print("Price Changes:")
  print(df_results[, c("Category", "Price_Per_Liter", "New_Price", "Price_Change_Pct")])
  }
  
  #Now calculate the percentage change in consumption volime
  price_change_vector <- as.matrix(df_results$Price_Change_Pct)
  demand_change_pct <- t(price_change_vector) %*% elasticity_data
  df_results$Demand_Change_Pct <- as.vector(demand_change_pct)

  #New sales volume
  df_results$New_Sales_Volume <- df_results$Sales_Volume_Liters * (1 + df_results$Demand_Change_Pct)
  if(verbose){
  print("Volume Changes:")
  print(df_results[, c("Category", "Sales_Volume_Liters", "Demand_Change_Pct", "New_Sales_Volume")])
  }
  return(df_results)
}
new_market_state <- calculate_impact(tax_scenario_increase = 0.20)
new_market_state <- as.data.frame(new_market_state)
rownames(new_market_state) <- new_market_state$Category



#============================================================================================#
# Calculating Health Impacts
#============================================================================================#

calculate_health_impact <- function(market_results, consumption_data, population_data){
  health_results <- left_join(population_data, consumption_data, by = c("Age-Group", "Sex"))
  health_results$Daily_Kcal_Reduction <- rep(0, nrow(health_results))

  for(cat in rownames(market_results)){
    pct_change <- market_results[cat, "Demand_Change_Pct"]
    energy_density <- market_results[cat, "Energy_kcal_per_100mL"]/100
    col_name <- paste0(cat, "_mL_day")
    if(col_name %in% names(health_results)){
      baseline_consumption <- health_results[[col_name]]
      calories_removed <- baseline_consumption * (pct_change * -1) * energy_density
      health_results$Daily_Kcal_Reduction <- health_results$Daily_Kcal_Reduction + calories_removed
    }else {
      warning(paste("Column", col_name, "not found in consumption data."))
    }
  } 

  #Energy Balance Equation
  KJ_to_Kcal_Conversion <-  22.5
  health_results$Weight_Loss_kg <- health_results$Daily_Kcal_Reduction / KJ_to_Kcal_Conversion
  #BMI Calculation using the formula New BMI = OLD - (Weight Loss/ Height^2)
  health_results$BMI_Change <- health_results$Weight_Loss_kg / (health_results$Mean_Height_m ^ 2)

  #Calculate the BMI change unit
  health_results$New_Mean_BMI <- health_results$Mean_BMI - health_results$BMI_Change
  print(health_results[, c("Age-Group", "Daily_Kcal_Reduction", "Weight_Loss_kg", "BMI_Change", "New_Mean_BMI")])

  return(health_results)
}

final_health_outcomes <- calculate_health_impact(new_market_state, df_consumption, df_population)
#============================================================================================#
# Disease Impact and Economic Modelling
#============================================================================================#
# disease_catalog <- data.frame(
#   Disease_Name = c("Type 2 Diabetes", "Ischemic Heart Disease", "Stroke"),
#   # Relative Risk per 1 unit BMI increase (e.g., 1.18 means 18% higher risk)
#   RR_per_BMI = c(1.18, 1.08, 1.06),       
#   # Baseline Incidence (Annual chance of getting the disease, e.g., 1%)
#   Incidence_Base = c(0.026, 0.00505 , 0.00153), 
#   # DALYs lost per case (Severity score: 0=Healthy, 1=Death)
#   DALYs_per_Case = c(0.60, 0.45, 0.55)     
# )


disease_catalog <- disease_catalog_final

disease_catalog$`Age-Group` <- gsub(" years", "", disease_catalog$`Age-Group`)
disease_catalog <- subset(disease_catalog, `Age-Group` != "70+")
disease_catalog <- subset(disease_catalog, Sex != "Both")
disease_catalog <- subset(disease_catalog, Disease_Name != "All causes")

GDP_Per_Capita_USD <- 1447.3 #using this value now

# calculate_outcomes <- function(health_data, diseases, market_base, market_new, tax_inc_rate, verbose = TRUE){
#   if(verbose) message("\n>> calculating Disease and Economic Impact")

#   #Disease Burden (DALYs)
#   health_data$Total_DALYs_Saved <- 0
#   total_cases_avoided <- 0

#   disease_breakdown <- data.frame(
#     Disease = character(),
#     Cases_Avoided = numeric(),
#     DALYs_Saved = numeric(),
#     Econ_Value_USD = numeric(),
#     stringsAsFactors = FALSE
#   )

#   pop_col <- health_data$Population_Count

#   for(i in 1:nrow(diseases)){
#     d_name <- diseases$Disease_Name[i]
#     d_rr <- diseases$RR_per_BMI[i]
#     d_inc <- diseases$Incidence_Base[i]
#     d_daly <- diseases$DALYs_per_Case[i]

#     #2. Calculate Potential Impact Fraction (PIF)
#     # Formula: 1 - (RR^ - BMI_Change)
#     #This tells us what % of the disease risk was removed due to weight loss
#     pif <- 1 - (d_rr ^ (-health_data$BMI_Change))

#     #Then calculate Avoided Case
#     #Population * Incidence Rate * % Risk Removed
 
#     avoided_cases <- pop_col * d_inc * pif
#     #Calculating DALYs for this disease
#     saved_dalys <- avoided_cases * d_daly
    
#     health_data$Total_DALYs_Saved <- health_data$Total_DALYs_Saved + saved_dalys
#     total_cases_avoided <- total_cases_avoided + sum(avoided_cases)

#     if(verbose) message(paste("  ->", d_name, "Cases Avoided:", round(sum(avoided_cases), 1)))
    
#     #Calculate totals specifically for this disease
#     this_disease_cases <- sum(avoided_cases)
#     this_disease_dalys <- sum(saved_dalys)
#     this_disease_values <- this_disease_dalys * GDP_Per_Capita_USD

#     #Store the results in our new breakdown table
#     disease_breakdown[i, ] <- list(
#       d_name,
#       this_disease_cases,
#       this_disease_dalys,
#       this_disease_values
#     )
#   }

#   # --- PART B: ECONOMIC VALUATION OF HEALTH RELATED VARIABLES -----
#   health_data$Econ_Value_USD <- health_data$Total_DALYs_Saved * GDP_Per_Capita_USD

#   # --- FISCAL REVENUE CHANGE ----
#   # Revenue = Volume * Price * Tax_Rate

#   #1. Baseline Revenue
#   old_tax_rates <- market_base$tax_er_rate
#   old_revenue <- sum(market_base$Sales_Volume_Liters * market_base$Price_Per_Liter * old_tax_rates)
#   #2. New Revenue (Scenario)
#   #Assuming that the tax increase is additive (e.g. 13% + 20% = 33%)
#   #tax_inc_rate is our current tax target rate
#   new_revenue <- sum(market_new$New_Sales_Volume * market_base$Price_Per_Liter * tax_inc_rate)

#   revenue_gain <- new_revenue - old_revenue
#   if (verbose) {
#   print("------------------------------------------------")
#   print(paste("TOTAL CASES AVOIDED:    ", format(round(total_cases_avoided, 0), big.mark=",")))
#   print(paste("TOTAL DALYs SAVED:      ", format(round(sum(health_data$Total_DALYs_Saved), 1), big.mark=",")))
#   print(paste("TOTAL ECON VALUE (USD): $", format(round(sum(health_data$Econ_Value_USD), 0), big.mark=",")))
#   print(paste("GOVT REVENUE GAIN:      ", format(round(revenue_gain, 0), big.mark=",")))
#   print("------------------------------------------------")
#   }
#   # Return a list containing the detailed dataframe and the revenue figure
#   return(list(Detailed_Health = health_data, Disease_Breakdown = disease_breakdown, Revenue_Gain = revenue_gain))
# }




final_outcomes <- calculate_outcomes(final_health_outcomes,
                                      disease_catalog,
                                      df_market,
                                      new_market_state,
                                      tax_inc_rate = .20)






#============================================================================================#
# Scenario Sensitivity Analysis
#============================================================================================#

#Define the Ranges
tax_rates <- c(.15, .20, .25, .30)
pass_throughs <- c(1, .9, .85, .80)

mat_revenue <- matrix(NA, nrow = length(tax_rates), ncol = length(pass_throughs), 
                        dimnames=list(paste0(tax_rates*100, "% Tax"), paste0(pass_throughs*100, "% Pass-through")))

mat_dalys <- matrix(NA, nrow = length(tax_rates), ncol = length(pass_throughs), 
                        dimnames=list(paste0(tax_rates*100, "% Tax"), paste0(pass_throughs*100, "% Pass-through")))

mat_cases <- matrix(NA, nrow = length(tax_rates), ncol = length(pass_throughs), 
                        dimnames=list(paste0(tax_rates*100, "% Tax"), paste0(pass_throughs*100, "% Pass-through")))

disease_names <- unique(disease_catalog$Disease_Name)
list_cases_by_disease <- list()

#Initializing a blank matrix for every disease in the catalog
for(d in disease_names){
  list_cases_by_disease[[d]] <- matrix(NA, nrow = length(tax_rates), ncol = length(pass_throughs),
                                        dimnames=list(paste0(tax_rates*100, "% Tax"), paste0(pass_throughs*100, "% Pass-through")))
}

message(">> Running the Sensitivity Analysis...")
#Nested Loop

for(i in 1:length(tax_rates)){
  for(j in 1:length(pass_throughs)){

    target_tax <- tax_rates[i]
    current_pt <- pass_throughs[j]

    #Setting up temporary market data
    temp_market <- df_market
    temp_market$Pass_Through_Rate <- current_pt

    #Fixing the net tax logic
    temp_market$Net_Tax_Increase <- temp_market$Price_Per_Liter * (target_tax - temp_market$tax_er_rate)
    
    temp_market$New_Price <- temp_market$Price_Per_Liter + (temp_market$Net_Tax_Increase * temp_market$Pass_Through_Rate)


    temp_market$Price_Change_Pct <- (temp_market$New_Price - temp_market$Price_Per_Liter) / temp_market$Price_Per_Liter
    
    price_vec <- as.matrix(temp_market$Price_Change_Pct)
    demand_change <- t(price_vec) %*% elasticity_matrix
    temp_market$Demand_Change_Pct <- as.vector(demand_change)
    temp_market$New_Sales_Volume <- temp_market$Sales_Volume_Liters * (1 + temp_market$Demand_Change_Pct)

    temp_market <- as.data.frame(temp_market)
    rownames(temp_market) <- temp_market$Category

    #Calculate Health Impact
    #First lets supress the message to avoid the spamming of console 16 times
    suppressMessages({
      temp_health <- calculate_health_impact(temp_market, df_consumption, df_population)
    })

    #Calculate Outcomes
    suppressMessages({
      temp_outcomes <- calculate_outcomes(temp_health, disease_catalog, df_market, temp_market, target_tax)
    })

    #Store these results in Matrices---
    mat_revenue[i,j] <- temp_outcomes$Revenue_Gain
    mat_dalys[i,j] <- sum(temp_outcomes$Detailed_Health$Total_DALYs_Saved)

    # Extract the Disease Specific Cases Avoided directly from the breakdown table
    for(d in disease_names){
      cases_for_d <- temp_outcomes$Disease_Breakdown$Cases_Avoided[temp_outcomes$Disease_Breakdown$Disease == d]
      
      # If the disease isn't found (e.g., PIF was 0), default to 0 to prevent crashes
      if(length(cases_for_d) == 0) cases_for_d <- 0 
      
      list_cases_by_disease[[d]][i,j] <- cases_for_d
    }
  }
}

print_matrix <- function(mat, title) {
  cat("\n===================================================\n")
  cat(title, "\n")
  cat("\n===================================================\n")
  print(format(round(mat, 0), big.mark = ","), quote=FALSE)
}

print_matrix(mat_revenue, "Total Revenue Gain (USD)")
print_matrix(mat_dalys, "Total DALYs Saved (All Diseases")
for(d in disease_names){
  print_matrix(list_cases_by_disease[[d]], paste("CASES AVOIDED:", toupper(d)))
}


#============================================================================================#
# Monte Carlo Simulation (Uncertainty Analysis)
#============================================================================================#

n_runs <- 1000
set.seed(123)
tax_rate_sim <- 0.20

sim_revenue <- numeric(n_runs)
sim_dalys <- numeric(n_runs)
sim_cases <- numeric(n_runs)

#3. Define Uncertainty Ranges (Standard Deviation )
elasticity_sd_pct <- .10

message(paste(">> Starting Monte Carlo Simulation ", n_runs, "runs)..."))
pb <- txtProgressBar(min = 0, max = n_runs, style = 3)




for(i in 1:n_runs){
  noise_matrix <- matrix(rnorm(16, mean = 1, sd = elasticity_sd_pct), nrow = 4, ncol = 4)

  current_run_elasticity <- elasticity_matrix * noise_matrix

  sim_market <- calculate_impact(
    tax_scenario_increase = tax_rate_sim,
    elasticity_data = current_run_elasticity,
    verbose = FALSE
  )
  sim_market <- as.data.frame(sim_market)
  rownames(sim_market) <- sim_market$Category

  sim_health <- calculate_health_impact(sim_market, df_consumption, df_population)
  sim_outcomes <- calculate_outcomes(sim_health, disease_catalog, df_market, sim_market, tax_rate_sim, verbose = FALSE)

  sim_revenue[i] <- sim_outcomes$Revenue_Gain
  sim_dalys[i] <- sum(sim_outcomes$Detailed_Health$Total_DALYs_Saved)

  setTxtProgressBar(pb, i)
}
close(pb)

#Helper function to get 95% CI
get_CI <- function(data) {
  qs <- quantile(data, probs = c(0.025, 0.50, 0.975))
  return(qs)
}

rev_ci <- get_CI(sim_revenue)
daly_ci <- get_CI(sim_dalys)

print("======================================================")
print(paste0("REVENUE GAIN:  ", 
             format(round(rev_ci[2], 0), big.mark=","), " (",
             format(round(rev_ci[1], 0), big.mark=","), " to ",
             format(round(rev_ci[3], 0), big.mark=","), ")"))
print("======================================================")




library(openxlsx)

message(">> Exporting results to 'Simulation_Results.xlsx'...")

# 1. Create a new Workbook
wb <- createWorkbook()

# 2. Add Sheets and Write Data
# --- Sheet 1: Revenue Sensitivity ---
addWorksheet(wb, "Revenue_Sensitivity")
writeData(wb, "Revenue_Sensitivity", mat_revenue, rowNames = TRUE)

# --- Sheet 2: DALYs Sensitivity ---
addWorksheet(wb, "DALYs_Sensitivity")
writeData(wb, "DALYs_Sensitivity", mat_dalys, rowNames = TRUE)

# --- Sheet 3: Disease Cases Avoided ---
# We will stack the disease matrices on one sheet with headers
addWorksheet(wb, "Cases_Avoided")
current_row <- 1

for(d in names(list_cases_by_disease)) {
  # Write the Disease Name as a header
  writeData(wb, "Cases_Avoided", paste("DISEASE:", toupper(d)), startRow = current_row)
  
  # Write the Matrix below it
  writeData(wb, "Cases_Avoided", list_cases_by_disease[[d]], startRow = current_row + 1, rowNames = TRUE)
  
  # Move the cursor down (Matrix rows + Header + Spacing)
  current_row <- current_row + nrow(list_cases_by_disease[[d]]) + 4
}

# --- Sheet 4: Monte Carlo Summary ---
addWorksheet(wb, "Monte_Carlo_Summary")

# Create a summary data frame for the CI
mc_summary <- data.frame(
  Metric = c("Revenue Gain", "DALYs Saved"),
  Lower_95_CI = c(rev_ci[1], daly_ci[1]),
  Median = c(rev_ci[2], daly_ci[2]),
  Upper_95_CI = c(rev_ci[3], daly_ci[3])
)
writeData(wb, "Monte_Carlo_Summary", mc_summary)

# 3. Save the Workbook
saveWorkbook(wb, "Simulation_Results.xlsx", overwrite = TRUE)

message(">> Export Complete! Check your working directory for 'Simulation_Results.xlsx'")

#============================================================================================#
# PRINT TO CONSOLE (For Quick View)
#============================================================================================#

print("====== FINAL SENSITIVITY RESULTS (REVENUE) ======")
print(mat_revenue)

print("====== MONTE CARLO 95% CI ======")
print(mc_summary)
  
final_results_list <- list(
    final_outcomes    = final_outcomes,
    mat_revenue       = mat_revenue,
    mat_dalys         = mat_dalys,
    mc_summary        = mc_summary,
    df_market         = df_market,
    disease_catalog   = disease_catalog
  )

