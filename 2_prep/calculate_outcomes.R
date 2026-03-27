library(dplyr)

calculate_outcomes <- function(health_data, diseases, market_base, market_new, tax_inc_rate, 
                               gdp_per_capita = 213476.75, verbose = TRUE) {
  if(verbose) message("\n>> Calculating Age-Specific Disease and Economic Impact")

  # 1. Merge the data matching Age-Group and Sex
  merged_data <- left_join(diseases, health_data, by = c("Age-Group", "Sex"), relationship = "many-to-many")

  # 2. Vectorized Math (Row-by-Row calculation)
  merged_data <- merged_data %>%
    mutate(
      PIF = 1 - (RR_per_BMI ^ (-BMI_Change)),
      Avoided_Cases = Population_Count * Incidence_Base * PIF,
      Averted_Deaths = Avoided_Cases * CFR,
      Saved_DALYs = Avoided_Cases * DALYs_per_Case,
      Econ_Value = Saved_DALYs * gdp_per_capita
    )

  # 3. Aggregate the totals back into a clean breakdown table by DISEASE
  disease_breakdown <- merged_data %>%
    group_by(Disease_Name) %>%
    summarise(
      Cases_Avoided = sum(Avoided_Cases, na.rm = TRUE),
      Deaths_Averted = sum(Averted_Deaths, na.rm = TRUE),
      DALYs_Saved = sum(Saved_DALYs, na.rm = TRUE),
      Econ_Value_USD = sum(Econ_Value, na.rm = TRUE)
    ) %>%
    rename(Disease = Disease_Name) %>%
    as.data.frame()

  # 4. NEW: Aggregate the totals into a clean breakdown table by AGE
  age_breakdown <- merged_data %>%
    group_by(`Age-Group`) %>%
    summarise(
      Cases_Avoided = sum(Avoided_Cases, na.rm = TRUE),
      Deaths_Averted = sum(Averted_Deaths, na.rm = TRUE),
      DALYs_Saved = sum(Saved_DALYs, na.rm = TRUE),
      Econ_Value_USD = sum(Econ_Value, na.rm = TRUE)
    ) %>%
    as.data.frame()

  # 5. Calculate total overall impact
  total_cases_avoided <- sum(disease_breakdown$Cases_Avoided)
  total_deaths_averted <- sum(disease_breakdown$Deaths_Averted)
  total_dalys_saved <- sum(disease_breakdown$DALYs_Saved)
  total_econ_value <- sum(disease_breakdown$Econ_Value_USD)

  # --- FISCAL REVENUE CHANGE ---
  old_revenue <- sum(market_base$Sales_Volume_Liters * market_base$Price_Per_Liter * market_base$tax_er_rate)
  new_revenue <- sum(market_new$New_Sales_Volume * market_base$Price_Per_Liter * tax_inc_rate)
  revenue_gain <- new_revenue - old_revenue

  if (verbose) {
    print("------------------------------------------------")
    print(paste("TOTAL CASES AVOIDED:    ", format(round(total_cases_avoided, 0), big.mark=",")))
    print(paste("TOTAL DEATHS AVERTED:   ", format(round(total_deaths_averted, 0), big.mark=",")))
    print(paste("TOTAL DALYs SAVED:      ", format(round(total_dalys_saved, 1), big.mark=",")))
    print(paste("TOTAL ECON VALUE (USD): $", format(round(total_econ_value, 0), big.mark=",")))
    print(paste("GOVT REVENUE GAIN:      ", format(round(revenue_gain, 0), big.mark=",")))
    print("------------------------------------------------")
  }
  
  # Return EVERYTHING safely!
  return(list(
    Detailed_Health = merged_data,          
    Disease_Breakdown = disease_breakdown, 
    Age_Breakdown = age_breakdown,          
    Total_DALYs_Saved = total_dalys_saved,  
    Revenue_Gain = revenue_gain, 
    Baseline_Revenue = old_revenue, 
    New_Revenue = new_revenue
  ))
}

