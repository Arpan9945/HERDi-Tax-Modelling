# ==============================================================================
# PHACES MODEL - Baseline Descriptives
# File: baseline_descriptives.R
#
# Generates a comprehensive baseline profile for the report.
# Run AFTER engine.R — requires these objects in your environment:
#   df_market, df_consumption, df_population,
#   disease_catalog_final, new_market_state, elasticity_matrix
#
# Exports: Baseline_Descriptives.xlsx
# ==============================================================================

library(dplyr)
library(tidyr)
library(openxlsx)

message(">> Building Baseline Descriptives...")

# ==============================================================================
# TABLE 1: MARKET COMPOSITION (Baseline)
# ==============================================================================

market_summary <- df_market %>%
  mutate(
    Market_Share_Pct     = round(Sales_Volume_Liters / sum(Sales_Volume_Liters) * 100, 1),
    Total_Revenue_Baseline = round(Sales_Volume_Liters * Price_Per_Liter * tax_er_rate, 0)
  ) %>%
  select(
    Category,
    Price_Per_Liter,
    Sales_Volume_Liters,
    Market_Share_Pct,
    tax_er_rate,
    Pass_Through_Rate,
    Energy_kcal_per_100mL,
    Total_Revenue_Baseline
  ) %>%
  rename(
    `Beverage Category`        = Category,
    `Price (NPR/Liter)`        = Price_Per_Liter,
    `Sales Volume (Liters)`    = Sales_Volume_Liters,
    `Market Share (%)`         = Market_Share_Pct,
    `Existing Tax Rate`        = tax_er_rate,
    `Pass-Through Rate`        = Pass_Through_Rate,
    `Energy Density (kcal/100mL)` = Energy_kcal_per_100mL,
    `Baseline Tax Revenue (NPR)` = Total_Revenue_Baseline
  )

message("  -> Table 1: Market Composition done")

# ==============================================================================
# TABLE 2: POST-TAX MARKET IMPACT (20% Tax, 100% Pass-Through)
# Uses new_market_state already computed in engine.R
# ==============================================================================

post_tax_summary <- new_market_state %>%
  mutate(
    Volume_Change_Liters = New_Sales_Volume - Sales_Volume_Liters,
    Volume_Change_Pct    = round(Demand_Change_Pct * 100, 2),
    Price_Change_NPR     = round(New_Price - Price_Per_Liter, 2),
    Price_Change_Pct_    = round(Price_Change_Pct * 100, 2)
  ) %>%
  select(
    Category,
    Price_Per_Liter,
    New_Price,
    Price_Change_NPR,
    Price_Change_Pct_,
    Sales_Volume_Liters,
    New_Sales_Volume,
    Volume_Change_Liters,
    Volume_Change_Pct
  ) %>%
  rename(
    `Beverage Category`          = Category,
    `Baseline Price (NPR/Liter)` = Price_Per_Liter,
    `Post-Tax Price (NPR/Liter)` = New_Price,
    `Price Increase (NPR)`       = Price_Change_NPR,
    `Price Change (%)`           = Price_Change_Pct_,
    `Baseline Volume (Liters)`   = Sales_Volume_Liters,
    `Post-Tax Volume (Liters)`   = New_Sales_Volume,
    `Volume Change (Liters)`     = Volume_Change_Liters,
    `Volume Change (%)`          = Volume_Change_Pct
  )

message("  -> Table 2: Post-Tax Market Impact done")

# ==============================================================================
# TABLE 3: CONSUMPTION PROFILE BY AGE-SEX GROUP
# Average daily mL per person across all beverages
# ==============================================================================

consumption_summary <- df_consumption %>%
  mutate(
    Total_Daily_mL = rowSums(select(., ends_with("_mL_day")), na.rm = TRUE),
    Total_Daily_kcal = 
      (Carbonated_drinks_mL_day * df_market$Energy_kcal_per_100mL[df_market$Category == "Carbonated_drinks"] / 100) +
      (Juice_mL_day            * df_market$Energy_kcal_per_100mL[df_market$Category == "Juice"] / 100) +
      (`Milk_Sweetened_mL_day` * df_market$Energy_kcal_per_100mL[df_market$Category == "Milk_Sweetened"] / 100) +
      (Energy_drinks_mL_day    * df_market$Energy_kcal_per_100mL[df_market$Category == "Energy_drinks"] / 100)
  ) %>%
  select(`Age-Group`, Sex, everything()) %>%
  rename(
    `Age Group`                      = `Age-Group`,
    `Carbonated Drinks (mL/day)`     = Carbonated_drinks_mL_day,
    `Juice (mL/day)`                 = Juice_mL_day,
    `Milk_Sweetened (mL/day)`        = `Milk_Sweetened_mL_day`,
    `Energy Drinks (mL/day)`         = Energy_drinks_mL_day,
    `Total SSB (mL/day)`             = Total_Daily_mL,
    `Total SSB Energy (kcal/day)`    = Total_Daily_kcal
  )

message("  -> Table 3: Consumption Profile done")

# ==============================================================================
# TABLE 4: POPULATION STRUCTURE
# ==============================================================================

population_summary <- df_population %>%
  rename(
    `Age Group`          = `Age-Group`,
    `Population Count`   = Population_Count,
    `Mean BMI`           = Mean_BMI,
    `Mean Height (m)`    = Mean_Height_m
  ) %>%
  arrange(`Age Group`, Sex)

# Add totals row
pop_totals <- data.frame(
  `Age Group`        = "TOTAL",
  Sex                = "",
  `Population Count` = sum(df_population$Population_Count),
  `Mean BMI`         = NA,
  `Mean Height (m)`  = NA,
  check.names        = FALSE
)
population_summary <- bind_rows(population_summary, pop_totals)

message("  -> Table 4: Population Structure done")

# ==============================================================================
# TABLE 5: DISEASE BASELINE BURDEN
# Current annual cases, deaths, and DALYs lost in Nepal
# This is the "before tax" disease burden — context for DALYs saved
# ==============================================================================

# Get the GBD data for Number metrics only
disease_catalog_clean <- disease_catalog_final %>%
  filter(Sex != "Both", !grepl("70\\+", `Age-Group`), Disease_Name != "All causes")

# Merge with population to compute current annual burden
disease_burden <- left_join(
  disease_catalog_clean,
  df_population,
  by = c("Age-Group", "Sex")
) %>%
  mutate(
    Annual_New_Cases   = round(Population_Count * Incidence_Base, 0),
    Annual_Deaths      = round(Annual_New_Cases * CFR, 0),
    Annual_DALYs_Lost  = round(Annual_New_Cases * DALYs_per_Case, 1)
  )

# Summary by disease
disease_burden_summary <- disease_burden %>%
  group_by(Disease_Name) %>%
  summarise(
    Annual_New_Cases  = sum(Annual_New_Cases,  na.rm = TRUE),
    Annual_Deaths     = sum(Annual_Deaths,     na.rm = TRUE),
    Annual_DALYs_Lost = sum(Annual_DALYs_Lost, na.rm = TRUE)
  ) %>%
  rename(
    Disease                      = Disease_Name,
    `Annual Incident Cases`      = Annual_New_Cases,
    `Annual Deaths`              = Annual_Deaths,
    `Annual DALYs Lost`          = Annual_DALYs_Lost
  ) %>%
  as.data.frame()

# Age-stratified burden for diabetes specifically (most important disease)
diabetes_by_age <- disease_burden %>%
  filter(Disease_Name == "Diabetes mellitus type 2") %>%
  group_by(`Age-Group`, Sex) %>%
  summarise(
    Incidence_Rate_per_100k = round(Incidence_Base * 100000, 1),
    Annual_New_Cases        = sum(Annual_New_Cases, na.rm = TRUE),
    Annual_DALYs_Lost       = sum(Annual_DALYs_Lost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(`Age-Group`, Sex) %>%
  rename(
    `Age Group`                    = `Age-Group`,
    `Incidence Rate (per 100,000)` = Incidence_Rate_per_100k,
    `Annual New Cases`             = Annual_New_Cases,
    `Annual DALYs Lost`            = Annual_DALYs_Lost
  )

message("  -> Table 5: Disease Baseline Burden done")

# ==============================================================================
# TABLE 6: ELASTICITY MATRIX
# ==============================================================================

elasticity_df <- as.data.frame(elasticity_matrix)
elasticity_df <- cbind(`Price Change In →` = rownames(elasticity_df), elasticity_df)
rownames(elasticity_df) <- NULL

message("  -> Table 6: Elasticity Matrix done")

# ==============================================================================
# CONSOLE SUMMARY PRINT
# ==============================================================================

cat("\n======================================================\n")
cat("  BASELINE DESCRIPTIVES SUMMARY\n")
cat("======================================================\n")

cat("\nMARKET COMPOSITION:\n")
cat(paste("  Total SSB Market Volume:", format(sum(df_market$Sales_Volume_Liters), big.mark=","), "Liters\n"))
cat(paste("  Total Baseline Tax Revenue: NPR", format(round(sum(df_market$Sales_Volume_Liters * df_market$Price_Per_Liter * df_market$tax_er_rate), 0), big.mark=","), "\n"))
cat(paste("  Dominant Category:", df_market$Category[which.max(df_market$Sales_Volume_Liters)], "\n"))

cat("\nPOPULATION:\n")
cat(paste("  Total Population Covered:", format(sum(df_population$Population_Count), big.mark=","), "\n"))
cat(paste("  Age Groups:", length(unique(df_population$`Age-Group`)), "\n"))

cat("\nDISEASE BURDEN (Current Annual):\n")
for(i in 1:nrow(disease_burden_summary)) {
  cat(paste0("  ", disease_burden_summary$Disease[i], ":\n"))
  cat(paste0("    New Cases/Year: ", format(disease_burden_summary$`Annual Incident Cases`[i], big.mark=","), "\n"))
  cat(paste0("    Deaths/Year:    ", format(disease_burden_summary$`Annual Deaths`[i], big.mark=","), "\n"))
  cat(paste0("    DALYs Lost/Year:", format(round(disease_burden_summary$`Annual DALYs Lost`[i], 0), big.mark=","), "\n"))
}
cat("======================================================\n")

# ==============================================================================
# EXPORT TO EXCEL
# ==============================================================================

wb <- createWorkbook()

# --- Sheet 1: Market Composition ---
addWorksheet(wb, "1_Market_Composition")
writeData(wb, "1_Market_Composition", market_summary)
setColWidths(wb, "1_Market_Composition", cols = 1:ncol(market_summary), widths = "auto")

# --- Sheet 2: Post-Tax Market Impact ---
addWorksheet(wb, "2_PostTax_Market")
writeData(wb, "2_PostTax_Market", post_tax_summary)
setColWidths(wb, "2_PostTax_Market", cols = 1:ncol(post_tax_summary), widths = "auto")

# --- Sheet 3: Consumption Profile ---
addWorksheet(wb, "3_Consumption_Profile")
writeData(wb, "3_Consumption_Profile", consumption_summary)
setColWidths(wb, "3_Consumption_Profile", cols = 1:ncol(consumption_summary), widths = "auto")

# --- Sheet 4: Population Structure ---
addWorksheet(wb, "4_Population_Structure")
writeData(wb, "4_Population_Structure", population_summary)
setColWidths(wb, "4_Population_Structure", cols = 1:ncol(population_summary), widths = "auto")

# --- Sheet 5: Disease Burden Summary ---
addWorksheet(wb, "5_Disease_Burden_Summary")
writeData(wb, "5_Disease_Burden_Summary", disease_burden_summary)
setColWidths(wb, "5_Disease_Burden_Summary", cols = 1:ncol(disease_burden_summary), widths = "auto")

# --- Sheet 6: Diabetes Age-Stratified ---
addWorksheet(wb, "6_Diabetes_By_Age")
writeData(wb, "6_Diabetes_By_Age", diabetes_by_age)
setColWidths(wb, "6_Diabetes_By_Age", cols = 1:ncol(diabetes_by_age), widths = "auto")

# --- Sheet 7: Elasticity Matrix ---
addWorksheet(wb, "7_Elasticity_Matrix")
writeData(wb, "7_Elasticity_Matrix", elasticity_df)
setColWidths(wb, "7_Elasticity_Matrix", cols = 1:ncol(elasticity_df), widths = "auto")

saveWorkbook(wb, "Baseline_Descriptives.xlsx", overwrite = TRUE)
message(">> Exported: Baseline_Descriptives.xlsx")
message(">> All baseline tables ready.")