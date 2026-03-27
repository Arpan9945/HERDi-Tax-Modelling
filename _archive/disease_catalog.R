library(dplyr)
library(tidyr)
library(stringr)

# 1. Load the raw GBD data
gbd_raw <- read.csv("IHME-GBD_2023_DATA-0547bf71-1.csv", stringsAsFactors = FALSE)

# 2. Filter and reshape the data
clean_gbd <- gbd_raw %>%
  # Create a combined column to identify what the value is (e.g., "Incidence_Rate")
  mutate(Measure_Metric = paste(measure_name, metric_name, sep = "_")) %>%
  
  # Keep only the columns we need
  select(cause_name, age_name, sex_name, Measure_Metric, val) %>%
  
  # Pivot the data so Incidence, Prevalence, Deaths, and DALYs become their own columns
  pivot_wider(names_from = Measure_Metric, values_from = val)

# 3. Calculate your model variables!
disease_catalog_age <- clean_gbd %>%
  mutate(
    # A. Baseline Incidence (GBD gives rate per 100,000)
    Incidence_Base = Incidence_Rate / 100000,
    
    # B. Case Fatality Rate (Deaths / Prevalence)
    CFR = Deaths_Number / Prevalence_Number,
    
    # C. DALYs per Case (Total DALYs / Prevalence)
    # Note: Check your exact column name for DALYs. GBD usually names it like below:
    DALYs_per_Case = `DALYs (Disability-Adjusted Life Years)_Number` / Prevalence_Number
  ) %>%
  
  # 4. Clean up names to match your existing model
  rename(
    Disease_Name = cause_name,
    Sex = sex_name
  ) %>%
  
  # 5. Format the Age-Group string to match your df_population (e.g., "20 to 24" -> "20 - 24")
  mutate(
    `Age-Group` = str_replace(age_name, " to ", "-"),
    `Age-Group` = str_replace(`Age-Group`, " years", "")
  )

# 6. Add the Relative Risk (RR_per_BMI) 
# We add this manually since it comes from the GHDx Risk file, not the Results tool.
# (These are the values converted to a 1-unit BMI increase)
disease_catalog_age <- disease_catalog_age %>%
  mutate(
    RR_per_BMI = case_when(
      Disease_Name == "Diabetes mellitus type 2" ~ 1.18,
      Disease_Name == "Ischemic heart disease" ~ 1.07,
      Disease_Name == "Stroke" ~ 1.05,
      TRUE ~ 1.0
    )
  )

# Keep only the final columns you need for the model
disease_catalog_final <- disease_catalog_age %>%
  select(Disease_Name, `Age-Group`, Sex, RR_per_BMI, Incidence_Base, CFR, DALYs_per_Case)

# View the first few rows of your shiny new, highly accurate catalog!
head(disease_catalog_final)