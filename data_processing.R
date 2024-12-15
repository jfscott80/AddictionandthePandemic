# build each dataset

source("setup.R")
source("functions.R")

# 1. Process Budget Data
#===============================================================================
# Funding for substance abuse prevention, treatment in TN
# https://www.tn.gov/transparenttn/state-financial-overview/interactive-budget/state-budget.html

process_budget_data <- function() {
  TN.FundingData <- read_excel(file.path(dir.budget, "2025BudgetPositionsData.xlsx"), 
                               sheet = "Data")
  
  TN.FundingData.Clean <- TN.FundingData %>%
    filter(
      `FiscalYear` >= 2018 & `FiscalYear` <= 2022,
      `DeptCode` >= 33900 & `DeptCode` <= 33940,
      `Type` == "Funding"
    ) %>%
    select(
      `FiscalYear`, `AllotName`, `Category`, `Amount`
    )
  
  return(TN.FundingData.Clean)
}

# 2. Process Dose Data
#===============================================================================
# Tennessee State Overdose Data
# https://www.tn.gov/content/tn/health/health-program-areas/pdo/pdo/data-dashboard.html#downloadabledata

process_dose_data <- function() {
  # Fatal OD data
  Fatal.TN.OD <- read_excel(file.path(dir.tn, "FatalOD_Downloadable_Data_2013_2022.xlsx"), 
                            sheet = "Fatal_Data_2013_2022")
  Fatal.TN.OD.clean <- Fatal.TN.OD %>%
    filter(`Geography Type` == "State",
           `Year` >= 2018 & `Year` <= 2022) %>%
    select(`Year`, `Indicator`, `Value`, `Value Type`)
  
  # Nonfatal OD data
  Nonfatal.TN.OD <- read_excel(file.path(dir.tn, "NonfatalOD_Downloadable_Data_2013_2022.xlsx"), 
                               sheet = "Nonfatal_Data_2013_2022")
  Nonfatal.TN.OD.clean <- Nonfatal.TN.OD %>%
    filter(`Geography Type` == "State",
           `Year` >= 2018 & `Year` <= 2022) %>%
    select(`Year`, `Indicator`, `Value`, `Value Type`)
  
  # DOSE dashboard data
  DOSE_dashboard <- read_excel(file.path(dir.tn, "DOSE_dashboard.xlsx"), 
                               sheet = "DOSE_dashboard_output-download")
  
  # Process DOSE dashboard data
  DOSE <- DOSE_dashboard %>%
    filter(row_number() > 5) %>%  # Skip first 5 filler rows
    select(everything())
  
  # Set column names from first row
  colnames(DOSE) <- DOSE[1,]
  
  # Initial cleaning
  DOSE <- DOSE %>% 
    select(-geo, -stateSymbol, -dataSymbol)
  
  # Filter years and age ranges, remove significance columns
  DOSE <- DOSE %>%
    filter(startYear < 2023,
           ageRange != "0-14") %>%
    select(-ends_with("IsSignificant"), 
           -ends_with("Significance"))
  
  # Remove suppressed values
  DOSE.clean <- DOSE %>%
    filter(across(ends_with("PercentageChange"), 
                  ~. != "suppressed"))
  
  # Create long format
  DOSE_long <- DOSE.clean %>%
    pivot_longer(
      cols = ends_with("PercentageChange"),
      names_to = "substance",
      values_to = "percentage_change"
    ) %>%
    mutate(
      date = as.Date(paste(startYear, startMonth, "01", sep="-")),
      data_year = as.numeric(startYear),
      substance = gsub("PercentageChange", "", substance),
      covid_period = factor(
        ifelse(date < as.Date("2020-03-01"), 
                            "Pre_COVID", "During_COVID"),
        levels = c("Pre_COVID", "During_COVID")
    )
  )
  
  # Return all processed datasets
  return(list(
    fatal = Fatal.TN.OD.clean,
    nonfatal = Nonfatal.TN.OD.clean,
    dose_raw = DOSE.clean,
    dose_long = DOSE_long
  ))
}

# 3. Process CDC Wonder Data
#===============================================================================
# CDC Wonder: Overdose deaths at national, state levels
# https://www.cdc.gov/overdose-prevention/data-research/facts-stats/sudors-dashboard-fatal-overdose-data-accessible.html

# Read and combine all years
process_mortality_data <- function(years = 2018:2021) {
  # Initialize empty list to store each year's data
  mortality_list <- list()

  # Read and process each year
  for (year in years) {
    message(paste("Processing year:", year))

    # Read the year's data using your existing function
    yearly_data <- read_mortality_year(year)

    # Apply your existing filtering criteria
    yearly_data_filtered <- yearly_data %>%
      filter(str_detect(ucod, "^F1[0-6]|^F19")) %>%
      filter(!ager27 %in% c(1:8, 23:26)) %>%
      filter(!placdth %in% c("5", "6")) %>%
      mutate(
        substance_label = case_when(
          str_detect(ucod, "^F10") ~ "Alcohol",
          str_detect(ucod, "^F11") ~ "Opioids",
          str_detect(ucod, "^F14") ~ "Cocaine",
          str_detect(ucod, "^F15") ~ "Other Stimulants",
          TRUE ~ "Multiple/Other Substances"
      ),
      covid_period = factor(
        ifelse(year < 2020 | (year == 2020 & monthdth < 3),
                            "Pre_COVID", "During_COVID"),
        levels = c("Pre_COVID", "During_COVID")
  )
)
    # Clean the data using your existing function
    yearly_data_cleaned <- clean_mortality_data(yearly_data_filtered)

    # Store in list
    mortality_list[[as.character(year)]] <- yearly_data_cleaned
  }

  # Combine all years into one dataset
  mortality_all_years <- bind_rows(mortality_list, .id = "data_year")

  return(mortality_all_years)
}