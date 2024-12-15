
source("setup.R")
source("functions.R")

#===============================================================================
# DOSE Data Analysis Functions
#===============================================================================

analyze_dose_trends <- function(dose_long_data) {
  # Basic summary statistics by substance, state, and COVID period
  summary_stats <- dose_long_data %>%
    filter(state %in% c("TN", "US")) %>%
    mutate(percentage_change = as.numeric(percentage_change)) %>%
    group_by(substance, state, covid_period) %>%
    summarise(
      mean_change = mean(percentage_change, na.rm = TRUE),
      sd_change = sd(percentage_change, na.rm = TRUE),
      n_observations = n(),
      .groups = 'drop'
    ) %>%
    arrange(substance, state) %>%
    mutate(
      mean_change = round(mean_change, 2),
      sd_change = round(sd_change, 2)
    )
  
  # Tennessee-specific analysis
  tn_analysis <- dose_long_data %>%
    filter(state == "TN",  
           substance %in% c("opioid", "stimulant", "heroin", "fentanyl")) %>%
    mutate(
      percentage_change = as.numeric(percentage_change),
      year = as.numeric(format(date, "%Y"))
    ) %>%
    group_by(year, substance) %>%
    summarise(
      avg_change = mean(percentage_change, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Period comparison for Tennessee
  period_comparison <- dose_long_data %>%
    filter(state == "TN") %>%
    mutate(percentage_change = as.numeric(percentage_change)) %>%
    group_by(substance, covid_period) %>%
    summarise(
      avg_nonfatal_change = mean(percentage_change, na.rm = TRUE),
      n_observations = n(),
      .groups = 'drop'
    ) %>%
    mutate(across(where(is.numeric), ~round(., 2)))
  
  # Return all analyses in a list
  return(list(
    summary = summary_stats,
    tn_trends = tn_analysis,
    period_comparison = period_comparison
  ))
}

#===============================================================================
# Mortality Analysis Functions
#===============================================================================

analyze_mortality_trends <- function(mortality_data) {
  mortality_summary <- mortality_data %>%
    mutate(data_year = as.numeric(data_year)) %>%
    group_by(data_year, covid_period) %>%
    summarise(
      fatal_count = n(),
      .groups = 'drop'
    )
  
  return(mortality_summary)
}

#===============================================================================
# Combined Analysis Functions
#===============================================================================

create_comparison_analysis <- function(dose_long_data, mortality_data) {
  # Analyze DOSE trends for TN
  dose_trends <- dose_long_data %>%
    filter(state == "TN",  
           substance %in% c("opioid", "stimulant", "heroin", "fentanyl")) %>%
    # mutate(data_year = as.numeric(format(date, "%Y"))) %>%
    group_by(data_year, substance) %>%
    summarise(
      avg_nonfatal_change = mean(as.numeric(percentage_change), na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Get mortality trends
  mortality_trends <- analyze_mortality_trends(mortality_data)
  
  # Add diagnostic prints
  print("Dose trends data_year type:")
  print(class(dose_trends$data_year))
  print("Mortality trends data_year type:")
  print(class(mortality_trends$data_year))
  
  # Ensure both are numeric
  dose_trends <- dose_trends %>% mutate(data_year = as.numeric(data_year))
  mortality_trends <- mortality_trends %>% mutate(data_year = as.numeric(data_year))
  
  # Combine datasets
  comparison_data <- mortality_trends %>%
    left_join(dose_trends,
              by = "data_year",
              relationship = "many-to-many") %>%
    na.omit()
  
  return(comparison_data)
}

#===============================================================================
# Statistical Analysis Functions 
#===============================================================================

calculate_correlations <- function(comparison_data) {
  correlations <- comparison_data %>%
    group_by(substance) %>%
    summarise(
      correlation = cor(fatal_count, avg_nonfatal_change),
      p_value = cor.test(fatal_count, avg_nonfatal_change)$p.value,
      .groups = 'drop'
    ) %>%
    mutate(
      correlation = round(correlation, 3),
      p_value = round(p_value, 3)
    )
  
  return(correlations)
}

analyze_covid_impact <- function(comparison_data) {
  # Period comparisons
  period_comparison <- comparison_data %>%
    group_by(substance, covid_period) %>%
    summarise(
      avg_fatal = mean(fatal_count, na.rm = TRUE),
      avg_nonfatal_change = mean(avg_nonfatal_change, na.rm = TRUE),
      n_observations = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      across(where(is.numeric), round, 2)
    )
  
  return(period_comparison)
}
#===============================================================================
# Budget Analysis Functions
#===============================================================================

analyze_budget_trends <- function(budget_data) {
  # Overall funding trends by category
  category_summary <- budget_data %>%
    group_by(FiscalYear, Category) %>%
    summarise(
      TotalAmount = sum(Amount),
      PercentOfYear = TotalAmount / sum(TotalAmount) * 100,
      .groups = 'drop'
    ) %>%
    mutate(
      TotalAmount = round(TotalAmount/1e6, 2),  # Convert to millions
      PercentOfYear = round(PercentOfYear, 1)
    )
  
  # Program-specific analysis
  program_summary <- budget_data %>%
    group_by(FiscalYear, AllotName) %>%
    summarise(
      TotalAmount = sum(Amount),
      NumCategories = n_distinct(Category),
      .groups = 'drop'
    ) %>%
    mutate(TotalAmount = round(TotalAmount/1e6, 2))  # Convert to millions
  
  # Year-over-year changes
  yoy_changes <- budget_data %>%
    group_by(Category) %>%
    arrange(FiscalYear) %>%
    summarise(
      YearlyChanges = list(diff(sum(Amount))/1e6),
      PercentChanges = list(diff(sum(Amount))/lag(sum(Amount)) * 100),
      .groups = 'drop'
    ) %>%
    mutate(
      AvgYearlyChange = round(sapply(YearlyChanges, mean), 2),
      AvgPercentChange = round(sapply(PercentChanges, mean), 1)
    ) %>%
    select(Category, AvgYearlyChange, AvgPercentChange)
  
  # Opioid-specific funding analysis NOT USEFUL CURRENTLY
  opioid_analysis <- budget_data %>%
    filter(grepl("Opioid", AllotName)) %>%
    group_by(FiscalYear, Category) %>%
    summarise(
      TotalAmount = sum(Amount)/1e6,
      PercentOfCategory = TotalAmount / 
        (sum(budget_data$Amount[budget_data$FiscalYear == first(FiscalYear) & 
                                  budget_data$Category == first(Category)])/1e6) * 100,
      .groups = 'drop'
    ) %>%
    mutate(
      TotalAmount = round(TotalAmount, 2),
      PercentOfCategory = round(PercentOfCategory, 1)
    )
  
  # Return all analyses in a list
  return(list(
    category_trends = category_summary,
    program_funding = program_summary,
    funding_changes = yoy_changes,
    opioid_funding = opioid_analysis
  ))
}


run_full_analysis <- function(dose_data, mortality_data, budget_data) {
  
  dose_results <- analyze_dose_trends(dose_data$dose_long)
  mortality_summary <- analyze_mortality_trends(mortality_data)
  comparison_data <- create_comparison_analysis(dose_data$dose_long, mortality_data)
  correlations <- calculate_correlations(comparison_data)
  covid_impact <- analyze_covid_impact(comparison_data)
  budget_results <- analyze_budget_trends(budget_data)
  
  # Return all results in a list
  return(list(
    
    dose_summary = dose_results$summary,
    dose_tn_trends = dose_results$tn_trends,
    dose_period_comparison = dose_results$period_comparison,
    mortality_summary = mortality_summary,
    comparison_data = comparison_data,
    correlations = correlations,
    covid_impact = covid_impact,
    
    budget_summary = budget_results$category_trends,
    budget_programs = budget_results$program_funding,
    budget_changes = budget_results$funding_changes,
    budget_opioid = budget_results$opioid_funding # not helpful currently
  ))
}