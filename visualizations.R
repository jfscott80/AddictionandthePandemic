# visualizations.R
source("setup.R")
source("functions.R")
source("analysis.R")
load("processed_data.RData")
# load("processed_teds.RData")

#===============================================================================
# CDC Wonder visualization function
#===============================================================================
create_mortality_summaries <- function(combined_data) {
  # Deaths by year and substance
  yearly_substance_plot <- combined_data %>%
    group_by(data_year, substance_label) %>%
    summarise(
      deaths = n(),
      .groups = 'drop'
    ) %>%
    ggplot(aes(x = data_year, y = deaths, fill = substance_label)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Substance-Related Deaths by Year",
      x = "Year",
      y = "Number of Deaths",
      fill = "Substance"
    ) +
    theme_minimal()
  
  return(yearly_substance_plot)
}

#===============================================================================
# DOSE visualization functions
#===============================================================================
create_dose_trend_plots <- function(analysis_results) {
  # TN trends over time - WITHOUT "all"
  tn_trends_plot_no_all <- ggplot(analysis_results$dose_tn_trends %>%
                                    filter(!substance == "all") %>%  # Exclude "all"
                                    filter(substance %in% c("opioid", "stimulant", "heroin", "fentanyl")), 
                                  aes(x = year, y = avg_change, color = substance)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Tennessee Substance Use Trends (Select Substances)",
         x = "Year",
         y = "Average Change (%)",
         color = "Substance") +
    scale_color_brewer(palette = "Set2")
  
  # TN trends over time - All substances including "all"
  tn_trends_plot_all <- ggplot(analysis_results$dose_tn_trends, 
                               aes(x = year, y = avg_change, color = substance)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Tennessee Substance Use Trends (All Categories)",
         subtitle = "Including aggregate 'all' category",
         x = "Year",
         y = "Average Change (%)",
         color = "Substance") +
    scale_color_brewer(palette = "Set2")
  
  # Revised TN vs US comparison
  tn_us_comparison <- ggplot(analysis_results$dose_summary %>% 
                               filter(substance %in% c("opioid", "heroin", "fentanyl")) %>%
                               filter(abs(mean_change) < 50), 
                             aes(x = covid_period, y = mean_change, fill = state)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~substance, scales = "free_y", nrow = 1) +
    theme_minimal() +
    labs(title = "Substance Use Changes: Tennessee vs National",
         subtitle = "Comparing Pre-COVID and During-COVID Periods",
         x = "Period",
         y = "Mean Percentage Change",
         fill = "State") +
    scale_fill_brewer(palette = "Set2") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(
    tn_trends_no_all = tn_trends_plot_no_all,
    tn_trends_all = tn_trends_plot_all,
    tn_us = tn_us_comparison
  ))
}

#===============================================================================
# Budget visualization functions
#===============================================================================
create_budget_visualizations <- function(budget_data) {
  # 1. Funding by Category Over Time
  category_trends <- ggplot(budget_data %>%
                              group_by(FiscalYear, Category) %>%
                              summarise(TotalAmount = sum(Amount)/1e6, .groups = 'drop')) +
    geom_bar(aes(x = FiscalYear, y = TotalAmount, fill = Category), 
             stat = "identity", position = "stack") +
    labs(title = "Tennessee Substance Abuse Funding by Category",
         subtitle = "Annual Funding Sources (2018-2022)",
         x = "Fiscal Year",
         y = "Amount (Millions $)",
         fill = "Funding Category") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(axis.text.x = element_text(angle = 0))
  
  # 2. Allotment Distribution
  allot_distribution <- ggplot(budget_data %>%
                                 group_by(AllotName, FiscalYear) %>%
                                 summarise(TotalAmount = sum(Amount)/1e6, .groups = 'drop')) +
    geom_bar(aes(x = FiscalYear, y = TotalAmount, fill = AllotName), 
             stat = "identity", position = "stack") +
    labs(title = "Distribution of Funding Across Programs",
         subtitle = "By Fiscal Year",
         x = "Fiscal Year",
         y = "Amount (Millions $)",
         fill = "Program") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3") +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "bottom",
          legend.text = element_text(size = 8))

  # Return all plots in a list
  return(list(
    funding_sources = category_trends,
    program_distribution = allot_distribution
  ))
}

#===============================================================================
# TEDS visualization functions
#===============================================================================
create_teds_visualizations <- function() {
  # Combine TN data across years for trend analysis
  tn_combined <- bind_rows(
    teds_2018$tn,
    teds_2019$tn,
    teds_2020$tn,
    teds_2021$tn
  )
  
  # Combine US data
  us_combined <- bind_rows(
    teds_2018$us,
    teds_2019$us,
    teds_2020$us,
    teds_2021$us
  ) %>% mutate(region = "United States")
  
  # Combine both for comparison plots
  all_data <- bind_rows(tn_combined, us_combined)
  
  # 1. Treatment Services Trends
  services_trend <- all_data %>%
    group_by(data_year, services_label, region) %>%
    summarise(count = n(), .groups = 'drop') %>%
    ggplot(aes(x = data_year, y = count, color = services_label)) +
    geom_line() +
    geom_point() +
    facet_wrap(~region, scales = "free_y") +
    theme_minimal() +
    labs(title = "Treatment Service Trends",
         x = "Year",
         y = "Number of Admissions",
         color = "Service Type") +
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Set2")
  
  # 2. Comorbidity Distribution
  comorbid_plot <- all_data %>%
    ggplot(aes(x = data_year, fill = comorbid_label)) +
    geom_bar(position = "fill") +
    facet_wrap(~region, scales = "free_y") +
    theme_minimal() +
    labs(title = "Substance Use Type Distribution Over Time",
         x = "Year",
         y = "Proportion",
         fill = "Type") +
    scale_fill_brewer(palette = "Set3")
  
  # 3. Demographics Summary
  demographics_plot <- all_data %>%
    group_by(data_year, age_label, gender_label, region) %>%
    summarise(count = n(), .groups = 'drop') %>%
    ggplot(aes(x = age_label, y = count, fill = gender_label)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(region ~ data_year) +
    theme_minimal() +
    labs(title = "Treatment Admissions by Age and Gender",
         x = "Age Group",
         y = "Number of Admissions",
         fill = "Gender") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(
    services = services_trend,
    comorbidity = comorbid_plot,
    demographics = demographics_plot
  ))
}
