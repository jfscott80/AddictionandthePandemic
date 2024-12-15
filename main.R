setwd("C:/Users/johnf/Desktop/Fall24/4151/Project/analysis")

# Load all necessary scripts
source("setup.R")
source("functions.R")
source("data_processing.R")
source("analysis.R")
source("visualizations.R")

# Process all data
data <- list()
data$budget <- process_budget_data()
data$dose <- process_dose_data()
data$mortality <- process_mortality_data()
# data$teds <- list(
#   y2018 = teds_2018,
#   y2019 = teds_2019,
#   y2020 = teds_2020,
#   y2021 = teds_2021)

# Run analyses
results <- run_full_analysis(data$dose, data$mortality, data$budget)

# Create visualizations
plots <- list(
  mortality = create_mortality_summaries(data$mortality),
  dose = create_dose_trend_plots(results),
  budget = create_budget_visualizations(data$budget),
  # teds <- create_teds_visualizations()
)
plots


