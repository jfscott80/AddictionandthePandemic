# helper functions
source("setup.R")

#===============================================================================
# Function to read specific year with consistent columns
read_mortality_year <- function(year) {
  filename <- paste0(dir.cdc, "Mort", year, "US.PubUse.csv")
  
  data <- read_csv(filename,
                   col_select = c(
                     "educ2003", "monthdth", "sex", "ager27",
                     "placdth", "marstat", "year", "mandeath", "ucod"
                   ))
  
  return(data)
}

# Create labeling functions for each categorical variable
label_sex <- function(data) {
  data %>% 
    mutate(sex_label = case_when(
      sex == "M" ~ "Male",
      sex == "F" ~ "Female",
      TRUE ~ "Unknown"
    ))
}

label_marital <- function(data) {
  data %>%
    mutate(marital_label = case_when(
      marstat == "S" ~ "Single",
      marstat == "M" ~ "Married",
      marstat == "W" ~ "Widowed",
      marstat == "D" ~ "Divorced",
      marstat == "U" ~ "Unknown",
      TRUE ~ "Not Stated"
    ))
}

label_education <- function(data) {
  data %>%
    mutate(education_label = case_when(
      educ2003 == "1" ~ "8th grade or less",
      educ2003 == "2" ~ "9-12th grade, no diploma",
      educ2003 == "3" ~ "High school graduate or GED",
      educ2003 == "4" ~ "Some college, no degree",
      educ2003 == "5" ~ "Associate degree",
      educ2003 == "6" ~ "Bachelor's degree",
      educ2003 == "7" ~ "Master's degree",
      educ2003 == "8" ~ "Doctorate or professional degree",
      educ2003 == "9" ~ "Unknown",
      TRUE ~ "Not Stated"
    ))
}

label_place <- function(data) {
  data %>%
    mutate(place_label = case_when(
      placdth == "1" ~ "Hospital, inpatient",
      placdth == "2" ~ "Hospital, outpatient/ER",
      placdth == "3" ~ "Hospital, Dead on arrival",
      placdth == "4" ~ "Home",
      placdth == "5" ~ "Hospice facility",
      placdth == "6" ~ "Nursing home/long term care",
      placdth == "7" ~ "Other",
      placdth == "9" ~ "Unknown",
      TRUE ~ "Not Stated"
    ))
}

label_manner <- function(data) {
  data %>%
    mutate(manner_label = case_when(
      mandeath == "1" ~ "Natural",
      mandeath == "2" ~ "Accident",
      mandeath == "3" ~ "Suicide",
      mandeath == "4" ~ "Homicide",
      mandeath == "5" ~ "Pending investigation",
      mandeath == "6" ~ "Self-inflicted",
      mandeath == "7" ~ "Could not determine",
      TRUE ~ "Not Stated"
    ))
}

# Create a function that applies all labels
add_all_labels <- function(data) {
  data %>%
    label_sex() %>%
    label_marital() %>%
    label_education() %>%
    label_place() %>%
    label_manner()
}

clean_mortality_data <- function(data) {
  data %>%
    # Add labels first
    add_all_labels() %>%
    
    # Convert numeric fields
    mutate(
      monthdth = as.integer(monthdth),
      year = as.integer(year),
      ager27 = as.integer(ager27)
    ) %>%
    
    # Add age groups
    mutate(
      age_group = case_when(
        ager27 <= 5 ~ "0-24",
        ager27 <= 10 ~ "25-44",
        ager27 <= 15 ~ "45-64",
        ager27 <= 20 ~ "65-84",
        ager27 > 20 ~ "85+",
        TRUE ~ "Unknown"
      )
    )
}

label_substance_codes <- function(data) {
  data %>%
    mutate(substance_label = case_when(
      str_detect(ucod, "^F10") ~ "Alcohol",
      str_detect(ucod, "^F11") ~ "Opioids",
      str_detect(ucod, "^F12") ~ "Cannabis",
      str_detect(ucod, "^F13") ~ "Sedatives/Hypnotics",
      str_detect(ucod, "^F14") ~ "Cocaine",
      str_detect(ucod, "^F15") ~ "Other Stimulants",
      str_detect(ucod, "^F16") ~ "Hallucinogens",
      str_detect(ucod, "^F17") ~ "Tobacco",
      str_detect(ucod, "^F18") ~ "Solvents",
      str_detect(ucod, "^F19") ~ "Multiple/Other Substances",
      TRUE ~ "Other"
    ))
}

# Function to create summary visualizations
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
