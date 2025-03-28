# -------------------------------
# 1. Load Required Libraries
# -------------------------------
library(tidyverse)
library(visdat) # For visualizing missing values

# -------------------------------
# 2. Specify Folder Path and Read CSVs
# -------------------------------
folder_path <- "data/"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read all CSVs into a list of data frames
data_m <- map(csv_files, read_csv)

# Assign names to the list based on file names (without extensions)
names(data_m) <- tools::file_path_sans_ext(basename(csv_files))

data <- data_m[-c(15,12,7,6)]

# -------------------------------
# 3. Data Cleaning Functions
# -------------------------------
clean_data <- function(df, value_col) {
  df %>%
    # Replace ".." with NA
    mutate(across(starts_with("19") | starts_with("20"), ~na_if(., ".."))) %>%
    # Remove brackets from year column names
    rename_with(~ gsub("\\[.*\\]", "", .)) %>%
    # Reshape to long format
    pivot_longer(
      cols = starts_with("19") | starts_with("20"),
      names_to = "Year",
      values_to = value_col
    ) %>%
    # Convert value column to numeric and clean column names
    mutate(
      !!value_col := as.numeric(.data[[value_col]]),
      year = factor(Year),  # Convert Year to factor
      country = `Country Name`,
      country_code = `Country Code`
    ) %>%
    # Remove rows with missing values
    filter(!is.na(.data[[value_col]])) %>%
    select(country, country_code, year, !!value_col)
}

# -------------------------------
# 4. Clean and Prepare Data Using map()
# -------------------------------
cleaned_data <- map2(data, 
                     c("CO2_Emission", 
                       "energy_consumption",
                       "energy_intensity",
                       "gdp",
                       "fdi",
                       "fosil_fuel",
                       "gini",
                       "hci",
                       "lft",
                       "trade",
                       "urban"), clean_data)