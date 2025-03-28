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

data <- data_m[-c(15,12,5,6)]

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
# 4. Clean and Prepare Data Using map2()
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

# -------------------------------
# Load Financial Development Data
# -------------------------------

# Financial Development Data for 2021
financial_2021 <- 
  data_m$`financial-development-Aug-2022` %>%
  filter(!is.na(country)) %>%
  mutate(across(where(is.character), as_factor)) %>%
  select(country:year)

# Financial Development Data for 2022
financial_2022 <- 
  data_m$`financial-development-Nov-2021` %>%
  filter(!is.na(country)) %>%
  mutate(across(where(is.character), as_factor)) %>%
  select(country:year)

# -------------------------------
# Load and Clean WGI Data
# -------------------------------

wgi <- data_m$wgi %>%
  mutate(
    year = factor(year),
    estimate = suppressWarnings(as.numeric(str_replace_all(estimate, "[^0-9.-]", ""))),
    stddev = suppressWarnings(as.numeric(str_replace_all(stddev, "[^0-9.-]", ""))),
    nsource = suppressWarnings(as.numeric(str_replace_all(nsource, "[^0-9.-]", ""))),
    pctrank = suppressWarnings(as.numeric(str_replace_all(pctrank, "[^0-9.-]", ""))),
    pctranklower = suppressWarnings(as.numeric(str_replace_all(pctranklower, "[^0-9.-]", ""))),
    pctrankupper = suppressWarnings(as.numeric(str_replace_all(pctrankupper, "[^0-9.-]", "")))
  ) %>%
  filter(!is.na(estimate)) %>%
  mutate(across(where(is.character), as_factor)) %>%
  select(country = countryname, year:pctrankupper)

# ---------------------------------
# swiid
# ---------------------------------

swiid <- 
  data_m$swiid_source %>%
  filter(!is.na(monetary)) %>%
  mutate(across(where(is.character), as_factor)) %>%
  select(-c(gini_se,series:link))
