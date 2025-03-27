# Load necessary packages
if (!requireNamespace("tidyverse", quietly = TRUE)) 
install.packages("tidyverse")
if (!requireNamespace("tidyverse", quietly = TRUE)) 
library("tidyverse")
install.packages("naniar")
library(naniar)

# Specify the folder path
folder_path <- "data/"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
# Use map to read all CSVs into a list of data frames
data <- map(csv_files, read_csv)

# Sample data: Let's assume 'co2' is your data frame
# Clean and prepare the dataset
co2_cleaned <- co2 %>%
  # Replace ".." with NA
  mutate(across(starts_with("19") | starts_with("20"), ~na_if(., ".."))) %>%
  # Convert year columns to numeric (remove the leading spaces and brackets in column names)
  rename_with(~ gsub("\\[.*\\]", "", .)) %>%
  # Reshape to long format, making year columns into key-value pairs
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),
    names_to = "Year",
    values_to = "CO2_Emission"
  ) %>%
  # Convert 'CO2_Emission' to numeric (it was likely character before)
  mutate(CO2_Emission = as.numeric(CO2_Emission))
# Handle missing values if needed, for example, remove rows with missing emissions
#drop_na(CO2_Emission) 
# Check the cleaned data
glimpse(co2_cleaned)

# Visualize missing values
vis_miss(co2_cleaned)
