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

data <- data_m[-c(1,8,15,12,5,6)]
data_1 <- data_m[c(1,8)]

# -------------------------------
# 3. Data Cleaning Functions
# -------------------------------
clean_df_names <- function(df) {
  df %>%
    # Replace ".." with NA
    mutate(across(starts_with("19") | starts_with("20"), ~na_if(., ".."))) %>%
    # Remove brackets from year column names
    rename_with(~ gsub("\\[.*\\]", "", .))
}

reshape_data <- function(df,value_col){
  # Reshape to long format
  df%>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),
    names_to = "Year",
    values_to = value_col,
    values_drop_na = TRUE
  ) %>%
    # Convert value column to numeric and clean column names
    mutate(
      !!value_col := as.numeric(.data[[value_col]]),
      year = as.numeric(Year),  # Convert Year to factor
      country = factor(`Country Name`),
      country_code = `Country Code`
    ) %>%
    # Remove rows with missing values
    filter(!is.na(.data[[value_col]])) %>%
    select(country, country_code, year, !!value_col)
}

# -------------------------------
# 4. Clean and Prepare Data Using map2()

# -------------------------------
df <- map(data, clean_df_names)

df_1 <- map2(df,c(
                  "energy_consumption",
                  "energy_intensity",
                  "FDI",
                  "fossil",
                  "GNI",
                  "HDI",
                  "LFT",
                  "trade",
                  "urban" ),reshape_data)
df_2 <- map2(data_1,c("co2","gdp"),reshape_data)


data_list_1 <- map(df_1, ~ .x %>%
                   group_by(country,year) %>%
                   summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop"))

data_list_2 <- map(df_2, ~ .x %>%
                   group_by(country,year) %>%
                   summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop"))

merged_data_1 <- reduce(data_list_1, full_join, by = c("country","year"))

vis_miss(merged_data_1)
merged_data_1 |>
  count(year)

merged_data_2 <- reduce(data_list_2, full_join, by = c("country","year"))

vis_miss(merged_data_2)
merged_data_2 |>
  count(year)


df_merged <- merged_data_1 %>%
  left_join(merged_data_2, by = c("country", "year")) |>
 select(c("country",
          "year",
          "co2",
          "gdp",
          "energy_consumption",
          "energy_intensity",
          "FDI",
          "fossil",
          "GNI",
          "HDI",
          "LFT",
          "trade",
          "urban"))
vis_miss(df_merged)


df_merged |>
  count(year) |>
  slice_tail(n = 10)

clean_df <- df_merged %>%
  mutate(
    gdp= ifelse(is.na(gdp), mean(gdp, na.rm = TRUE), gdp),
    co2= ifelse(is.na(co2), mean(co2, na.rm = TRUE), co2),
    FDI= ifelse(is.na(FDI), mean(FDI, na.rm = TRUE), FDI),
    trade = ifelse(is.na(trade), mean(trade, na.rm = TRUE), trade)
  ) %>% select(-HDI, -GNI)

vis_miss(clean_df)

clean_df |>
  count(year)|>
  arrange(year)


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

vis_miss(wgi)

# ---------------------------------
# swiid
# ---------------------------------

swiid <- data_m$swiid9_6_summary |>
  select(country:gini_mkt_se)

vis_miss(swiid)

# ---------------------------------
# Final data
# ---------------------------------
clean <- clean_df |>
  left_join(swiid, by = c("country", "year")) |>
  left_join(financial_2021, by = c("country", "year")) |>
  left_join(wgi, by = c("country", "year"))

vis_miss(clean, warn_large_data = FALSE)

clean |>
  count(country)
