# 1.
# Load Required Libraries-------------------------------
library(tidyverse)
library(visdat) # For visualizing missing values
library(countrycode)
library(zoo)
library(VIM)


# 2. 
# Specify Folder Path and Read CSVs-------------------------------
folder_path <- "data/"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read all CSVs into a list of data frames
data_m <- map(csv_files, read_csv)

# Assign names to the list based on file names (without extensions)
names(data_m) <- tools::file_path_sans_ext(basename(csv_files))

data <- data_m[-c(1,8,15,12,5,6)]
data_1 <- data_m[c(1,8)]

# 3. 
# Creating helper function
# Data Cleaning Functions----------------------------
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

# 3. 
# creating two data frames with similar structure
# Merged and join Data Using map(), reduce()------------------------------------
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


# Creating two data list-------------------------------
data_list_1 <- map(df_1, ~ .x %>%
                   group_by(country,year) %>%
                   summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop"))

data_list_2 <- map(df_2, ~ .x %>%
                   group_by(country,year) %>%
                   summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop"))

merged_data_1 <- reduce(data_list_1, full_join, by = c("country","year"))

 # vis_miss(merged_data_1)

merged_data_2 <- reduce(data_list_2, full_join, by = c("country","year"))

# vis_miss(merged_data_2)

df_merged <- merged_data_1 %>%
  full_join(merged_data_2, by = c("country", "year")) |>
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
# vis_miss(df_merged)

income_classification <- df_merged %>%
  filter(year >= 1996, year <= 2020) %>%
  group_by(country) %>%
  summarise(avg_gdp = mean(gdp, na.rm = TRUE)) %>%
  mutate(income_level = case_when(
    avg_gdp <= 1135 ~ "Low income",
    avg_gdp <= 4465 ~ "Lower-middle income",
    avg_gdp <= 13845 ~ "Upper-middle income",
    avg_gdp > 13845 ~ "High income",
    TRUE ~ "Unknown"
  ))


df_merged_new <- df_merged %>%
  left_join(income_classification, by = "country")

# 5. 
# Create a vector of non-country entries--------------------
non_countries <- c(  
  "Heavily indebted poor countries (HIPC)",  
  "High income",  
  "IBRD only",  
  "IDA & IBRD total",  
  "IDA blend",  
  "IDA only",  
  "IDA total",  
  "Late-demographic dividend",  
  "Latin America & Caribbean",  
  "Latin America & Caribbean (excluding high income)",  
  "Latin America & the Caribbean (IDA & IBRD countries)",  
  "Least developed countries: UN classification",  
  "Low & middle income",  
  "Low income",  
  "Lower middle income",  
  "Middle East & North Africa",  
  "Middle East & North Africa (IDA & BRD countries)",  
  "Middle East & North Africa (excluding high income)",  
  "Middle income",  
  "North America",  
  "OECD members",  
  "Other small states",  
  "Pacific island small states",  
  "Palestinian Territories",  
  "Post-demographic dividend",  
  "Pre-demographic dividend",  
  "Réunion",  
  "Small states",  
  "South Asia",  
  "South Asia (IDA & BRD)",  
  "Soviet Union",  
  "Sub-Saharan Africa",  
  "Sub-Saharan Africa (IDA & BRD countries)",  
  "Sub-Saharan Africa (excluding high income)",  
  "Upper middle income",  
  "West Bank and Gaza",  
  "World",  
  "Jersey, Channel Islands" ,
  "Africa Eastern and Southern",
  "Africa Western and Central",
  "Arab World",
  "Central Europe and the Baltics",
  "Early-demographic dividend",
  "East Asia & Pacific",
  "East Asia & Pacific (IDA & IBRD countries)",
  "East Asia & Pacific (excluding high income)",
  "Europe & Central Asia (IDA & IBRD countries)",
  "Europe & Central Asia (excluding high income)",
  "Europe & Central Asia",
  "European Union",
  "Fragile and conflict affected situations",
  "French Guiana",
  "French Polynesia",
  "Hong Kong SAR, China",
  "Hong Kong",
  "Hong Kong SAR, China",
  "Middle East & North Africa (IDA & IBRD countries)",
  "Netherlands Antilles (former)",
  "Sint Maarten (Dutch part)",
  "South Asia (IDA & IBRD)",
  "St. Vincent and Grenadines",
  "Sub-Saharan Africa (IDA & IBRD countries)",
  "S�o Tom� and Principe" ,
  "São Tomé and Príncipe",
  "Bahamas",
  "Brunei",
  "Gambia",
  "	Laos",
  "	Martinique",
  "Syria",
  "Turkey",
  "T�rkiye"  ,"Fragile and conflict affected situations",
  "Middle East & North Africa (IDA & IBRD countries)",
  "Netherlands Antilles (former)",
  "South Asia (IDA & IBRD)",
  "	Euro area",
  "Micronesia",
  "Niue",
  "Micronesia",
  "	Yugoslavia (Historical)",
  "R�union" ,
  "	S�o Tom� and Principe") 


# Add comment
# Main data -------------------------------
main <- df_merged_new %>%
  filter(!(country %in% non_countries), year >= 1993, year <= 2022) %>%
  group_by(country) %>%
  mutate(years_available = n_distinct(year)) %>%
  ungroup() %>%
  filter(
    !(income_level %in% c("high income", "upper middle income") & years_available < 25)
  ) %>%
  select(-c(GNI, HDI, fossil)) %>%
  rename(
    gdp_per_capita = gdp,
    renew_energy_cons = energy_consumption,
    labour_force_total = LFT,
    foreign_direct_inv = FDI
  )


# 7. 
# Load and Clean WGI Data-------------------------------

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

wgi_df <- wgi |>
  filter(indicator == "rq") |>
  select(country,year,reg_quality = estimate)

# vis_miss(wgi_df)

# Step 1: Extend year range to include 1993 to 2002
reg_df <- wgi_df %>%
  complete(country, year = full_seq(1993:2002, 1)) %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    was_missing = is.na(reg_quality),
    reg_quality = na.approx(reg_quality, x = year, na.rm = FALSE, rule = 2)
  ) %>%
  ungroup()

# Step 2: Count interpolated values
n_interpolated <- sum(reg_df$was_missing & !is.na(reg_df$reg_quality))

# Step 3: View result and summary
cat("✅ Number of interpolated years:", n_interpolated, "\n")
print(reg_df)

# vis_miss(reg_df)


# 8. 
# Load and Clean swiid Data---------------------------------

new_swiid <- read_csv("data/swiid9_8_summary.csv")
# View(new_swiid)
swid_df <- new_swiid %>%
  filter(!(country %in% non_countries), year >= 1993, year <= 2022) %>%
  group_by(country) %>%
  mutate(years_available = n_distinct(year)) %>%
  ungroup() %>%
  filter(years_available >= 25) %>%
  select(country, year, gini_disp)



# 9. 
# Merge main, swiid and wgi  data--------------------
income_df <- main %>%
  inner_join(swid_df, by = c("country", "year")) %>%
  left_join(reg_df, by = c("country", "year"))

# vis_miss(income_df)


# 10. Final data
# Handling missing data in he energy intensity variable---------------------------------

## Inspecting the missing-------------------------------------
summary(income_df$energy_intensity)
sum(is.na(income_df$energy_intensity))

## counting missing data--------------------------------------
income_df %>%
  group_by(year) %>%
  summarise(missing_count = sum(is.na(energy_intensity ))) %>%
  arrange(desc(missing_count)) |>
  head()

# Calculate global fallback value before processing
global_mean <- income_df %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

# Interpolate only energy_intensity, keep other variables NA where missing
income_df_interpolated <- income_df %>% 
  complete(country, year = full_seq(min(year):max(year), 1)) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    across(
      where(is.numeric),
      ~ {
        has_any_data <- any(!is.na(.x))
        
        # Step 1: Linear interpolation + extrapolation
        vals <- if (has_any_data) {
          na.approx(.x, x = year, na.rm = FALSE, rule = 2)
        } else {
          rep(NA_real_, length(.x))
        }
        
        # Step 2: Forward fill
        vals <- na.locf(vals, na.rm = FALSE)
        
        # Step 3: Backward fill
        vals <- na.locf(vals, fromLast = TRUE, na.rm = FALSE)
        
        # Step 4: Fallback to global mean (if country has no data)
        ifelse(is.na(vals), global_mean[[cur_column()]], vals)
      }
    )
  ) %>%
  ungroup() %>%
  {
    # Print remaining missing counts
    missing_summary <- summarise_all(., ~ sum(is.na(.)))
    print(missing_summary)
    .
  }

# Visualize missingness (to confirm other vars keep NA, only energy_intensity filled)
vis_miss(income_df_interpolated)

income_df_interpolated <- income_df_interpolated %>%
  mutate(
    income_level_stable = case_when(
      avg_gdp <= 1135 ~ "Low income",
      avg_gdp <= 4465 ~ "Lower-middle income",
      avg_gdp <= 13845 ~ "Upper-middle income",
      avg_gdp > 13845 ~ "High income",
      TRUE ~ "Unknown"
    ),
    # fill NA in income_level with stable classification
    income_level = if_else(is.na(income_level), income_level_stable, income_level)
  ) |>
  filter(year >= 1996, year <= 2022)


income_df_interpolated %>%
  filter(year >= 1996, year <= 2022) %>%
   select(country, income_level) %>%
   distinct() %>%
   count(income_level, name = "n_countries") 

 # Count the number of years present for each country
 income_df_interpolated %>%
   filter(year >= 1996, year <= 2022) %>%
   group_by(country) %>%
   summarise(years_available = n_distinct(year)) %>%
   arrange(desc(years_available))
 
# 11. 
# Write clean data to csv-----------------------
write_csv(income_df_interpolated,"clean data/income_inequality_data_01.csv")

