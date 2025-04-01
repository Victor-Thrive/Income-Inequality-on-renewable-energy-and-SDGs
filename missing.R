data_list <- map(cleaned_data, ~ .x %>%
                   group_by(country,year) %>%
                   summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
)

merged_data <- reduce(data_list, full_join, by = c("country","year"))

merged_data |>
  count(country)
vis_miss(merged_data)


df <- merged_data %>%
  mutate(
    gdp = ifelse(is.na(gdp), median(gdp, na.rm = TRUE), gdp),
    fosil_fuel = ifelse(is.na(fosil_fuel), mean(fosil_fuel, na.rm = TRUE), fosil_fuel),
    trade = ifelse(is.na(trade), median(trade, na.rm = TRUE), trade)
  )

vis_miss(df2)

df2 <- df %>% select(-hci, -gini) %>% mutate(country = factor(country))

library(mice)
imputed <- mice(df, method = "pmm", m = 5)
df_complete <- complete(imputed)


library(missForest)
df_imputed <- missForest(df)$ximp


# -------------------------------------
# Load required libraries
library(dplyr)
library(mice)        # Multiple Imputation
library(missForest)  # Random Forest Imputation
library(VIM)         # Visualization

# Load dataset (assuming `df` is your dataset)
df <- read.csv("your_data.csv")

# Step 1: Drop highly missing variables (hci, gini)
df <- df %>% select(-hci, -gini)  

# Step 2: Simple Imputation for Low Missingness Variables
df3 <- df %>%
  mutate(
    gdp = ifelse(is.na(gdp), median(gdp, na.rm = TRUE), gdp),
    fosil_fuel = ifelse(is.na(fosil_fuel), mean(fosil_fuel, na.rm = TRUE), fosil_fuel),
    trade = ifelse(is.na(trade), median(trade, na.rm = TRUE), trade)
  )

# Step 3: Multiple Imputation using MICE for Moderate Missingness
imputed_data <- mice(df, method = "pmm", m = 5, ridge = 0.01)  # Ridge to avoid singularity
df_complete <- complete(imputed_data)

# Step 4: Predictive Imputation using Random Forest for Complex Cases
df_rf_imputed <- missForest(df)$ximp  

# Step 5: Save Imputed Data
write.csv(df_complete, "imputed_data.csv", row.names = FALSE)
write.csv(df_rf_imputed, "imputed_rf_data.csv", row.names = FALSE)

merged_data |>
  count(country,year)
vis_miss(merged_data)

df_merged <- cleaned_data$Co2 %>%
  inner_join(cleaned_data$energy_consumption, by = c("country", "year"))

vis_miss(df_merged)

df_merged_2 <- df_merged %>%
  inner_join(cleaned_data$energy_intensity, by = c("country", "year"))

vis_miss(df_merged_2)


df_merged_3  <- df_merged_2 %>%
  inner_join(cleaned_data$gdp, by = c("country", "year"))

vis_miss(df_merged_3)


df_merged_4  <- df_merged_3 %>%
  inner_join(cleaned_data$FDI, by = c("country", "year"))

vis_miss(df_merged_4)

df_merged_5  <- df_merged_4 %>%
  inner_join(cleaned_data$fossil, by = c("country", "year"))

vis_miss(df_merged_5)
View(df_merged_5)
names(df_merged_5)
summary(df_merged_5)


df_merged_6  <- df_merged_5 %>%
  inner_join(cleaned_data$GNI, by = c("country", "year"))
vis_miss(df_merged_6)


df_merged_7  <- df_merged_6 %>%
  inner_join(cleaned_data$HDI, by = c("country", "year"))
vis_miss(df_merged_7)



df_merged_6 |>
  select(country,year,CO2_Emission,energy_consumption,energy_intensity,gdp,fdi,gini)

df_merged |>
  count(country) 

