library(dplyr)

# 1️⃣ Dynamic: year-by-year classification
df_income_dynamic <- df_merged %>%
  mutate(income_level_dynamic = case_when(
    gdp <= 1135 ~ "Low income",
    gdp <= 4465 ~ "Lower-middle income",
    gdp <= 13845 ~ "Upper-middle income",
    gdp > 13845 ~ "High income",
    TRUE ~ "Unknown"
  ))

# 2️⃣ Stable: average GDP classification (1996–2020)
income_classification <- df_merged %>%
  filter(year >= 1996, year <= 2020) %>%
  group_by(country) %>%
  summarise(avg_gdp = mean(gdp, na.rm = TRUE), .groups = "drop") %>%
  mutate(income_level_stable = case_when(
    avg_gdp <= 1135 ~ "Low income",
    avg_gdp <= 4465 ~ "Lower-middle income",
    avg_gdp <= 13845 ~ "Upper-middle income",
    avg_gdp > 13845 ~ "High income",
    TRUE ~ "Unknown"
  ))

# 3️⃣ Identify transitions (countries with >1 unique dynamic level)
transition_flags <- df_income_dynamic %>%
  group_by(country) %>%
  summarise(
    transitioned = n_distinct(income_level_dynamic[!is.na(income_level_dynamic)]) > 1,
    .groups = "drop"
  )

# 4️⃣ Merge everything
df_income_final <- df_income_dynamic %>%
  left_join(income_classification %>% select(country, income_level_stable),
            by = "country") %>%
  left_join(transition_flags, by = "country")

names(df_income_final)

# Add comment
# Main data -------------------------------

# 1. Create main_2 dataset ---------------------------------------------------
main_2 <- df_income_final %>%
  filter(!(country %in% non_countries), year >= 1993, year <= 2022) %>%
  group_by(country) %>%
  mutate(years_available = n_distinct(year)) %>%
  ungroup() %>%
  filter(
    !(income_level_dynamic %in% c("High income", "Upper-middle income") & years_available < 25)
  ) %>%
  select(-c(GNI, HDI, fossil)) %>%
  rename(
    gdp_per_capita    = gdp,
    renew_energy_cons = energy_consumption,
    labour_force_total = LFT,
    foreign_direct_inv = FDI
  )

# 2. Merge main with SWIID and WGI data -------------------------------------
income_df_2 <- main_2 %>%
  inner_join(swid_df, by = c("country", "year")) %>%
  left_join(reg_df, by = c("country", "year"))

# Inspect missingness
vis_miss(income_df_2)

# 3. Handle missing data: Interpolation for ALL numeric vars -----------------

# Global means for fallback
global_means_2 <- income_df_2 %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

income_df_interpolated_2 <- income_df_2 %>%
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
        ifelse(is.na(vals), global_means_2[[cur_column()]], vals)
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

# 4. Quick diagnostics -------------------------------------------------------
# Missingness visualization
vis_miss(income_df_interpolated_2)


# Number of countries per income group
income_df_interpolated_2 %>%
  select(country, income_level_stable) %>%
  distinct() %>%
  count(income_level_stable, name = "n_countries")

# Number of years available per country
income_df_interpolated_2 %>%
  group_by(country) %>%
  summarise(years_available = n_distinct(year)) %>%
  arrange(desc(years_available))
