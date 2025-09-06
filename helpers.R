clean <- clean_df |>
  full_join(wgi_df, by = c("country", "year")) |>
  full_join(swiid, by = c("country", "year"))
# vis_miss(clean)

# Assuming your dataframe is named 'df_filtered'
df_cleaned <- df_filtered |>
  # --- Step 1: Address Potential Encoding Issues ---
  # Note: The exact escape sequences might vary depending on the original encoding.
  # Inspect your data carefully to identify and replace them correctly.
  mutate(country = str_replace_all(country, "C\u00E4\u0090te d'Ivoire", "Côte d'Ivoire")) |>
  mutate(country = str_replace_all(country, "R\u00C3\u00A9union", "Réunion")) |>
  mutate(country = str_replace_all(country, "S\u00C3\u00A3o Tom\u00C3\u00A9 and Principe", "São Tomé and Principe")) |>
  mutate(country = str_replace_all(country, "T\u00C3\u00BCrkiye", "Türkiye")) |>
  
  # --- Step 2: Standardize Country Names using case_when() ---
  mutate(country = case_when(
    country %in% c("Congo, Dem. Rep.", "Congo-Kinshasa") ~ "Congo (Democratic Republic)",
    country %in% c("Congo, Rep.", "Congo-Brazzaville") ~ "Congo (Republic)",
    country %in% c("Cote d'Ivoire", "C\u00E2te d'Ivoire") ~ "Côte d'Ivoire",
    country %in% c("Czech Republic", "Czechia") ~ "Czech Republic",
    country == "Czechoslovakia" ~ "Czechoslovakia (Historical)",
    country == "Egypt, Arab Rep." ~ "Egypt",
    country == "Gambia, The" ~ "Gambia",
    country == "Iran, Islamic Rep." ~ "Iran",
    country %in% c("Korea, Dem. People's Rep.", "Korea, Dem. Rep.") ~ "Korea (Democratic People's Republic)",
    country == "Korea, Rep." ~ "Korea (Republic)",
    country %in% c("Kyrgyz Republic", "Kyrgyzstan") ~ "Kyrgyzstan",
    country == "Lao PDR" ~ "Laos",
    country == "Micronesia, Fed. Sts." ~ "Micronesia (Federated States)",
    country == "Martinique" ~ "Martinique", # Keep as is if it's a distinct territory
    country %in% c("R\u00C3\u00A9union", "Réunion") ~ "Réunion",
    country == "Russian Federation" ~ "Russia",
    country == "Slovak Republic" ~ "Slovakia",
    country == "Syrian Arab Republic" ~ "Syria",
    country %in% c("Sao Tome and Principe", "S\u00C3\u00A3o Tom\u00C3\u00A9 and Principe") ~ "São Tomé and Principe",
    country == "Taiwan, China" ~ "Taiwan",
    country %in% c("Turkey", "Turkiye", "T\u00C3\u00BCrkiye") ~ "Türkiye",
    country == "Venezuela, RB" ~ "Venezuela",
    country == "Viet Nam" ~ "Vietnam",
    country == "Yemen, Rep." ~ "Yemen",
    country == "Yugoslavia" ~ "Yugoslavia (Historical)",
    TRUE ~ country # Keep all other country names as they are
  ))

# View the updated counts to verify the cleaning
df_cleaned %>%
  filter(year > 1990) |>
  filter(country != "Afghanistan") %>%
  vis_miss()