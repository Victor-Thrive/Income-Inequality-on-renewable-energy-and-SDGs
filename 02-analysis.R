# ===============================================================
# ECONOMETRIC ANALYSIS OF RENEWABLE ENERGY CONSUMPTION DETERMINANTS
# Categories: Economic, Social, Environmental, Institutional
# ===============================================================
# -----------------------
# 1. Load Required Libraries
# -----------------------
library(tidyverse)       # Data wrangling & visualization
library(plm)             # Panel data estimation
library(psych)           # Descriptive statisticss
library(ggplot2)         # Visualization
library(tmap)            # Thematic maps
library(sf)              # Spatial data
library(corrplot)        # Correlation matrix
library(urca)            # Unit root tests
library(tseries)         # Time series tests
library(panelAR)         # Panel AR models
library(pgmm)            # GMM estimators
library(quantreg)        # Quantile regression
library(lmtest)          # Diagnostics
library(sandwich)        # Robust SEs
library(vars)            # Lag order selection
library(gt)
library(rnaturalearth)
library(rnaturalearth)
# Map GINI
library(tmap)
library(ggplot2)
library(sf)
library(ggspatial)


# -----------------------
# 2. Load and Inspect Dataset
# -----------------------
income_inequality <- read_csv("clean data/income_inequality_data_01.csv")  # Replace with your dataset
glimpse(income_inequality)

income_inequality <- income_inequality |>
  mutate(across(where(is.character), as.factor)) |>
  rename(
    country = country,
    year = year,
    REC = renew_energy_cons,
    GINI = gini_disp,
    GDP = gdp_per_capita,
    LABOUR = labour_force_total,
    FDI = foreign_direct_inv,
    TRADE = trade,
    URBAN = urban,
    CARBON = co2,
    ENERGY_INTENSITY = energy_intensity,
    REG_QUALITY = reg_quality,
    income_group = income_level_stable
  ) |>
  select(-c(was_missing,years_available,avg_gdp,income_level))

# quick checks
summary(income_inequality)
anyNA(income_inequality)
names(income_inequality)

# -------------------------
# 2) Re-grouping variables for interpretability
# -------------------------
economic_vars  <- c("GDP", "FDI", "TRADE", "LABOUR")
social_vars    <- c("GINI", "URBAN")
environmental_vars <- c("REC", "CARBON", "ENERGY_INTENSITY")
institutional_vars  <- c("REG_QUALITY")

# To be used later for grouped summaries
groups <- list(
  Economic = economic_vars,
  Social = social_vars,
  Environmental = environmental_vars,
  Institutional = institutional_vars
)

# -------------------------
# 3) Define X and Y (dependent & independent)
# -------------------------
Y_var <- "REC"
X_vars <- c("GINI", "GDP", "LABOUR", "FDI", "TRADE", "URBAN", "CARBON", "ENERGY_INTENSITY", "REG_QUALITY")

# create formula string
formula_full <- as.formula(paste(Y_var, "~", paste(X_vars, collapse = " + ")))
formula_full

# -------------------------
# 4) Convert to panel data using plm
# -------------------------
pdata <- pdata.frame(income_inequality, index = c("country", "year"))
# check panel structure
pdim(pdata)

# -------------------------
# 5) Descriptive statistics (overall and by income_group)
# -------------------------
# overall descriptive (psych::describe)
desc_overall <- describe(pdata[, c(Y_var, X_vars)], na.rm = TRUE)
print(desc_overall)

# Descriptive stats by income group
desc_by_group <- income_inequality %>%
  group_by(income_group) %>%
  summarise(
    # sample size once
    sample_size = n(),
    across(
      all_of(c(Y_var, X_vars)), 
      list(mean = ~mean(.x, na.rm = TRUE),
           sd = ~sd(.x, na.rm = TRUE),
           min = ~min(.x, na.rm = TRUE),
           max = ~max(.x, na.rm = TRUE)),
      .names = "{col}_{fn}")) %>%
  arrange(income_group)

# show nicely
# Convert to gt table
desc_table <- desc_by_group %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics by Income Group",
    subtitle = "Mean, Standard Deviation, and Sample Size"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = 12,
    table.border.top.style = "solid",
    table.border.bottom.style = "solid"
  )

desc_table

# ----------------------------
# 2. Create tables by category
# ----------------------------
economic_table <- make_desc_table(income_inequality, economic_vars, title = "Economic Variables")
social_table <- make_desc_table(income_inequality, social_vars, title = "Social Variables")
environmental_table <- make_desc_table(income_inequality, environmental_vars, title = "Environmental Variables")
institutional_table <- make_desc_table(income_inequality, institutional_vars, title = "Institutional Variables")

# Print tables
economic_table
social_table
environmental_table
institutional_table

# visualizing relationship between variables across all income levels
# -----------------------
# 6. Visualizations
# -----------------------

# Boxplot to check outliers
# 6.1 Boxplot for outliers (overall) - for numeric vars
income_inequality %>%
  pivot_longer(cols = all_of(c(Y_var, X_vars)), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(outlier.alpha = 0.6) +
  coord_flip() +
  ggtitle("Boxplots for variables (identify outliers)") +
  theme_minimal()

# 6.2 Boxplot REC across years
ggplot(income_inequality, aes(x = factor(year), y = REC)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Year", y = "Renewable energy consumption (REC)", title = "REC distribution across years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# 6.3 Trend: Renewable Energy vs Income Inequality (scatter + smoothing)
ggplot(income_inequality, aes(x = GINI, y = REC)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ income_group) +
  theme_minimal() +
  labs(title = "REC vs GINI by Income Group", x = "GINI index", y = "REC")

# Thematic Map: GINI (requires spatial shapefile merged with data)
# 6.4 Maps (income inequality & REC) - using rnaturalearth for country shapes
# Note: mapping requires matching country naming conventions. You may need to harmonize country names.
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")

# prepare data for joining - average by country across years
map_data <- income_inequality %>%
  group_by(country) %>%
  summarise(
    GINI_mean = mean(GINI, na.rm = TRUE),
    REC_mean  = mean(REC, na.rm = TRUE)
  )

# join (this will match only where names align)
world_map <- left_join(world, map_data, by = c("name" = "country"))



tm_shape(world_map) +
  tm_polygons("GINI_mean", palette = "RdYlBu", title = "Mean GINI") +
  tm_layout(title = "Mean Income Inequality (GINI) by Country")


# Make sure world_map is an sf object with GINI_mean column
ggplot(world_map) +
  geom_sf(aes(fill = GINI_mean), color = "gray60", size = 0.2) +
  scale_fill_distiller(palette = "RdYlBu", direction = 1, name = "Mean GINI") +
  labs(
    title = "Mean Income Inequality (GINI) by Country",
    subtitle = "Average values (1990–2022)",
    caption = "Source: SWIID | Author's computation, 2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0.5),
    legend.position = "right"
  )

# Map REC
tm_shape(world_map) +
  tm_polygons("REC_mean", palette = "Greens", title = "Mean REC (%)") +
  tm_layout(title = "Mean Renewable Energy Consumption by Country")

# Make sure world_map is an sf object with REC_mean column
ggplot(world_map) +
  geom_sf(aes(fill = REC_mean), color = "gray60", size = 0.2) +
  scale_fill_distiller(palette = "Greens", direction = 1, name = "Mean REC (%)") +
  labs(
    title = "Mean Renewable Energy Consumption by Country",
    subtitle = "Average values (1990–2022)",
    caption = "Source: World Bank (WDI) | Author's computation, 2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0.5),
    legend.position = "right"
  )



# -----------------------
# 7. Correlation Matrix
# -----------------------
cor_mat <- cor(income_inequality[, c(dependent_var, independent_vars)], use = "pairwise.complete.obs")
corrplot(cor_mat, method = "circle", tl.cex = 0.7)

# -----------------------
# 8. Panel Unit Root Tests
# -----------------------
# Levin-Lin-Chu test
library(plm)
library(broom)
purtest(data_panel$gdp_per_capita, test = "levinlin", exo = "intercept", lags = "AIC")

results_df <- data_panel %>%
  select(where(is.numeric)) %>%                 # keep only numeric vars
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  group_by(variable) %>%                        # run test by variable
  summarise(
    test_result = list(
      tryCatch(
        purtest(value, test = "levinlin", exo = "intercept", lags = "AIC"),
        error = function(e) NULL
      )
    ),
    .groups = "drop"
  ) %>%
  mutate(
    tidy_result = map(test_result, ~ if(!is.null(.x)) broom::tidy(.x) else NULL)
  ) %>%
  unnest(tidy_result)

results_df

# IPS, MW, and CIPS tests can be added similarly if needed

# -----------------------
# 9. Lag Order Selection
# -----------------------
VARselect(data[, c(dependent_var, independent_vars)], lag.max = 5, type = "const")

# -----------------------
# 10. ARDL Model 
# -----------------------


# PMG Test 4 panel data
# MG Test

# -----------------------
# 11. PMG and MG Estimation
# -----------------------

# PMG (use panelAR or xtpmg if using Stata-style)
pmg_model <- panelAR(Renewable_Energy_Consumption ~ ., data = data, 
                     panelVar = "country", timeVar = "year",
                     autoCorr = "AR1", panelCorrMethod = "pcse")
summary(pmg_model)

# MG Model
mg_model <- plm(Renewable_Energy_Consumption ~ ., data = data_panel, model = "mg")
summary(mg_model)

# -----------------------
# 12. Hausman Test (PMG vs MG)
# -----------------------
hausman_result <- phtest(mg_model, pooled)  # Replace `pooled` with PMG object if convertible
print(hausman_result)

# based on the result we choose between PMG and MG

# -----------------------
# 13. Final Model Estimation (Random) Take this part out since we have selected PMG or MG
# -----------------------
final_model <- plm(Renewable_Energy_Consumption ~ ., data = data_panel)
summary(final_model)

# -----------------------
# 14. Causality Test
# -----------------------
grangertest(Renewable_Energy_Consumption ~ GINI_Index, order = 2, data = data_panel)

# -----------------------
# 15. Diagnostic Tests
# -----------------------
# Heteroskedasticity
bptest(final_model)

# Serial correlation
pbgtest(final_model)


# -----------------------
# 16. System GMM Estimation
# everything has to be stationary for GMM
# -----------------------
gmm_model <- pgmm(
  Renewable_Energy_Consumption ~ lag(Renewable_Energy_Consumption, 1) + . |
    lag(Renewable_Energy_Consumption, 2:3),
  data = data_panel, effect = "individual", model = "twosteps", transformation = "ld"
)
summary(gmm_model)

# -----------------------
# 17. Quantile Regression (0.25, 0.5, 0.75)
# -----------------------
qreg <- rq(Renewable_Energy_Consumption ~ ., data = data, tau = c(0.25, 0.5, 0.75))
summary(qreg)

# -----------------------
# 18. Save Model Results
# -----------------------
sink("model_results_summary.txt")
cat("=== Final Model ===\n")
summary(final_model)

cat("\n=== GMM Model ===\n")
summary(gmm_model)

cat("\n=== Quantile Regression ===\n")
summary(qreg)
sink()
