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

# -----------------------
# 2. Load and Inspect Dataset
# -----------------------
income_inequality <- read_csv("clean data/income_inequality_data_01.csv")  # Replace with your dataset
glimpse(income_inequality)

income_inequality <- income_inequality |>
  mutate(across(where(is.character), as.factor)) |>
  select(-c(was_missing,years_available,avg_gdp,income_level_stable))

names(income_inequality)

# -----------------------
# 3. Define Variable Groups for Interpretability
# -----------------------
economic_vars <- c("gdp_per_capita",
                   "labour_force_total",
                   "foreign_direct_inv",
                   "trade")

social_vars <- c("gini_disp","urban")

environmental_vars <- c("co2","energy_intensity")
institutional_vars <- c("reg_quality")

independent_vars <- c(economic_vars, social_vars, environmental_vars, institutional_vars)
dependent_var <- "renew_energy_cons"

Y <- income_inequality[,dependent_var]
X <- income_inequality[,independent_vars]

# -----------------------
# 4. Convert to Panel Data Format
# -----------------------
# Ensure your data has 'country' and 'year' columns
data_panel <- pdata.frame(income_inequality, index = c("country", "year"))
head(data_panel)
View(data_panel)

panel_data <- data_panel %>%
  group_by(country) %>%              # Group by panel unit
  arrange(year, .by_group = TRUE) %>%# Ensure observations are in time order
  mutate(panel_index = row_number()) %>%
  ungroup()

View(data_panel)

# -----------------------
# 5. Descriptive Statistics
# -----------------------
summary(Y)
summary(X)

describe(data_panel) 

# By Income Group
data_panel %>%
  group_by(income_level) %>%
  summarise(across(all_of(c(dependent_var, independent_vars)), ~mean(., na.rm = TRUE)))
# visualizing relationship between variables across all income levels
# -----------------------
# 6. Visualizations
# -----------------------

# Boxplot to check outliers
panel_data %>%
  pivot_longer(cols = all_of(independent_vars)) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Boxplot of REC across years
ggplot(income_inequality, aes(x = factor(year), y = renew_energy_cons)) +
  geom_boxplot(fill = "lightgreen")

# Trend between REC and GINI
ggplot(income_inequality, aes(x = gini_disp, y = renew_energy_cons)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "REC vs Income Inequality")

# Thematic Map: GINI (requires spatial shapefile merged with data)
# world_map <- st_read("world_shapefile.shp")
# merged <- left_join(world_map, data, by = "country")
# tm_shape(merged) + tm_polygons("GINI_Index", palette = "YlOrRd")

# Thematic Map: Renewable Energy
# tm_shape(merged) + tm_polygons("Renewable_Energy_Consumption", palette = "BuGn")

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
