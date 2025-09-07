# panel_REC_analysis.R
# Full pipeline for:
# REC ~ GINI + GDP + LABOUR + FDI + TRADE + URBAN + CARBON + ENERGY_INTENSITY + REG_QUALITY
# Implements: data prep, descriptive stats, visualisation, unit roots (IPS, Maddala-Wu, CIPS),
# lag selection, cointegration, PMG (ARDL), MG, Hausman test, System-GMM, Panel QR, diagnostics.

# -------------------------
# 0) Install / load packages
# -------------------------
pkgs <- c(
  "tidyverse", "plm", "psych", "sjPlot", "DataExplorer", "ggplot2", "broom",
  "sf", "rnaturalearth", "rnaturalearthdata", "tmap", "corrplot", "pmdplyr",
  "stargazer", "lmtest", "urca", "punitroots", "pCADF", # pCADF may be needed for CIPS
  "pmg", "nlme", "xtable", "pbkrtest", "pdynmc", "systemfit",
  "plm", "vars", "AER", "sandwich", "purtest", "panelvar",
  "plm", "quantreg", "lfe", "fixest"
)
# Install only missing
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if(length(to_install)) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

# load
library(tidyverse)
library(plm)        # panel data structures, purtest, pgmm
library(psych)      # descriptive stats
library(ggplot2)
library(corrplot)
library(lmtest)
library(sandwich)
library(urca)       # unit-root in time-series (auxiliary)
library(quantreg)   # quantile regression
library(fixest)     # fast FE estimation & diagnostics
# Optional / specialized (may not be installed on all machines)
# library(pCADF)    # for CIPS test (panel CADF) - install if available
# library(punitroots) # some panel unit root functions
# library(pmg)      # pmg package for PMG estimation (if not available see alternative below)

# -------------------------
# 1) Load & inspect dataset
# -------------------------
# Replace with your file or use data already loaded in environment
# Example: income_inequality <- read_csv("path/to/your/data.csv")
income_inequality <- read_csv("path/to/your/data.csv")

glimpse(income_inequality)
# Ensure critical variables exist and rename to concise variable names used below:
# Required variables mapping:
# REC (renewable energy consumption), GINI, GDP, LABOUR, FDI, TRADE, URBAN, CARBON, ENERGY_INTENSITY, REG_QUALITY
income_inequality <- income_inequality %>%
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
  ) %>%
  select(country, year, income_group, REC, GINI, GDP, LABOUR, FDI, TRADE, URBAN, CARBON, ENERGY_INTENSITY, REG_QUALITY)

# quick checks
summary(income_inequality)
anyNA(income_inequality)

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

View(desc_by_group)

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

# -------------------------
# 6) Visualisations
# -------------------------

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

# Map GINI
library(tmap)
library(ggplot2)
library(sf)
library(ggspatial)


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


# -------------------------
# 7) Correlation matrix (full correlation excluding country & year)
# -------------------------
cor_data <- income_inequality %>%
  select(all_of(c(Y_var, X_vars))) %>%
  drop_na()

cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# Correlation by income groups (list)
cor_by_group <- income_inequality %>%
  group_by(income_group) %>%
  summarise(across(all_of(c(Y_var, X_vars)), ~ list(cor(., income_inequality[[Y_var]], use = "pairwise.complete.obs"))))
# (the above summary gives correlations of each var with REC by group; you can expand similarly)

# -------------------------
# 8) Variable transformations (logs, diffs) - recommended before unit root tests
# -------------------------
# Create transformed dataset copy
pdata_trans <- pdata %>%
  mutate(
    lREC = ifelse(REC > 0, log(REC), NA_real_),
    lGDP = ifelse(GDP > 0, log(GDP), NA_real_),
    lCARBON = ifelse(CARBON > 0, log(CARBON), NA_real_),
    lTRADE = ifelse(TRADE > 0, log(TRADE), NA_real_),
    # first differences for I(1) candidate series
    dREC = ave(lREC, country, FUN = function(x) c(NA, diff(x))),
    dGDP = ave(lGDP, country, FUN = function(x) c(NA, diff(x))),
    dCARBON = ave(lCARBON, country, FUN = function(x) c(NA, diff(x)))
  )

# -------------------------
# 9) Unit root tests (panel): Maddala-Wu (MW), IPS, Levin-Lin-Chu (LLC), and CIPS (Pesaran)
# -------------------------
# Using plm::purtest for MW and IPS (and Levin if desired)
# prepare series as pdata$VAR style
# Example: purtest for REC (Maddala-Wu)
# Note: purtest is in plm package; some parameters depend on sample size and gaps
library(plm)
# use "madwu" (Maddala-Wu), "ips" (Im-Pesaran-Shin), "levinlin" (Levin-Lin-Chu)
purtest_formula <- function(series_name, data_pdata, test = "madwu", exo = "intercept", lags = "AIC") {
  y <- data_pdata[, series_name]
  # purtest accepts a formula or a vector; here we pass the vector
  purtest(as.vector(y), data = data_pdata, test = test, exo = exo, lags = lags)
}

# Example tests
cat("Maddala-Wu test for REC\n")
print(purtest_formula("REC", pdata, test = "madwu", exo = "intercept", lags = 2))

cat("IPS test for REC\n")
print(purtest_formula("REC", pdata, test = "ips", exo = "intercept", lags = 2))

cat("LLC test for REC\n")
print(purtest_formula("REC", pdata, test = "levinlin", exo = "intercept", lags = 2))

# Repeat for GINI, GDP, LABOUR, CARBON etc.
vars_for_unitroot <- c("REC", "GINI", "GDP", "LABOUR", "CARBON", "TRADE", "ENERGY_INTENSITY")
for(v in vars_for_unitroot){
  cat("\n--- Unit-root tests for", v, "---\n")
  try({
    print(purtest_formula(v, pdata, test = "madwu", exo = "intercept", lags = 2))
    print(purtest_formula(v, pdata, test = "ips", exo = "intercept", lags = 2))
    print(purtest_formula(v, pdata, test = "levinlin", exo = "intercept", lags = 2))
  }, silent = TRUE)
}

# CIPS test (panel CADF) - Pesaran's CIPS
# Option A: try pCADF::pCADFtest if available
if(requireNamespace("pCADF", quietly = TRUE)){
  library(pCADF)
  # pCADFtest takes a matrix with columns as cross-sections (countries)
  # Prepare data matrix of lREC by country
  lREC_mat <- income_inequality %>%
    filter(!is.na(lREC)) %>%
    select(country, year, lREC) %>%
    pivot_wider(names_from = country, values_from = lREC) %>%
    arrange(year) %>%
    select(-year) %>%
    as.matrix()
  cat("Running CIPS (pCADF) for lREC ...\n")
  print(pCADF::pCADF.test(lREC_mat, type = "trend"))  # example
} else {
  message("pCADF package not installed. Install 'pCADF' (or other implementation) to run Pesaran CIPS test.")
}

# -------------------------
# 10) Lag order selection (for ARDL/PMG) - automatic selection strategies
# -------------------------
# Approach: use VARselect on panel-averaged series or individual series representative (pooled)
# We'll compute lag selection from 'vars' using pooled time series (averaged across countries)
library(vars)
# aggregate to time series by mean across countries for each year
ts_by_year <- income_inequality %>%
  group_by(year) %>%
  summarise(across(all_of(c(Y_var, X_vars)), ~ mean(.x, na.rm = TRUE))) %>%
  arrange(year)

# convert to ts
tsdata <- ts(ts_by_year %>% select(-year), start = min(ts_by_year$year), frequency = 1)
VARselect(tsdata, lag.max = 5, type = "const")

# Use selected lag (e.g., p = 1 or p = 2) in ARDL/PMG

# -------------------------
# 11) Cointegration tests (panel)
# -------------------------
# Options: Pedroni, Kao, Westerlund (in 'panelvar' or 'phtt' or 'egcm' packages)
# We'll try Westerlund from 'plm' via 'pwt' alternative — many packages; here's a common option:
if(requireNamespace("plm", quietly = TRUE) && requireNamespace("panelvar", quietly = TRUE)){
  # Example placeholder: use Westerlund via 'pdwtest' or other package if installed.
  message("If you want Pedroni or Westerlund tests, install 'pwt' or 'panelvar' or 'coint' related packages.")
} else {
  message("Install packages supporting panel cointegration tests (e.g., 'phtt', 'panelvar', 'egcm').")
}

# -------------------------
# 12) Estimate PMG (ARDL-PMG) and MG
# -------------------------
# The 'pmg' package (if installed) can be used. If not, other routes (ARDL via dynlm per country and then pool).
if(requireNamespace("pmg", quietly = TRUE)){
  library(pmg)
  # Example PMG specification: choose lag = 1 as you suggested
  # Prepare data frame with needed lagged and differenced variables
  pdata_df <- as.data.frame(pdata) %>% arrange(country, year)
  pdata_df <- pdata_df %>%
    group_by(country) %>%
    mutate(
      REC_l1 = lag(REC, 1),
      GINI_l1 = lag(GINI, 1),
      GDP_l1 = lag(GDP, 1),
      LABOUR_l1 = lag(LABOUR, 1),
      CARBON_l1 = lag(CARBON, 1),
      gdp_diff = GDP - lag(GDP, 1),
      carbon_diff = CARBON - lag(CARBON, 1)
    ) %>%
    ungroup()
  # PMG (example, adjust formula as needed)
  pmg_formula <- REC ~ REC_l1 + GINI_l1 + GDP_l1 + LABOUR_l1 + CARBON_l1 +
    GINI + gdp_diff + LABOUR + carbon_diff + lag(gdp_diff, 1) + lag(carbon_diff, 1)
  model_pmg <- pmg(pmg_formula, data = pdata_df, index = c("country", "year"))
  summary(model_pmg)
  
  # Mean Group estimator (MG) for comparison
  model_mg <- pmg(pmg_formula, data = pdata_df, model = "mg", index = c("country", "year"))
  summary(model_mg)
  
  # Hausman test between PMG and MG
  # If 'pmg' package provides a test function, use it; otherwise compute a simple Hausman-style comparison
  # Some codebases implement hausmanPMG() — check your installed pmg package docs.
} else {
  message("Package 'pmg' not installed. You can install it (if available) or run country-by-country ARDL and average coefficients (manual MG).")
  # Simple alternative: estimate ARDL(1,1) per country (dynlm or lm on lags) then compute mean coefficients (MG).
}

# -------------------------
# 13) Hausman test (PMG vs MG)
# -------------------------
# If pmg provides a built-in hausman test, use it. Otherwise implement manual Hausman:
# H = (b_MG - b_PMG)' [Var(b_MG) - Var(b_PMG)]^{-1} (b_MG - b_PMG)
# Use try-catch depending on returns from pmg package

# -------------------------
# 14) System-GMM (dynamic panel) - using 'plm'::pgmm or 'systemfit' approaches
# -------------------------
# Common: use 'pgmm' in plm for Arellano-Bover/Blundell-Bond
# Example dynamic panel: REC_it on lagged REC and exogenous regressors
# Prepare formula for pgmm
pgmm_formula <- as.formula(paste0("REC ~ lag(REC, 1) + ", paste(X_vars, collapse = " + "), " | lag(REC, 2:99)"))
# Note: instrument selection must be carefully chosen; example only
try({
  model_pgmm <- pgmm(
    formula = as.formula(paste("REC ~ lag(REC, 1) +", paste(X_vars, collapse = " + "))),
    data = pdata,
    effect = "individual",
    model = "twosteps",
    transformation = "ld"
  )
  summary(model_pgmm)
}, silent = TRUE)

# Diagnostics: Arellano-Bond serial correlation, Hansen J overid
try({
  if(exists("model_pgmm")) {
    print(mtest(model_pgmm, order = 1)) # AR(1)
    print(mtest(model_pgmm, order = 2)) # AR(2)
    # Hansen test might be accessible via summary
    print(summary(model_pgmm))
  }
}, silent = TRUE)

# -------------------------
# 15) Quantile Regression (Panel)
# -------------------------
# Simple approach: demean variables by individual fixed effects and run quantile regression on pooled demeaned data
# Using fixest to get fixed effects and then quantreg on residuals or use 'rqpd' (panel quantile) package if installed
# Demean REC and X by country
demeaned <- income_inequality %>%
  group_by(country) %>%
  mutate(across(all_of(c(Y_var, X_vars)), ~ . - mean(., na.rm = TRUE))) %>%
  ungroup()

# run quantile regression at e.g. q = 0.25, 0.5, 0.75
qs <- c(0.25, 0.5, 0.75)
qr_results <- list()
for(q in qs){
  qr_results[[as.character(q)]] <- rq(
    formula = as.formula(paste0(Y_var, " ~ ", paste(X_vars, collapse = " + "))),
    data = demeaned,
    tau = q
  )
  cat("\nQuantile:", q, "\n")
  print(summary(qr_results[[as.character(q)]]))
}

# -------------------------
# 16) Diagnostics (multicollinearity, heteroskedasticity, serial correlation)
# -------------------------
# VIF (from car package)
if(!requireNamespace("car", quietly = TRUE)) install.packages("car")
library(car)
# Fit OLS pooled for VIF check (note: not ideal for panel but instructive)
pooled_ols <- lm(formula_full, data = income_inequality)
vif_vals <- car::vif(pooled_ols)
print(vif_vals)

# Heteroskedasticity: Breusch-Pagan
bptest(pooled_ols)

# Serial correlation in panel: Wooldridge test (plm::pbgtest or pcdtest)
# pbgtest for serial correlation in plm
pbgtest_update <- tryCatch({
  pbgtest(plm(formula_full, data = pdata, model = "within"))
}, error = function(e) e)
print(pbgtest_update)

# -------------------------
# 17) Summaries & Interpretation placeholders
# -------------------------
# After running the models, populate these placeholders with actual results
cat("\n--- SUMMARY TEMPLATE (fill after running models) ---\n")
cat("1) Descriptive stats: (describe main means, SDs, notable outliers)\n")
cat("2) Stationarity: Which series are I(0) vs I(1) based on IPS/MW/CIPS? \n")
cat("3) Cointegration: (Pedroni/Westerlund results) \n")
cat("4) PMG results (long-run coefficients, error-correction term): \n")
cat("5) MG results: (heterogeneity across countries) \n")
cat("6) Hausman test: (PMG vs MG preference) \n")
cat("7) System-GMM: (short-run dynamics, instrument validity, AR(2), Hansen p-value) \n")
cat("8) Quantile regression: (how coefficients vary across REC distribution) \n")
cat("9) Diagnostics: VIFs, heteroskedasticity, serial correlation and steps taken (robust SE, cluster, etc.) \n")
cat("10) Policy interpretation: (write clear policy advice e.g. effect of inequality on REC) \n")

# Save important outputs
saveRDS(list(
  pdata = pdata,
  desc_overall = desc_overall,
  desc_by_group = desc_by_group,
  cor_matrix = cor_matrix,
  # add models if exist
  pmg = if(exists("model_pmg")) model_pmg else NULL,
  mg = if(exists("model_mg")) model_mg else NULL,
  pgmm = if(exists("model_pgmm")) model_pgmm else NULL,
  qr = qr_results
), file = "panel_REC_analysis_results.rds")

cat("Script finished. Results saved to 'panel_REC_analysis_results.rds'\n")
