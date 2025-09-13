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
library(tmap)            # Thematic maps
library(sf)              # Spatial data
library(corrplot)        # Correlation matrix
library(urca)            # Unit root tests
library(tseries)         # Time series tests
library(pgmm)            # GMM estimators
library(quantreg)        # Quantile regression
library(lmtest)          # Diagnostics
library(sandwich)        # Robust SEs
library(vars)            # Lag order selection
library(gt)
library(rnaturalearth)
library(rnaturalearth)
library(moments)
# Map GINI
library(ggspatial)
source("functions.R")

# -----------------------
# 2. Load and Inspect Dataset
# -----------------------
income_inequality <- read_csv("clean data/income_inequality_data_01.csv")  # Replace with your dataset
#glimpse(income_inequality)

income_inequality <- income_inequality %>%
  mutate(across(where(is.character), as.factor))  %>%
  rename(
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
  )  %>%
  dplyr::select(-was_missing, -years_available, -avg_gdp, -income_level)

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
# make sure we are working with a data frame
numeric_data <- as.data.frame(pdata[, c(Y_var, X_vars)]) %>%
  dplyr::select(where(is.numeric))

# overall descriptive (psych::describe)
desc_overall <- describe(numeric_data, na.rm = TRUE)

# --- Add JB stats ---
jb_stats <- numeric_data %>%
  summarise(across(
    everything(),
    list(
      JB   = ~suppressWarnings(jarque.bera.test(na.omit(.x))$statistic),
      JB_p = ~suppressWarnings(jarque.bera.test(na.omit(.x))$p.value)
    )
  ))

# reshape JB stats
jb_tidy <- as.data.frame(t(jb_stats))
colnames(jb_tidy) <- "value"
jb_tidy$var <- sub("_(JB|JB_p)$", "", rownames(jb_tidy))
jb_tidy$stat <- sub(".*_(JB|JB_p)$", "\\1", rownames(jb_tidy))
jb_wide <- tidyr::pivot_wider(jb_tidy, names_from = stat, values_from = value)

# combine describe with JB
desc_overall_clean <- desc_overall %>%
  dplyr::select(Mean = mean,
                Median = median,
                Min = min,
                Max = max,
                SD =sd,
                Skewness = skew,
                Kurtosis = kurtosis) %>%                # drop vars and n
  tibble::rownames_to_column("Variable") %>%
  left_join(jb_wide, by = c("Variable" = "var"))

# print nicely
descriptive_stat <-
  desc_overall_clean %>%
  gt() %>%
  tab_header(
    title = md("**Descriptive Statistics**"),   # bold title
    subtitle = NULL
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 3
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "white")  # bold + white text
    ),
    locations = cells_column_labels(everything()) # column headers
  ) %>%
  tab_options(
    table.font.size = "smaller",
    data_row.padding = px(2),
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.background.color = "#2C3E50",    # dark blue
    column_labels.background.color = "#34495E" # lighter blue
  ) %>%
  tab_source_note(
    source_note = md("**Note.** Table reports descriptive statistics for all study variables. Values represent overall mean, median, dispersion, and distributional characteristics across countries and years.")
  )

# Descriptive stats by income group
desc_by_group <- income_inequality %>%
  group_by(income_group) %>%
  summarise(
    # sample size once
    sample_size = n(),
    across(
      all_of(c(Y_var, X_vars)), 
      list(
        mean   = ~mean(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        min    = ~min(.x, na.rm = TRUE),
        max    = ~max(.x, na.rm = TRUE),
        sd     = ~sd(.x, na.rm = TRUE),
        skew   = ~skewness(.x, na.rm = TRUE),
        kurt   = ~kurtosis(.x, na.rm = TRUE),
        JB     = ~suppressWarnings(jarque.bera.test(na.omit(.x))$statistic),
        JB_p   = ~suppressWarnings(jarque.bera.test(na.omit(.x))$p.value)
      ),
      .names = "{col}_{fn}"
    )
  ) %>%
  arrange(income_group)

# show nicely
# Convert to gt table
desc_table <- # ---- Convert to GT table ----
desc_by_group %>%
  gt() %>%
  tab_header(
    title = md("**Descriptive Statistics by Income Group**")
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 3
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold", color = "white")),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = "small",
    data_row.padding = px(2),
    heading.background.color = "#2C3E50",    # dark header
    column_labels.background.color = "#34495E"
  ) %>%
  tab_source_note(
    source_note = md("**Note.** Table reports descriptive statistics (mean, median, spread, distributional shape, and normality tests) by World Bank income groups.")
  )


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
  labs(x = "", y = "", title = "Renewable energy concumption across years") +
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

# Make sure world_map is an sf object with GINI_mean column
ggplot(world_map) +
  geom_sf(aes(fill = GINI_mean), color = "gray60", size = 0.2) +
  scale_fill_distiller(palette = "RdYlBu", direction = 1, name = "Mean GINI") +
  labs(
    title = "Mean Income Inequality (GINI) by Country",
    subtitle = "Average values (1996–2022)",
    caption = "Source: SWIID | Author's computation, 2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0.5),
    legend.position = "right"
  )

# Make sure world_map is an sf object with REC_mean column
ggplot(world_map) +
  geom_sf(aes(fill = REC_mean), color = "gray60", size = 0.2) +
  scale_fill_distiller(palette = "Greens", direction = 1, name = "Mean REC (%)") +
  labs(
    title = "Mean Renewable Energy Consumption by Country",
    subtitle = "Average values (1996–2022)",
    caption = "Source: World Bank (WDI) | Author's computation, 2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0.5),
    legend.position = "right"
  )

library(ggrepel)

# Get last available year for each income_group
label_data <- gini_summary %>%
  group_by(income_group) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup()

ggplot(gini_summary, aes(x = year, y = mean_gini, color = income_group, fill = income_group)) +
  geom_line(size = 0.8) +
  geom_ribbon(aes(ymin = mean_gini - se_gini, ymax = mean_gini + se_gini),
              alpha = 0.25, color = NA) +
  geom_text_repel(
    data = label_data,
    aes(label = income_group),
    nudge_x = 2,
    hjust = 0,
    show.legend = FALSE,
    size = 4,
    fontface = "bold"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Income Inequality Trends by Income Group",
    subtitle = "SWIID Gini Index (Disposable Income)",
    x = "Year",
    y = "Gini Index",
    caption = "Source: SWIID"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13, color = "grey30"),
    axis.title = element_text(face = "bold")
  ) +
  scale_color_manual(values = line_colors) +
  scale_fill_manual(values = fill_colors) +
  scale_x_continuous(
    breaks = seq(min(gini_summary$year, na.rm = TRUE),
                 max(gini_summary$year, na.rm = TRUE) + 5,
                 by = 5),                  # 5-year intervals
    limits = c(min(gini_summary$year, na.rm = TRUE),
               max(gini_summary$year, na.rm = TRUE) + 5)  # extend x-axis
  )


# -----------------------
# 7. Correlation Matrix
# -----------------------
library(corrplot)
library(Hmisc)

# --- 1. Compute correlations and p-values ---
cor_data <- income_inequality %>%
  dplyr::select(all_of(c(Y_var, X_vars))) %>%
  drop_na()

rc <- Hmisc::rcorr(as.matrix(cor_data))
R <- rc$r    # correlation matrix
P <- rc$P    # p-value matrix

# --- 2. Convert to long format ---
vars <- colnames(R)
m <- as.data.frame(as.table(R))
names(m) <- c("Var1", "Var2", "corr")
p_df <- as.data.frame(as.table(P))
names(p_df) <- c("Var1", "Var2", "pval")

df <- left_join(m, p_df, by = c("Var1", "Var2"))

# Keep only lower triangle
df <- df %>%
  mutate(i = match(Var1, vars),
         j = match(Var2, vars)) %>%
  filter(i >= j)

# Factor ordering so first var is at the top
df$Var1 <- factor(df$Var1, levels = rev(vars))
df$Var2 <- factor(df$Var2, levels = vars)

# --- 3. Adaptive text color ---
df <- df %>%
  mutate(text_col = ifelse(abs(corr) > 0.5, "white", "black"),
         label = sprintf("%.2f", corr))

# --- 4. Plot ---
ggplot(df, aes(x = Var2, y = Var1, fill = corr)) +
  geom_tile(color = "grey90", width = 0.95, height = 0.95) +
  geom_text(aes(label = label, color = text_col), size = 4, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient2(
    low = "#b2182b", mid = "white", high = "#2166ac",
    midpoint = 0, limits = c(-1, 1), space = "Lab"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_fixed() +
  guides(fill = guide_colorbar(title = "Correlation", barwidth = 10, barheight = 0.6))

# --- 1. Compute correlations and p-values ---
cor_data <- income_inequality %>%
  dplyr::select(all_of(c(Y_var, X_vars))) %>%
  drop_na()

rc <- Hmisc::rcorr(as.matrix(cor_data))
R <- rc$r    # correlation matrix
P <- rc$P    # p-value matrix

# --- 2. Format correlation with p-values ---
corr_labels <- matrix(
  paste0(sprintf("%.2f", R), " (", signif(P, 2), ")"),
  nrow = nrow(R),
  dimnames = dimnames(R)
)

# --- 3. Keep only lower triangle (including diagonal) ---
corr_labels[upper.tri(corr_labels)] <- ""

# --- 4. Convert to data frame ---
cor_df <- as.data.frame(corr_labels) %>%
  rownames_to_column("Variable")

# --- 5. GT table ---
cor_gt <- cor_df %>%
  gt() %>%
  tab_header(
    title = "Correlation Matrix (Lower Triangle)",
    subtitle = "Pearson correlations with p-values in parentheses"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  opt_table_font(
    font = list(google_font("Lato"), default_fonts())
  ) %>%
  tab_options(
    table.font.size = "small",
    data_row.padding = px(2)
  ) %>%
  tab_options(
    table.font.size = "small",
    data_row.padding = px(2),
    heading.background.color = "#2C3E50",    # dark header
    column_labels.background.color = "#34495E"
  ) %>%
  tab_source_note(
    source_note = md("**Note.** Table reports correlations.")
  )


# --- Output ---
cor_gt



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
