# Income-Inequality-on-renewable-energy-and-SDGs

# 📦 Dataset Inventory

# Data Cleaning and Preprocessing Pipeline Documentation

This README file provides a comprehensive description of the R script used to load, clean, transform, and prepare multiple datasets for analysis. The primary goal is to investigate the relationship between income inequality, renewable energy consumption, and other socio-economic indicators across countries over time.

---

## 1. Load Required Libraries

Essential R packages for data manipulation, visualization, and cleaning are loaded:

* `tidyverse`: Core package for data wrangling and visualization.
* `visdat`: Visualize missing data.
* `countrycode`: Convert country names and codes.
* `zoo`: Used for interpolation.

```r
library(tidyverse)
library(visdat)
library(countrycode)
library(zoo)
```

---

## 2. Load and Organize Raw CSV Files

* All `.csv` files from the `data/` directory are read into a list.
* File names are cleaned and used as keys for referencing.
* Files are split into two groups: `data` (main datasets) and `data_1` (CO2 and GDP).

---

## 3. Data Cleaning and Reshaping

### Functions:

* `clean_df_names()`: Replaces missing values, removes brackets from column names.
* `reshape_data()`: Converts wide format data into long format.

Data is grouped and summarized:

* `df_1`: Transformed socio-economic datasets.
* `df_2`: CO2 and GDP datasets.
* `data_list_1` & `data_list_2`: Grouped by `country` and `year`.

All data are merged using `reduce()` and `full_join()`.

---

## 4. Combine and Label Income Levels

* GDP values are used to classify countries into income levels:

  * Low, Lower-middle, Upper-middle, and High income.
* `income_classification` calculates average GDP from 1996-2020.

The resulting dataset is merged and renamed:

* E.g., `gdp_per_capita`, `renew_energy_cons`, etc.
* Non-country aggregates are removed using `non_countries` vector.

---

## 5. Remove Non-Country Entities

A comprehensive list of regional aggregates and historical/political territories is filtered out using the `non_countries` vector.

---

## 6. Clean and Interpolate WGI Data

* Focuses on "Regulatory Quality" (`rq` indicator).
* Non-numeric characters removed.
* Missing years (1993-2002) are interpolated using `zoo::na.approx()`.

---

## 7. Load and Filter SWIID Data

* GINI coefficients are filtered by year and minimum availability of 25 years.
* Only relevant columns (`country`, `year`, `gini_disp`) are retained.

---

## 8. Merge All Data Sources

* `main`, `swid_df`, and `reg_df` are merged into a single dataset: `income_df`.
* Missing values in `energy_intensity` are interpolated per country.

---

## 9. Final Data Cleaning

* `income_final_df` created:

  * Removes unnecessary columns.
  * Replaces missing numeric values with column means.

---

## 10. Save Final Dataset

* The cleaned and integrated dataset is saved as a CSV:

```r
write_csv(income_final_df,"clean data/income_inequality_on_renewable_energy2")
```

---

## Summary

This script builds a robust pipeline to:

* Load and clean multi-country socio-economic data.
* Handle missing values with interpolation.
* Standardize and reshape data.
* Classify countries by income.
* Prepare a ready-to-use dataset for analysis of income inequality and renewable energy usage.

## 📁 Dataset Files

| File Name                              | Size     | Description                                      | Source            |
|---------------------------------------|----------|--------------------------------------------------|-------------------|
| `CO2.csv`                             | 134.5 KB | Carbon dioxide emissions data                    | World Bank Data   |
| `energy_consumption.csv`             | 106.1 KB | Energy consumption per capita                    | World Bank Data   |
| `energy_intensity.csv`               | 99.4 KB  | Energy intensity (energy used per GDP)           | World Bank Data   |
| `FDI.csv`                             | 229.1 KB | Foreign direct investment inflows                | World Bank Data   |
| `financial-development-Aug-2023.csv` | 3.4 MB   | Financial development indicators (Aug 2023)      | World Bank Data   |
| `financial-development-Nov-2023.csv` | 3.3 MB   | Financial development indicators (Nov 2023)      | World Bank Data   |
| `fossil.csv`                          | 179.3 KB | Fossil fuel energy consumption                   | World Bank Data   |
| `GDP.csv`                             | 183.8 KB | Gross Domestic Product                           | World Bank Data   |
| `GNI.csv`                             | 65.7 KB  | Gross National Income                            | World Bank Data   |
| `HDI.csv`                             | 75.9 KB  | Human Development Index                          | World Bank Data   |
| `LFT.csv`                             | 107.1 KB | Labor force total                                | World Bank Data   |
| `swiid9_6_summary.csv`                | 263.6 KB | Summary from Standardized World Income Inequality Database | World Bank Data   |
| `trade.csv`                           | 212.3 KB | Trade as a % of GDP                              | World Bank Data   |
| `urban.csv`                           | 2.4 MB   | Urban population and growth                      | World Bank Data   |
| `wgi.csv`                             | 1.9 MB   | Worldwide Governance Indicators                  | World Bank Data   |




## 📁 Meta data Files

| File Name                          | Size     | Description                            | Source            |
|-----------------------------------|----------|----------------------------------------|-------------------|
| `Co2.csv`                         | 108.6 KB | CO2 emissions data                     | World Bank Data   |
| `energy_consumption- Metadata.csv`| 106.7 KB | Metadata for energy consumption data   | World Bank Data   |
| `energy_intensity - Metadata.csv` | 100.7 KB | Metadata for energy intensity data     | World Bank Data   |
| `f0aae2ae-fd0a-4ada-afc0-85572...`| 261.3 KB | Possibly a downloaded dataset          | World Bank Data   |
| `FDI - Metadata.csv`              | 233.3 KB | Metadata for foreign direct investment | World Bank Data   |
| `fossil - Metadata.csv`           | 182.2 KB | Metadata for fossil fuel data          | World Bank Data   |
| `gdp.csv`                         | 255.2 KB | GDP data                               | World Bank Data   |
| `GNI- Metadata.csv`               | 70.9 KB  | Metadata for Gross National Income     | World Bank Data   |
| `HDI - Metadata.csv`              | 76.8 KB  | Metadata for Human Development Index   | World Bank Data   |
| `LFT - Metadata.csv`              | 110 KB   | Metadata for Labor Force Total         | World Bank Data   |
| `swiid_source.csv`                | 4.3 MB   | Source file for income inequality      | World Bank Data   |
| `trade - Metadata.csv`            | 212.7 KB | Metadata for trade data                | World Bank Data   |
| `urban - Metadata.csv`            | 2.5 MB   | Metadata for urbanization data         | World Bank Data   |