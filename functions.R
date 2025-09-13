make_desc_table <- function(data, vars, group_var = "income_group", 
                            title = "Descriptive Statistics") {
  
  # Compute descriptive stats
  desc <- data %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      sample_size = n(),
      across(
        all_of(vars), 
        list(
          mean   = ~mean(.x, na.rm = TRUE),
          sd     = ~sd(.x, na.rm = TRUE),
          min    = ~min(.x, na.rm = TRUE),
          max    = ~max(.x, na.rm = TRUE),
          skew   = ~moments::skewness(.x, na.rm = TRUE),
          kurt   = ~moments::kurtosis(.x, na.rm = TRUE),
          JB     = ~suppressWarnings(tseries::jarque.bera.test(na.omit(.x))$statistic),
          JB_p   = ~suppressWarnings(tseries::jarque.bera.test(na.omit(.x))$p.value)
        ),
        .names = "{col}_{fn}"
      )
    ) %>%
    arrange(.data[[group_var]])
  
  # Build GT table
  gt_tbl <- desc %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", title, "**")),
      subtitle = "By Income Group: Mean, Standard Deviation, Min, Max, Skewness, Kurtosis, and JB test"
    ) %>%
    fmt_number(
      columns = where(is.numeric),
      decimals = 3
    ) %>%
    data_color(
      columns = contains("_mean"),
      fn = scales::col_numeric(
        palette = RColorBrewer::brewer.pal(9, "Blues"),
        domain = NULL
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", color = "white"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_options(
      table.font.size = "small",
      data_row.padding = px(2),
      heading.background.color = "#2C3E50",
      column_labels.background.color = "#34495E"
    ) %>%
    cols_align(
      align = "left",
      columns = 1   # group column
    ) %>%
    cols_align(
      align = "center",
      columns = where(is.numeric)
    ) %>%
    tab_source_note(
      source_note = md("**Note.** Table reports descriptive statistics by income group, including distributional tests (Jarque-Bera).")
    )
  
  return(gt_tbl)
}
