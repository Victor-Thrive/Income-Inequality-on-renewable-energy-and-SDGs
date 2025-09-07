make_desc_table <- function(data, vars, group_var = "income_group", title = "Descriptive Statistics") {
  
  desc <- data %>%
    group_by(.data[[group_var]]) %>%
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
    arrange(.data[[group_var]])
  
  # Enhance with gt
  gt_tbl <- desc %>%
    gt() %>%
    tab_header(
      title = title,
      subtitle = "Mean, SD, and Sample Size by Income Group"
    ) %>%
    fmt_number(
      columns = where(is.numeric),
      decimals = 2
    ) %>%
    data_color(
      columns = contains("_mean"),
      fn = scales::col_numeric(
        palette = RColorBrewer::brewer.pal(9, "Blues"),
        domain = NULL
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_options(
      table.font.size = 12,
      table.border.top.style = "solid",
      table.border.bottom.style = "solid"
    ) %>%
    # 🔹 Alignment
    cols_align(
      align = "left",
      columns = 1     # first column (income group)
    ) %>%
    cols_align(
      align = "center",
      columns = where(is.numeric)   # all numeric columns
    )
  
  return(gt_tbl)
}
