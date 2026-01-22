# ==============================================================================
# EXCEL AUDIT WORKBOOK GENERATOR - RECREATES LM STATS FROM SCRATCH
# ==============================================================================
# Creates a complete Labour Market Statistics workbook with:
# - Dashboard with summary metrics
# - All data sheets with source data from database
# - Column names mapped to dataset identifier codes
# ==============================================================================

suppressPackageStartupMessages({
  library(openxlsx)
  library(scales)
  library(dplyr)
  library(lubridate)
  library(DBI)
  library(RPostgres)
  library(tibble)
  library(tidyr)
})

# ==============================================================================
# COLUMN NAMES - CONSTRUCTED FROM DATABASE COLUMNS
# ==============================================================================
# Column names are constructed dynamically from descriptive columns in the
# database rather than using hardcoded mappings of dataset_indentifier_code.
#
# Table                     | Column(s) used for names
# --------------------------|--------------------------------------------------
# age_group                 | economic_activity, value_type, age_group
# vacancies_business        | business_metric
# payrolled_employees       | unit_type
# employees_industry        | industry
# weekly_earnings_total     | sector, earnings_metric, time_basis
# weekly_earnings_regular   | sector, earnings_metric, time_basis
# weekly_earnings_cpi       | earnings_type, earnings_metric
# inactivity                | inactivity_measure, inactivity_reason
# redundancies              | population_group, value_type
# disputes                  | dispute_type
# redundancies_region       | region
# ==============================================================================

# ==============================================================================
# STYLES
# ==============================================================================

style_header <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#0C275C", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", wrapText = TRUE,
  fontSize = 10, fontName = "Arial"
)

style_metric <- createStyle(
  fgFill = "#CEDFF0", halign = "left", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight",
  fontSize = 10, fontName = "Arial"
)

style_metric_italic <- createStyle(
  fgFill = "#CEDFF0", halign = "left", valign = "center",
  textDecoration = "italic", border = "TopBottomLeftRight",
  fontSize = 9, fontName = "Arial"
)

style_value <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial"
)

style_positive <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#006400"
)

style_negative <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#8B0000"
)

style_data_header <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#404040", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", wrapText = TRUE
)

style_summary_header <- createStyle(
  fontColour = "#FFFFFF", fgFill = "#0C275C", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", wrapText = TRUE
)

style_summary_label <- createStyle(
  fgFill = "#D6DCE4", halign = "left", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight"
)

style_summary_value <- createStyle(
  fgFill = "#D6DCE4", halign = "right", valign = "center",
  border = "TopBottomLeftRight", numFmt = "#,##0.0"
)

style_election_label <- createStyle(
  fgFill = "#FFFF00", halign = "left", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight"
)

style_title <- createStyle(
  textDecoration = "bold", fontSize = 14, fontName = "Arial"
)

style_subtitle <- createStyle(
  fontSize = 10, fontName = "Arial", fontColour = "#666666"
)

# Number format styles
style_value_int <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  numFmt = "#,##0"
)

style_value_dec1 <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  numFmt = "#,##0.0"
)

style_positive_int <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#006400", numFmt = "+#,##0;-#,##0;0"
)

style_negative_int <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#8B0000", numFmt = "+#,##0;-#,##0;0"
)

style_positive_dec1 <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#006400", numFmt = "+#,##0.0;-#,##0.0;0.0"
)

style_negative_dec1 <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#8B0000", numFmt = "+#,##0.0;-#,##0.0;0.0"
)

style_neutral_int <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#000000", numFmt = "+#,##0;-#,##0;0"
)

style_neutral_dec1 <- createStyle(
  fgFill = "#CEDFF0", halign = "center", valign = "center",
  border = "TopBottomLeftRight", fontSize = 10, fontName = "Arial",
  fontColour = "#000000", numFmt = "+#,##0.0;-#,##0.0;0.0"
)

# ==============================================================================
# DATABASE HELPER
# ==============================================================================

fetch_db <- function(query) {
  conn <- DBI::dbConnect(RPostgres::Postgres())
  tryCatch({
    result <- DBI::dbGetQuery(conn, query)
    tibble::as_tibble(result)
  }, error = function(e) {
    warning("DB error: ", e$message)
    tibble::tibble()
  }, finally = DBI::dbDisconnect(conn))
}

# ==============================================================================
# LABEL GENERATORS
# ==============================================================================

make_lfs_label <- function(end_date) {
  start_date <- end_date %m-% months(2)
  sprintf("%s-%s %s",
          format(start_date, "%b"),
          format(end_date, "%b"),
          format(end_date, "%Y"))
}

make_payroll_label <- function(date) {
  format(date, "%B %Y")
}

make_ymd_label <- function(date) {
  format(date, "%Y-%m-%d")
}

make_datetime_label <- function(date) {
  paste0(format(date, "%Y-%m-%d"), " 00:00:00")
}

# ==============================================================================
# EXCEL COLUMN LETTER HELPER
# ==============================================================================
# Converts column number to Excel letter (1=A, 26=Z, 27=AA, 28=AB, etc.)

col_to_letter <- function(col_num) {
  result <- ""
  while (col_num > 0) {
    remainder <- (col_num - 1) %% 26
    result <- paste0(LETTERS[remainder + 1], result)
    col_num <- (col_num - 1) %/% 26
  }
  result
}

# ==============================================================================
# CHRONOLOGICAL SORT HELPER
# ==============================================================================

make_sort_key <- function(date_str) {
  x <- as.character(date_str)
  if (is.na(x) || x == "") return(NA_real_)

  clean_x <- trimws(gsub("\\s*\\[.*\\]\\s*$", "", x))

  # Format: "Jul-Sep 2025"
  if (grepl("[A-Za-z]{3}.*[A-Za-z]{3}.*[0-9]{4}", clean_x)) {
    month_map <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)
    months_found <- regmatches(clean_x, gregexpr("[A-Za-z]{3}", clean_x))[[1]]
    year_found <- regmatches(clean_x, gregexpr("[0-9]{4}", clean_x))[[1]]

    if (length(months_found) >= 2 && length(year_found) >= 1) {
      end_month <- month_map[tolower(months_found[2])]
      yr <- as.integer(year_found[1])
      if (!is.na(end_month) && !is.na(yr)) {
        return(yr * 100 + end_month)
      }
    }
  }

  # Format: "July 2025"
  d <- suppressWarnings(as.Date(paste0("01 ", clean_x), format = "%d %B %Y"))
  if (!is.na(d)) {
    return(as.numeric(format(d, "%Y")) * 100 + as.numeric(format(d, "%m")))
  }

  # Format: "2025-07-01"
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", clean_x)) {
    yr <- as.integer(substr(clean_x, 1, 4))
    mo <- as.integer(substr(clean_x, 6, 7))
    if (!is.na(yr) && !is.na(mo)) {
      return(yr * 100 + mo)
    }
  }

  NA_real_
}

sort_chronologically <- function(df) {
  if (nrow(df) == 0 || !"Date" %in% names(df)) return(df)
  sort_keys <- vapply(df$Date, make_sort_key, numeric(1))
  df[order(sort_keys, na.last = TRUE), ]
}

# ==============================================================================
# DATA FETCH FUNCTIONS - WIDE FORMAT WITH CODE NAMES
# ==============================================================================

fetch_lfs_wide <- function() {
  query <- 'SELECT time_period, economic_activity, value_type, age_group, value
            FROM "ons"."labour_market__age_group"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = paste(economic_activity, value_type, age_group)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)

  sort_chronologically(result)
}

fetch_vacancies_wide <- function() {
  query <- 'SELECT time_period, business_metric, value
            FROM "ons"."labour_market__vacancies_business"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  result <- raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, business_metric) %>%
    summarise(value = first(value), .groups = "drop") %>%
    pivot_wider(names_from = business_metric, values_from = value) %>%
    rename(Date = time_period)

  sort_chronologically(result)
}

fetch_payroll_wide <- function() {
  query <- 'SELECT time_period, unit_type, value
            FROM "ons"."labour_market__payrolled_employees"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  result <- raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, unit_type) %>%
    summarise(value = first(value), .groups = "drop") %>%
    pivot_wider(names_from = unit_type, values_from = value) %>%
    rename(Date = time_period)

  sort_chronologically(result)
}

fetch_industry_wide <- function() {
  query <- 'SELECT time_period, industry, value
            FROM "ons"."labour_market__employees_industry"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  result <- raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, industry) %>%
    summarise(value = first(value), .groups = "drop") %>%
    pivot_wider(names_from = industry, values_from = value) %>%
    rename(Date = time_period)

  sort_chronologically(result)
}

fetch_wages_total_wide <- function() {
  query <- 'SELECT time_period, sector, earnings_metric, time_basis, value
            FROM "ons"."labour_market__weekly_earnings_total"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = paste(sector, earnings_metric, time_basis)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)

  sort_chronologically(result)
}

fetch_wages_regular_wide <- function() {
  query <- 'SELECT time_period, sector, earnings_metric, time_basis, value
            FROM "ons"."labour_market__weekly_earnings_regular"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = paste(sector, earnings_metric, time_basis)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)

  sort_chronologically(result)
}

fetch_wages_cpi_wide <- function() {
  query <- 'SELECT time_period, earnings_type, earnings_metric, value
            FROM "ons"."labour_market__weekly_earnings_cpi"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = paste(earnings_type, "-", earnings_metric)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)

  sort_chronologically(result)
}

fetch_inactivity_wide <- function() {
  query <- 'SELECT time_period, inactivity_measure, inactivity_reason, value
            FROM "ons"."labour_market__inactivity"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = paste(inactivity_measure, inactivity_reason)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)

  sort_chronologically(result)
}

fetch_redundancy_wide <- function() {
  query <- 'SELECT time_period, population_group, value_type, value
            FROM "ons"."labour_market__redundancies"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  result <- raw %>%
    mutate(
      value = as.numeric(value),
      col_name = paste(population_group, value_type)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period)

  sort_chronologically(result)
}

fetch_days_lost_wide <- function() {
  query <- 'SELECT time_period, dispute_type, value
            FROM "ons"."labour_market__disputes"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  result <- raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, dispute_type) %>%
    summarise(value = first(value), .groups = "drop") %>%
    pivot_wider(names_from = dispute_type, values_from = value) %>%
    rename(Date = time_period)

  sort_chronologically(result)
}

fetch_hr1_wide <- function() {
  query <- 'SELECT time_period, region, value
            FROM "ons"."labour_market__redundancies_region"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  result <- raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, region) %>%
    summarise(value = first(value), .groups = "drop") %>%
    pivot_wider(names_from = region, values_from = value) %>%
    rename(Date = time_period)

  sort_chronologically(result)
}

# ==============================================================================
# BUILD DASHBOARD SHEET
# ==============================================================================

build_dashboard_sheet <- function(wb, lfs_period_label, envir = parent.frame()) {

  sheet <- "Dashboard"
  addWorksheet(wb, sheet)

  get_val <- function(name) {
    if (exists(name, envir = envir)) get(name, envir = envir) else NA_real_
  }

  metrics <- tribble(
    ~Metric, ~type, ~invert, ~italic,
    "Employment (000s) 16+", "count", FALSE, FALSE,
    "Employment rate (16-64)", "rate", FALSE, FALSE,
    "Unemployment, 16+ (000s)", "count", TRUE, FALSE,
    "Unemployment rate, 16+", "rate", TRUE, FALSE,
    "Economic inactivity (000s) (16-64)", "count", TRUE, FALSE,
    "50-64s inactivity (000s)", "count", TRUE, TRUE,
    "Economic inactivity rate (16-64)", "rate", TRUE, FALSE,
    "50-64s inactivity rate", "rate", TRUE, TRUE,
    "Vacancies (000s)", "exempt", NA, FALSE,
    "Payroll employees (000s)", "exempt", FALSE, FALSE,
    "Annual average wages in cash terms (total pay, incl. bonuses)", "wages", FALSE, FALSE,
    "Annual average wages adjusted for CPI inflation (total pay, incl. bonuses)", "wages", FALSE, FALSE
  )

  var_map <- list(
    "Employment (000s) 16+" = list(cur = "emp16_cur", dq = "emp16_dq", dy = "emp16_dy", dc = "emp16_dc", de = "emp16_de"),
    "Employment rate (16-64)" = list(cur = "emp_rt_cur", dq = "emp_rt_dq", dy = "emp_rt_dy", dc = "emp_rt_dc", de = "emp_rt_de"),
    "Unemployment, 16+ (000s)" = list(cur = "unemp16_cur", dq = "unemp16_dq", dy = "unemp16_dy", dc = "unemp16_dc", de = "unemp16_de"),
    "Unemployment rate, 16+" = list(cur = "unemp_rt_cur", dq = "unemp_rt_dq", dy = "unemp_rt_dy", dc = "unemp_rt_dc", de = "unemp_rt_de"),
    "Economic inactivity (000s) (16-64)" = list(cur = "inact_cur", dq = "inact_dq", dy = "inact_dy", dc = "inact_dc", de = "inact_de"),
    "50-64s inactivity (000s)" = list(cur = "inact5064_cur", dq = "inact5064_dq", dy = "inact5064_dy", dc = "inact5064_dc", de = "inact5064_de"),
    "Economic inactivity rate (16-64)" = list(cur = "inact_rt_cur", dq = "inact_rt_dq", dy = "inact_rt_dy", dc = "inact_rt_dc", de = "inact_rt_de"),
    "50-64s inactivity rate" = list(cur = "inact5064_rt_cur", dq = "inact5064_rt_dq", dy = "inact5064_rt_dy", dc = "inact5064_rt_dc", de = "inact5064_rt_de"),
    "Vacancies (000s)" = list(cur = "vac_cur", dq = "vac_dq", dy = "vac_dy", dc = "vac_dc", de = "vac_de"),
    "Payroll employees (000s)" = list(cur = "payroll_cur", dq = "payroll_dq", dy = "payroll_dy", dc = "payroll_dc", de = "payroll_de"),
    "Annual average wages in cash terms (total pay, incl. bonuses)" = list(cur = "latest_wages", dq = "wages_change_q", dy = "wages_change_y", dc = "wages_change_covid", de = "wages_change_election"),
    "Annual average wages adjusted for CPI inflation (total pay, incl. bonuses)" = list(cur = "latest_wages_cpi", dq = "wages_cpi_change_q", dy = "wages_cpi_change_y", dc = "wages_cpi_change_covid", de = "wages_cpi_change_election")
  )

  # Title
  writeData(wb, sheet, paste0("Key Metrics, ", lfs_period_label), startRow = 1, startCol = 1)
  addStyle(wb, sheet, style_title, rows = 1, cols = 1)

  writeData(wb, sheet, paste("Generated:", format(Sys.time(), "%d %B %Y %H:%M")), startRow = 2, startCol = 1)
  addStyle(wb, sheet, style_subtitle, rows = 2, cols = 1)

  # Headers
  headers <- c("Metric", "Current", "Change on quarter", "Change on the year",
               "Change since Covid-19", "Change since 2024 election")

  header_row <- 4
  for (i in seq_along(headers)) {
    writeData(wb, sheet, headers[i], startRow = header_row, startCol = i)
  }
  addStyle(wb, sheet, style_header, rows = header_row, cols = 1:6, gridExpand = TRUE)

  # Data rows
  for (row_idx in seq_len(nrow(metrics))) {
    metric_name <- metrics$Metric[row_idx]
    metric_type <- metrics$type[row_idx]
    is_invert <- metrics$invert[row_idx]
    is_italic <- metrics$italic[row_idx]

    vars <- var_map[[metric_name]]
    if (is.null(vars)) next

    cur_val <- get_val(vars$cur)
    dq_val <- get_val(vars$dq)
    dy_val <- get_val(vars$dy)
    dc_val <- get_val(vars$dc)
    de_val <- get_val(vars$de)

    excel_row <- header_row + row_idx

    # Metric name
    writeData(wb, sheet, metric_name, startRow = excel_row, startCol = 1)
    addStyle(wb, sheet, if(is_italic) style_metric_italic else style_metric, rows = excel_row, cols = 1)

    is_int_type <- metric_type %in% c("count", "exempt", "wages")

    # Transform current value
    if (metric_type == "count") {
      cur_display <- cur_val / 1000
    } else {
      cur_display <- cur_val
    }

    # Write current value
    writeData(wb, sheet, cur_display, startRow = excel_row, startCol = 2)
    addStyle(wb, sheet, if(is_int_type) style_value_int else style_value_dec1, rows = excel_row, cols = 2)

    # Style helper
    get_change_style <- function(val, is_int) {
      if (is.na(is_invert)) {
        return(if(is_int) style_neutral_int else style_neutral_dec1)
      }
      if (is.na(val) || val == 0) {
        return(if(is_int) style_value_int else style_value_dec1)
      }
      is_positive <- val > 0
      if (is_invert) is_positive <- !is_positive

      if (is_positive) {
        if(is_int) style_positive_int else style_positive_dec1
      } else {
        if(is_int) style_negative_int else style_negative_dec1
      }
    }

    transform_change <- function(val) {
      if (metric_type == "count") val / 1000 else val
    }

    # Write change values
    changes <- list(dq_val, dy_val, dc_val, de_val)
    for (i in seq_along(changes)) {
      col <- i + 2
      change_val <- transform_change(changes[[i]])
      writeData(wb, sheet, change_val, startRow = excel_row, startCol = col)
      addStyle(wb, sheet, get_change_style(changes[[i]], is_int_type), rows = excel_row, cols = col)
    }
  }

  # Column widths
  setColWidths(wb, sheet, cols = 1, widths = 55)
  setColWidths(wb, sheet, cols = 2:6, widths = 18)
  setRowHeights(wb, sheet, rows = header_row, heights = 30)

  freezePane(wb, sheet, firstRow = TRUE, firstCol = TRUE, firstActiveRow = 5, firstActiveCol = 2)

  wb
}

# ==============================================================================
# BUILD DATA SHEET WITH SUMMARY AND SOURCE DATA
# ==============================================================================

build_data_sheet <- function(wb, sheet_name, title, data, source_info = NULL,
                             label_type = "lfs", anchor_date = NULL,
                             covid_label = NULL, election_label = NULL,
                             code_map = NULL) {

  addWorksheet(wb, sheet_name)

  if (is.null(data) || nrow(data) == 0) {
    writeData(wb, sheet_name, "No data available", startRow = 1, startCol = 1)
    return(wb)
  }

  # Title section
  writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
  addStyle(wb, sheet_name, style_title, rows = 1, cols = 1)

  if (!is.null(source_info)) {
    writeData(wb, sheet_name, paste("Source:", source_info), startRow = 2, startCol = 1)
    addStyle(wb, sheet_name, style_subtitle, rows = 2, cols = 1)
  }

  # Show code mapping if provided
  if (!is.null(code_map)) {
    writeData(wb, sheet_name, "Dataset Codes:", startRow = 3, startCol = 1)
    code_info <- paste(names(code_map), "=", code_map, collapse = " | ")
    writeData(wb, sheet_name, code_info, startRow = 3, startCol = 2)
    addStyle(wb, sheet_name, style_subtitle, rows = 3, cols = 1:2)
  }

  writeData(wb, sheet_name, paste("Rows:", nrow(data), "| Generated:", format(Sys.time(), "%d %B %Y %H:%M")),
            startRow = 4, startCol = 1)
  addStyle(wb, sheet_name, style_subtitle, rows = 4, cols = 1)

  value_cols <- setdiff(names(data), "Date")
  n_cols <- length(value_cols)

  # Data table
  data_start <- 15

  writeData(wb, sheet_name, "Source Data", startRow = data_start - 1, startCol = 1)
  addStyle(wb, sheet_name, createStyle(textDecoration = "bold", fontSize = 11), rows = data_start - 1, cols = 1)

  writeData(wb, sheet_name, data, startRow = data_start, startCol = 1,
            colNames = TRUE, headerStyle = style_data_header)

  # Summary section with formulas
  summary_start <- 6
  can_calc_summary <- !is.null(anchor_date)

  if (can_calc_summary) {
    # Generate labels based on type
    if (label_type == "lfs") {
      lab_cur <- make_lfs_label(anchor_date)
      lab_q <- make_lfs_label(anchor_date %m-% months(3))
      lab_y <- make_lfs_label(anchor_date %m-% months(12))
      lab_covid <- if (!is.null(covid_label)) covid_label else "Dec-Feb 2020"
      lab_election <- if (!is.null(election_label)) election_label else "Apr-Jun 2024"
    } else if (label_type == "payroll") {
      lab_cur <- make_payroll_label(anchor_date)
      lab_q <- make_payroll_label(anchor_date %m-% months(3))
      lab_y <- make_payroll_label(anchor_date %m-% months(12))
      lab_covid <- if (!is.null(covid_label)) covid_label else "February 2020"
      lab_election <- if (!is.null(election_label)) election_label else "June 2024"
    } else if (label_type == "ymd") {
      lab_cur <- make_ymd_label(anchor_date)
      lab_q <- make_ymd_label(anchor_date %m-% months(3))
      lab_y <- make_ymd_label(anchor_date %m-% months(12))
      lab_covid <- if (!is.null(covid_label)) covid_label else "2020-02-01"
      lab_election <- if (!is.null(election_label)) election_label else "2024-06-01"
    } else {
      lab_cur <- make_datetime_label(anchor_date)
      lab_q <- make_datetime_label(anchor_date %m-% months(3))
      lab_y <- make_datetime_label(anchor_date %m-% months(12))
      lab_covid <- if (!is.null(covid_label)) covid_label else "2020-02-01 00:00:00"
      lab_election <- if (!is.null(election_label)) election_label else "2024-06-01 00:00:00"
    }

    # Find data rows
    find_data_row <- function(label) {
      idx <- which(trimws(data$Date) == trimws(label))
      if (length(idx) > 0) return(idx[1])
      idx <- which(startsWith(trimws(data$Date), trimws(label)))
      if (length(idx) > 0) return(idx[1])
      NA
    }

    row_cur <- find_data_row(lab_cur)
    row_q <- find_data_row(lab_q)
    row_y <- find_data_row(lab_y)
    row_covid <- find_data_row(lab_covid)
    row_election <- find_data_row(lab_election)

    to_excel_row <- function(data_idx) {
      if (is.null(data_idx) || length(data_idx) == 0 || is.na(data_idx)) return(NA)
      data_start + data_idx
    }

    excel_row_cur <- to_excel_row(row_cur)
    excel_row_q <- to_excel_row(row_q)
    excel_row_y <- to_excel_row(row_y)
    excel_row_covid <- to_excel_row(row_covid)
    excel_row_election <- to_excel_row(row_election)

    # Summary header
    writeData(wb, sheet_name, "Summary", startRow = summary_start, startCol = 1)
    for (i in seq_along(value_cols)) {
      writeData(wb, sheet_name, value_cols[i], startRow = summary_start, startCol = i + 1)
    }
    addStyle(wb, sheet_name, style_summary_header, rows = summary_start, cols = 1:(n_cols + 1), gridExpand = TRUE)

    # Summary rows
    summary_rows <- list(
      list(label = paste0("Current (", lab_cur, ")"), comp_excel_row = NULL, is_current = TRUE),
      list(label = paste0("Change on quarter (vs ", lab_q, ")"), comp_excel_row = excel_row_q, is_current = FALSE),
      list(label = paste0("Change on year (vs ", lab_y, ")"), comp_excel_row = excel_row_y, is_current = FALSE),
      list(label = paste0("Change since Covid (vs ", lab_covid, ")"), comp_excel_row = excel_row_covid, is_current = FALSE),
      list(label = paste0("Change since election (vs ", lab_election, ")"), comp_excel_row = excel_row_election, is_current = FALSE)
    )

    for (s in seq_along(summary_rows)) {
      row_num <- summary_start + s
      sr <- summary_rows[[s]]

      writeData(wb, sheet_name, sr$label, startRow = row_num, startCol = 1)

      if (s == 5) {
        addStyle(wb, sheet_name, style_election_label, rows = row_num, cols = 1)
      } else {
        addStyle(wb, sheet_name, style_summary_label, rows = row_num, cols = 1)
      }

      for (i in seq_along(value_cols)) {
        col_letter <- col_to_letter(i + 1)

        if (sr$is_current) {
          if (!is.na(excel_row_cur)) {
            formula <- paste0("=", col_letter, excel_row_cur)
            writeFormula(wb, sheet_name, formula, startRow = row_num, startCol = i + 1)
          }
        } else {
          if (!is.na(excel_row_cur) && !is.na(sr$comp_excel_row)) {
            formula <- paste0("=", col_letter, excel_row_cur, "-", col_letter, sr$comp_excel_row)
            writeFormula(wb, sheet_name, formula, startRow = row_num, startCol = i + 1)
          }
        }
        addStyle(wb, sheet_name, style_summary_value, rows = row_num, cols = i + 1)
      }
    }
  }

  # Column widths
  setColWidths(wb, sheet_name, cols = 1, widths = 45)
  for (i in seq_along(value_cols)) {
    max_width <- max(nchar(as.character(value_cols[i])), 12, na.rm = TRUE)
    setColWidths(wb, sheet_name, cols = i + 1, widths = min(max_width + 2, 25))
  }

  freezePane(wb, sheet_name, firstRow = TRUE, firstActiveRow = data_start + 1)
  addFilter(wb, sheet_name, row = data_start, cols = 1:ncol(data))

  wb
}

# ==============================================================================
# BUILD PAYROLL SHEET WITH 3 SUMMARY TABLES
# ==============================================================================
# Table 1: Current = latest month (e.g., December)
# Table 2: Current = 3-month average ending at latest (Oct-Nov-Dec)
# Table 3: Current = 3-month average ending at previous month (Sep-Oct-Nov)

build_payroll_sheet <- function(wb, sheet_name, title, data, source_info = NULL,
                                anchor_date = NULL, covid_label = "February 2020",
                                election_label = "June 2024") {

  addWorksheet(wb, sheet_name)

  if (is.null(data) || nrow(data) == 0) {
    writeData(wb, sheet_name, "No data available", startRow = 1, startCol = 1)
    return(wb)
  }

  # Title section
  writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
  addStyle(wb, sheet_name, style_title, rows = 1, cols = 1)

  if (!is.null(source_info)) {
    writeData(wb, sheet_name, paste("Source:", source_info), startRow = 2, startCol = 1)
    addStyle(wb, sheet_name, style_subtitle, rows = 2, cols = 1)
  }

  writeData(wb, sheet_name, paste("Rows:", nrow(data), "| Generated:", format(Sys.time(), "%d %B %Y %H:%M")),
            startRow = 3, startCol = 1)
  addStyle(wb, sheet_name, style_subtitle, rows = 3, cols = 1)

  value_cols <- setdiff(names(data), "Date")
  n_cols <- length(value_cols)

  # Data table starts lower to make room for 3 vertical summary tables
  # Each table has 6 rows (header + 5 data rows) plus 1 gap row
  data_start <- 28

  writeData(wb, sheet_name, "Source Data", startRow = data_start - 1, startCol = 1)
  addStyle(wb, sheet_name, createStyle(textDecoration = "bold", fontSize = 11), rows = data_start - 1, cols = 1)

  writeData(wb, sheet_name, data, startRow = data_start, startCol = 1,
            colNames = TRUE, headerStyle = style_data_header)

  # Summary section with 3 tables stacked vertically
  can_calc_summary <- !is.null(anchor_date)

  if (can_calc_summary) {
    # Generate month labels
    lab_cur <- make_payroll_label(anchor_date)
    lab_m1 <- make_payroll_label(anchor_date %m-% months(1))
    lab_m2 <- make_payroll_label(anchor_date %m-% months(2))
    lab_m3 <- make_payroll_label(anchor_date %m-% months(3))
    lab_m4 <- make_payroll_label(anchor_date %m-% months(4))

    # Comparison labels
    lab_q <- make_payroll_label(anchor_date %m-% months(3))
    lab_y <- make_payroll_label(anchor_date %m-% months(12))
    lab_covid <- covid_label
    lab_election <- election_label

    # For 3-month averages, need comparison periods
    lab_q_m1 <- make_payroll_label(anchor_date %m-% months(4))
    lab_q_m2 <- make_payroll_label(anchor_date %m-% months(5))
    lab_y_m1 <- make_payroll_label(anchor_date %m-% months(13))
    lab_y_m2 <- make_payroll_label(anchor_date %m-% months(14))

    # Find data rows
    find_data_row <- function(label) {
      idx <- which(trimws(data$Date) == trimws(label))
      if (length(idx) > 0) return(idx[1])
      idx <- which(startsWith(trimws(data$Date), trimws(label)))
      if (length(idx) > 0) return(idx[1])
      NA
    }

    to_excel_row <- function(data_idx) {
      if (is.null(data_idx) || length(data_idx) == 0 || is.na(data_idx)) return(NA)
      data_start + data_idx
    }

    # Find all needed rows
    row_cur <- to_excel_row(find_data_row(lab_cur))
    row_m1 <- to_excel_row(find_data_row(lab_m1))
    row_m2 <- to_excel_row(find_data_row(lab_m2))
    row_m3 <- to_excel_row(find_data_row(lab_m3))
    row_m4 <- to_excel_row(find_data_row(lab_m4))
    row_q <- to_excel_row(find_data_row(lab_q))
    row_q_m1 <- to_excel_row(find_data_row(lab_q_m1))
    row_q_m2 <- to_excel_row(find_data_row(lab_q_m2))
    row_y <- to_excel_row(find_data_row(lab_y))
    row_y_m1 <- to_excel_row(find_data_row(lab_y_m1))
    row_y_m2 <- to_excel_row(find_data_row(lab_y_m2))
    row_covid <- to_excel_row(find_data_row(lab_covid))
    row_election <- to_excel_row(find_data_row(lab_election))

    # Helper to write a summary table (vertically stacked, columns align with data)
    write_summary_table <- function(start_row, table_title, current_formula_fn, compare_formula_fn) {
      # Header row
      writeData(wb, sheet_name, table_title, startRow = start_row, startCol = 1)
      for (i in seq_along(value_cols)) {
        writeData(wb, sheet_name, value_cols[i], startRow = start_row, startCol = i + 1)
      }
      addStyle(wb, sheet_name, style_summary_header, rows = start_row,
               cols = 1:(n_cols + 1), gridExpand = TRUE)

      # Row labels
      row_labels <- c(
        paste0("Current (", table_title, ")"),
        paste0("Change on quarter"),
        paste0("Change on year"),
        paste0("Change since Covid"),
        paste0("Change since election")
      )

      for (s in 1:5) {
        row_num <- start_row + s
        writeData(wb, sheet_name, row_labels[s], startRow = row_num, startCol = 1)

        if (s == 5) {
          addStyle(wb, sheet_name, style_election_label, rows = row_num, cols = 1)
        } else {
          addStyle(wb, sheet_name, style_summary_label, rows = row_num, cols = 1)
        }

        for (i in seq_along(value_cols)) {
          data_col_letter <- col_to_letter(i + 1)

          formula <- if (s == 1) {
            current_formula_fn(data_col_letter)
          } else {
            compare_formula_fn(data_col_letter, s)
          }

          if (!is.null(formula) && formula != "") {
            writeFormula(wb, sheet_name, formula, startRow = row_num, startCol = i + 1)
          }
          addStyle(wb, sheet_name, style_summary_value, rows = row_num, cols = i + 1)
        }
      }
    }

    # TABLE 1: Single month (latest) - starts at row 5
    write_summary_table(
      start_row = 5,
      table_title = lab_cur,
      current_formula_fn = function(col) {
        if (isTRUE(!is.na(row_cur))) paste0("=", col, row_cur) else ""
      },
      compare_formula_fn = function(col, row_type) {
        comp_row <- switch(as.character(row_type),
          "2" = row_q,
          "3" = row_y,
          "4" = row_covid,
          "5" = row_election
        )
        if (isTRUE(!is.na(row_cur)) && isTRUE(!is.na(comp_row))) {
          paste0("=", col, row_cur, "-", col, comp_row)
        } else ""
      }
    )

    # TABLE 2: 3-month average ending at latest (e.g., Oct-Nov-Dec) - starts at row 12
    table2_title <- paste0(format(anchor_date %m-% months(2), "%b"), "-",
                           format(anchor_date, "%b %Y"), " (3mo avg)")

    write_summary_table(
      start_row = 12,
      table_title = table2_title,
      current_formula_fn = function(col) {
        rows_to_avg <- c(row_cur, row_m1, row_m2)
        rows_to_avg <- rows_to_avg[!is.na(rows_to_avg)]
        if (length(rows_to_avg) > 0) {
          refs <- paste0(col, rows_to_avg, collapse = ",")
          paste0("=AVERAGE(", refs, ")")
        } else ""
      },
      compare_formula_fn = function(col, row_type) {
        # Current 3mo avg
        cur_rows <- c(row_cur, row_m1, row_m2)
        cur_rows <- cur_rows[!is.na(cur_rows)]

        # Comparison 3mo avg
        comp_rows <- switch(as.character(row_type),
          "2" = c(row_q, row_q_m1, row_q_m2),
          "3" = c(row_y, row_y_m1, row_y_m2),
          "4" = c(row_covid),  # Single month for covid/election
          "5" = c(row_election)
        )
        if (is.null(comp_rows)) comp_rows <- NA
        comp_rows <- comp_rows[!is.na(comp_rows)]

        if (length(cur_rows) > 0 && length(comp_rows) > 0) {
          cur_refs <- paste0(col, cur_rows, collapse = ",")
          comp_refs <- paste0(col, comp_rows, collapse = ",")
          paste0("=AVERAGE(", cur_refs, ")-AVERAGE(", comp_refs, ")")
        } else ""
      }
    )

    # TABLE 3: 3-month average ending at previous month (e.g., Sep-Oct-Nov) - starts at row 19
    table3_title <- paste0(format(anchor_date %m-% months(3), "%b"), "-",
                           format(anchor_date %m-% months(1), "%b %Y"), " (3mo avg)")

    write_summary_table(
      start_row = 19,
      table_title = table3_title,
      current_formula_fn = function(col) {
        rows_to_avg <- c(row_m1, row_m2, row_m3)
        rows_to_avg <- rows_to_avg[!is.na(rows_to_avg)]
        if (length(rows_to_avg) > 0) {
          refs <- paste0(col, rows_to_avg, collapse = ",")
          paste0("=AVERAGE(", refs, ")")
        } else ""
      },
      compare_formula_fn = function(col, row_type) {
        # Current 3mo avg (prev month)
        cur_rows <- c(row_m1, row_m2, row_m3)
        cur_rows <- cur_rows[!is.na(cur_rows)]

        # Comparison - shift by 1 month
        comp_rows <- switch(as.character(row_type),
          "2" = c(row_q_m1, row_q_m2, to_excel_row(find_data_row(make_payroll_label(anchor_date %m-% months(6))))),
          "3" = c(row_y_m1, row_y_m2, to_excel_row(find_data_row(make_payroll_label(anchor_date %m-% months(15))))),
          "4" = c(row_covid),
          "5" = c(row_election)
        )
        if (is.null(comp_rows)) comp_rows <- NA
        comp_rows <- comp_rows[!is.na(comp_rows)]

        if (length(cur_rows) > 0 && length(comp_rows) > 0) {
          cur_refs <- paste0(col, cur_rows, collapse = ",")
          comp_refs <- paste0(col, comp_rows, collapse = ",")
          paste0("=AVERAGE(", cur_refs, ")-AVERAGE(", comp_refs, ")")
        } else ""
      }
    )

    # Column widths (all tables use same columns, stacked vertically)
    setColWidths(wb, sheet_name, cols = 1, widths = 35)
    for (i in seq_along(value_cols)) {
      setColWidths(wb, sheet_name, cols = i + 1, widths = 18)
    }
  }

  freezePane(wb, sheet_name, firstRow = TRUE, firstActiveRow = data_start + 1)
  addFilter(wb, sheet_name, row = data_start, cols = 1:ncol(data))

  wb
}

# ==============================================================================
# MAIN FUNCTION - CREATE COMPLETE WORKBOOK
# ==============================================================================

create_audit_workbook <- function(output_path,
                                  calculations_path = "utils/calculations.R",
                                  config_path = "utils/config.R",
                                  vac_payroll_mode = c("latest", "aligned"),
                                  vacancies_mode = NULL,
                                  payroll_mode = NULL,
                                  verbose = TRUE) {

  if (verbose) message("=== Creating Labour Market Stats Workbook ===")
  if (verbose) message("Sourcing calculations...")

  calc_env <- new.env()

  if (file.exists(config_path)) {
    source(config_path, local = calc_env)
  }

  # Pass vacancies/payroll mode into calculations - separate modes take precedence
  vac_payroll_mode <- match.arg(vac_payroll_mode)
  calc_env$vacancies_mode <- if (!is.null(vacancies_mode)) vacancies_mode else vac_payroll_mode
  calc_env$payroll_mode <- if (!is.null(payroll_mode)) payroll_mode else vac_payroll_mode

  if (file.exists(calculations_path)) {
    source(calculations_path, local = calc_env)
  } else {
    stop("calculations.R not found at: ", calculations_path)
  }

  # Get anchor dates
  lfs_period_label <- if (exists("lfs_period_label", envir = calc_env)) {
    get("lfs_period_label", envir = calc_env)
  } else {
    format(Sys.Date(), "%B %Y")
  }

  lfs_anchor <- if (exists("lfs", envir = calc_env)) {
    get("lfs", envir = calc_env)$emp16$end
  } else NULL

  payroll_anchor <- if (exists("payroll", envir = calc_env)) {
    get("payroll", envir = calc_env)$anchor
  } else NULL

  wages_anchor <- if (exists("wages_nom", envir = calc_env)) {
    get("wages_nom", envir = calc_env)$anchor
  } else NULL

  wb <- createWorkbook()

  # 1. DASHBOARD
  if (verbose) message("Building: Dashboard...")
  wb <- build_dashboard_sheet(wb, lfs_period_label, envir = calc_env)

  # 2. LFS DATA (Sheet "2" equivalent)
  if (verbose) message("Building: 2 (LFS Data)...")
  data <- tryCatch(fetch_lfs_wide(), error = function(e) tibble())
  wb <- build_data_sheet(wb, "2", "Labour Force Survey - A01",
                         data, "labour_market__age_group",
                         label_type = "lfs", anchor_date = lfs_anchor,
                         covid_label = "Dec-Feb 2020", election_label = "Apr-Jun 2024")

  # 3. PAYROLL (Sheet "1. Payrolled employees (UK)" equivalent)
  # Uses special function with 3 summary tables: single month + two 3-month averages
  if (verbose) message("Building: 1. Payrolled employees (UK)...")
  data <- tryCatch(fetch_payroll_wide(), error = function(e) tibble())
  wb <- build_payroll_sheet(wb, "1. Payrolled employees (UK)", "Payrolled Employees",
                            data, "labour_market__payrolled_employees",
                            anchor_date = payroll_anchor,
                            covid_label = "February 2020", election_label = "June 2024")

  # 4. INDUSTRY (Sheet "23. Employees Industry" equivalent)
  if (verbose) message("Building: 23. Employees Industry...")
  data <- tryCatch(fetch_industry_wide(), error = function(e) tibble())
  wb <- build_data_sheet(wb, "23. Employees Industry", "Employees by Industry (SIC)",
                         data, "labour_market__employees_industry",
                         label_type = "payroll", anchor_date = payroll_anchor,
                         covid_label = "February 2020", election_label = "June 2024")

  # 5. VACANCIES (Sheet "5" equivalent)
  if (verbose) message("Building: 5 (Vacancies)...")
  data <- tryCatch(fetch_vacancies_wide(), error = function(e) tibble())
  wb <- build_data_sheet(wb, "5", "Job Vacancies",
                         data, "labour_market__vacancies_business",
                         label_type = "lfs", anchor_date = lfs_anchor,
                         covid_label = "Jan-Mar 2020", election_label = "Apr-Jun 2024")

  # 6. REDUNDANCY (Sheet "10" equivalent)
  if (verbose) message("Building: 10 (Redundancies)...")
  data <- tryCatch(fetch_redundancy_wide(), error = function(e) tibble())
  wb <- build_data_sheet(wb, "10", "LFS Redundancy Rate",
                         data, "labour_market__redundancies",
                         label_type = "lfs", anchor_date = lfs_anchor,
                         covid_label = "Dec-Feb 2020", election_label = "Apr-Jun 2024")

  # 7. INACTIVITY (Sheet "11" equivalent)
  if (verbose) message("Building: 11 (Inactivity by Reason)...")
  data <- tryCatch(fetch_inactivity_wide(), error = function(e) tibble())
  wb <- build_data_sheet(wb, "11", "Inactivity by Reason",
                         data, "labour_market__inactivity",
                         label_type = "lfs", anchor_date = lfs_anchor,
                         covid_label = "Dec-Feb 2020", election_label = "Apr-Jun 2024")

  # 8. AWE TOTAL (Sheet "13" equivalent)
  if (verbose) message("Building: 13 (AWE Total)...")
  data <- tryCatch(fetch_wages_total_wide(), error = function(e) tibble())
  wb <- build_data_sheet(wb, "13", "AWE - Total Pay",
                         data, "labour_market__weekly_earnings_total",
                         label_type = "ymd", anchor_date = wages_anchor,
                         covid_label = "2020-02-01", election_label = "2024-06-01")

  # 9. AWE REGULAR (Sheet "15" equivalent)
  if (verbose) message("Building: 15 (AWE Regular)...")
  data <- tryCatch(fetch_wages_regular_wide(), error = function(e) tibble())
  wb <- build_data_sheet(wb, "15", "AWE - Regular Pay",
                         data, "labour_market__weekly_earnings_regular",
                         label_type = "ymd", anchor_date = wages_anchor,
                         covid_label = "2020-02-01", election_label = "2024-06-01")

  # 10. AWE CPI (Sheet "AWE Real_CPI" equivalent)
  if (verbose) message("Building: AWE Real_CPI...")
  data <- tryCatch(fetch_wages_cpi_wide(), error = function(e) tibble())
  wb <- build_data_sheet(wb, "AWE Real_CPI", "Real AWE (CPI Adjusted)",
                         data, "labour_market__weekly_earnings_cpi",
                         label_type = "datetime", anchor_date = wages_anchor,
                         covid_label = "2020-02-01 00:00:00", election_label = "2024-06-01 00:00:00")

  # 11. DAYS LOST (Sheet "18" equivalent)
  if (verbose) message("Building: 18 (Days Lost)...")
  data <- tryCatch(fetch_days_lost_wide(), error = function(e) tibble())
  days_lost_anchor <- if (exists("days_lost", envir = calc_env)) {
    get("days_lost", envir = calc_env)$anchor
  } else payroll_anchor
  wb <- build_data_sheet(wb, "18", "Working Days Lost",
                         data, "labour_market__disputes",
                         label_type = "payroll", anchor_date = days_lost_anchor,
                         covid_label = "February 2020", election_label = "June 2024")

  # 12. HR1 (Sheet "1a" equivalent)
  if (verbose) message("Building: 1a (HR1 Notifications)...")
  data <- tryCatch(fetch_hr1_wide(), error = function(e) tibble())
  hr1_anchor <- if (exists("hr1", envir = calc_env)) {
    get("hr1", envir = calc_env)$anchor
  } else NULL
  wb <- build_data_sheet(wb, "1a", "HR1 Redundancy Notifications",
                         data, "labour_market__redundancies_region",
                         label_type = "datetime", anchor_date = hr1_anchor,
                         covid_label = "2020-02-01 00:00:00", election_label = "2024-06-01 00:00:00")

  # Save
  saveWorkbook(wb, output_path, overwrite = TRUE)
  if (verbose) message("=== Created: ", output_path, " ===")
  invisible(output_path)
}

# ==============================================================================
# USAGE
# ==============================================================================

# Run to create the workbook:
# create_audit_workbook("LM_Stats_Audit.xlsx")
