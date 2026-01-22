# ==============================================================================
# EXCEL TEMPLATE FILLER
# ==============================================================================
# Fills an Excel template with placeholders using calculated values and source data.
# Similar approach to word_output.R but for Excel.
#
# Placeholder format: {{PLACEHOLDER_NAME}}
# Example: {{EMP16_CUR}}, {{EMP16_DQ}}, {{UNEMP_RT_DY}}
#
# For source data: columns should have dataset identifier codes as headers
# (e.g., MGRZ, LF24, MGSC) and data will be matched and populated below.
# ==============================================================================

suppressPackageStartupMessages({

  library(openxlsx)
  library(dplyr)
  library(tibble)
  library(DBI)
  library(RPostgres)
})

# ==============================================================================
# PLACEHOLDER DEFINITIONS
# ==============================================================================
# Maps placeholder names to the variable names from calculations.R

PLACEHOLDER_MAP <- list(
  # Employment 16+
  EMP16_CUR = "emp16_cur",
  EMP16_DQ = "emp16_dq",
  EMP16_DY = "emp16_dy",
  EMP16_DC = "emp16_dc",
  EMP16_DE = "emp16_de",


  # Employment rate
  EMP_RT_CUR = "emp_rt_cur",
  EMP_RT_DQ = "emp_rt_dq",
  EMP_RT_DY = "emp_rt_dy",
  EMP_RT_DC = "emp_rt_dc",
  EMP_RT_DE = "emp_rt_de",

  # Unemployment 16+
  UNEMP16_CUR = "unemp16_cur",
  UNEMP16_DQ = "unemp16_dq",
  UNEMP16_DY = "unemp16_dy",
  UNEMP16_DC = "unemp16_dc",
  UNEMP16_DE = "unemp16_de",

  # Unemployment rate
  UNEMP_RT_CUR = "unemp_rt_cur",
  UNEMP_RT_DQ = "unemp_rt_dq",
  UNEMP_RT_DY = "unemp_rt_dy",
  UNEMP_RT_DC = "unemp_rt_dc",
  UNEMP_RT_DE = "unemp_rt_de",

  # Inactivity 16-64
  INACT_CUR = "inact_cur",
  INACT_DQ = "inact_dq",
  INACT_DY = "inact_dy",
  INACT_DC = "inact_dc",
  INACT_DE = "inact_de",

  # Inactivity 50-64
  INACT5064_CUR = "inact5064_cur",
  INACT5064_DQ = "inact5064_dq",
  INACT5064_DY = "inact5064_dy",
  INACT5064_DC = "inact5064_dc",
  INACT5064_DE = "inact5064_de",

  # Inactivity rate 16-64
  INACT_RT_CUR = "inact_rt_cur",
  INACT_RT_DQ = "inact_rt_dq",
  INACT_RT_DY = "inact_rt_dy",
  INACT_RT_DC = "inact_rt_dc",
  INACT_RT_DE = "inact_rt_de",

  # Inactivity rate 50-64
  INACT5064_RT_CUR = "inact5064_rt_cur",
  INACT5064_RT_DQ = "inact5064_rt_dq",
  INACT5064_RT_DY = "inact5064_rt_dy",
  INACT5064_RT_DC = "inact5064_rt_dc",
  INACT5064_RT_DE = "inact5064_rt_de",

  # Vacancies
  VAC_CUR = "vac_cur",
  VAC_DQ = "vac_dq",
  VAC_DY = "vac_dy",
  VAC_DC = "vac_dc",
  VAC_DE = "vac_de",

  # Payroll
  PAYROLL_CUR = "payroll_cur",
  PAYROLL_DQ = "payroll_dq",
  PAYROLL_DY = "payroll_dy",
  PAYROLL_DC = "payroll_dc",
  PAYROLL_DE = "payroll_de",

  # Wages nominal
  WAGES_CUR = "latest_wages",
  WAGES_DQ = "wages_change_q",
  WAGES_DY = "wages_change_y",
  WAGES_DC = "wages_change_covid",
  WAGES_DE = "wages_change_election",

  # Wages CPI adjusted
  WAGES_CPI_CUR = "latest_wages_cpi",
  WAGES_CPI_DQ = "wages_cpi_change_q",
  WAGES_CPI_DY = "wages_cpi_change_y",
  WAGES_CPI_DC = "wages_cpi_change_covid",
  WAGES_CPI_DE = "wages_cpi_change_election",

  # Period labels
  LFS_PERIOD = "lfs_period_label",
  PAYROLL_PERIOD = "payroll_period_label",
  RENDER_DATE = "render_date"
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
# COLUMN NAME TO DATASET CODE MAPPINGS
# ==============================================================================
# Maps human-readable column names (in template) to dataset identifier codes
# COMPLETE LIST - covers ALL codes from the database
#
# Structure: "Column Name in Excel" = "DATASET_CODE"
# Multiple column name variations map to the same code for flexibility

# ------------------------------------------------------------------------------
# LFS DATA (labour_market__age_group)
# Codes: MGRZ, LF24, MGSC, MGSX, LF2M, LF2S, LF2A, LF2W
# ------------------------------------------------------------------------------
LFS_COLUMN_MAP <- c(
  # MGRZ - Employment 16+ level (thousands)
  "Employment 16+ (000s)" = "MGRZ",
  "Employment 16+" = "MGRZ",
  "Employment (000s) 16+" = "MGRZ",
  "Employment level" = "MGRZ",
  "level" = "MGRZ",  # Generic from Sheet 2

  # LF24 - Employment rate 16-64 (%)
  "Employment Rate 16-64 (%)" = "LF24",
  "Employment rate (16-64)" = "LF24",
  "Employment rate" = "LF24",
  "Employment rate (%)" = "LF24",
  "rate (%)" = "LF24",  # Generic from Sheet 2

  # MGSC - Unemployment 16+ level (thousands)
  "Unemployment 16+ (000s)" = "MGSC",
  "Unemployment 16+" = "MGSC",
  "Unemployment (000s) 16+" = "MGSC",
  "Unemployment, 16+ (000s)" = "MGSC",
  "Unemployment level" = "MGSC",

  # MGSX - Unemployment rate 16+ (%)
  "Unemployment Rate 16+ (%)" = "MGSX",
  "Unemployment rate, 16+" = "MGSX",
  "Unemployment rate" = "MGSX",
  "Unemployment rate (%)" = "MGSX",

  # LF2M - Inactivity 16-64 level (thousands)
  "Inactivity 16-64 (000s)" = "LF2M",
  "Inactivity 16-64" = "LF2M",
  "Inactivity (000s) 16-64" = "LF2M",
  "Economic inactivity (000s) (16-64)" = "LF2M",
  "Inactivity level" = "LF2M",
  "Total economically inactive aged 16-64 (thousands)4" = "LF2M",

  # LF2S - Inactivity rate 16-64 (%)
  "Inactivity Rate 16-64 (%)" = "LF2S",
  "Inactivity rate 16-64" = "LF2S",
  "Economic inactivity rate (16-64)" = "LF2S",
  "Inactivity rate (%)" = "LF2S",

  # LF2A - Inactivity 50-64 level (thousands)
  "Inactivity 50-64 (000s)" = "LF2A",
  "Inactivity 50-64" = "LF2A",
  "50-64s inactivity (000s)" = "LF2A",

  # LF2W - Inactivity rate 50-64 (%)
  "Inactivity Rate 50-64 (%)" = "LF2W",
  "Inactivity rate 50-64" = "LF2W",
  "50-64s inactivity rate" = "LF2W"
)

# ------------------------------------------------------------------------------
# VACANCIES (labour_market__vacancies_business)
# Codes: AP2Y, AP3K
# ------------------------------------------------------------------------------
VACANCIES_COLUMN_MAP <- c(
  # AP2Y - Vacancies level (thousands)
  "Vacancies (000s)" = "AP2Y",
  "Vacancies" = "AP2Y",
  "Total vacancies" = "AP2Y",

  # AP3K - Quarterly change (thousands)
  "Quarterly Change (000s)" = "AP3K",
  "Quarterly change" = "AP3K"
)

# ------------------------------------------------------------------------------
# WAGES TOTAL (labour_market__weekly_earnings_total)
# Codes: KAB9, KAC3, KAC6, KAC9
# ------------------------------------------------------------------------------
WAGES_TOTAL_COLUMN_MAP <- c(
  # KAB9 - AWE Total weekly (£)
  "AWE Total (£/week)" = "KAB9",
  "AWE Total" = "KAB9",
  "Weekly Earnings     (£)" = "KAB9",
  "Weekly Earnings (£)" = "KAB9",
  "Total pay" = "KAB9",

  # KAC3 - AWE Total YoY growth (%)
  "AWE Total YoY (%)" = "KAC3",
  "AWE Total YoY" = "KAC3",
  "% changes year on year" = "KAC3",
  "Total pay growth" = "KAC3",

  # KAC6 - AWE Total Private YoY (%)
  "AWE Total Private YoY (%)" = "KAC6",
  "AWE Total Private YoY" = "KAC6",

  # KAC9 - AWE Total Public YoY (%)
  "AWE Total Public YoY (%)" = "KAC9",
  "AWE Total Public YoY" = "KAC9"
)

# ------------------------------------------------------------------------------
# WAGES REGULAR (labour_market__weekly_earnings_regular)
# Codes: KAI7, KAI9, KAJ4, KAJ7
# ------------------------------------------------------------------------------
WAGES_REGULAR_COLUMN_MAP <- c(
  # KAI7 - AWE Regular weekly (£)
  "AWE Regular (£/week)" = "KAI7",
  "AWE Regular" = "KAI7",
  "Regular pay" = "KAI7",

  # KAI9 - AWE Regular YoY growth (%)
  "AWE Regular YoY (%)" = "KAI9",
  "AWE Regular YoY" = "KAI9",
  "Regular pay growth" = "KAI9",

  # KAJ4 - AWE Regular Private YoY (%)
  "AWE Regular Private YoY (%)" = "KAJ4",
  "AWE Regular Private YoY" = "KAJ4",

  # KAJ7 - AWE Regular Public YoY (%)
  "AWE Regular Public YoY (%)" = "KAJ7",
  "AWE Regular Public YoY" = "KAJ7"
)

# ------------------------------------------------------------------------------
# INACTIVITY BY REASON (labour_market__inactivity)
# Codes: LF63, LF65, LF67, LF69, LFL8, LF6B, LF6D
# ------------------------------------------------------------------------------
INACTIVITY_REASON_COLUMN_MAP <- c(
  # LF63 - Student
  "Student" = "LF63",

  # LF65 - Looking after family/home
  "Looking after family / home" = "LF65",
  "Looking after family/home" = "LF65",
  "Family/Home" = "LF65",

  # LF67 - Temporary sick
  "Temp sick" = "LF67",
  "Temporary sick" = "LF67",
  "Temp Sick" = "LF67",

  # LF69 - Long-term sick
  "Long-term sick" = "LF69",
  "Long-term Sick" = "LF69",

  # LFL8 - Discouraged workers
  "Discouraged workers1" = "LFL8",
  "Discouraged workers" = "LFL8",
  "Discouraged" = "LFL8",

  # LF6B - Retired
  "Retired" = "LF6B",

  # LF6D - Other
  "Other2" = "LF6D",
  "Other" = "LF6D"
)

# ------------------------------------------------------------------------------
# REDUNDANCY (labour_market__redundancies)
# Codes: BEIR
# ------------------------------------------------------------------------------
REDUNDANCY_COLUMN_MAP <- c(
  # BEIR - Redundancy rate per 1000
  "Redundancy Rate (per 1000)" = "BEIR",
  "Redundancy rate" = "BEIR",
  "Rate per thousand2" = "BEIR",
  "Rate per thousand" = "BEIR"
)

# ------------------------------------------------------------------------------
# DAYS LOST (labour_market__disputes)
# Codes: BBFW
# ------------------------------------------------------------------------------
DAYS_LOST_COLUMN_MAP <- c(
  # BBFW - Working days lost (thousands)
  "Days Lost (000s)" = "BBFW",
  "Days lost" = "BBFW",
  "Working days lost" = "BBFW"
)

# ------------------------------------------------------------------------------
# INDUSTRY (labour_market__employees_industry)
# Codes: A-S (SIC sections)
# ------------------------------------------------------------------------------
INDUSTRY_COLUMN_MAP <- c(
  # A - Agriculture, forestry and fishing
  "Agriculture" = "A",
  "A-Agriculture" = "A",

  # B - Mining and quarrying
  "Mining" = "B",
  "B-Mining" = "B",

  # C - Manufacturing
  "Manufacturing" = "C",
  "C-Manufacturing" = "C",

  # D - Electricity, gas, steam
  "Electricity" = "D",
  "D-Electricity" = "D",

  # E - Water supply, sewerage
  "Water" = "E",
  "E-Water" = "E",

  # F - Construction
  "Construction" = "F",
  "F-Construction" = "F",

  # G - Wholesale and retail trade
  "Retail" = "G",
  "Wholesale and retail" = "G",
  "G-Retail" = "G",

  # H - Transportation and storage
  "Transport" = "H",
  "Transportation" = "H",
  "H-Transport" = "H",

  # I - Accommodation and food service
  "Hospitality" = "I",
  "Accommodation and food" = "I",
  "I-Hospitality" = "I",

  # J - Information and communication
  "IT" = "J",
  "Information and communication" = "J",
  "IT & Comms" = "J",
  "J-IT & Comms" = "J",

  # K - Financial and insurance
  "Finance" = "K",
  "Financial services" = "K",
  "K-Finance" = "K",

  # L - Real estate
  "Real Estate" = "L",
  "Real estate" = "L",
  "L-Real Estate" = "L",

  # M - Professional, scientific, technical
  "Professional" = "M",
  "Professional services" = "M",
  "M-Professional" = "M",

  # N - Administrative and support
  "Admin" = "N",
  "Administrative" = "N",
  "N-Admin" = "N",

  # O - Public administration and defence
  "Public Admin" = "O",
  "Public administration" = "O",
  "O-Public Admin" = "O",

  # P - Education
  "Education" = "P",
  "P-Education" = "P",

  # Q - Human health and social work
  "Health" = "Q",
  "Health and social" = "Q",
  "Q-Health" = "Q",

  # R - Arts, entertainment, recreation
  "Arts" = "R",
  "Arts and recreation" = "R",
  "R-Arts" = "R",

  # S - Other service activities
  "Other Services" = "S",
  "Other services" = "S",
  "S-Other Services" = "S"
)

# ==============================================================================
# SOURCE DATA FETCHERS
# ==============================================================================
# These return data with dataset_indentifier_code as columns

fetch_lfs_source <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__age_group"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, dataset_indentifier_code) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = dataset_indentifier_code, values_from = value) %>%
    rename(Date = time_period) %>%
    arrange(Date)
}

fetch_vacancies_source <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__vacancies_business"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, dataset_indentifier_code) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = dataset_indentifier_code, values_from = value) %>%
    rename(Date = time_period) %>%
    arrange(Date)
}

fetch_payroll_source <- function() {
  query <- 'SELECT time_period, unit_type, value
            FROM "ons"."labour_market__payrolled_employees"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, unit_type) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = unit_type, values_from = value) %>%
    rename(Date = time_period) %>%
    arrange(Date)
}

fetch_wages_total_source <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__weekly_earnings_total"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, dataset_indentifier_code) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = dataset_indentifier_code, values_from = value) %>%
    rename(Date = time_period) %>%
    arrange(Date)
}

fetch_wages_cpi_source <- function() {
  query <- 'SELECT time_period, earnings_type, earnings_metric, value
            FROM "ons"."labour_market__weekly_earnings_cpi"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  raw %>%
    mutate(
      value = as.numeric(value),
      col_name = paste0(earnings_type, "_", earnings_metric)
    ) %>%
    group_by(time_period, col_name) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = col_name, values_from = value) %>%
    rename(Date = time_period) %>%
    arrange(Date)
}

fetch_wages_regular_source <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__weekly_earnings_regular"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, dataset_indentifier_code) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = dataset_indentifier_code, values_from = value) %>%
    rename(Date = time_period) %>%
    arrange(Date)
}

fetch_inactivity_source <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__inactivity"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, dataset_indentifier_code) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = dataset_indentifier_code, values_from = value) %>%
    rename(Date = time_period) %>%
    arrange(Date)
}

fetch_redundancy_source <- function() {
  query <- 'SELECT time_period, dataset_indentifier_code, value
            FROM "ons"."labour_market__redundancies"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, dataset_indentifier_code) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = dataset_indentifier_code, values_from = value) %>%
    rename(Date = time_period) %>%
    arrange(Date)
}

fetch_industry_source <- function() {
  query <- 'SELECT time_period, sic_section, value
            FROM "ons"."labour_market__employees_industry"'
  raw <- fetch_db(query)
  if (nrow(raw) == 0) return(tibble())

  raw %>%
    mutate(value = as.numeric(value)) %>%
    group_by(time_period, sic_section) %>%
    summarise(value = first(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = sic_section, values_from = value) %>%
    rename(Date = time_period) %>%
    arrange(Date)
}

# ==============================================================================
# PLACEHOLDER REPLACEMENT FUNCTIONS
# ==============================================================================

#' Find all cells containing placeholders in a worksheet
#' @param wb Workbook object
#' @param sheet Sheet name
#' @return Data frame with row, col, placeholder_name for each found placeholder
find_placeholders <- function(wb, sheet) {
  # Read the sheet as text to find placeholders
  data <- tryCatch({
    read.xlsx(wb, sheet = sheet, colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  }, error = function(e) {
    return(data.frame())
  })

  if (nrow(data) == 0 || ncol(data) == 0) return(data.frame())

  placeholders <- data.frame(
    row = integer(),
    col = integer(),
    placeholder = character(),
    stringsAsFactors = FALSE
  )

  for (r in seq_len(nrow(data))) {
    for (c in seq_len(ncol(data))) {
      cell_val <- as.character(data[r, c])
      if (!is.na(cell_val) && grepl("\\{\\{[A-Z0-9_]+\\}\\}", cell_val)) {
        # Extract placeholder name(s)
        matches <- regmatches(cell_val, gregexpr("\\{\\{([A-Z0-9_]+)\\}\\}", cell_val))[[1]]
        for (m in matches) {
          name <- gsub("\\{\\{|\\}\\}", "", m)
          placeholders <- rbind(placeholders, data.frame(
            row = r,
            col = c,
            placeholder = name,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  placeholders
}

#' Replace a placeholder in a cell with a value
#' @param wb Workbook object
#' @param sheet Sheet name
#' @param row Row number
#' @param col Column number
#' @param placeholder Placeholder name (without braces)
#' @param value Value to insert
replace_placeholder <- function(wb, sheet, row, col, placeholder, value) {
  # Write the value to the cell
  writeData(wb, sheet, value, startRow = row, startCol = col, colNames = FALSE)
  invisible(wb)
}

#' Fill source data into a sheet based on column headers
#' Uses column_map to translate human-readable names to dataset codes
#'
#' @param wb Workbook object
#' @param sheet Sheet name
#' @param source_data Data frame with Date column and dataset code columns
#' @param column_map Named vector mapping column names to dataset codes
#' @param header_row Row number containing column headers (human-readable names)
#' @param data_start_row Row number where data should start
#' @param date_col Column number for dates (default 1)
fill_source_data <- function(wb, sheet, source_data, column_map,
                              header_row, data_start_row, date_col = 1) {
  if (is.null(source_data) || nrow(source_data) == 0) {
    message("  No source data to fill for sheet: ", sheet)
    return(wb)
  }

  # Read the header row to find column mappings
  headers <- tryCatch({
    read.xlsx(wb, sheet = sheet, rows = header_row, colNames = FALSE)
  }, error = function(e) {
    return(data.frame())
  })

  if (ncol(headers) == 0) {
    message("  Could not read headers for sheet: ", sheet)
    return(wb)
  }

  headers <- as.character(unlist(headers[1, ]))

  # Get available dataset codes from source data
  source_codes <- names(source_data)
  filled_cols <- 0

  for (i in seq_along(headers)) {
    header_val <- trimws(headers[i])
    if (is.na(header_val) || header_val == "") next

    # Check if header is "Date" or similar
    if (tolower(header_val) %in% c("date", "period", "time_period", "time period")) {
      if ("Date" %in% source_codes) {
        date_data <- source_data[["Date"]]
        for (r in seq_along(date_data)) {
          writeData(wb, sheet, date_data[r], startRow = data_start_row + r - 1, startCol = i, colNames = FALSE)
        }
        message("    Filled column ", i, " (", header_val, ") with dates: ", length(date_data), " rows")
        filled_cols <- filled_cols + 1
      }
      next
    }

    # Column headers are ALWAYS human-readable names
    # We use the column_map to find the corresponding dataset code
    dataset_code <- NA

    # Use column_map to translate column name -> dataset code
    if (!is.null(column_map) && length(column_map) > 0) {

      # 1. Exact match
      if (header_val %in% names(column_map)) {
        dataset_code <- column_map[header_val]
      }

      # 2. Case-insensitive match
      if (is.na(dataset_code)) {
        matching_names <- names(column_map)[tolower(names(column_map)) == tolower(header_val)]
        if (length(matching_names) > 0) {
          dataset_code <- column_map[matching_names[1]]
        }
      }

      # 3. Partial match (column name contains a mapped name or vice versa)
      if (is.na(dataset_code)) {
        for (map_name in names(column_map)) {
          # Check if header contains the map name
          if (grepl(map_name, header_val, fixed = TRUE, ignore.case = TRUE)) {
            dataset_code <- column_map[map_name]
            break
          }
          # Check if map name contains the header (for short headers)
          if (nchar(header_val) >= 3 && grepl(header_val, map_name, fixed = TRUE, ignore.case = TRUE)) {
            dataset_code <- column_map[map_name]
            break
          }
        }
      }
    }

    # Skip if no match found in column_map
    if (is.na(dataset_code)) {
      next
    }

    # Check if this dataset code exists in source data
    if (dataset_code %in% source_codes) {
      col_data <- source_data[[dataset_code]]

      # Write the column data starting at data_start_row
      for (r in seq_along(col_data)) {
        writeData(wb, sheet, col_data[r], startRow = data_start_row + r - 1, startCol = i, colNames = FALSE)
      }

      message("    Filled column ", i, " (", header_val, " -> ", dataset_code, ") with ", length(col_data), " rows")
      filled_cols <- filled_cols + 1
    } else {
      message("    Warning: Dataset code '", dataset_code, "' for '", header_val, "' not found in source data")
    }
  }

  message("  Filled ", filled_cols, " columns total")
  invisible(wb)
}

# ==============================================================================
# MAIN TEMPLATE FILLER FUNCTION
# ==============================================================================

#' Fill an Excel template with calculated values and source data
#'
#' @param template_path Path to Excel template file
#' @param output_path Path for output file
#' @param calculations_path Path to calculations.R
#' @param config_path Path to config.R
#' @param sheet_config List defining how to fill each sheet (see below)
#' @param verbose Print progress messages
#'
#' @details
#' sheet_config should be a list where each element is named by sheet name and contains:
#'   - source_fetcher: Function name to fetch source data (e.g., "fetch_lfs_source")
#'   - header_row: Row number with column headers (dataset codes)
#'   - data_start_row: Row number where data should start
#'
#' @examples
#' sheet_config <- list(
#'   "LFS Data" = list(
#'     source_fetcher = "fetch_lfs_source",
#'     header_row = 10,
#'     data_start_row = 11
#'   )
#' )
fill_excel_template <- function(template_path,
                                 output_path,
                                 calculations_path = "utils/calculations.R",
                                 config_path = "utils/config.R",
                                 sheet_config = NULL,
                                 verbose = TRUE) {

  if (!file.exists(template_path)) {
    stop("Template file not found: ", template_path)
  }

  # Load the template
  if (verbose) message("Loading template: ", template_path)
  wb <- loadWorkbook(template_path)

  # Source config and calculations
  if (verbose) message("Sourcing calculations...")
  calc_env <- new.env()

  if (file.exists(config_path)) {
    source(config_path, local = calc_env)
  }

  if (file.exists(calculations_path)) {
    source(calculations_path, local = calc_env)
  } else {
    stop("calculations.R not found at: ", calculations_path)
  }

  # Add render date
  calc_env$render_date <- format(Sys.Date(), "%d %B %Y")

  # Get all sheet names
  sheets <- names(wb)
  if (verbose) message("Found ", length(sheets), " sheets")

  # Process each sheet for placeholders
  for (sheet in sheets) {
    if (verbose) message("Processing sheet: ", sheet)

    # Find and replace placeholders
    placeholders <- find_placeholders(wb, sheet)

    if (nrow(placeholders) > 0) {
      if (verbose) message("  Found ", nrow(placeholders), " placeholders")

      for (i in seq_len(nrow(placeholders))) {
        ph <- placeholders[i, ]
        ph_name <- ph$placeholder

        # Look up the variable name
        var_name <- PLACEHOLDER_MAP[[ph_name]]

        if (is.null(var_name)) {
          if (verbose) message("    Warning: Unknown placeholder {{", ph_name, "}}")
          next
        }

        # Get the value from calc_env
        if (exists(var_name, envir = calc_env)) {
          value <- get(var_name, envir = calc_env)
          replace_placeholder(wb, sheet, ph$row, ph$col, ph_name, value)
          if (verbose) message("    Replaced {{", ph_name, "}} at R", ph$row, "C", ph$col, " with ", value)
        } else {
          if (verbose) message("    Warning: Variable '", var_name, "' not found for {{", ph_name, "}}")
        }
      }
    }

    # Fill source data if configured
    if (!is.null(sheet_config) && sheet %in% names(sheet_config)) {
      config <- sheet_config[[sheet]]

      if (!is.null(config$source_fetcher)) {
        if (verbose) message("  Fetching source data using: ", config$source_fetcher)

        fetcher_fn <- get(config$source_fetcher)
        source_data <- tryCatch(fetcher_fn(), error = function(e) {
          message("    Error fetching data: ", e$message)
          tibble()
        })

        if (nrow(source_data) > 0) {
          # Get the column map (either by name or direct)
          col_map <- NULL
          if (!is.null(config$column_map)) {
            if (is.character(config$column_map)) {
              # It's a name of a map variable
              col_map <- get(config$column_map)
            } else {
              # It's the map itself
              col_map <- config$column_map
            }
          }

          fill_source_data(
            wb = wb,
            sheet = sheet,
            source_data = source_data,
            column_map = col_map,
            header_row = config$header_row,
            data_start_row = config$data_start_row
          )
        }
      }
    }
  }

  # Save the filled workbook
  if (verbose) message("Saving to: ", output_path)
  saveWorkbook(wb, output_path, overwrite = TRUE)

  if (verbose) message("Done!")
  invisible(output_path)
}

# ==============================================================================
# EXAMPLE USAGE
# ==============================================================================

# Define sheet configuration for source data
# Each entry specifies:
#   - source_fetcher: function name to fetch data from DB
#   - column_map: mapping of column names to dataset codes
#   - header_row: row number with column headers
#   - data_start_row: row number where data begins

DEFAULT_SHEET_CONFIG <- list(
  # ====================
  # YOUR EXCEL SHEETS
  # ====================

  # Sheet "2" - LFS main indicators (Employment, Unemployment, Inactivity)
  # Header row 15 has: Date, Employment level, Employment rate, Unemployment level, etc.
  "2" = list(
    source_fetcher = "fetch_lfs_source",
    column_map = "LFS_COLUMN_MAP",
    header_row = 15,
    data_start_row = 16
  ),

  # Sheet "11" - Inactivity by reason
  # Header row 2 has: Date, Total inactivity, Student, Family/home, Temp sick, Long-term sick, etc.
  "11" = list(
    source_fetcher = "fetch_inactivity_source",
    column_map = "INACTIVITY_REASON_COLUMN_MAP",
    header_row = 2,
    data_start_row = 10
  ),

  # Sheet "10" - Redundancies
  # Header row 2 has: Date, People Level, Rate per thousand, Men Level, etc.
  "10" = list(
    source_fetcher = "fetch_redundancy_source",
    column_map = "REDUNDANCY_COLUMN_MAP",
    header_row = 2,
    data_start_row = 10
  ),

  # Sheet "13" - AWE Total wages (nominal)
  # Header row 3 has column types, source data starts around row 18
  "13" = list(
    source_fetcher = "fetch_wages_total_source",
    column_map = "WAGES_TOTAL_COLUMN_MAP",
    header_row = 3,
    data_start_row = 18
  ),

  # Sheet "15" - AWE Regular wages (nominal)
  "15" = list(
    source_fetcher = "fetch_wages_regular_source",
    column_map = "WAGES_REGULAR_COLUMN_MAP",
    header_row = 3,
    data_start_row = 18
  ),

  # Sheet "AWE Real_CPI" - CPI adjusted wages
  # Header row 1 has: Date, Total Pay Real AWE, Total Pay %, Regular Pay Real AWE, etc.
  "AWE Real_CPI" = list(
    source_fetcher = "fetch_wages_cpi_source",
    column_map = NULL,  # Uses combined column names from the query
    header_row = 1,
    data_start_row = 17
  ),

  # Sheet "1. Payrolled employees (UK)" - HMRC Payroll
  # Header row 10 has: Date, Payrolled employees, Change on previous month
  "1. Payrolled employees (UK)" = list(
    source_fetcher = "fetch_payroll_source",
    column_map = NULL,  # Uses unit_type from query
    header_row = 10,
    data_start_row = 11
  ),

  # Sheet "23. Employees Industry" - Industry breakdown
  "23. Employees Industry" = list(
    source_fetcher = "fetch_industry_source",
    column_map = "INDUSTRY_COLUMN_MAP",
    header_row = 13,
    data_start_row = 14
  )
)

# ==============================================================================
# CONVENIENCE FUNCTION FOR YOUR WORKBOOK
# ==============================================================================

#' Fill the Labour Market Stats workbook
#'
#' @param template_path Path to your Excel template (with placeholders)
#' @param output_path Path for the filled output
#' @param verbose Show progress messages
fill_lm_stats_workbook <- function(template_path = "LM_Stats_Template.xlsx",
                                    output_path = "LM_Stats_Filled.xlsx",
                                    verbose = TRUE) {

  fill_excel_template(
    template_path = template_path,
    output_path = output_path,
    calculations_path = "utils/calculations.R",
    config_path = "utils/config.R",
    sheet_config = DEFAULT_SHEET_CONFIG,
    verbose = verbose
  )
}

# Run with defaults
# fill_lm_stats_workbook(
#   template_path = "December 25 LM Statss.xlsx",
#   output_path = "December 25 LM Stats FILLED.xlsx"
# )
