# ==============================================================================
# COMMON HELPER FUNCTIONS
# ==============================================================================

library(dplyr)
library(lubridate)
library(DBI)
library(RPostgres)
library(tibble)

# ==============================================================================
# MANUAL MONTH PARSING
# ==============================================================================

parse_manual_month <- function(mm) {
  mm <- tolower(mm)
  mm <- gsub("[[:space:]]+", "", mm)
  
  letters_part <- gsub("[^a-z]", "", mm)
  digits_part <- gsub("[^0-9]", "", mm)
  
  if (nchar(letters_part) < 3 || nchar(digits_part) < 4) {
    stop("manual_month must look like 'oct2025', got: ", mm, call. = FALSE)
  }
  
  mon3 <- substr(letters_part, 1, 3)
  m <- match(mon3, tolower(month.abb))
  yr <- suppressWarnings(as.integer(substr(digits_part, 1, 4)))
  
  if (is.na(m) || is.na(yr)) {
    stop("manual_month must look like 'oct2025', got: ", mm, call. = FALSE)
  }
  
  as.Date(sprintf("%04d-%02d-01", yr, m))
}

# ==============================================================================
# TIME PERIOD FORMAT HELPERS
# ==============================================================================

#' Convert Date to LFS 3-month format: "Jul-Sep 2025"
make_lfs_label <- function(end_date) {
  start_date <- end_date %m-% months(2)
  sprintf("%s-%s %s",
          format(start_date, "%b"),
          format(end_date, "%b"),
          format(end_date, "%Y"))
}

#' Convert Date to payroll/sector format: "July 2025"
make_payroll_label <- function(date) {
  format(date, "%B %Y")
}

#' Convert Date to Sheet 13/15 format: "2000-01-01"
make_ymd_label <- function(date) {
  format(date, "%Y-%m-%d")
}

#' Convert Date to CPI/HR1 format: "2000-01-01 00:00:00"
make_datetime_label <- function(date) {
  paste0(format(date, "%Y-%m-%d"), " 00:00:00")
}

#' For narrative text: "June to August 2025"
lfs_label_narrative <- function(end_date) {
  start_date <- end_date %m-% months(2)
  sprintf("%s to %s",
          format(start_date, "%B %Y"),
          format(end_date, "%B %Y"))
}

# ==============================================================================
# GENERIC LOOKUP FUNCTION
# ==============================================================================

#' Get value by dataset_indentifier_code and time_period
val_by_code <- function(pg_data, code, period_label) {
  if (is.null(pg_data) || nrow(pg_data) == 0) return(NA_real_)
  
  match_row <- pg_data %>%
    filter(
      dataset_indentifier_code == code,
      trimws(time_period) == trimws(period_label)
    )
  
  if (nrow(match_row) == 0) return(NA_real_)
  
  suppressWarnings(as.numeric(match_row$value[1]))
}

# ==============================================================================
# FORMATTING FUNCTIONS (SIGNED) - with smart decimal handling
# ==============================================================================

format_int <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- round(as.numeric(x), 0)
  if (is.na(v)) return(NA_character_)
  if (v == 0) return("0")
  paste0(ifelse(v > 0, "+", "-"), format(abs(v), big.mark = ","))
}

# Smart pp format: use 2 decimals if 1 decimal rounds to 0
format_pp <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- as.numeric(x)
  if (is.na(v)) return(NA_character_)
  if (v == 0) return("0pp")
  
  # Check if 1 decimal rounds to 0
  v1 <- round(v, 1)
  if (v1 == 0 && v != 0) {
    # Use 2 decimals
    v2 <- round(v, 2)
    paste0(ifelse(v2 > 0, "+", "-"), format(abs(v2), nsmall = 2), "pp")
  } else {
    paste0(ifelse(v1 > 0, "+", "-"), format(abs(v1), nsmall = 1), "pp")
  }
}

# Smart pct format: use 2 decimals if 1 decimal rounds to 0
format_pct1 <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- as.numeric(x)
  if (is.na(v)) return(NA_character_)
  if (v == 0) return("0%")
  
  v1 <- round(v, 1)
  if (v1 == 0 && v != 0) {
    v2 <- round(v, 2)
    paste0(ifelse(v2 > 0, "+", "-"), format(abs(v2), nsmall = 2), "%")
  } else {
    paste0(ifelse(v1 > 0, "+", "-"), format(abs(v1), nsmall = 1), "%")
  }
}

format_gbp_signed0 <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- round(as.numeric(x), 0)
  if (is.na(v)) return(NA_character_)
  if (v == 0) return("£0")
  paste0(ifelse(v > 0, "+£", "-£"), format(abs(v), big.mark = ","))
}

# ==============================================================================
# FORMATTING FUNCTIONS (UNSIGNED) - with smart decimal handling
# ==============================================================================

format_int_unsigned <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- round(abs(as.numeric(x)), 0)
  if (is.na(v)) return(NA_character_)
  format(v, big.mark = ",")
}

# Smart pct unsigned: use 2 decimals if 1 decimal rounds to 0
format_pct1_unsigned <- function(x) {
  if (is.na(x)) return(NA_character_)
  v <- abs(as.numeric(x))
  if (is.na(v)) return(NA_character_)
  
  v1 <- round(v, 1)
  if (v1 == 0 && v != 0) {
    v2 <- round(v, 2)
    paste0(format(v2, nsmall = 2), "%")
  } else {
    paste0(format(v1, nsmall = 1), "%")
  }
}

# ==============================================================================
# NARRATIVE FORMATTING FUNCTIONS - with smart decimal handling
# ==============================================================================

fmt_pct <- function(x) {
  if (is.na(x)) return('—')
  v1 <- round(x, 1)
  if (v1 == 0 && x != 0) {
    v2 <- round(x, 2)
    paste0(format(v2, nsmall = 2), '%')
  } else {
    paste0(format(v1, nsmall = 1), '%')
  }
}

fmt_pp <- function(x) {
  if (is.na(x)) return('—')
  v <- abs(x)
  v1 <- round(v, 1)
  if (v1 == 0 && v != 0) {
    v2 <- round(v, 2)
    paste0(format(v2, nsmall = 2), ' percentage points')
  } else {
    paste0(format(v1, nsmall = 1), ' percentage points')
  }
}

fmt_dir <- function(x, up_word = 'up', down_word = 'down') {
  ifelse(is.na(x) || x == 0, '', ifelse(x > 0, up_word, down_word))
}

fmt_mill <- function(x) {
  ifelse(is.na(x), '—', paste0(format(round(x, 1), nsmall = 1)))
}

fmt_int <- function(x) {
  ifelse(is.na(x), '—', format(round(x, 0), big.mark = ','))
}

fmt_one_dec <- function(x) {
  if (is.na(x)) return("—")
  v1 <- round(x, 1)
  if (v1 == 0 && x != 0) {
    format(round(x, 2), nsmall = 2)
  } else {
    format(v1, nsmall = 1)
  }
}
