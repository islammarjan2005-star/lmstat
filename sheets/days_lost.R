
# DAYS LOST MODULE - A01 Sheet 18 (Working Days Lost)



# CONFIG


DAYS_LOST_CODE <- "BBFW"


# FETCH


fetch_days_lost <- function() {
  conn <- DBI::dbConnect(RPostgres::Postgres())
  
  tryCatch({
    query <- 'SELECT time_period, dataset_indentifier_code, value
FROM "ons"."labour_market__disputes"'
    result <- DBI::dbGetQuery(conn, query)
    tibble::as_tibble(result)
  },
  error = function(e) {
    warning("Failed to fetch days lost: ", e$message)
    tibble::tibble(
      time_period = character(),
      dataset_indentifier_code = character(),
      value = numeric()
    )
  },
  finally = {
    DBI::dbDisconnect(conn)
  })
}


# COMPUTE


compute_days_lost <- function(pg_data, manual_mm) {
  cm <- parse_manual_month(manual_mm)
  
  # Days lost uses 2 month lag from manual month
  anchor <- cm %m-% months(2)
  
  # Format as "August 2025"
  lab_cur <- make_payroll_label(anchor)
  
  # use startsWith to handle [p] or [r] suffixes
  match_row <- pg_data %>%
    filter(
      dataset_indentifier_code == DAYS_LOST_CODE,
      startsWith(time_period, lab_cur)
    )
  
  if (nrow(match_row) == 0) {
    cur <- NA_real_
  } else {
    cur <- suppressWarnings(as.numeric(match_row$value[1]))
  }
  
  list(
    cur = cur,
    label = lab_cur,
    anchor = anchor
  )
}


# CALCULATE DAYS LOST


calculate_days_lost <- function(manual_mm) {
  pg_data <- fetch_days_lost()
  compute_days_lost(pg_data, manual_mm)
}