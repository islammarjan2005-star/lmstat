# ==============================================================================
# LABOUR MARKET STATISTICS BRIEF - SHINY APPLICATION
# ==============================================================================
# GOV.UK Design System styled application for generating Labour Market briefings
# Uses Shiny's built-in withProgress for reliable progress indicators
# ==============================================================================

library(shiny)
library(lubridate)
library(DBI)
library(RPostgres)

# ==============================================================================
# UI - GOV.UK DESIGN SYSTEM
# ==============================================================================

ui <- fluidPage(

  # Page title
  tags$head(
    tags$title("Labour Market Statistics Brief"),
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),

    # GOV.UK Design System CSS
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@400;600;700&display=swap');

      *, *::before, *::after { box-sizing: border-box; }
      html, body { margin: 0; padding: 0; min-height: 100vh; }

      body {
        font-family: 'Source Sans Pro', Arial, sans-serif;
        font-size: 19px;
        line-height: 1.31579;
        color: #0b0c0c;
        background-color: #f3f2f1;
      }

      .govuk-header {
        border-bottom: 10px solid #1d70b8;
        color: #ffffff;
        background: #0b0c0c;
      }

      .govuk-header__container {
        position: relative;
        margin-bottom: -10px;
        padding-top: 10px;
        border-bottom: 10px solid #1d70b8;
        max-width: 960px;
        margin-left: auto;
        margin-right: auto;
        padding-left: 15px;
        padding-right: 15px;
      }

      .govuk-header__logotype-text {
        font-weight: 700;
        font-size: 30px;
        line-height: 1;
        color: #ffffff;
      }

      .govuk-header__link {
        text-decoration: none;
        color: #ffffff;
      }

      .govuk-header__service-name {
        display: inline-block;
        margin-bottom: 10px;
        font-weight: 700;
        font-size: 24px;
        color: #ffffff;
      }

      .govuk-width-container {
        max-width: 960px;
        margin: 0 auto;
        padding: 0 15px;
      }

      .govuk-main-wrapper {
        padding: 40px 0;
      }

      .govuk-heading-xl {
        font-weight: 700;
        font-size: 48px;
        margin: 0 0 50px 0;
      }

      .govuk-heading-m {
        font-weight: 700;
        font-size: 24px;
        margin: 0 0 20px 0;
      }

      .govuk-body {
        font-size: 19px;
        margin: 0 0 20px 0;
      }

      .govuk-button {
        font-weight: 400;
        font-size: 19px;
        line-height: 1;
        display: inline-block;
        position: relative;
        padding: 8px 10px 7px;
        border: 2px solid transparent;
        border-radius: 0;
        color: #ffffff;
        background-color: #00703c;
        box-shadow: 0 2px 0 #002d18;
        text-align: center;
        cursor: pointer;
        margin-right: 15px;
        margin-bottom: 15px;
      }

      .govuk-button:hover { background-color: #005a30; }
      .govuk-button:focus {
        border-color: #ffdd00;
        outline: 3px solid transparent;
        box-shadow: inset 0 0 0 1px #ffdd00;
        background-color: #ffdd00;
        color: #0b0c0c;
      }

      .govuk-button--blue {
        background-color: #1d70b8;
        box-shadow: 0 2px 0 #003078;
      }
      .govuk-button--blue:hover { background-color: #003078; }

      .govuk-button--secondary {
        background-color: #f3f2f1;
        box-shadow: 0 2px 0 #929191;
        color: #0b0c0c;
      }
      .govuk-button--secondary:hover { background-color: #dbdad9; }

      .govuk-button--warning {
        background-color: #d4351c;
        box-shadow: 0 2px 0 #6e1509;
      }
      .govuk-button--warning:hover { background-color: #aa2a16; }

      .govuk-button.shiny-download-link { text-decoration: none; }

      .govuk-form-group { margin-bottom: 30px; }

      .govuk-label {
        font-weight: 400;
        font-size: 19px;
        display: block;
        margin-bottom: 5px;
      }

      .govuk-hint {
        font-size: 19px;
        margin-bottom: 15px;
        color: #505a5f;
      }

      .govuk-input {
        font-size: 19px;
        width: 100%;
        max-width: 200px;
        height: 40px;
        padding: 5px;
        border: 2px solid #0b0c0c;
        border-radius: 0;
      }

      .govuk-input:focus {
        outline: 3px solid #ffdd00;
        outline-offset: 0;
        box-shadow: inset 0 0 0 2px;
      }

      .govuk-select {
        font-size: 19px;
        width: 100%;
        max-width: 320px;
        height: 40px;
        padding: 5px;
        border: 2px solid #0b0c0c;
        border-radius: 0;
        background-color: #ffffff;
      }

      .govuk-select:focus {
        outline: 3px solid #ffdd00;
        outline-offset: 0;
        box-shadow: inset 0 0 0 2px;
      }

      .govuk-width-container--wide {
        max-width: 1400px;
      }

      .preview-scroll {
        max-height: 460px;
        overflow: auto;
      }

      .govuk-phase-banner {
        padding: 10px 0;
        border-bottom: 1px solid #b1b4b6;
      }

      .govuk-tag {
        font-weight: 700;
        font-size: 16px;
        display: inline-block;
        padding: 5px 8px 4px;
        color: #ffffff;
        background-color: #1d70b8;
        letter-spacing: 1px;
        text-transform: uppercase;
        margin-right: 10px;
      }

      .govuk-tag--green {
        background-color: #00703c;
      }

      .dashboard-card {
        background-color: #ffffff;
        border: 1px solid #b1b4b6;
        margin-bottom: 20px;
      }

      .dashboard-card__header {
        background-color: #1d70b8;
        color: #ffffff;
        padding: 15px 20px;
        font-weight: 700;
        font-size: 19px;
      }

      .dashboard-card__content { padding: 20px; }

      .govuk-section-break {
        margin: 30px 0;
        border: 0;
        border-bottom: 1px solid #b1b4b6;
      }

      .govuk-grid-row {
        display: flex;
        flex-wrap: wrap;
        margin: 0 -15px;
      }

      .govuk-grid-column-one-half {
        width: 50%;
        padding: 0 15px;
      }

      @media (max-width: 768px) {
        .govuk-grid-column-one-half { width: 100%; }
      }

      .govuk-footer {
        padding: 25px 0;
        border-top: 1px solid #b1b4b6;
        background: #f3f2f1;
        text-align: center;
        color: #505a5f;
      }

      .container-fluid { padding: 0 !important; margin: 0 !important; max-width: none !important; }

      /* Month confirmation status */
      .month-status {
        display: inline-block;
        padding: 5px 10px;
        margin-left: 10px;
        font-size: 16px;
        border-radius: 3px;
      }

      .month-status--confirmed {
        background-color: #00703c;
        color: #ffffff;
      }

      .month-status--pending {
        background-color: #f47738;
        color: #ffffff;
      }

      /* Input row with button */
      .input-row {
        display: flex;
        align-items: flex-end;
        gap: 15px;
        flex-wrap: wrap;
      }

      .input-row .govuk-form-group {
        margin-bottom: 0;
      }

      /* Stats table */
      .stats-table {
        width: 100%;
        border-collapse: collapse;
        font-size: 14px;
      }

      .stats-table th {
        background-color: #0b0c0c;
        color: #ffffff;
        font-weight: 700;
        padding: 10px 8px;
        text-align: left;
        border: 1px solid #0b0c0c;
        font-size: 12px;
      }

      .stats-table td {
        padding: 8px;
        border: 1px solid #b1b4b6;
        background-color: #ffffff;
      }

      .stats-table tr:nth-child(even) td { background-color: #f8f8f8; }
      .stats-table tr:hover td { background-color: #f3f2f1; }

      .stat-positive { color: #00703c; font-weight: 700; }
      .stat-negative { color: #d4351c; font-weight: 700; }
      .stat-neutral { color: #505a5f; }

      /* Top Ten List */
      .top-ten-list {
        list-style: none;
        padding: 0;
        margin: 0;
        counter-reset: item;
      }

      .top-ten-list li {
        padding: 12px 12px 12px 50px;
        margin-bottom: 8px;
        background-color: #ffffff;
        border-left: 4px solid #1d70b8;
        position: relative;
        font-size: 15px;
        line-height: 1.4;
      }

      .top-ten-list li::before {
        counter-increment: item;
        content: counter(item);
        position: absolute;
        left: 12px;
        top: 12px;
        font-weight: 700;
        font-size: 18px;
        color: #1d70b8;
      }

      .govuk-list { padding-left: 20px; }
      .govuk-list li { margin-bottom: 5px; }

      /* Shiny progress bar customization */
      .shiny-notification {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 400px;
        background: #ffffff;
        border: 3px solid #1d70b8;
        border-radius: 0;
        box-shadow: 0 4px 20px rgba(0,0,0,0.3);
        padding: 20px;
        z-index: 99999;
      }

      .shiny-notification-message {
        font-family: 'Source Sans Pro', Arial, sans-serif;
        font-size: 16px;
        color: #0b0c0c;
        margin-bottom: 15px;
      }

      .shiny-notification .progress {
        height: 10px;
        background-color: #f3f2f1;
        border-radius: 0;
        margin-top: 10px;
      }

      .shiny-notification .progress-bar {
        background-color: #00703c;
        border-radius: 0;
      }

      .shiny-notification-close {
        display: none;
      }
    "))
  ),

  # Header
  tags$header(class = "govuk-header",
    div(class = "govuk-header__container",
      div(style = "margin-bottom: 10px;",
        a(href = "#", class = "govuk-header__link",
          span(class = "govuk-header__logotype-text", "GOV.UK")
        )
      ),
      span(class = "govuk-header__service-name", "Labour Market Statistics Brief")
    )
  ),

  # Main Content
  div(class = "govuk-width-container",

    div(class = "govuk-phase-banner",
      span(class = "govuk-tag", "BETA"),
      span("This is a new service.")
    ),

    tags$main(class = "govuk-main-wrapper",

      h1(class = "govuk-heading-xl", "Labour Market Statistics Brief Generator"),

      # Configuration
      div(class = "dashboard-card",
        div(class = "dashboard-card__header", "Configuration"),
        div(class = "dashboard-card__content",
          uiOutput("reference_month_display"),
          div(class = "govuk-grid-row", style = "margin-top: 20px;",
            div(class = "govuk-grid-column-one-half",
              div(class = "govuk-form-group",
                tags$label(class = "govuk-label", `for` = "vacancies_period", "Vacancies"),
                uiOutput("vacancies_dropdown")
              )
            ),
            div(class = "govuk-grid-column-one-half",
              div(class = "govuk-form-group",
                tags$label(class = "govuk-label", `for` = "payroll_period", "Payroll"),
                uiOutput("payroll_dropdown")
              )
            )
          )
        )
      ),

      # Actions
      div(class = "dashboard-card",
        div(class = "dashboard-card__header", "Actions"),
        div(class = "dashboard-card__content",
          h2(class = "govuk-heading-m", "Preview Data"),
          p(class = "govuk-body", "Load and preview statistics before generating documents."),
          actionButton("preview_dashboard", "Preview Dashboard", class = "govuk-button govuk-button--blue"),
          actionButton("preview_topten", "Preview Top Ten Stats", class = "govuk-button govuk-button--blue"),

          tags$hr(class = "govuk-section-break"),

          h2(class = "govuk-heading-m", "Download Documents"),
          p(class = "govuk-body", "Generate and download briefing documents."),
          downloadButton("download_word", "Download Word Document", class = "govuk-button"),
          downloadButton("download_excel", "Download Excel Workbook", class = "govuk-button govuk-button--secondary")
        )
      ),

      # Preview sections moved below (full-width)
    )
  ),

  # Full-width preview area (stacked so it is readable)
  div(class = "govuk-width-container govuk-width-container--wide",
    tags$main(class = "govuk-main-wrapper", style = "padding-top: 0;",
      div(class = "dashboard-card",
        div(class = "dashboard-card__header", "Dashboard Preview"),
        div(class = "dashboard-card__content preview-scroll", uiOutput("dashboard_preview"))
      ),
      div(class = "dashboard-card",
        div(class = "dashboard-card__header", "Top Ten Statistics Preview"),
        div(class = "dashboard-card__content", uiOutput("topten_preview"))
      )
    )
  ),

  # Footer
  tags$footer(class = "govuk-footer",
    div(class = "govuk-width-container",
      "Labour Market Statistics Brief Generator | Department for Business and Trade"
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # File paths
  config_path       <- "utils/config.R"
  calculations_path <- "utils/calculations.R"
  word_script_path  <- "utils/word_output.R"
  excel_script_path <- "sheets/excel_audit.R"
  summary_path      <- "sheets/summary.R"
  top_ten_path      <- "sheets/top_ten_stats.R"
  template_path     <- "utils/DB.docx"

  # Reactive values
  dashboard_data <- reactiveVal(NULL)
  topten_data <- reactiveVal(NULL)
  auto_month <- reactiveVal(NULL)

  # Helper to create LFS-style label from end date
  make_lfs_label_local <- function(end_date) {
    start_date <- end_date %m-% months(2)
    sprintf("%s - %s %s",
            format(start_date, "%b"),
            format(end_date, "%b"),
            format(end_date, "%Y"))
  }

  # Function to detect latest quarter from LFS data
  detect_latest_quarter <- function() {
    tryCatch({
      conn <- DBI::dbConnect(RPostgres::Postgres())
      on.exit(DBI::dbDisconnect(conn), add = TRUE)

      # Fetch LFS data and find latest period
      query <- 'SELECT DISTINCT time_period FROM "ons"."labour_market__age_group"'
      result <- DBI::dbGetQuery(conn, query)

      if (nrow(result) == 0) return(NULL)

      # Parse periods like "Jul-Sep 2025" to get the end month
      periods <- result$time_period
      parsed <- lapply(periods, function(p) {
        tryCatch({
          # Pattern: "Mon-Mon YYYY" e.g., "Jul-Sep 2025"
          parts <- strsplit(trimws(p), " ")[[1]]
          if (length(parts) != 2) return(NULL)
          year <- as.integer(parts[2])
          months_part <- strsplit(parts[1], "-")[[1]]
          if (length(months_part) != 2) return(NULL)
          end_mon <- months_part[2]
          m <- match(end_mon, month.abb)
          if (is.na(m)) return(NULL)
          as.Date(sprintf("%04d-%02d-01", year, m))
        }, error = function(e) NULL)
      })

      # Filter out NULLs and find max
      valid_dates <- Filter(Negate(is.null), parsed)
      if (length(valid_dates) == 0) return(NULL)

      latest_end <- do.call(max, valid_dates)

      # Convert to manual_month format: add 2 months to get the release month
      release_month <- latest_end %m+% months(2)
      tolower(format(release_month, "%b%Y"))
    }, error = function(e) {
      message("Failed to detect latest quarter: ", e$message)
      NULL
    })
  }

  # Auto-detect latest quarter from LFS data on startup
  observe({
    detected <- detect_latest_quarter()
    if (!is.null(detected)) {
      auto_month(detected)
    } else {
      # Fallback to config.R if database unavailable
      if (file.exists(config_path)) {
        tryCatch({
          env <- new.env()
          source(config_path, local = env)
          if (exists("manual_month", envir = env)) {
            auto_month(tolower(env$manual_month))
          }
        }, error = function(e) {
          auto_month("dec2025")
        })
      } else {
        auto_month("dec2025")
      }
    }
  })

  # Display the auto-selected reference month

  output$reference_month_display <- renderUI({
    mm <- auto_month()
    if (is.null(mm)) return(NULL)

    # Parse to get the quarter end
    tryCatch({
      mm_date <- lubridate::parse_date_time(mm, orders = c("my", "bY", "by"))
      if (is.na(mm_date)) {
        mm_lower <- tolower(mm)
        mon3 <- substr(gsub("[^a-z]", "", mm_lower), 1, 3)
        yr <- as.integer(gsub("[^0-9]", "", mm_lower))
        m <- match(mon3, tolower(month.abb))
        mm_date <- as.Date(sprintf("%04d-%02d-01", yr, m))
      }
      quarter_end <- mm_date %m-% months(2)
      period_label <- make_lfs_label_local(quarter_end)

      div(
        span(style = "font-size: 19px;", "The latest quarter is "),
        span(style = "font-weight: 600; font-size: 19px;", period_label)
      )
    }, error = function(e) {
      div(span(style = "font-weight: 600;", mm))
    })
  })

  # Generate vacancies dropdown with actual period labels
  output$vacancies_dropdown <- renderUI({
    mm <- auto_month()
    if (is.null(mm)) return(NULL)

    tryCatch({
      mm_lower <- tolower(mm)
      mon3 <- substr(gsub("[^a-z]", "", mm_lower), 1, 3)
      yr <- as.integer(gsub("[^0-9]", "", mm_lower))
      m <- match(mon3, tolower(month.abb))
      mm_date <- as.Date(sprintf("%04d-%02d-01", yr, m))

      # Aligned period (dashboard quarter): mm - 2 months
      aligned_end <- mm_date %m-% months(2)
      aligned_label <- make_lfs_label_local(aligned_end)

      # Latest period: one month ahead of aligned
      latest_end <- mm_date %m-% months(1)
      latest_label <- make_lfs_label_local(latest_end)

      tags$select(
        class = "govuk-select",
        id = "vacancies_period",
        name = "vacancies_period",
        tags$option(value = "aligned", aligned_label),
        tags$option(value = "latest", latest_label)
      )
    }, error = function(e) {
      tags$select(
        class = "govuk-select",
        id = "vacancies_period",
        name = "vacancies_period",
        tags$option(value = "aligned", "Aligned"),
        tags$option(value = "latest", "Latest")
      )
    })
  })

  # Generate payroll dropdown with actual period labels
  output$payroll_dropdown <- renderUI({
    mm <- auto_month()
    if (is.null(mm)) return(NULL)

    tryCatch({
      mm_lower <- tolower(mm)
      mon3 <- substr(gsub("[^a-z]", "", mm_lower), 1, 3)
      yr <- as.integer(gsub("[^0-9]", "", mm_lower))
      m <- match(mon3, tolower(month.abb))
      mm_date <- as.Date(sprintf("%04d-%02d-01", yr, m))

      # Aligned period (dashboard quarter): mm - 2 months
      aligned_end <- mm_date %m-% months(2)
      aligned_label <- make_lfs_label_local(aligned_end)

      # Latest period: one month ahead of aligned
      latest_end <- mm_date %m-% months(1)
      latest_label <- make_lfs_label_local(latest_end)

      tags$select(
        class = "govuk-select",
        id = "payroll_period",
        name = "payroll_period",
        tags$option(value = "aligned", aligned_label),
        tags$option(value = "latest", latest_label)
      )
    }, error = function(e) {
      tags$select(
        class = "govuk-select",
        id = "payroll_period",
        name = "payroll_period",
        tags$option(value = "aligned", "Aligned"),
        tags$option(value = "latest", "Latest")
      )
    })
  })

  # ============================================================================
  # PREVIEW: DASHBOARD
  # ============================================================================

  observeEvent(input$preview_dashboard, {

    withProgress(message = "Loading Dashboard Data", value = 0, {

      incProgress(0.1, detail = "Step 1/6: Checking configuration files...")
      Sys.sleep(0.3)

      if (!file.exists(calculations_path)) {
        showNotification("Error: calculations.R not found", type = "error")
        return()
      }

      incProgress(0.15, detail = "Step 2/6: Loading configuration...")
      Sys.sleep(0.2)

      calc_env <- new.env(parent = globalenv())

      if (file.exists(config_path)) {
        source(config_path, local = calc_env)
      }

      incProgress(0.15, detail = "Step 3/6: Setting reference month...")
      Sys.sleep(0.2)

      # Use auto-detected month
      cm <- auto_month()
      if (!is.null(cm)) {
        calc_env$manual_month <- cm
      }

      # Use separate modes for vacancies and payroll
      vac_mode <- if (!is.null(input$vacancies_period) && nzchar(input$vacancies_period)) {
        tolower(input$vacancies_period)
      } else {
        "aligned"
      }
      payroll_mode_val <- if (!is.null(input$payroll_period) && nzchar(input$payroll_period)) {
        tolower(input$payroll_period)
      } else {
        "aligned"
      }
      calc_env$vacancies_mode <- vac_mode
      calc_env$payroll_mode <- payroll_mode_val

      incProgress(0.2, detail = "Step 4/6: Running calculations...")

      tryCatch({
        source(calculations_path, local = calc_env)
      }, error = function(e) {
        showNotification(paste("Calculation error:", e$message), type = "error", duration = 5)
        return()
      })

      incProgress(0.2, detail = "Step 5/6: Building metrics table...")
      Sys.sleep(0.2)

      gv <- function(name) {
        if (exists(name, envir = calc_env)) {
          val <- get(name, envir = calc_env)
          if (is.numeric(val)) return(val)
        }
        NA_real_
      }

      metrics <- list(
        list(name = "Employment 16+ (000s)", cur = gv("emp16_cur") / 1000, dq = gv("emp16_dq") / 1000, dy = gv("emp16_dy") / 1000, dc = gv("emp16_dc") / 1000, de = gv("emp16_de") / 1000, invert = FALSE, type = "count"),
        list(name = "Employment rate 16-64 (%)", cur = gv("emp_rt_cur"), dq = gv("emp_rt_dq"), dy = gv("emp_rt_dy"), dc = gv("emp_rt_dc"), de = gv("emp_rt_de"), invert = FALSE, type = "rate"),
        list(name = "Unemployment 16+ (000s)", cur = gv("unemp16_cur") / 1000, dq = gv("unemp16_dq") / 1000, dy = gv("unemp16_dy") / 1000, dc = gv("unemp16_dc") / 1000, de = gv("unemp16_de") / 1000, invert = TRUE, type = "count"),
        list(name = "Unemployment rate 16+ (%)", cur = gv("unemp_rt_cur"), dq = gv("unemp_rt_dq"), dy = gv("unemp_rt_dy"), dc = gv("unemp_rt_dc"), de = gv("unemp_rt_de"), invert = TRUE, type = "rate"),
        list(name = "Inactivity 16-64 (000s)", cur = gv("inact_cur") / 1000, dq = gv("inact_dq") / 1000, dy = gv("inact_dy") / 1000, dc = gv("inact_dc") / 1000, de = gv("inact_de") / 1000, invert = TRUE, type = "count"),
        list(name = "Inactivity 50-64 (000s)", cur = gv("inact5064_cur") / 1000, dq = gv("inact5064_dq") / 1000, dy = gv("inact5064_dy") / 1000, dc = gv("inact5064_dc") / 1000, de = gv("inact5064_de") / 1000, invert = TRUE, type = "count"),
        list(name = "Inactivity rate 16-64 (%)", cur = gv("inact_rt_cur"), dq = gv("inact_rt_dq"), dy = gv("inact_rt_dy"), dc = gv("inact_rt_dc"), de = gv("inact_rt_de"), invert = TRUE, type = "rate"),
        list(name = "Inactivity rate 50-64 (%)", cur = gv("inact5064_rt_cur"), dq = gv("inact5064_rt_dq"), dy = gv("inact5064_rt_dy"), dc = gv("inact5064_rt_dc"), de = gv("inact5064_rt_de"), invert = TRUE, type = "rate"),
        list(name = "Vacancies (000s)", cur = gv("vac_cur"), dq = gv("vac_dq"), dy = gv("vac_dy"), dc = gv("vac_dc"), de = gv("vac_de"), invert = NA, type = "exempt"),
        list(name = "Payroll employees (000s)", cur = gv("payroll_cur"), dq = gv("payroll_dq"), dy = gv("payroll_dy"), dc = gv("payroll_dc"), de = gv("payroll_de"), invert = FALSE, type = "exempt"),
        list(name = "Wages total pay (%)", cur = gv("latest_wages"), dq = gv("wages_change_q"), dy = gv("wages_change_y"), dc = gv("wages_change_covid"), de = gv("wages_change_election"), invert = FALSE, type = "wages"),
        list(name = "Wages CPI-adjusted (%)", cur = gv("latest_wages_cpi"), dq = gv("wages_cpi_change_q"), dy = gv("wages_cpi_change_y"), dc = gv("wages_cpi_change_covid"), de = gv("wages_cpi_change_election"), invert = FALSE, type = "wages")
      )

      incProgress(0.2, detail = "Step 6/6: Finalizing dashboard...")
      Sys.sleep(0.2)

      dashboard_data(metrics)
    })

    showNotification("Dashboard loaded successfully!", type = "message", duration = 3)
  })

  # ============================================================================
  # PREVIEW: TOP TEN
  # ============================================================================

  observeEvent(input$preview_topten, {

    withProgress(message = "Loading Top Ten Statistics", value = 0, {

      incProgress(0.1, detail = "Step 1/6: Checking required files...")
      Sys.sleep(0.3)

      if (!file.exists(calculations_path)) {
        showNotification("Error: calculations.R not found", type = "error")
        return()
      }

      if (!file.exists(top_ten_path)) {
        showNotification("Error: top_ten_stats.R not found", type = "error")
        return()
      }

      incProgress(0.15, detail = "Step 2/6: Loading configuration...")
      Sys.sleep(0.2)

      if (file.exists(config_path)) {
        source(config_path, local = FALSE)
      }

      incProgress(0.15, detail = "Step 3/6: Setting reference month...")
      Sys.sleep(0.2)

      # Use auto-detected month
      cm <- auto_month()
      if (!is.null(cm)) {
        manual_month <<- cm
      }

      # Use separate modes for vacancies and payroll
      vac_mode <- if (!is.null(input$vacancies_period) && nzchar(input$vacancies_period)) {
        tolower(input$vacancies_period)
      } else {
        "aligned"
      }
      payroll_mode_val <- if (!is.null(input$payroll_period) && nzchar(input$payroll_period)) {
        tolower(input$payroll_period)
      } else {
        "aligned"
      }
      vacancies_mode <<- vac_mode
      payroll_mode <<- payroll_mode_val

      incProgress(0.2, detail = "Step 4/6: Running calculations...")

      tryCatch({
        source(calculations_path, local = FALSE)
      }, error = function(e) {
        showNotification(paste("Calculation error:", e$message), type = "error", duration = 5)
        return()
      })

      incProgress(0.2, detail = "Step 5/6: Loading top ten generator...")

      source(top_ten_path, local = FALSE)

      incProgress(0.2, detail = "Step 6/6: Generating statistics...")

      if (exists("generate_top_ten")) {
        top10 <- generate_top_ten()
        topten_data(top10)
      } else {
        showNotification("Error: generate_top_ten function not found", type = "error")
        return()
      }
    })

    showNotification("Top Ten statistics loaded successfully!", type = "message", duration = 3)
  })

  # ============================================================================
  # DOWNLOAD: WORD
  # ============================================================================

  output$download_word <- downloadHandler(
    filename = function() {
      paste0("Labour_Market_Brief_", format(Sys.Date(), "%Y-%m-%d"), ".docx")
    },
    content = function(file) {

      withProgress(message = "Generating Word Document", value = 0, {

        incProgress(0.15, detail = "Step 1/6: Checking officer package...")
        Sys.sleep(0.2)

        if (!requireNamespace("officer", quietly = TRUE)) {
          showNotification("Error: officer package not installed", type = "error")
          writeLines("Error: officer package required", file)
          return()
        }

        incProgress(0.15, detail = "Step 2/6: Locating template file...")
        Sys.sleep(0.2)

        if (!file.exists(template_path)) {
          incProgress(0.7, detail = "Creating basic document (no template)...")

          doc <- officer::read_docx()
          doc <- officer::body_add_par(doc, "Labour Market Statistics Brief", style = "heading 1")
          doc <- officer::body_add_par(doc, paste("Generated:", format(Sys.Date(), "%d %B %Y")))
          doc <- officer::body_add_par(doc, "Note: Template file (utils/DB.docx) not found.")
          print(doc, target = file)

          showNotification("Word document created (basic - no template)", type = "warning", duration = 3)
          return()
        }

        incProgress(0.2, detail = "Step 3/6: Loading word output script...")

        source(word_script_path, local = FALSE)

        incProgress(0.2, detail = "Step 4/6: Running calculations...")

        incProgress(0.15, detail = "Step 5/6: Generating document content...")

        incProgress(0.15, detail = "Step 6/6: Writing Word file...")

        cm <- auto_month()
        month_override <- cm

        # Get separate modes for vacancies and payroll
        vac_mode <- if (!is.null(input$vacancies_period) && nzchar(input$vacancies_period)) tolower(input$vacancies_period) else "aligned"
        payroll_mode_val <- if (!is.null(input$payroll_period) && nzchar(input$payroll_period)) tolower(input$payroll_period) else "aligned"

        tryCatch({
          generate_word_output(
            template_path = template_path,
            output_path = file,
            calculations_path = calculations_path,
            config_path = config_path,
            summary_path = summary_path,
            top_ten_path = top_ten_path,
            manual_month_override = month_override,
            vacancies_mode_override = vac_mode,
            payroll_mode_override = payroll_mode_val,
            verbose = FALSE
          )
        }, error = function(e) {
          # Create error document
          doc <- officer::read_docx()
          doc <- officer::body_add_par(doc, "Error Generating Document", style = "heading 1")
          doc <- officer::body_add_par(doc, paste("Error:", e$message))
          print(doc, target = file)
          showNotification(paste("Word error:", e$message), type = "error", duration = 5)
        })
      })

      showNotification("Word document generated!", type = "message", duration = 3)
    }
  )

  # ============================================================================
  # DOWNLOAD: EXCEL - Uses exact excel_audit.R script
  # ============================================================================

  output$download_excel <- downloadHandler(
    filename = function() {
      # Keep the browser filename stable (and with a real .xlsx extension)
      "LM_Stats_Audit.xlsx"
    },
    content = function(file) {
      # IMPORTANT:
      # Shiny passes a tempfile path *without* an .xlsx extension.
      # openxlsx::saveWorkbook() requires a filename ending in .xlsx/.xlsm.
      # So we write to a real *.xlsx temp file, then copy it to Shiny's target.
      tryCatch({
        withProgress(message = "Generating Excel Workbook", value = 0, {

          incProgress(0.1, detail = "Step 1/4: Checking openxlsx...")
          if (!requireNamespace("openxlsx", quietly = TRUE)) {
            stop("openxlsx package not installed")
          }

          incProgress(0.2, detail = "Step 2/4: Loading excel_audit.R...")
          excel_env <- new.env(parent = globalenv())
          source(excel_script_path, local = excel_env)

          if (!exists("create_audit_workbook", envir = excel_env)) {
            stop("create_audit_workbook() not found after sourcing excel_audit.R")
          }

          incProgress(0.5, detail = "Step 3/4: Building workbook (this may take a moment)...")
          tmp_xlsx <- tempfile(fileext = ".xlsx")

          # Get separate modes for vacancies and payroll
          vac_mode <- if (!is.null(input$vacancies_period) && nzchar(input$vacancies_period)) tolower(input$vacancies_period) else "aligned"
          payroll_mode_val <- if (!is.null(input$payroll_period) && nzchar(input$payroll_period)) tolower(input$payroll_period) else "aligned"

          excel_env$create_audit_workbook(
            output_path = tmp_xlsx,
            calculations_path = calculations_path,
            config_path = config_path,
            vacancies_mode = vac_mode,
            payroll_mode = payroll_mode_val,
            verbose = FALSE
          )

          incProgress(0.15, detail = "Step 4/4: Preparing download...")
          ok <- file.copy(tmp_xlsx, file, overwrite = TRUE)
          if (!isTRUE(ok) || !file.exists(file)) {
            stop("Excel workbook could not be copied to Shiny download location")
          }
        })

        showNotification("Excel workbook generated!", type = "message", duration = 3)

      }, error = function(e) {
        # On error, still return a valid .xlsx to the user.
        message("Excel download error: ", e$message)
        showNotification(paste("Excel error:", e$message), type = "error", duration = 5)

        if (requireNamespace("openxlsx", quietly = TRUE)) {
          tmp_xlsx <- tempfile(fileext = ".xlsx")
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Error")
          openxlsx::writeData(wb, "Error", data.frame(
            Error = paste("Failed to generate workbook:", e$message),
            Suggestion = "Check database connectivity / package availability, then try again."
          ))
          openxlsx::saveWorkbook(wb, tmp_xlsx, overwrite = TRUE)
          file.copy(tmp_xlsx, file, overwrite = TRUE)
        } else {
          # Worst-case: write a plain-text error so the download isn't empty
          writeLines(paste("Failed to generate Excel workbook:", e$message), con = file)
        }
      })
    }
  )

  # ============================================================================
  # RENDER: DASHBOARD PREVIEW
  # ============================================================================

  output$dashboard_preview <- renderUI({
    metrics <- dashboard_data()

    if (is.null(metrics)) {
      return(div(
        p(class = "govuk-body", "Click 'Preview Dashboard' to load statistics."),
        tags$ul(class = "govuk-list",
          tags$li("Employment and unemployment figures"),
          tags$li("Inactivity rates"),
          tags$li("Vacancies and payroll data"),
          tags$li("Wage statistics")
        )
      ))
    }

    format_change <- function(val, invert, type) {
      if (is.na(val)) return(tags$span(class = "stat-neutral", "-"))

      css_class <- if (is.na(invert)) "stat-neutral"
                   else if (val > 0) { if (invert) "stat-negative" else "stat-positive" }
                   else if (val < 0) { if (invert) "stat-positive" else "stat-negative" }
                   else "stat-neutral"

      sign_str <- if (val > 0) "+" else ""
      formatted <- if (type == "rate") paste0(sign_str, round(val, 1), "pp")
                   else if (type == "wages") paste0(sign_str, round(val, 1), "%")
                   else paste0(sign_str, format(round(val), big.mark = ","))

      tags$span(class = css_class, formatted)
    }

    format_current <- function(val, type) {
      if (is.na(val)) return("-")
      if (type == "rate" || type == "wages") paste0(round(val, 1), "%")
      else format(round(val), big.mark = ",")
    }

    rows <- lapply(metrics, function(m) {
      tags$tr(
        tags$td(m$name),
        tags$td(format_current(m$cur, m$type)),
        tags$td(format_change(m$dq, m$invert, m$type)),
        tags$td(format_change(m$dy, m$invert, m$type)),
        tags$td(format_change(m$dc, m$invert, m$type)),
        tags$td(format_change(m$de, m$invert, m$type))
      )
    })

    tags$table(class = "stats-table",
      tags$thead(tags$tr(
        tags$th("Metric"), tags$th("Current"), tags$th("vs Qtr"),
        tags$th("vs Year"), tags$th("vs Covid"), tags$th("vs Election")
      )),
      tags$tbody(rows)
    )
  })

  # ============================================================================
  # RENDER: TOP TEN PREVIEW
  # ============================================================================

  output$topten_preview <- renderUI({
    top10 <- topten_data()

    if (is.null(top10)) {
      return(div(
        p(class = "govuk-body", "Click 'Preview Top Ten Stats' to load statistics."),
        tags$ul(class = "govuk-list",
          tags$li("Wage growth (nominal and CPI-adjusted)"),
          tags$li("Employment and unemployment rates"),
          tags$li("Payroll employment"),
          tags$li("Inactivity trends"),
          tags$li("Vacancies and redundancies")
        )
      ))
    }

    items <- lapply(1:10, function(i) {
      line_key <- paste0("line", i)
      line_text <- top10[[line_key]]
      if (is.null(line_text) || line_text == "") line_text <- "(Data not available)"
      tags$li(line_text)
    })

    tags$ol(class = "top-ten-list", items)
  })
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================

shinyApp(ui = ui, server = server)
