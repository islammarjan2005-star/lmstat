# ==============================================================================
# SUMMARY MODULE - Automated narrative for Summary section
# ==============================================================================

library(glue)

# ------------------------------------------------------------------------------
# GENERATE SUMMARY
# ------------------------------------------------------------------------------

generate_summary <- function() {
  
  # Payroll directions
  payroll_dy_dir <- ifelse(payroll_dy < 0, "fall", "rise")
  payroll_dq_dir <- ifelse(payroll_dq < 0, "declining", "rising")
  payroll_dq_dir2 <- ifelse(payroll_dq < 0, "fall", "rise")
  payroll_dy_dir2 <- ifelse(payroll_dy < 0, "fall", "rise")
  
  # Flash directions
  flash_dm_dir <- ifelse(payroll_flash_dm < 0, "fell", "rose")
  flash_dy_dir <- ifelse(payroll_flash_dy < 0, "fell", "rose")
  flash_dm_word <- ifelse(payroll_flash_dm < 0, "fall", "rise")
  flash_dy_word <- ifelse(payroll_flash_dy < 0, "fall", "rise")
  
  # Vacancy directions
  vac_dq_dir <- ifelse(vac_dq < 0, "fell", "rose")
  
  # LFS directions
  emp_rt_dq_dir <- ifelse(emp_rt_dq >= 0, "rose", "fell")
  inact_rt_dq_dir <- ifelse(inact_rt_dq >= 0, "rose", "fell")
  unemp_rt_dq_dir <- ifelse(unemp_rt_dq >= 0, "rose", "fell")
  
  # Wage directions
  wages_dq_dir <- ifelse(wages_total_qchange >= 0, "rose", "fell")
  wages_cpi_dir <- ifelse(latest_wages_cpi >= 0, "grew", "fell")
  
  # Redundancy directions
  redund_dq_dir <- ifelse(redund_dq >= 0, "rose", "fell")
  
  # Payroll percentage changes (need to calculate)
  # payroll_cur is in thousands, so calculate % changes
  payroll_pct_dq <- if (!is.na(payroll_dq) && !is.na(payroll_cur) && payroll_cur != 0) {
    (payroll_dq / payroll_cur) * 100
  } else NA_real_
  
  payroll_pct_dy <- if (!is.na(payroll_dy) && !is.na(payroll_cur) && payroll_cur != 0) {
    (payroll_dy / payroll_cur) * 100
  } else NA_real_
  
  # Line 1: Payroll weakening/strengthening
  labour_market_dir <- ifelse(payroll_dy < 0 || unemp_rt_dy > 0, "weaken", "strengthen")
  
  line1 <- glue(
    'The labour market appeared to {labour_market_dir} further in {lfs_period_label}. ',
    'The number of payrolled employees (PAYE) continued to {payroll_dq_dir2}, ',
    '{payroll_dq_dir} by {fmt_int(abs(payroll_dq))}k ({format(round(abs(payroll_pct_dq), 1), nsmall = 1)}%) on the quarter, ',
    'and {fmt_int(abs(payroll_dy))}k ({format(round(abs(payroll_pct_dy), 1), nsmall = 1)}%) on the year ',
    '(the period comparable with LFS).'
  )
  
  # Line 2: Flash estimate
  flash_dm_pct <- if (!is.na(payroll_flash_dm) && !is.na(payroll_flash_cur) && payroll_flash_cur != 0) {
    (payroll_flash_dm / (payroll_flash_cur * 1000)) * 100
  } else NA_real_
  
  flash_dy_pct <- if (!is.na(payroll_flash_dy) && !is.na(payroll_flash_cur) && payroll_flash_cur != 0) {
    (payroll_flash_dy / (payroll_flash_cur * 1000)) * 100
  } else NA_real_
  
  line2 <- glue(
    'The latest "flash" estimate for {payroll_flash_label} suggests payroll employees {flash_dm_dir} by ',
    '{fmt_int(abs(payroll_flash_dm * 1000))} ({format(round(abs(flash_dm_pct), 1), nsmall = 1)}%) on the month, ',
    'and {fmt_int(abs(payroll_flash_dy * 1000))}k ({format(round(abs(flash_dy_pct), 1), nsmall = 1)}%) on the year.'
  )
  
  # Line 3: Vacancies
  vac_dq_pct <- if (!is.na(vac_dq) && !is.na(vac_cur) && vac_cur != 0) {
    (vac_dq / vac_cur) * 100
  } else NA_real_
  
  line3 <- glue(
    'Vacancies {vac_dq_dir} by {fmt_int(abs(vac_dq * 1000))}k ({format(round(abs(vac_dq_pct), 1), nsmall = 1)}%) ',
    'on the quarter to {fmt_int(vac_cur * 1000)}k in {lfs_period_label}.'
  )
  
  # Line 4: LFS summary
  line4 <- glue(
    'Labour Force Survey (LFS) suggests that in {lfs_period_label}, ',
    'the employment rate {emp_rt_dq_dir} to {format(round(emp_rt_cur, 1), nsmall = 1)}% ',
    '({ifelse(emp_rt_dq >= 0, "+", "")}{format(round(emp_rt_dq, 1), nsmall = 1)} percentage points) ',
    'compared to the previous three-month period, while inactivity {inact_rt_dq_dir} to ',
    '{format(round(inact_rt_cur, 1), nsmall = 1)}% ',
    '({ifelse(inact_rt_dq >= 0, "+", "")}{format(round(inact_rt_dq, 1), nsmall = 1)} percentage points) ',
    'and unemployment {unemp_rt_dq_dir} to {format(round(unemp_rt_cur, 1), nsmall = 1)}% ',
    '({ifelse(unemp_rt_dq >= 0, "+", "")}{format(round(unemp_rt_dq, 1), nsmall = 1)}ppts).'
  )
  
  # Line 5: Wages
  line5 <- glue(
    'Annual wage growth in average weekly earnings (inc. bonuses) {wages_dq_dir} to ',
    '{format(round(latest_wages, 1), nsmall = 1)}% in {lfs_period_label} ',
    '({ifelse(wages_total_qchange >= 0, "+", "")}{format(round(wages_total_qchange, 1), nsmall = 1)} percentage points). ',
    'Adjusted for inflation, wages {wages_cpi_dir} by {format(round(abs(latest_wages_cpi), 1), nsmall = 1)}% on the year.'
  )
  
  # Line 6: Redundancy
  # Convert redund_cur (per 1000) to actual thousands - need employment level
  # redund_cur is rate per 1000, emp16_cur is employment in thousands
  redund_level <- if (!is.na(redund_cur) && !is.na(emp16_cur)) {
    (redund_cur / 1000) * emp16_cur
  } else NA_real_
  
  line6 <- glue(
    'Redundancy levels remain in line with long-term averages. ',
    'LFS redundancies {redund_dq_dir} on the quarter to {fmt_int(redund_level)}k in {lfs_period_label}.'
  )
  
  list(
    line1 = line1,
    line2 = line2,
    line3 = line3,
    line4 = line4,
    line5 = line5,
    line6 = line6
  )
}

# ------------------------------------------------------------------------------
# PRINT SUMMARY
# ------------------------------------------------------------------------------

print_summary <- function(summary) {
  cat('\n')
  cat('•', summary$line1, '\n\n')
  cat('•', summary$line2, '\n\n')
  cat('•', summary$line3, '\n\n')
  cat('•', summary$line4, '\n\n')
  cat('•', summary$line5, '\n\n')
  cat('•', summary$line6, '\n\n')
}