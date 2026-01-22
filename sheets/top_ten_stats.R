# ==============================================================================
# TOP TEN STATS MODULE - Narrative generation for briefing
# ==============================================================================

library(glue)

# ------------------------------------------------------------------------------
# HELPER FUNCTIONS
# ------------------------------------------------------------------------------

dir_word <- function(x) {
  if (is.na(x)) "a change of"
  else if (x > 0) "an increase of"
  else if (x < 0) "a decrease of"
  else "no change,"
}

# Format flash change - input is in thousands, output shows full number
fmt_flash_change <- function(x) {
  if (is.na(x)) return("—")
  full_val <- round(x * 1000, 0)
  format(abs(full_val), big.mark = ",")
}

# ------------------------------------------------------------------------------
# GENERATE TOP TEN STATS
# ------------------------------------------------------------------------------

generate_top_ten <- function() {
  
  # Calculate public/private difference
  pub_priv_diff_total <- if (!is.na(wages_total_public) && !is.na(wages_total_private)) {
    wages_total_public - wages_total_private
  } else NA_real_
  
  pub_priv_diff_reg <- if (!is.na(wages_reg_public) && !is.na(wages_reg_private)) {
    wages_reg_public - wages_reg_private
  } else NA_real_
  
  # Conditional direction words
  pub_priv_dir <- ifelse(!is.na(pub_priv_diff_reg) && pub_priv_diff_reg >= 0, "higher", "lower")
  wages_cpi_total_dir <- ifelse(!is.na(latest_wages_cpi) && latest_wages_cpi >= 0, "grew by", "fell by")
  wages_cpi_reg_dir <- ifelse(!is.na(latest_regular_cpi) && latest_regular_cpi >= 0, "grew by", "fell by")
  vs_dec2007_dir <- ifelse(!is.na(wages_cpi_total_vs_dec2007) && wages_cpi_total_vs_dec2007 >= 0, "higher", "lower")
  vs_pandemic_dir <- ifelse(!is.na(wages_cpi_total_vs_pandemic) && wages_cpi_total_vs_pandemic >= 0, "higher", "lower")
  emp_rt_covid_dir <- ifelse(!is.na(emp_rt_dc) && emp_rt_dc >= 0, "higher", "lower")
  inact_rt_covid_dir <- ifelse(!is.na(inact_rt_dc) && inact_rt_dc >= 0, "higher", "lower")
  unemp_rt_covid_dir <- ifelse(!is.na(unemp_rt_dc) && unemp_rt_dc >= 0, "higher", "lower")
  
  # Line 1: Wages nominal
  line1 <- glue(
    'Annual growth in employees\' average earnings was {fmt_pct(latest_wages)} for total pay ',
    '(including bonuses) and {fmt_pct(latest_regular_cash)} for regular pay (excluding bonuses) ',
    'in {lfs_period_label}. Public sector changes of {fmt_pct(wages_total_public)} and ',
    '{fmt_pct(wages_reg_public)} respectively are {fmt_one_dec(abs(pub_priv_diff_reg))}pp {pub_priv_dir} than the private sector. ',
    'Wage growth is {ifelse(wages_total_qchange < 0, "easing", "accelerating")}, with this representing a quarterly ',
    '{ifelse(wages_total_qchange >= 0, "increase", "decline")} of {fmt_one_dec(abs(wages_total_qchange))}pp ',
    'and a quarterly {ifelse(wages_reg_qchange >= 0, "increase", "decline")} of {fmt_one_dec(abs(wages_reg_qchange))}pp respectively.'
  )
  
  # Line 2: Wages CPI
  line2 <- glue(
    'Adjusted for CPI inflation, total and regular pay in {lfs_period_label} {wages_cpi_total_dir} ',
    '{fmt_pct(abs(latest_wages_cpi))} and {wages_cpi_reg_dir} {fmt_pct(abs(latest_regular_cpi))} respectively. ',
    'Inflation-adjusted total wages are now around {fmt_one_dec(abs(wages_cpi_total_vs_dec2007))}% {vs_dec2007_dir} than they were in ',
    'December 2007 prior to the global financial crisis and {fmt_one_dec(abs(wages_cpi_total_vs_pandemic))}% {vs_pandemic_dir} than ',
    'before the pandemic (2019 average).'
  )
  
  # Line 3: Employment rate
  line3 <- glue(
    'The employment rate was {format(round(emp_rt_cur, 1), nsmall = 1)}% in ',
    '{lfs_period_label}, which is {fmt_dir(emp_rt_dy, "up", "down")} ',
    '{fmt_pp(emp_rt_dy)} from a year ago and {fmt_dir(emp_rt_dq, "up", "down")} ',
    '{fmt_pp(emp_rt_dq)} on the last quarter. The employment rate is ',
    '{fmt_pp(emp_rt_dc)} {emp_rt_covid_dir} than before the pandemic.'
  )
  
  # Line 4: Payroll flash
  flash_de_dir <- if (!is.na(payroll_flash_de) && payroll_flash_de < 0) "a fall of around" else "a rise of around"
  flash_dy_dir <- if (!is.na(payroll_flash_dy) && payroll_flash_dy < 0) "a fall of" else "a rise of"
  flash_dm_dir <- if (!is.na(payroll_flash_dm) && payroll_flash_dm < 0) "a fall of" else "a rise of"
  
  line4 <- glue(
    'Early "flash" estimates for {payroll_flash_label} indicate that ',
    'there were {format(round(payroll_flash_cur, 2), nsmall = 2)}M payrolled employees. ',
    'This represents {flash_de_dir} ',
    '{fmt_flash_change(payroll_flash_de)} since the 2024 election and {flash_dy_dir} {fmt_flash_change(payroll_flash_dy)} ',
    'compared to the same period a year ago, and {flash_dm_dir} ',
    '{fmt_flash_change(payroll_flash_dm)} from the previous month. This varies ',
    'between sectors – changes in employee numbers on the year have been ',
    'concentrated in sectors such as hospitality and ',
    'retail which saw an annual change of {fmt_int(hosp_dy * 1000)} and ',
    '{fmt_int(retail_dy * 1000)} respectively. Employee numbers in the health and ',
    'social work sector changed by {fmt_int(health_dy * 1000)}. [Note: payroll ',
    'employment data is frequently revised; data here is a "flash" estimate ',
    'and so does not align with the table above].'
  )
  
  # Line 5: Inactivity rate
  line5 <- glue(
    'The economic inactivity rate was {format(round(inact_rt_cur, 1), nsmall = 1)}% ',
    'in {lfs_period_label}, {fmt_dir(inact_rt_dy, "down", "up")} {fmt_pp(inact_rt_dy)} ',
    'from a year ago, and {fmt_dir(inact_rt_dq, "down", "up")} {fmt_pp(inact_rt_dq)} ',
    'from the previous quarter. The inactivity rate is ',
    '{fmt_pp(inact_rt_dc)} {inact_rt_covid_dir} than before the pandemic, ',
    'driven by {inact_driver_text}.'
  )
  
  # Line 6: Unemployment rate
  line6 <- glue(
    'The unemployment rate was {format(round(unemp_rt_cur, 1), nsmall = 1)}% ',
    'in {lfs_period_label}, {fmt_dir(unemp_rt_dq, "an increase of", "a decrease of")} ',
    '{fmt_pp(unemp_rt_dq)} from the previous quarter, and ',
    '{fmt_dir(unemp_rt_dy, "an increase of", "a decrease of")} {fmt_pp(unemp_rt_dy)} ',
    'from this time last year. The unemployment rate is {fmt_pp(unemp_rt_dc)} ',
    '{unemp_rt_covid_dir} than before the pandemic.'
  )
  
  # Line 7: Days lost
  days_lost_vs_2019 <- ifelse(days_lost_cur < 19.5, "below", "above")
  
  line7 <- glue(
    'There were an estimated {fmt_int(days_lost_cur * 1000)} ',
    'working days lost because of labour disputes across the UK. ',
    'This is {days_lost_vs_2019} the monthly average in 2019 (19,500).'
  )
  
  # Line 8: Vacancies
  vac_dir <- ifelse(vac_dy < 0, "decreased", "increased")
  vac_cur_vs_peak <- ifelse(vac_cur < 1300, "falling", "rising")
  vac_cur_vs_prepandemic <- ifelse(vac_dc < 0, "below", "above")
  vac_cur_vs_2010avg <- ifelse(vac_cur < 660, "below", "above")
  
  line8 <- glue(
    'The number of job vacancies {vac_dir} on the year by {fmt_int(abs(vac_dy * 1000))} ',
    'to {fmt_int(vac_cur * 1000)} in {lfs_period_label}. Vacancies have been ',
    '{vac_cur_vs_peak} since the peak in March to May 2022 (1.3 million); ',
    'and stand {vac_cur_vs_prepandemic} their pre-pandemic level ({fmt_int(abs(vac_dc * 1000))}). They ',
    'are {vac_cur_vs_2010avg} the average in the 2010s (around 660,000).'
  )
  
  # Line 9: Redundancies
  redund_q_dir <- dir_word(redund_dq)
  redund_y_dir <- dir_word(redund_dy)
  avg2010 <- if (!is.na(redund_cur) && redund_cur < 4.7) "below" else "above"
  
  line9 <- glue(
    "The number of people reporting redundancy in {lfs_period_label}, ",
    "according to the Labour Force Survey, was {fmt_one_dec(redund_cur)} ",
    "per thousand employees, {redund_q_dir} ",
    "{fmt_one_dec(abs(redund_dq))} per thousand employees ",
    "from the previous quarter and {redund_y_dir} ",
    "{fmt_one_dec(abs(redund_dy))} per thousand employees ",
    "compared to the same period last year. This is {avg2010} the average ",
    "in the 2010s of 4.7 redundancies per thousand employees."
  )
  
  # Line 10: HR1
  hr1_vs_prepandemic <- if (!is.na(hr1_cur) && hr1_cur < 27600) "below" else "above"
  
  line10 <- glue(
    'The Insolvency Service were notified of {fmt_int(hr1_cur)} ',
    'potential redundancies in {hr1_month_label}. This is ',
    '{hr1_vs_prepandemic} the pre-pandemic average of 27,600 (Apr 2019 – Feb 2020).'
  )
  
  list(
    line1 = line1,
    line2 = line2,
    line3 = line3,
    line4 = line4,
    line5 = line5,
    line6 = line6,
    line7 = line7,
    line8 = line8,
    line9 = line9,
    line10 = line10
  )
}

# ------------------------------------------------------------------------------
# PRINT TOP TEN STATS
# ------------------------------------------------------------------------------

print_top_ten <- function(stats) {
  cat('\n')
  cat('1.', stats$line1, '\n\n')
  cat('2.', stats$line2, '\n\n')
  cat('3.', stats$line3, '\n\n')
  cat('4.', stats$line4, '\n\n')
  
  cat('5.', stats$line5, '\n\n')
  cat('6.', stats$line6, '\n\n')
  cat('7.', stats$line7, '\n\n')
  cat('8.', stats$line8, '\n\n')
  cat('9.', stats$line9, '\n\n')
  cat('10.', stats$line10, '\n\n')
 
}