# ==============================================================================
# CALCULATIONS - Sources all sheets and computes all metrics
# ==============================================================================
# RUN FROM: Project root directory
# ==============================================================================

# Load common helpers
source("utils/helpers.R")

# Load config
if (!exists("manual_month", inherits = TRUE)) {
  source("utils/config.R")
}

# Load all sheets
source("sheets/lfs.R")
source("sheets/vacancies.R")
source("sheets/payroll.R")
source("sheets/wages_nominal.R")
source("sheets/wages_cpi.R")
source("sheets/days_lost.R")
source("sheets/redundancy.R")
source("sheets/sector_payroll.R")
source("sheets/hr1.R")
source("sheets/inactivity_reasons.R")
source("sheets/summary.R")



# ==============================================================================
# CALCULATE ALL METRICS
# ==============================================================================

# LFS metrics (employment, unemployment, inactivity)
lfs <- calculate_lfs(manual_month)

# Vacancies
# Some releases have vacancies/payroll updated ahead of the core LFS quarter.
# Allow callers (e.g., Shiny) to choose whether to use the latest available
# period for these series or align them to the dashboard reference quarter.
vac_mode <- if (exists("vacancies_mode", inherits = TRUE)) get("vacancies_mode", inherits = TRUE) else "latest"
vac <- calculate_vacancies(manual_month, mode = vac_mode)

# Payrolled employees
payroll_mode <- if (exists("payroll_mode", inherits = TRUE)) get("payroll_mode", inherits = TRUE) else "latest"
payroll <- calculate_payroll(manual_month, mode = payroll_mode)

# Nominal wages (total + regular)
wages_nom <- calculate_wages_nominal(manual_month)

# CPI-adjusted wages (total + regular)
wages_cpi <- calculate_wages_cpi(manual_month)

# Working days lost
days_lost <- calculate_days_lost(manual_month)

# Redundancy (LFS)
redund <- calculate_redundancy(manual_month)

# Sector payroll (hospitality, retail, health)
sectors <- calculate_sector_payroll(manual_month)

# HR1 redundancy notifications
hr1 <- calculate_hr1()

# Inactivity by reason
inact_reasons <- calculate_inactivity_reasons(manual_month)

# ==============================================================================
# ANCHOR DATE FOR LABELS
# ==============================================================================

cm <- parse_manual_month(manual_month)
anchor_m <- cm %m-% months(2)

# ==============================================================================
# CONVENIENCE VARIABLES FOR DASHBOARD
# ==============================================================================

# Employment
emp16_cur <- lfs$emp16$cur
emp16_dq <- lfs$emp16$dq
emp16_dy <- lfs$emp16$dy
emp16_dc <- lfs$emp16$dc
emp16_de <- lfs$emp16$de

emp_rt_cur <- lfs$emp_rt$cur
emp_rt_dq <- lfs$emp_rt$dq
emp_rt_dy <- lfs$emp_rt$dy
emp_rt_dc <- lfs$emp_rt$dc
emp_rt_de <- lfs$emp_rt$de

# Unemployment
unemp16_cur <- lfs$unemp16$cur
unemp16_dq <- lfs$unemp16$dq
unemp16_dy <- lfs$unemp16$dy
unemp16_dc <- lfs$unemp16$dc
unemp16_de <- lfs$unemp16$de

unemp_rt_cur <- lfs$unemp_rt$cur
unemp_rt_dq <- lfs$unemp_rt$dq
unemp_rt_dy <- lfs$unemp_rt$dy
unemp_rt_dc <- lfs$unemp_rt$dc
unemp_rt_de <- lfs$unemp_rt$de

# Inactivity
inact_cur <- lfs$inact$cur
inact_dq <- lfs$inact$dq
inact_dy <- lfs$inact$dy
inact_dc <- lfs$inact$dc
inact_de <- lfs$inact$de

inact_rt_cur <- lfs$inact_rt$cur
inact_rt_dq <- lfs$inact_rt$dq
inact_rt_dy <- lfs$inact_rt$dy
inact_rt_dc <- lfs$inact_rt$dc
inact_rt_de <- lfs$inact_rt$de

# 50-64 Inactivity
inact5064_cur <- lfs$inact5064$cur
inact5064_dq <- lfs$inact5064$dq
inact5064_dy <- lfs$inact5064$dy
inact5064_dc <- lfs$inact5064$dc
inact5064_de <- lfs$inact5064$de

inact5064_rt_cur <- lfs$inact5064_rt$cur
inact5064_rt_dq <- lfs$inact5064_rt$dq
inact5064_rt_dy <- lfs$inact5064_rt$dy
inact5064_rt_dc <- lfs$inact5064_rt$dc
inact5064_rt_de <- lfs$inact5064_rt$de

# Vacancies
vac_cur <- vac$cur
vac_dq <- vac$dq
vac_dy <- vac$dy
vac_dc <- vac$dc
vac_de <- vac$de

# Payroll (dashboard - 3 month avg)
payroll_cur <- payroll$cur
payroll_dq <- payroll$dq
payroll_dy <- payroll$dy
payroll_dc <- payroll$dc
payroll_de <- payroll$de



# Payroll flash (narrative - latest single month)
payroll_flash_cur <- payroll$flash_cur
payroll_flash_dy <- payroll$flash_dy
payroll_flash_de <- payroll$flash_de
payroll_flash_dm <- payroll$flash_dm

# Nominal wages - Total
latest_wages <- wages_nom$total$cur
wages_change_q <- wages_nom$total$dq
wages_change_y <- wages_nom$total$dy
wages_change_covid <- wages_nom$total$dc
wages_change_election <- wages_nom$total$de
wages_total_public <- wages_nom$total$public
wages_total_private <- wages_nom$total$private
wages_total_qchange <- wages_nom$total$qchange

# Nominal wages - Regular
latest_regular_cash <- wages_nom$regular$cur
wages_reg_change_q <- wages_nom$regular$dq
wages_reg_change_y <- wages_nom$regular$dy
wages_reg_change_covid <- wages_nom$regular$dc
wages_reg_change_election <- wages_nom$regular$de
wages_reg_public <- wages_nom$regular$public
wages_reg_private <- wages_nom$regular$private
wages_reg_qchange <- wages_nom$regular$qchange

# CPI wages - Total
latest_wages_cpi <- wages_cpi$total$cur
wages_cpi_change_q <- wages_cpi$total$dq
wages_cpi_change_y <- wages_cpi$total$dy
wages_cpi_change_covid <- wages_cpi$total$dc
wages_cpi_change_election <- wages_cpi$total$de
wages_cpi_total_vs_dec2007 <- wages_cpi$total$pct_vs_dec2007
wages_cpi_total_vs_pandemic <- wages_cpi$total$pct_vs_pandemic

# CPI wages - Regular
latest_regular_cpi <- wages_cpi$regular$cur
wages_reg_cpi_change_q <- wages_cpi$regular$dq
wages_reg_cpi_change_y <- wages_cpi$regular$dy
wages_reg_cpi_change_covid <- wages_cpi$regular$dc
wages_reg_cpi_change_election <- wages_cpi$regular$de
wages_cpi_reg_vs_dec2007 <- wages_cpi$regular$pct_vs_dec2007
wages_cpi_reg_vs_pandemic <- wages_cpi$regular$pct_vs_pandemic

# Days lost
days_lost_cur <- days_lost$cur
days_lost_label <- days_lost$label

# Redundancy
redund_cur <- redund$cur
redund_dq <- redund$dq
redund_dy <- redund$dy
redund_dc <- redund$dc
redund_de <- redund$de

# Sectors - Hospitality
hosp_cur <- sectors$hospitality$cur
hosp_dm <- sectors$hospitality$dm
hosp_dy <- sectors$hospitality$dy
hosp_dc <- sectors$hospitality$dc
hosp_de <- sectors$hospitality$de

# Sectors - Retail
retail_cur <- sectors$retail$cur
retail_dm <- sectors$retail$dm
retail_dy <- sectors$retail$dy
retail_dc <- sectors$retail$dc
retail_de <- sectors$retail$de

# Sectors - Health
health_cur <- sectors$health$cur
health_dm <- sectors$health$dm
health_dy <- sectors$health$dy
health_dc <- sectors$health$dc
health_de <- sectors$health$de

# HR1
hr1_cur <- hr1$cur
hr1_dm <- hr1$dm
hr1_dy <- hr1$dy
hr1_dc <- hr1$dc
hr1_de <- hr1$de

# Inactivity driver text (for narrative)
inact_driver_text <- generate_inactivity_driver_text(inact_reasons)

# ==============================================================================
# NARRATIVE LABELS
# ==============================================================================

lfs_period_label <- lfs_label_narrative(lfs$emp16$end)
payroll_month_label <- format(payroll$anchor, "%B %Y")
payroll_flash_label <- format(payroll$flash_anchor, "%B %Y")
sector_month_label <- format(sectors$hospitality$anchor, "%B %Y")
hr1_month_label <- format(hr1$anchor, "%B %Y")