setwd("/Dropbox (Partners HealthCare)/Dickerson lab/Projects/Active_In Progress/Flaherty_GMWMMRI_tau_AD")
library(tidyverse)
library(eeptools)
library(qwraps2)
library(arsenal)
library(knitr)
library(survival)

pt_npsych_scan <- read_csv('MSSM_Npsych_and_Scan_Meta_Final.csv')
#calculate age, first by converting dob column to date
pt_npsych_scan$`Date of Birth` <- format(as.Date(pt_npsych_scan$`Date of Birth`, "%m/%d/%y"), "19%y-%m-%d")
#somehow still not a date column?
pt_npsych_scan$`Date of Birth` <- as.Date(pt_npsych_scan$`Date of Birth`)
#add age column
pt_npsych_scan <- pt_npsych_scan %>% 
  add_column(age = age_calc(pt_npsych_scan$`Date of Birth`, units = 'years'))

cn_abneg <- read_csv('RedCap Data/Johnson AB- Controls.csv')
cn_abneg$birth_date <- format(as.Date(cn_abneg$birth_date, "%m/%d/%y"), "19%y-%m-%d")
cn_abneg$birth_date <- as.Date(cn_abneg$birth_date)
cn_abneg <- cn_abneg %>%
  add_column(age = age_calc(cn_abneg$birth_date, units = 'years')) %>%
  rename(`Subject ID` = subject_id, `Diagnostic Cohort` = diagnostic_cohort, `Date of Birth` = birth_date,
         Sex = sex, `PetSurfer FLR_pib` = PIB_FS_DVR_FLR) %>%
  select(-name_last, -name_first, -subjid_datacentral, -PIB_FS_DVR_PVC_FLR)

all_npsych_scan <- bind_rows(pt_npsych_scan, cn_abneg) %>% rename(group = `Suspected AD pathology? (choice=Yes)`)
all_npsych_scan$group[all_npsych_scan$group=='Checked'] <- 'AD'
all_npsych_scan$group[is.na(all_npsych_scan$group)] <- 'Control'

# demographics_sex <- all_npsych_scan %>% group_by(Sex) %>% 
#   summarise(mean_age = mean(age), mean_cdr_sob = mean(`Standard CDR sum of boxes`), mean_cdr_global = mean(`Standard Global CDR`), n = n())
# demographics_group <- all_npsych_scan %>% ungroup() %>% group_by(group) %>%
#   summarise(mean_age = mean(age), mean_cdr_sob = mean(`Standard CDR sum of boxes`), mean_cdr_global = mean(`Standard Global CDR`), n = n())
# demographics_sex_group <- all_npsych_scan %>% group_by(Sex, group) %>%
#   summarise(mean_age = mean(age), mean_cdr_sob = mean(`Standard CDR sum of boxes`), mean_cdr_global = mean(`Standard Global CDR`), n = n())
# demographics_total <- all_npsych_scan %>% ungroup() %>%
#   summarise(mean_age = mean(age), mean_cdr_sob = mean(`Standard CDR sum of boxes`), mean_cdr_global = mean(`Standard Global CDR`), n = n())
# demographics <- bind_rows(demographics_sex_group, demographics_group, demographics_sex, demographics_total)
# write_csv(demographics, 'MSSM_demographics.csv', na = '')
# 
# #grouping all PPAs together, 9 PPA-L and 1 PPA-other, for a total of 10 PPA
# demographics_dx_cohort <- pt_npsych_scan %>% group_by(`Diagnostic Cohort`) %>%
#   summarise(mean_age = mean(age), mean_cdr_sob = mean(`Standard CDR sum of boxes`), mean_cdr_global = mean(`Standard Global CDR`), n = n())
# write_csv(demographics_dx_cohort, 'MSSM_dx_cohort_summary.csv', na = '')
# 
# dx_summary <- 
#   list("Age" =
#          list("min"       = ~ min(age),
#               "max"       = ~ max(age),
#               "mean (sd)" = ~qwraps2::mean_sd(age)),
#        "CDR Sum of Boxes" = 
#          list("min"       = ~ min(`Standard CDR sum of boxes`),
#               "max"       = ~ max(`Standard CDR sum of boxes`),
#               "mean (sd)" = ~qwraps2::mean_sd(`Standard CDR sum of boxes`)),
#        "CDR Global" = 
#          list("min"       = ~ min(`Standard Global CDR`),
#               "max"       = ~ max(`Standard Global CDR`),
#               "mean (sd)" = ~qwraps2::mean_sd(`Standard Global CDR`)))
# 
# summary_table(all_npsych_scan, dx_summary)
# mssm_dx

attr(all_npsych_scan$age, 'label') <- "Age in Years"
results='asis'
mssm_dx <- tableby(group ~ Sex + age, data = all_npsych_scan)
summary(mssm_dx, text=TRUE)
write2word(mssm_dx, "Total Cohort Dx Table.doc")

attr(pt_npsych_scan$age, 'label') <- "Age in Years"
pt_dx <- tableby(`Diagnostic Cohort` ~ Sex + age + `Standard CDR sum of boxes` + 
                   `Standard Global CDR`, data = pt_npsych_scan, total=FALSE)
summary(pt_dx, text = TRUE)
write2word(pt_dx, "Patient Demographics Table.doc")
