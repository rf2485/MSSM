setwd("/Users/rf699/Dropbox (Partners HealthCare)/Dickerson lab/Projects/Active_In Progress/Flaherty_GMWMMRI_tau_AD/RedCap Data")
library(tidyverse)
library(lubridate)

cohort <- read_csv('DickersonMasterEnrol-MSSMTauStudyProjectC_DATA_LABELS_2020-10-08_0905.csv')
scans <- read_csv('DickersonMasterEnrol-MSSMTauStudyScans_DATA_LABELS_2020-10-08_0906.csv')
nacc <- read_csv('NACCDownloads-MSSMTauStudyProject_DATA_LABELS_2020-10-21_1332.csv', col_types = cols(.default = "c"))

#pull NACC data for the cohort
cohort$`Visit year` <- year(cohort$`Date of baseline cog testing:`)
#cohort$`Visit month` <- month(cohort$`Date of baseline cog testing:`)
nacc$`Visit year` <- as.double(nacc$`Visit year`)
#nacc$`Visit month` <- as.double(nacc$`Visit month`)
cohort <- cohort %>% select(-`Event Name`) %>% left_join(.,nacc)
missing_nacc <- cohort %>% filter(is.na(`Event Name`)) %>% select(`Subject ID`:`Date of baseline cog testing:`)
missing_nacc <- left_join(missing_nacc, nacc)
crossyear_nacc <- missing_nacc %>% filter(`Event Name`=='T1')
full <- cohort %>% filter(!is.na(`Event Name`)) %>% bind_rows(.,crossyear_nacc)
missing_nacc <- missing_nacc %>% filter(is.na(`Event Name`))
full <- bind_rows(full, missing_nacc)

#pivot scan data into a wide format
mri <- scans %>% filter(`Scan session TYPE`=='MR')
pib <- scans %>% filter(`PET tracer`=='11C-PIB (amyloid; Pittsburgh B, PiB)')
t807 <- scans %>% filter(`PET tracer`=='18F-AV1451  (tau; 18F-T807, flortaucipir, FTP)')


scans_wide <- full_join(mri, pib, by = "Subject ID", suffix = c("_mri","_pib"))
colnames(t807) <- paste(colnames(t807), "t807", sep = "_")
t807 <- t807 %>% rename(`Subject ID` = `Subject ID_t807`)
scans_wide <- full_join(scans_wide, t807, by = "Subject ID")

full <- full_join(full, scans_wide)
full <- full[, colSums(is.na(full)) != nrow(full)]

write_csv(full, 'MSSM Npsych and Scan Meta.csv', na = '')

#manually selected NACC timepoints for patients with two timepoints in the same year