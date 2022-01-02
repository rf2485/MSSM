setwd("~/OneDrive - nyu.edu/Flaherty_GMWMMRI_tau_AD")
library(ggpubr)
library(tidyverse)
library(readxl)
library(gridExtra)

#load in the excel sheets
roi_xlsx <- "Analyses/roi_analyses/mssm_stats_thick_contrast_ftp.xlsx"
roi <- read_excel(path = roi_xlsx, sheet = "concat") %>% filter(dx != "pt_z")
roi_pt <- roi %>% filter(dx == 'pt')
pt_list <- roi %>% select(1,3) %>% filter(!is.na(dx))
roi_thickness <- read_excel(path = roi_xlsx, sheet = "thickness") %>% 
  rename(subj = lh.mssm.rois.thick.thickness) %>% left_join(.,pt_list) %>%
  rename_with(~ gsub("_thickness", "", .x))
roi_contrast <- read_excel(path = roi_xlsx, sheet = "contrast") %>%
  rename(subj = lh.mssm.rois.contrast.thickness) %>% left_join(.,pt_list) %>%
  rename_with(~ gsub("_contrast", "", .x))
roi_ftp <- read_excel(path = roi_xlsx, sheet = "ftp") %>%
  rename(subj = lh.mssm.rois.ftp.thickness) %>% left_join(.,pt_list) %>%
  rename_with(~ gsub("_ftp", "", .x))
roi_names <- gsub("_thickness", "", colnames(roi_thickness[, !colnames(roi_thickness) %in% c("subj","dx")])) #don't include the first column

roi_thickness <- roi_thickness %>% pivot_longer(cols = -c("subj", "dx"), names_to = "roi", values_to = "thickness")
roi_contrast <- roi_contrast %>% pivot_longer(cols = -c("subj", "dx"), names_to = "roi", values_to = "contrast")
roi_ftp <- roi_ftp %>% pivot_longer(cols = -c("subj", "dx"), names_to = "roi", values_to = "ftp")

#what determines tau positivity? see Ossenkopple et al. 2018 
##https://jamanetwork.com/journals/jama/fullarticle/2702872
#roi_ftp$tau_positivity[roi_ftp$ftp > 1.3] <- "positive"
#roi_ftp$tau_positivity[roi_ftp$ftp < 1.3] <- "negative"

roi_concat <- inner_join(roi_thickness, roi_contrast) %>% inner_join(.,roi_ftp) #%>% filter(!is.na(tau_positivity))
pt_roi_concat <- roi_concat %>% filter(dx == "pt") 

for(name in roi_names) { #for every roi in the list...
  df <- roi %>% select(subj, dx, starts_with(name)) #pull the roi columns into separate dataframe
  df <- df %>% rename_with(~ gsub(name, "", .x)) %>% #remove roi name from column names in new dataframe
    rename_with(~ gsub("_", "", .x)) #remove "_" from column names
  #df$tau_positivity[df$ftp > 1.65] <- "positive"
  #df$tau_positivity[df$ftp < 1.65] <- "negative"
  cohensd_thickness <- rstatix::cohens_d(df, thickness~dx)
  cohensd_contrast <- rstatix::cohens_d(df, contrast~dx)
  
  thickness_means <- ggboxplot(df, x = "dx", y = "thickness") +
    stat_compare_means(method = 't.test', label.x.npc = 'center', position = 'jitter',
                       aes(label = paste0("p = ", ..p.format.., " Cohen's D = ", round(cohensd_thickness$effsize, digits = 2))))
  contrast_means <- ggboxplot(df, x = 'dx', y = 'contrast') +
    stat_compare_means(method = 't.test', label.x.npc = 'center', position = 'jitter',
                       aes(label = paste0("p = ", ..p.format.., " Cohen's D = ", round(cohensd_contrast$effsize, digits = 2))))
  grid <- grid.arrange(thickness_means, contrast_means, ncol = 2, top = name)
  filename = paste('Analyses/Cohens D PT vs CN/', name, ".pdf", sep = "") #the file name is the roi name and ".pdf"
  ggsave(filename, plot = grid, width = 12) #save the grid
}

cohensd_thickness <- rstatix::cohens_d(roi_concat, thickness~dx)
cohensd_contrast <- rstatix::cohens_d(roi_concat, contrast~dx)

thickness_means <- ggboxplot(roi_concat, x = "dx", y = "thickness") +
  stat_compare_means(method = 't.test', label.x.npc = 'center', position = 'jitter',
                     aes(label = paste0("p = ", ..p.format.., " Cohen's D = ", round(cohensd_thickness$effsize, digits = 2)))) +
  xlab("") +
  ylab("") +
  ggtitle("Cortical Thickness (mm)") +
  scale_x_discrete(limits=c("pt","cn"),labels=c("Patient", "Control")) +
  theme_bw()

contrast_means <- ggboxplot(roi_concat, x = 'dx', y = 'contrast') +
  stat_compare_means(method = 't.test', label.x.npc = 'center', position = 'jitter',
                     aes(label = paste0("p = ", ..p.format.., " Cohen's D = ", round(cohensd_contrast$effsize, digits = 2)))) +
  xlab("") +
  ylab("") +
  ggtitle("Gray-White Contrast Ratio (GWC)") +
  scale_x_discrete(limits=c("pt","cn"),labels=c("Patient", "Control")) +
  theme_bw()

ftp_means <- ggboxplot(roi_concat, x = 'dx', y = 'ftp') +  
  stat_compare_means(method = 't.test', label.x.npc = 'left', position = 'jitter',
                     aes(label = paste0("p = ", ..p.format.., " Cohen's D = ", round(cohensd_contrast$effsize, digits = 2)))) +
  xlab("") +
  ylab("") +
  scale_x_discrete(limits=c("pt","cn"),labels=c("Patient", "Control")) +
  ggtitle("Tau PET Uptake Ratio (SUVR)") +
  theme_bw()

grid <- grid.arrange(contrast_means, ftp_means, thickness_means, ncol=3)
ggsave('Analyses/Cohens D PT vs CN/global.pdf', plot = grid, width = 15)

#GLMs / ANOVAs for effect of cohort on contrast vs ftp and thickness vs ftp, for each roi
##see AAIC 2020 poster folder for code

