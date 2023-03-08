# ------------------------------------------------------------------------------------------------------------------------ #
#                         Script for the analysis of myoglobin levels in elite cyclists and controls                       #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Retrieve results for myoglobin concentration and mRNA expression levels in elite cyclists and controls     #
# Authors:      Stephan van der Zwaard [s.vander.zwaard@vu.nl]                                                             #
# Date:         20-09-2022                                                                                                 #
# Version:      1.0                                                                                                        #
# R.version:    4.2.2 (2022-10-31) 'Innocent and Trusting'                                                                 #
#                                                                                                                          #
# Publication:  Jacobs et al. 2023. Low myoglobin concentration in skeletal muscle of elite cyclists is associated with    #
#               low mRNA expression levels                                                                                 #
# DOI:          https://doi.org/10.1007/s00421-023-05161-z                                                                 #
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #

    # ------------------------------------------------------
    # Import libraries
    # ------------------------------------------------------   
    
      library(tidyverse)
      library(readxl)
      library(lubridate)
      library(ggsignif)

    
    # ------------------------------------------------------
    # Set options
    # ------------------------------------------------------   
    
      # set options
      options(stringsAsFactors = FALSE, dplyr.summarise.inform = FALSE)
    
    
    # ------------------------------------------------------
    # Load helper scripts
    # ------------------------------------------------------   
    
      source('scripts/compare_groups.R')
      source('scripts/R1_plot_fig1.R')
      source('scripts/R1_plot_fig2.R')
      source('scripts/R1_make_table1.R')
    
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                     Data collection                                                      #
# ------------------------------------------------------------------------------------------------------------------------ #
    
    
    # Collect analyzed data from spreadsheet    
    data <- read_excel('data/final-data.xlsx', skip=1)
    
    
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                     Data preprocessing                                                   #
# ------------------------------------------------------------------------------------------------------------------------ #
    
    
    # Rename columns, select data from participants and make data columns numeric    
    colnames(data) <- c('code','group','weight','height','BMI','age','total_circ_cells','total_cells_nuclei','total_csa_nuclei','fcsa','fcsa_I','fcsa_II','ft_I','ft_II','n_nuclei_fiber','total_nuclei','n_nuclei_fiber_length','MDS','MD','total_satellite_cells',
                    'n_satellite_fiber','p_satellite_nuclei','length_nuclei','ppcode','mb_avg','mb_I','mb_II','mb_perc','ppcode2','CT_mb_mean','CT_mb_sd','CT_s18_mean','CT_s18_sd','delta_CT','mRNA_mb','tRNA_muscle')
    data           <- data %>% filter(!is.na(group)) %>% 
                      select(-contains('ppcode')) %>%
                      mutate_at(.vars = vars(!contains('code')),
                                .funs = list(~as.numeric(.)))
    
    # Calculate average nuclei length
    length_nuclei  <- paste0(round(mean(data$length_nuclei, na.rm=T),1),"±",round(sd(data$length_nuclei, na.rm=T),1))
    print(length_nuclei)

    # Select relevant variables and groups
    data           <- data.frame(data) %>% select(code,group,fcsa:ft_II,n_nuclei_fiber,n_satellite_fiber,p_satellite_nuclei,MDS,MD,mb_avg:mb_perc,mRNA_mb,tRNA_muscle) %>%
                      mutate(group = case_when(group==1 ~ 'cyclists',
                                               group==2 ~ 'controls')) %>%
                      mutate(group = factor(group, levels = c('cyclists','controls'))) %>%
                      mutate(mb_perc = mb_perc*100)

# ------------------------------------------------------------------------------------------------------------------------ #
#                                                 Results for Mb manuscript                                                #
# ------------------------------------------------------------------------------------------------------------------------ #
    
    
    # ------------------------------------------------------
    # Stats
    # ------------------------------------------------------ 
    
      stats <- compare_groups(data)
    
    
    # ------------------------------------------------------
    # Figures
    # ------------------------------------------------------ 
    
      R1_plot_fig1(data, stats)
      R1_plot_fig2(data, stats)
      
      
    # ------------------------------------------------------
    # Table
    # ------------------------------------------------------ 
  
      table <- R1_make_table1(stats)

      
    # ------------------------------------------------------
    # In-text results
    # ------------------------------------------------------ 
      
      # Correlation between Mb concentration and Mb mRNA expression levels
      cor(data %>% select(mRNA_mb,mb_avg), method='spearman', use="pairwise.complete.obs")
      cor.test(data[,c('mb_avg')],data[,c('mRNA_mb')], method='spearman')

      
   # ------------------------------------------------------
   # Rebuttal
   # ------------------------------------------------------ 
      
      source('scripts/R1_rebuttal_analysis.R')
      