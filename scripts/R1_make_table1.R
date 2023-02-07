R1_make_table1 <- function(stats) {
  
  
  # ------------------------------------------------------------------------------------------------------------------------ #
  #                                               Settings & dependencies                                                    #
  # ------------------------------------------------------------------------------------------------------------------------ #
  
  require(writexl)
  
  table1 <- data.frame(stats) %>% 
            mutate_at(.vars = vars(mean1:sd2),
                      .funs = list(~ case_when(row_number()<=3 ~ round(unlist(.)),
                                               row_number()<=5  ~ round(unlist(.), digits=1),
                                               TRUE             ~ round(unlist(.), digits=2)))) %>%
            mutate(p.value = unlist(p.value)) %>%
            mutate(cyclists = paste0(mean1,' ± ',sd1,ifelse(p.value<0.05,'*','')),
                   controls = paste0(mean2,' ± ',sd2)) %>%
            filter(row_number() %in% c(2:8)) %>%
            select(var,cyclists,controls,p.value) %>% 
            unnest(cols=c(var,`p.value`))
  
  # ------------------------------------------------------
  # Save results for table 3
  # ------------------------------------------------------   
  
  # Write to excel
  write_xlsx(table1, path = "./R1_Table1.xlsx")
  
  return(table1)
  
}

##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################