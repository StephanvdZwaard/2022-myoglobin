make_table1 <- function(stats) {
  
  
  # ------------------------------------------------------------------------------------------------------------------------ #
  #                                               Settings & dependencies                                                    #
  # ------------------------------------------------------------------------------------------------------------------------ #
  
  require(writexl)
  
  table1 <- data.frame(stats) %>% 
            mutate(cyclists = paste0(mean1,' ± ',sd1,ifelse(p.value<0.05,'*','')),
                   controls = paste0(mean2,' ± ',sd2)) %>%
            filter(row_number()<=4) %>%
            select(var,cyclists,controls,p.value) %>% 
            unnest(cols=c(var,`p.value`))
  
  # ------------------------------------------------------
  # Save results for table 3
  # ------------------------------------------------------   
  
  # Write to excel
  write_xlsx(table1, path = "./Table1.xlsx")
  
  return(table1)
  
}

##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################