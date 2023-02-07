plot_fig2 <- function(data, stats) {
  
  
  # ------------------------------------------------------------------------------------------------------------------------ #
  #                                               Settings & dependencies                                                    #
  # ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Load required libraries 
  require(stats)
  require(tidyverse)
  require(ggpubr)
  
  theme_specific <- theme_classic() + theme(legend.position='none',
                                            axis.title.y = element_text(size=12, face='bold'),
                                            axis.text.y  = element_text(size=11, face='bold'),
                                            axis.text.x  = element_text(size=12, face='bold'))
  
  A <-  data %>% group_by(group) %>% summarise(mean = mean(mb_I,na.rm=T), sd=sd(mb_avg,na.rm=T)) %>%
        ggplot(aes(x=group, y=mean, ymin=0.95*mean, ymax=mean+sd, fill=group)) + geom_errorbar(width=.2, size=1) + geom_col(position='dodge',width=.9) +
        geom_signif(comparisons = list(c("cyclists", "controls")), 
                    annotation= case_when(stats[8,3]$`p-value` < .01 ~ '**',
                                          stats[8,3]$`p-value` < .05 ~ '*'),
                    textsize = 5,
                    map_signif_level = TRUE,
                    y_position = .1*1+0.806, tip_length = 0, vjust = 0.2) +
        labs(x='', y='Mb concentration in type I fibers (mM)\n') +
        scale_y_continuous(limits=c(0,1), n.breaks = 10) + 
        scale_fill_manual(values=c('#c5c5c5',"black")) + theme_specific
  

  B <-  data %>% group_by(group) %>% summarise(mean = mean(mb_II,na.rm=T), sd=sd(mRNA_mb,na.rm=T)) %>%
        ggplot(aes(x=group, y=mean, ymin=0.95*mean, ymax=mean+sd, fill=group)) + geom_errorbar(width=.2, size=1) + geom_col(position='dodge',width=.9) +
        labs(x='', y='Mb concentration in type II fibers (mM)\n') +
        scale_y_continuous(limits=c(0,1), n.breaks = 10) + 
        scale_fill_manual(values=c('#c5c5c5',"black")) + theme_specific

  # ------------------------------------------------------
  # Return and save figure
  # ------------------------------------------------------   
  
  # Save image as png
  
  tiff(paste0("./Figure2_",format(Sys.Date(),"%d%m%y"),".tiff"),
       bg = "transparent", width = 8, height = 4, unit = "in", pointsize = 10, res = 1200)
  
  graphic <- ggarrange(A, B, 
                       labels = c("A","B"),
                       ncol = 2, nrow = 1,
                       align = "v")
  print({graphic})
  
  dev.off()
  
  
  # Return plot object
  return(graphic)
  
  
}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################