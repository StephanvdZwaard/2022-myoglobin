plot_fig1 <- function(data,stats) {
  
  
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
  
A <-  data %>% group_by(group) %>% summarise(mean = mean(mb_avg,na.rm=T), sd=sd(mb_avg,na.rm=T)) %>%
      ggplot(aes(x=group, y=mean, ymin=0.95*mean, ymax=mean+sd, fill=group)) + geom_errorbar(width=.2, size=1) + geom_col(position='dodge',width=.9) +
      geom_signif(comparisons = list(c("cyclists", "controls")), 
                  annotation= case_when(stats[7,3]$`p-value` < .01 ~ '**',
                                        stats[7,3]$`p-value` < .05 ~ '*'),
                  textsize = 5,
                  map_signif_level = TRUE,
                  y_position = .1*0.8+.663, tip_length = 0, vjust = 0.2) +
      labs(x='', y='Mb concentration (mM)\n') +
      scale_y_continuous(limits=c(0,.8), n.breaks = 8) + 
      scale_fill_manual(values=c('#c5c5c5',"black")) + theme_specific

D <-  data %>% group_by(group) %>% summarise(mean = mean(MDS,na.rm=T), sd=sd(MDS,na.rm=T)) %>%
      ggplot(aes(x=group, y=mean, ymin=0.95*mean, ymax=mean+sd, fill=group)) + geom_errorbar(width=.2, size=1) + geom_col(position='dodge',width=.9) +
      labs(x='', y='MDS (pL)\n') +
      scale_y_continuous(limits=c(0,50), n.breaks = 5) +
      scale_fill_manual(values=c('#c5c5c5',"black")) + theme_specific

B <-  data %>% group_by(group) %>% summarise(mean = mean(mRNA_mb,na.rm=T), sd=sd(mRNA_mb,na.rm=T)) %>%
      ggplot(aes(x=group, y=mean, ymin=0.95*mean, ymax=mean+sd, fill=group)) + geom_errorbar(width=.2, size=1) + geom_col(position='dodge',width=.9) +
      geom_signif(comparisons = list(c("cyclists", "controls")), 
                  annotation= case_when(stats[10,3]$`p-value` < .01 ~ '**',
                                        stats[10,3]$`p-value` < .05 ~ '*'),
                  textsize = 5,
                  map_signif_level = TRUE,
                  y_position = .1*0.14+0.115, tip_length = 0, vjust = 0.2) +
        labs(x='', y='mRNA Mb expression (relative to 18S)\n') +
      scale_y_continuous(limits=c(0,.14), n.breaks = 8) + 
      scale_fill_manual(values=c('#c5c5c5',"black")) + theme_specific

C <-  data %>% group_by(group) %>% summarise(mean = mean(tRNA_muscle,na.rm=T), sd=sd(tRNA_muscle,na.rm=T)) %>%
      ggplot(aes(x=group, y=mean, ymin=0.95*mean, ymax=mean+sd, fill=group)) + geom_errorbar(width=.2, size=1) + geom_col(position='dodge',width=.9) +
      labs(x='', y=bquote(bold(total~mRNA~expression~(ng%*%mg^-1)))) +
      scale_y_continuous(limits=c(0,100), n.breaks = 6) + 
      scale_fill_manual(values=c('#c5c5c5',"black")) + theme_specific

# ------------------------------------------------------
# Return and save figure
# ------------------------------------------------------   

    # Save image as png

tiff(paste0("./Figure1_",format(Sys.Date(),"%d%m%y"),".tiff"),
        bg = "transparent", width = 8, height = 8, unit = "in", pointsize = 10, res = 1200)

    graphic <- ggarrange(A, B, C, D,
                         labels = c("A","B","C","D"),
                         ncol = 2, nrow = 2,
                         align = "v")
    print({graphic})
    
    dev.off()


    # Return plot object
    return(graphic)


}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################