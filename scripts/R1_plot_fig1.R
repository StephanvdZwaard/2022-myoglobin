R1_plot_fig1 <- function(data,stats) {
  
  
  # ------------------------------------------------------------------------------------------------------------------------ #
  #                                               Settings & dependencies                                                    #
  # ------------------------------------------------------------------------------------------------------------------------ #
  
  
  # Load required libraries 
  require(stats)
  require(tidyverse)
  require(ggpubr)
  
  theme_specific <- theme_classic() + theme(legend.position='none',
                                            axis.title   = element_text(size=12, face='bold'),
                                            axis.text.y  = element_text(size=11, face='bold'),
                                            axis.text.x  = element_text(size=12, face='bold'))
  
  A <-  data %>% 
        ggplot(aes(x=group, y=mb_avg, fill=group)) + geom_dotplot(binaxis='y', stackdir='center', binwidth = .75*1.2/30) +
        stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                     geom="pointrange", color="red") +
        geom_signif(comparisons = list(c("cyclists", "controls")), 
                    annotation= case_when(stats[7,3]$`p-value` < .01 ~ '**',
                                          stats[7,3]$`p-value` < .05 ~ '*'),
                    textsize = 5,
                    map_signif_level = TRUE,
                    y_position = .1*1.1+1, tip_length = 0, vjust = 0.2) +
        labs(x='', y='Mb concentration (mM)\n') +
        scale_y_continuous(limits=c(0,1.2), n.breaks = 8) + 
        scale_fill_manual(values=c('#c5c5c5',"black")) + theme_specific
  
  B <-  data %>% 
        ggplot(aes(x=group, y=mRNA_mb, fill=group)) + geom_dotplot(binaxis='y', stackdir='center', binwidth = .75*.15/30) +
        stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                     geom="pointrange", color="red") +
        geom_signif(comparisons = list(c("cyclists", "controls")), 
                    annotation= case_when(stats[10,3]$`p-value` < .01 ~ '**',
                                          stats[10,3]$`p-value` < .05 ~ '*'),
                    textsize = 5,
                    map_signif_level = TRUE,
                    y_position = .1*0.15+0.123, tip_length = 0, vjust = 0.2) +
        labs(x='', y='mRNA Mb expression (relative to 18S)\n') +
        scale_y_continuous(limits=c(0,.15), n.breaks = 8) + 
        scale_fill_manual(values=c('#c5c5c5',"black")) + theme_specific
      
  D <-  data %>% 
        ggplot(aes(x=group, y=tRNA_muscle, fill=group)) + geom_dotplot(binaxis='y', stackdir='center', binwidth = .75*125/30) +
        stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                     geom="pointrange", color="red") +
        labs(x='', y=bquote(bold(total~mRNA~expression~(ng%*%mg^-1)))) +
        scale_y_continuous(limits=c(0,130), n.breaks = 6) + 
        scale_fill_manual(values=c('#c5c5c5',"black")) + theme_specific
  
  E <-  data %>% 
        ggplot(aes(x=group, y=MDS, fill=group)) + geom_dotplot(binaxis='y', stackdir='center', binwidth = .75*50/30) +
        stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                     geom="pointrange", color="red") +
        labs(x='', y='MDS (pL)\n') +
        scale_y_continuous(limits=c(0,50), n.breaks = 6) +
        scale_fill_manual(values=c('#c5c5c5',"black")) + theme_specific
  
  C <-  data %>% #filter(mb_avg<1) %>%
        ggplot(aes(x=mRNA_mb, y=mb_avg)) + geom_smooth(method='lm', color='red', se=F) + geom_point() +
        labs(y='Mb concentration (mM)\n', x='\nmRNA Mb expression') +
        annotate('text',x=0.01, y=0.95, label='bold("r = 0.37")', parse=T, hjust = 0) +
        annotate('text',x=0.01, y=0.9, label='bold("P = 0.010")', parse=T, hjust = 0) +
        scale_y_continuous(limits=c(0,1.05), n.breaks = 8) + 
        scale_x_continuous(limits=c(0,.15),  n.breaks = 5) + 
        theme_specific + theme(axis.text.x  = element_text(size=11, face='bold'))
  
  # ------------------------------------------------------
  # Return and save figure
  # ------------------------------------------------------   
  
  # Save image as png
  
  tiff(paste0("./Figure1_",format(Sys.Date(),"%d%m%y"),".tiff"),
       bg = "transparent", width = 12, height = 8, unit = "in", pointsize = 10, res = 1200)
  
  graphic <- ggarrange(A, B, C, D, E,
                       labels = c("A","B","C","D","E"),
                       ncol = 3, nrow = 2,
                       align = "v")
  print({graphic})
  
  dev.off()
  
  
  # Return plot object
  return(graphic)
  
  
}


##############################################################################################################################
#                                                   End of syntax                                                            #
##############################################################################################################################