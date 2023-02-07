create_fig3 <- function() {
  
colnames(data) <- c('study','var1','end_trained','str_trained','sex')
data <- data %>% mutate(end_trained = as.numeric(end_trained),
                        str_trained = as.numeric(str_trained)) %>%
        mutate(end_trained = case_when(end_trained == 1 ~ 'untrained',
                                       end_trained == 2 ~ 'intermediate',
                                       end_trained == 3 ~ 'advanced',
                                       TRUE ~ '')) %>%
        mutate(str_trained = case_when(str_trained == 1 ~ 'untrained',
                                       str_trained == 2 ~ 'intermediate',
                                       str_trained == 3 ~ 'advanced',
                                       TRUE ~ '')) %>%
        mutate(end_trained = factor(end_trained, levels = c('untrained','intermediate','advanced'), ordered = T),
               str_trained = factor(str_trained, levels = c('untrained','intermediate','advanced'), ordered = T))

data %>% group_by(end_trained) %>% summarise(n = n()) %>% mutate(type = 'endurance training status') %>% filter(!is.na(end_trained)) %>%
          ggplot(aes(x=end_trained, y=n, fill=n, color='black')) + geom_col() + 
          geom_text(aes(x=end_trained, y=1.6, label=n)) +
          facet_wrap(~type) +
          #scale_fill_manual(values="#228B22") +
          scale_fill_gradient(low="white", high="#228B22", limits=c(1, 30)) +
          scale_y_continuous(limits = c(0,30)) + labs(y='Number of studies\n') +
          scale_color_manual(values='black') + 
          theme_classic() + theme(legend.position = 'none')

data %>% group_by(str_trained) %>% summarise(n = n()) %>% mutate(type = 'strength training status') %>% filter(!is.na(str_trained)) %>%
          ggplot(aes(x=str_trained, y=n, fill=n, color='black')) + geom_col() + 
          geom_text(aes(x=str_trained, y=1.6, label=n)) +
          facet_wrap(~type) +
          #scale_fill_manual(values="#228B22") +
          scale_fill_gradient(low="white", high="#228B22", limits=c(1, 30)) +
          scale_y_continuous(limits = c(0,30)) + labs(y='Number of studies\n') +
          scale_color_manual(values='black') + 
          theme_classic() + theme(legend.position = 'none')

data %>% group_by(end_trained,str_trained, .drop=F) %>% summarise(n = n()) %>% filter(!is.na(end_trained) & !is.na(str_trained)) %>%
          ggplot(aes(x=str_trained, y=end_trained, fill=n, color='black')) + geom_tile(aes(color='black'),size=.75) + 
          geom_text(aes(x=str_trained, y=end_trained, label=n)) +
          scale_fill_gradient(low="white", high="#228B22", limits=c(1, 15), na.value = '#d3d3d3') +
          scale_color_manual(values='black') + 
          theme_classic() + theme(legend.position = 'none')

}
