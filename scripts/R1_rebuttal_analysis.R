# R1_rebuttal_analysis

# Check correlation Mb and capillarization in elite cyclists
  print('Capillarization check:')
  datas <- read_excel('/Users/stephanvanderzwaard/Documents/Publicaties/2018.Faseb/ps_data.xlsx')
  datas <- datas %>% filter(row_number()<=29) %>% select(Mb,CAF,`C/F`) %>% na.omit()
  cor(datas, method='spearman', use="pairwise.complete.obs")
  print(cor.test(unlist(datas[,1]),unlist(datas[,3]), method='spearman'))
  

# Check ratio of SDH activity in type I and II fibers in elite cyclists vs controls
  print('SDH ratio check:')
  data_add <- read_excel('data/sdh-ratio.xlsx', skip=1)
  colnames(data_add) <- c('ppcode','group','sdh_ratio')
  data_add <- data_add %>%  filter(!is.na(group)) %>% mutate_at(.vars = vars(!contains('code')),.funs = list(~as.numeric(.))) 
  datas    <- left_join(data,data_add, by=c('code'='ppcode','group')) %>% filter(!is.na(sdh_ratio)) %>% select(code,group,sdh_ratio) %>%
              mutate(group = case_when(group==1 ~ 'cyclists',
                                       group==2 ~ 'controls')) %>%
              mutate(group = factor(group, levels = c('cyclists','controls'))) 
  
  #statistics
  shapiro.test(unlist(datas[,3]))
  print(data.frame(value=unlist(datas[,3])) %>% 
        ungroup() %>% group_by(datas$group) %>% 
        summarize(mean=mean(value,na.rm=T), sd=sd(value,na.rm=T)))
  print(wilcox.test(sdh_ratio ~ group, data=datas, exact=F))
  