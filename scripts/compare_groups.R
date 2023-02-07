compare_groups <- function(data) {
  
  # Test for normality 
  
    p_normality <- c()
    for (c in 3:dim(data)[2]) {
      norm_stats <- shapiro.test(data[,c])
      p_normality <- cbind(p_normality,norm_stats$p.value)
    }
    colnames(p_normality) <- colnames(data)[3:dim(data)[2]]
  
  # Test for equal variance
  
    eq_variance <- c()
    for (c in 3:dim(data)[2]) {
      norm_stats <- var.test(data[1:20,c], data[21:49,c])
      eq_variance <- cbind(eq_variance,norm_stats$p.value)
    }
    colnames(eq_variance) <- colnames(data)[3:dim(data)[2]]
  
  # Statistical comparison between groups with parametric (two-sample T-test) or non-parametric (Mann-Whitney U test) data.
    
    stats <- c()
    for (c in 3:dim(data)[2]) {
        group_means <- data.frame(value=data[,c]) %>% 
                        ungroup() %>% group_by(data$group) %>% 
                        summarize(mean=mean(value,na.rm=T), sd=sd(value,na.rm=T)) %>% t() 
        group_means <- group_means[-1,] %>% as.data.frame() %>% mutate_all(list(~as.numeric(.)))
      
      if (p_normality[,colnames(data)[c]] < 0.05) {
        
        stat <- wilcox.test(data[,c] ~ group, data=data, exact=F)
        
      } else {
        
        if (eq_variance[,colnames(data)[c]] < 0.05) {
          stat <- t.test(data[,c] ~ group, data = data, var.equal=F)
        } else {
          stat <- t.test(data[,c] ~ group, data = data, var.equal=T)
        }
      }
      print(colnames(data)[c])
      
      # Adjust p-values for multiple testing in case of Mb concentrations in type I and II fibers.
      if (colnames(data)[c] %in% c('mb_I','mb_II')) {

        stat$p.value <- p.adjust(stat$p.value, method='bonferroni', n=2)
      }
      
      stats <- rbind(stats,
                     c(colnames(data)[c],stat$method,ifelse(stat$p.value<0.001,'P<0.001',round(stat$p.value,3)),round(group_means[1,],3),round(group_means[2,],3)))
    }
    colnames(stats) <- c('var','method','p-value','mean1','mean2','sd1','sd2')
    
    return(stats)
}