wilcox.test(confirmed ~ group, data = df_mean)
summary_table <- aggregate(confirmed ~ group, data = df_mean,
                           FUN = function(x) c(mean = mean(x), median = median(x)))
