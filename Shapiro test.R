shapiro.test(df_mean$confirmed)
boxplot(confirmed ~ group, data = df_mean,
        main = "Distribution of Confirmed Cases by Cluster Group",
        xlab = "Cluster Group (TRUE = Cluster, FALSE = Non-Cluster)",
        ylab = "Number of Confirmed Cases (Count)",
        ylim = c(0, 120),
        col = c("#66b2ff", "#ff9999"),   # blue and soft red
        border = "black")
