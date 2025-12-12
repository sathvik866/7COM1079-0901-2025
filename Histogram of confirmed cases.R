hist(df_mean$confirmed[df_mean$confirmed < 500], 
     main = "Histogram of Confirmed Cases (Values < 500)",
     xlab = "Number of Confirmed Cases",
     ylab = "Frequency",
     col = "lightgray",
     border = "black")
