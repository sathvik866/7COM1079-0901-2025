# 7️⃣ Calculate and Display Mean / Median Values
mean_cost <- mean(df$cost_num, na.rm = TRUE)
median_cost <- median(df$cost_num, na.rm = TRUE)
mean_rating <- mean(df$rating_num, na.rm = TRUE)
median_rating <- median(df$rating_num, na.rm = TRUE)
mean_votes <- mean(df$votes, na.rm = TRUE)
median_votes <- median(df$votes, na.rm = TRUE)
cat("\n--- Calculated Averages ---\n")
cat("Average Cost for Two (Mean):", mean_cost, "\n")
cat("Median Cost for Two:", median_cost, "\n")
cat("Average Rating (Mean):", mean_rating, "\n")
cat("Median Rating:", median_rating, "\n")
cat("Average Votes (Mean):", mean_votes, "\n")
cat("Median Votes:", median_votes, "\n")
# 8️⃣ Save a Histogram of Votes
cat("\n?? Creating Histogram...\n")
png("votes_histogram.png", width = 800, height = 600)
hist(df$votes, breaks = 30, col = "skyblue",
main = "Histogram of Restaurant Votes",
xlab = "Number of Votes", ylab = "Frequency")
dev.off()
cat("✅ Histogram saved as 'votes_histogram.png' in your working directory.\n")
# 9️⃣ Show Top 5 Cuisines (if available)
if ("cuisines" %in% names(df)) {
cat("\n--- Top 5 Cuisines ---\n")
print(sort(table(df$cuisines), decreasing = TRUE)[1:5])
}
# ?? Done
cat("\n✅ Analysis Completed Successfully!\n")
cat("Check the file 'votes_histogram.png' in your folder for the graph.\n")
# =============================================================
q()
data_df <- read.csv("student_depression_dataset.csv")
setwd("C:/Users/parth/OneDrive/Desktop/Case")
setwd("C:/Users/parth/OneDrive/Desktop/Airlines")
setwd("C:/Users/parth/OneDrive/Desktop/Case")
# Read the dataset
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Keep only the variables we need for this RQ
# confirmed = number of cases, latitude = geographic position
df_corr <- df[, c("confirmed", "latitude")]
# Make sure they are numeric (any '-' etc. will become NA)
df_corr$confirmed <- as.numeric(df_corr$confirmed)
df_corr$latitude  <- as.numeric(df_corr$latitude)
# Remove rows with missing values in either variable
df_corr <- na.omit(df_corr)
# Histogram of confirmed cases
hist(df_corr$confirmed,
main = "Histogram of Confirmed Cases",
xlab = "Number of Confirmed Cases")
# Histogram of latitude
hist(df_corr$latitude,
main = "Histogram of Latitude",
xlab = "Latitude")
# Normality test for confirmed
shapiro.test(df_corr$confirmed)
# Normality test for latitude
shapiro.test(df_corr$latitude)
# Scatterplot of latitude vs confirmed cases
plot(df_corr$latitude, df_corr$confirmed,
main = "Scatterplot of Latitude vs Confirmed Cases",
xlab = "Latitude",
ylab = "Number of Confirmed Cases",
pch = 19)
# Load the dataset
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Select only the variables needed
df_corr <- df[, c("confirmed", "latitude")]
# Convert to numeric and remove missing values
df_corr$confirmed <- as.numeric(df_corr$confirmed)
df_corr$latitude <- as.numeric(df_corr$latitude)
df_corr <- na.omit(df_corr)
# Histogram for confirmed cases with normal curve overlay
hist(df_corr$confirmed,
main = "Histogram of Confirmed Cases",
xlab = "Number of Confirmed Cases",
ylab = "Frequency")
mean_confirmed <- mean(df_corr$confirmed)
sd_confirmed <- sd(df_corr$confirmed)
xfit <- seq(min(df_corr$confirmed), max(df_corr$confirmed), length = 40)
yfit <- dnorm(xfit, mean_confirmed, sd_confirmed)
yfit <- yfit * diff(hist(df_corr$confirmed, plot = FALSE)$mids[1:2]) * length(df_corr$confirmed)
lines(xfit, yfit, col = "red", lwd = 2)
# Histogram for latitude with normal curve overlay
hist(df_corr$latitude,
main = "Histogram of Latitude",
xlab = "Latitude (Degrees)",
ylab = "Frequency")
mean_lat <- mean(df_corr$latitude)
sd_lat <- sd(df_corr$latitude)
xfit2 <- seq(min(df_corr$latitude), max(df_corr$latitude), length = 40)
yfit2 <- dnorm(xfit2, mean_lat, sd_lat)
yfit2 <- yfit2 * diff(hist(df_corr$latitude, plot = FALSE)$mids[1:2]) * length(df_corr$latitude)
lines(xfit2, yfit2, col = "blue", lwd = 2)
# Scatterplot to show the relationship
plot(df_corr$latitude, df_corr$confirmed,
main = "Latitude vs Confirmed Cases",
xlab = "Latitude (Degrees)",
ylab = "Number of Confirmed Cases",
pch = 19)
# Add a simple regression line
model <- lm(confirmed ~ latitude, data = df_corr)
abline(model, col = "red", lwd = 2)
# Table showing mean, SD, and Shapiro p-values
summary_table <- data.frame(
Variable = c("Confirmed", "Latitude"),
Mean = c(mean(df_corr$confirmed), mean(df_corr$latitude)),
SD = c(sd(df_corr$confirmed), sd(df_corr$latitude)),
Shapiro_p = c(shapiro.test(df_corr$confirmed)$p.value,
shapiro.test(df_corr$latitude)$p.value)
)
summary_table
# Pearson correlation (normal distribution)
cor.test(df_corr$latitude, df_corr$confirmed, method = "pearson")
# Spearman correlation (not normal)
cor.test(df_corr$latitude, df_corr$confirmed, method = "spearman")
# Load the dataset
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Choose the two provinces you want to compare
prov1 <- "Seoul"
prov2 <- "Daegu"   # change this to another province name if needed
# Keep only rows from these two provinces
df_prop <- subset(df, province %in% c(prov1, prov2))
# Keep only the variables we need
df_prop <- df_prop[, c("province", "group")]
# Make province and group factors
df_prop$province <- factor(df_prop$province, levels = c(prov1, prov2))
df_prop$group <- factor(df_prop$group, levels = c(FALSE, TRUE),
labels = c("Non_cluster", "Cluster"))
# Create contingency table: province by cluster status
tab <- table(df_prop$province, df_prop$group)
tab    # this is your contingency table for the report
# Convert table to row-wise proportions
tab_prop <- prop.table(tab, margin = 1)
# Stacked bar chart showing proportions
barplot(tab_prop,
main = "Proportion of Cluster vs Non-Cluster Cases by Province",
xlab = "Province",
ylab = "Proportion of Cases",
legend = TRUE)
# Chi-square test
chi_result <- chisq.test(tab)
# Check expected cell counts
chi_result$expected
# Print full chi-square result
chi_result
fisher.test(tab)
# Load data
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Select two provinces only
prov1 <- "Seoul"
prov2 <- "Daegu"
df_prop <- subset(df, province %in% c(prov1, prov2))
# Keep only province and group
df_prop <- df_prop[, c("province", "group")]
# Convert to factors
df_prop$province <- factor(df_prop$province)
df_prop$group <- factor(df_prop$group)
# Contingency table of province by cluster status
tab <- table(df_prop$province, df_prop$group)
tab
# Proportion stacked bar chart
tab_prop <- prop.table(tab, margin = 1)
barplot(tab_prop,
main = "Proportion of Cluster vs Non-Cluster Cases by Province",
xlab = "Province",
ylab = "Proportion",
legend = TRUE)
# Chi-square test
chisq.test(tab)
# Load the dataset
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Keep only confirmed and group
df_mean <- df[, c("confirmed", "group")]
# Convert confirmed to numeric and remove missing
df_mean$confirmed <- as.numeric(df_mean$confirmed)
df_mean <- na.omit(df_mean)
# Convert group into factor
df_mean$group <- factor(df_mean$group)
# Histogram of confirmed cases
hist(df_mean$confirmed,
main = "Histogram of Confirmed Cases",
xlab = "Number of Confirmed Cases",
ylab = "Frequency")
# Shapiro test
shapiro.test(df_mean$confirmed)
# Load the dataset
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Keep only confirmed and group
df_mean <- df[, c("confirmed", "group")]
# Convert confirmed to numeric and remove missing
df_mean$confirmed <- as.numeric(df_mean$confirmed)
df_mean <- na.omit(df_mean)
# Convert group into factor
df_mean$group <- factor(df_mean$group)
# Histogram of confirmed cases
hist(df_mean$confirmed,
main = "Histogram of Confirmed Cases",
xlab = "Number of Confirmed Cases",
ylab = "Frequency")
# Shapiro test
shapiro.test(df_mean$confirmed)
boxplot(confirmed ~ group, data = df_mean,
main = "Confirmed Cases by Cluster Group",
xlab = "Cluster Group (TRUE/FALSE)",
ylab = "Number of Confirmed Cases")
wilcox.test(confirmed ~ group, data = df_mean)
# Load the dataset
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Keep only confirmed and group
df_mean <- df[, c("confirmed", "group")]
# Convert confirmed to numeric and remove missing
df_mean$confirmed <- as.numeric(df_mean$confirmed)
df_mean <- na.omit(df_mean)
# Convert group into factor
df_mean$group <- factor(df_mean$group)
# Histogram of confirmed cases
hist(df_mean$confirmed,
main = "Histogram of Confirmed Cases",
xlab = "Number of Confirmed Cases",
ylab = "Frequency")
# Shapiro test
shapiro.test(df_mean$confirmed)
boxplot(confirmed ~ group, data = df_mean,
main = "Confirmed Cases by Cluster Group",
xlab = "Cluster Group (TRUE/FALSE)",
ylab = "Number of Confirmed Cases",
ylim = c(0, 500))
wilcox.test(confirmed ~ group, data = df_mean)
# Load the dataset
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Keep only confirmed and group
df_mean <- df[, c("confirmed", "group")]
# Convert confirmed to numeric and remove missing
df_mean$confirmed <- as.numeric(df_mean$confirmed)
df_mean <- na.omit(df_mean)
# Convert group into factor
df_mean$group <- factor(df_mean$group)
# Histogram of confirmed cases
hist(df_mean$confirmed,
main = "Histogram of Confirmed Cases",
xlab = "Number of Confirmed Cases",
ylab = "Frequency")
# Shapiro test
shapiro.test(df_mean$confirmed)
boxplot(confirmed ~ group, data = df_mean,
main = "Confirmed Cases by Cluster Group",
xlab = "Cluster Group (TRUE/FALSE)",
ylab = "Number of Confirmed Cases",
ylim = c(0, 120))
wilcox.test(confirmed ~ group, data = df_mean)
# Load the dataset
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Keep only confirmed and group
df_mean <- df[, c("confirmed", "group")]
# Convert confirmed to numeric and remove missing
df_mean$confirmed <- as.numeric(df_mean$confirmed)
df_mean <- na.omit(df_mean)
# Convert group into factor
df_mean$group <- factor(df_mean$group)
# Histogram of confirmed cases
hist(df_mean$confirmed,
main = "Histogram of Confirmed Cases",
xlab = "Number of Confirmed Cases",
ylab = "Frequency")
# Shapiro test
shapiro.test(df_mean$confirmed)
boxplot(confirmed ~ group, data = df_mean,
main = "Comparison of Mean Confirmed Cases by Cluster Group",
xlab = "Cluster Group (TRUE = Cluster, FALSE = Non-Cluster)",
ylab = "Number of Confirmed Cases (Count)",
ylim = c(0,120),
notch = TRUE,
col = c("lightgray", "darkgray"))
wilcox.test(confirmed ~ group, data = df_mean)
# Load the dataset
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Keep only confirmed and group
df_mean <- df[, c("confirmed", "group")]
# Convert confirmed to numeric and remove missing
df_mean$confirmed <- as.numeric(df_mean$confirmed)
df_mean <- na.omit(df_mean)
# Convert group into factor
df_mean$group <- factor(df_mean$group)
# Histogram of confirmed cases
hist(df_mean$confirmed,
main = "Histogram of Confirmed Cases",
xlab = "Number of Confirmed Cases",
ylab = "Frequency")
# Shapiro test
shapiro.test(df_mean$confirmed)
boxplot(confirmed ~ group, data = df_mean,
main = "Distribution of Confirmed Cases by Cluster Group",
xlab = "Cluster Group (TRUE = Cluster, FALSE = Non-Cluster)",
ylab = "Number of Confirmed Cases (Count)",
ylim = c(0, 120),
border = "black",
col = c("white", "white"))
)
# Load the dataset
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Keep only confirmed and group
df_mean <- df[, c("confirmed", "group")]
# Convert confirmed to numeric and remove missing
df_mean$confirmed <- as.numeric(df_mean$confirmed)
df_mean <- na.omit(df_mean)
# Convert group into factor
df_mean$group <- factor(df_mean$group)
# Histogram of confirmed cases
hist(df_mean$confirmed,
main = "Histogram of Confirmed Cases",
xlab = "Number of Confirmed Cases",
ylab = "Frequency")
# Shapiro test
shapiro.test(df_mean$confirmed)
boxplot(confirmed ~ group, data = df_mean,
main = "Distribution of Confirmed Cases by Cluster Group",
xlab = "Cluster Group (TRUE = Cluster, FALSE = Non-Cluster)",
ylab = "Number of Confirmed Cases (Count)",
ylim = c(0, 120),
col = c("#66b2ff", "#ff9999"),   # blue and soft red
border = "black")
wilcox.test(confirmed ~ group, data = df_mean)
# Load the dataset
df <- read.csv("Case.csv", stringsAsFactors = FALSE)
# Keep only confirmed and group
df_mean <- df[, c("confirmed", "group")]
# Convert confirmed to numeric and remove missing
df_mean$confirmed <- as.numeric(df_mean$confirmed)
df_mean <- na.omit(df_mean)
