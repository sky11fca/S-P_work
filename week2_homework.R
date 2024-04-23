#I.3)

data <- read.csv("life_expect.csv")

num_bins <- 7

hist(data$male, breaks=seq(min(data$male), max(data$male), length.out=num_bins+1),
     main="Male life expectancy",
     xlab="life expect",
     ylab="Freq",
     col="blue",
     border="black")

hist(data$female, breaks=seq(min(data$female), max(data$female), length.out=num_bins+1),
     main="Female life expectancy",
     xlab="life expect",
     ylab="Freq",
     col="pink",
     border="black")



#III.1)

outliers_mean <- function(sample) {
  mean_val <- mean(sample)
  std_dev <- sd(sample)
  
  outliers <- sample[abs(sample - mean_val) > 2 * std_dev]
  
  return(outliers)
}

#III.2)

outliers_iqr <- function(sample) {
  Q1 <- quantile(sample, 0.25)
  Q3 <- quantile(sample, 0.75)
  
  IQR <- Q3 - Q1
  
  outliers <- sample[sample < (Q1 - 1.5 * IQR) | sample > (Q3 + 1.5 * IQR)]
  
  return(outliers)
}

#III.3)

# Read sample data from file
sample <- scan("sample2.txt")

# Summary of the sample data
cat("Summary of the sample data:\n")
summary(sample)

# Find outliers using mean method
outliers_mean_result <- outliers_mean(sample)
cat("\nOutliers using mean method:\n")
print(outliers_mean_result)

# Find outliers using 1.5 * IQR method
outliers_iqr_result <- outliers_iqr(sample)
cat("\nOutliers using 1.5 * IQR method:\n")
print(outliers_iqr_result)

