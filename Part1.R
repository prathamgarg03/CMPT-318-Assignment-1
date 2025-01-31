getwd()
setwd("D:\\SFU\\CMPT 318\\Assignment 1")

df <- read.csv("Group_Assignment_Dataset.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(df)

sum(is.na(df))

interpolate_na <- function(x) {
  if (is.numeric(x)) {
    return(approx(seq_along(x), x, xout = seq_along(x), method = "linear", rule = 2)$y)
  } else {
    return(x)
  }
}

calculate_z_score <- function(x) {
  if (is.numeric(x)) {
    return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))  # Z-score calculation
  } else {
    return(x)  # Return non-numeric columns unchanged
  }
}

getAnomalies <- function(x) {
  numeric_cols <- sapply(x, is.numeric)
  
  anomalies <- as.data.frame(abs(x[, numeric_cols, drop = FALSE]) > 3)
  
  return(anomalies)
}

# calcuateZScore <- function(data) {
#     cols <- sapply(data, is.numeric)
#     data_copy <- data
#     data_copy[, cols] <- scale(data_copy[, cols, drop = FALSE])
#     return(data_copy)
# }

df_interpolated <- as.data.frame(lapply(df, interpolate_na))

sum(is.na(df_interpolated))

head(df_interpolated)

# df_interpolated$newDate <- as.POSIXlt(df_interpolated$Date, format = "%d/%m/%Y") 
# df_interpolated$week <- strftime(df_interpolated$newDate, format = "%W")
# groupData <- subset(df_interpolated, week == "04")


head(groupData)

# z_scores <- as.data.frame(lapply(df_interpolated, calculate_z_score))
z_scores <- as.data.frame(scale(df_interpolated[, sapply(df_interpolated, is.numeric)], center = TRUE, scale = TRUE))

head(z_scores)
head(groupData)

anomalies <- getAnomalies(z_scores)
head(anomalies)

anomaly_counts_per_feature <- colSums(anomalies)
print(anomaly_counts_per_feature)

percentage_count_per_feature <- (anomaly_counts_per_feature / nrow(z_scores)) * 100
print(percentage_count_per_feature)


anomaly_counts_dataset <- sum(anomalies == TRUE)
print(anomaly_counts_dataset)

percentage_count_dataset <- (anomaly_counts_dataset / (nrow(z_scores) * ncol(z_scores))) * 100
print(percentage_count_dataset)





