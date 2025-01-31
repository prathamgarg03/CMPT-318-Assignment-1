# getwd()
# setwd("D:\\SFU\\CMPT 318\\Assignment 1")

df <- read.csv("Group_Assignment_Dataset.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)
interpolate_na <- function(x) {
  if (is.numeric(x)) {
    return(approx(seq_along(x), x, xout = seq_along(x), method = "linear", rule = 2)$y)
  } else {
    return(x)
  }
}

getAnomalies <- function(x) {
  numeric_cols <- sapply(x, is.numeric)
  anomalies <- as.data.frame(abs(x[, numeric_cols, drop = FALSE]) > 3)
  return(anomalies)
}

df_interpolated <- as.data.frame(lapply(df, interpolate_na))

z_scores <- as.data.frame(scale(df_interpolated[, sapply(df_interpolated, is.numeric)], center = TRUE, scale = TRUE))

anomalies <- getAnomalies(z_scores)

anomaly_counts_per_feature <- colSums(anomalies)
# print(anomaly_counts_per_feature)

percentage_count_per_feature <- (anomaly_counts_per_feature / nrow(z_scores)) * 100
# print(percentage_count_per_feature)

anomaly_counts_dataset <- sum(anomalies == TRUE)
# print(anomaly_counts_dataset)

percentage_count_dataset <- (anomaly_counts_dataset / (nrow(z_scores) * ncol(z_scores))) * 100
# print(percentage_count_dataset)





