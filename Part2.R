df <- read.csv("Group_Assignment_Dataset.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)

interpolate_na <- function(x) {
  if (is.numeric(x)) {
    return(approx(seq_along(x), x, xout = seq_along(x), method = "linear", rule = 2)$y)
  } else {
    return(x)
  }
}

df_interpolated <- as.data.frame(lapply(df, interpolate_na))

df_interpolated$newDate <- as.POSIXlt(df_interpolated$Date, format = "%d/%m/%Y")
df_interpolated$week <- strftime(df_interpolated$newDate, format = "%W")
week4_data <- subset(df_interpolated, week == "04")

numeric_cols <- sapply(week4_data, is.numeric)
correlation_data <- week4_data[, numeric_cols]

correlation_matrix <- cor(correlation_data, method = "pearson")