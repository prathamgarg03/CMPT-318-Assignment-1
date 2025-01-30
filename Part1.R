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

df_interpolated <- as.data.frame(lapply(df, interpolate_na))

sum(is.na(df_interpolated))

head(df_interpolated)

df_interpolated$newDate <- as.POSIXlt(df_interpolated$Date, format = "%d/%m/%Y") 
df_interpolated$week <- strftime(df_interpolated$newDate, format = "%W")
groupData <- subset(df_interpolated, week == "04")


head(groupData)


