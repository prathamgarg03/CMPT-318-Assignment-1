# Read and prepare the data

df <- read.csv("Group_Assignment_Dataset.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)

interpolate_na <- function(x) {
  if (is.numeric(x)) {
    return(approx(seq_along(x), x, xout = seq_along(x), method = "linear", rule = 2)$y)
  } else {
    return(x)
  }
}

df_interpolated <- as.data.frame(lapply(df, interpolate_na))

df_interpolated$DateTime <- as.POSIXct(paste(df_interpolated$Date, df_interpolated$Time), 
                                       format="%d/%m/%Y %H:%M:%S")
df_interpolated$week <- strftime(df_interpolated$DateTime, format = "%W")
week4_data <- subset(df_interpolated, week == "04")

week4_data$WeekDay <- weekdays(week4_data$DateTime)
week4_data$Hour <- format(week4_data$DateTime, "%H:%M:%S")

day_start <- "07:30:00"
day_end <- "17:00:00"
night_start <- "22:00:00"
night_end <- "04:00:00"

weekdays_data <- week4_data[week4_data$WeekDay %in% 
                              c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), ]
weekend_data <- week4_data[week4_data$WeekDay %in% 
                             c("Saturday", "Sunday"), ]

get_time_window_data <- function(data, start_time, end_time) {
  if(start_time < end_time) {
    return(data[data$Hour >= start_time & data$Hour <= end_time, ])
  } else {
    return(data[data$Hour >= start_time | data$Hour <= end_time, ])
  }
}

weekday_day <- get_time_window_data(weekdays_data, day_start, day_end)
weekday_night <- get_time_window_data(weekdays_data, night_start, night_end)
weekend_day <- get_time_window_data(weekend_data, day_start, day_end)
weekend_night <- get_time_window_data(weekend_data, night_start, night_end)

prepare_regression_data <- function(data) {
  # Convert time to minutes since midnight
  time_mins <- as.numeric(substr(data$Hour, 1, 2)) * 60 + 
    as.numeric(substr(data$Hour, 4, 5))
  return(data.frame(x=time_mins, y=data$Global_intensity))
}

weekday_day_reg <- prepare_regression_data(weekday_day)
weekday_night_reg <- prepare_regression_data(weekday_night)
weekend_day_reg <- prepare_regression_data(weekend_day)
weekend_night_reg <- prepare_regression_data(weekend_night)

fit_models <- function(data) {
  linear <- lm(y ~ x, data=data)
  poly <- lm(y ~ poly(x, 3, raw=TRUE), data=data)
  return(list(linear=linear, poly=poly))
}

weekday_day_fits <- fit_models(weekday_day_reg)
weekday_night_fits <- fit_models(weekday_night_reg)
weekend_day_fits <- fit_models(weekend_day_reg)
weekend_night_fits <- fit_models(weekend_night_reg)

par(mfrow=c(2,1))

plot(weekday_day_reg$x, weekday_day_reg$y, col="blue", pch=".", 
     main="Global Intensity - Day Hours (Week 4)", 
     xlab="Time (minutes after midnight)", ylab="Global Intensity")
points(weekend_day_reg$x, weekend_day_reg$y, col="red", pch=".")

lines(sort(weekday_day_reg$x), predict(weekday_day_fits$linear, 
                                       newdata=data.frame(x=sort(weekday_day_reg$x))), col="blue", lwd=2)
lines(sort(weekday_day_reg$x), predict(weekday_day_fits$poly, 
                                       newdata=data.frame(x=sort(weekday_day_reg$x))), col="blue", lwd=2, lty=2)
lines(sort(weekend_day_reg$x), predict(weekend_day_fits$linear, 
                                       newdata=data.frame(x=sort(weekend_day_reg$x))), col="red", lwd=2)
lines(sort(weekend_day_reg$x), predict(weekend_day_fits$poly, 
                                       newdata=data.frame(x=sort(weekend_day_reg$x))), col="red", lwd=2, lty=2)

legend("topright", 
       legend=c("Weekday Data", "Weekend Data", "Weekday Linear", "Weekday Polynomial", 
                "Weekend Linear", "Weekend Polynomial"),
       col=c("blue", "red", "blue", "blue", "red", "red"),
       pch=c(".", ".", NA, NA, NA, NA),
       lty=c(NA, NA, 1, 2, 1, 2),
       lwd=c(NA, NA, 2, 2, 2, 2))

plot(weekday_night_reg$x, weekday_night_reg$y, col="blue", pch=".", 
     main="Global Intensity - Night Hours (Week 4)", 
     xlab="Time (minutes after midnight)", ylab="Global Intensity")
points(weekend_night_reg$x, weekend_night_reg$y, col="red", pch=".")

lines(sort(weekday_night_reg$x), predict(weekday_night_fits$linear, 
                                         newdata=data.frame(x=sort(weekday_night_reg$x))), col="blue", lwd=2)
lines(sort(weekday_night_reg$x), predict(weekday_night_fits$poly, 
                                         newdata=data.frame(x=sort(weekday_night_reg$x))), col="blue", lwd=2, lty=2)
lines(sort(weekend_night_reg$x), predict(weekend_night_fits$linear, 
                                         newdata=data.frame(x=sort(weekend_night_reg$x))), col="red", lwd=2)
lines(sort(weekend_night_reg$x), predict(weekend_night_fits$poly, 
                                         newdata=data.frame(x=sort(weekend_night_reg$x))), col="red", lwd=2, lty=2)

legend("topright", 
       legend=c("Weekday Data", "Weekend Data", "Weekday Linear", "Weekday Polynomial", 
                "Weekend Linear", "Weekend Polynomial"),
       col=c("blue", "red", "blue", "blue", "red", "red"),
       pch=c(".", ".", NA, NA, NA, NA),
       lty=c(NA, NA, 1, 2, 1, 2),
       lwd=c(NA, NA, 2, 2, 2, 2))

# Print model summaries
print("Weekday Day Linear Model Summary:")
summary(weekday_day_fits$linear)
print("Weekday Day Polynomial Model Summary:")
summary(weekday_day_fits$poly)
print("Weekend Day Linear Model Summary:")
summary(weekend_day_fits$linear)
print("Weekend Day Polynomial Model Summary:")
summary(weekend_day_fits$poly)