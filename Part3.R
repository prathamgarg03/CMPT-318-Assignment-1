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
week4_data$day_type <- ifelse(week4_data$WeekDay %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
week4_data$time_period <- ifelse(
  (week4_data$Hour >= "07:30:00" & week4_data$Hour <= "17:00:00"),
  "Day",
  ifelse(
    (week4_data$Hour >= "18:00:00" | week4_data$Hour <= "24:00:00"),
    "Night",
    "Other"
  )
)

week4_weekday <- subset(week4_data, day_type == "Weekday")
week4_weekend <- subset(week4_data, day_type == "Weekend")

weekday_day_data <- subset(week4_weekday, time_period == "Day")
weekday_night_data <- subset(week4_weekday, time_period == "Night")
weekend_day_data <- subset(week4_weekend, time_period == "Day")
weekend_night_data <- subset(week4_weekend, time_period == "Night")

calculate_average_intensity <- function(data) {
  avg_data <- aggregate(Global_intensity ~ Hour, data, mean)
  avg_data$minutes <- as.numeric(substr(avg_data$Hour, 1, 2)) * 60 + 
    as.numeric(substr(avg_data$Hour, 4, 5))
  return(avg_data)
}

weekday_day_avg <- calculate_average_intensity(weekday_day_data)
weekday_night_avg <- calculate_average_intensity(weekday_night_data)
weekend_day_avg <- calculate_average_intensity(weekend_day_data)
weekend_night_avg <- calculate_average_intensity(weekend_night_data)

fit_linear_wd_day <- lm(Global_intensity ~ minutes, data=weekday_day_avg)
fit_linear_wd_night <- lm(Global_intensity ~ minutes, data=weekday_night_avg)
fit_linear_we_day <- lm(Global_intensity ~ minutes, data=weekend_day_avg)
fit_linear_we_night <- lm(Global_intensity ~ minutes, data=weekend_night_avg)

fit_poly_wd_day <- lm(Global_intensity ~ poly(minutes, 3, raw=TRUE), data=weekday_day_avg)
fit_poly_wd_night <- lm(Global_intensity ~ poly(minutes, 3, raw=TRUE), data=weekday_night_avg)
fit_poly_we_day <- lm(Global_intensity ~ poly(minutes, 3, raw=TRUE), data=weekend_day_avg)
fit_poly_we_night <- lm(Global_intensity ~ poly(minutes, 3, raw=TRUE), data=weekend_night_avg)

png("part3.png", width = 800, height = 1200)
par(mfrow=c(2,1), mar=c(5, 4, 4, 2) + 0.5)

plot(weekday_day_avg$minutes, weekday_day_avg$Global_intensity, 
     type="n", col="blue", pch=16,
     main="Linear Regression - Global Intensity (Week 4)",
     xlab="Time (minutes after midnight)", 
     ylab="Average Global Intensity")

lines(sort(weekday_day_avg$minutes), predict(fit_linear_wd_day, 
                                             newdata=data.frame(minutes=sort(weekday_day_avg$minutes))), col="blue", lwd=2)
lines(sort(weekday_night_avg$minutes), predict(fit_linear_wd_night, 
                                               newdata=data.frame(minutes=sort(weekday_night_avg$minutes))), col="red", lwd=2)
lines(sort(weekend_day_avg$minutes), predict(fit_linear_we_day, 
                                             newdata=data.frame(minutes=sort(weekend_day_avg$minutes))), col="green", lwd=2)
lines(sort(weekend_night_avg$minutes), predict(fit_linear_we_night, 
                                               newdata=data.frame(minutes=sort(weekend_night_avg$minutes))), col="purple", lwd=2)

legend("topright", 
       legend=c("Weekday Day", "Weekday Night", "Weekend Day", "Weekend Night"),
       col=c("blue", "red", "green", "purple"),
       lwd=2)

plot(weekday_day_avg$minutes, weekday_day_avg$Global_intensity, 
     type="n", col="blue", pch=16,
     main="Polynomial Regression - Global Intensity (Week 4)",
     xlab="Time (minutes after midnight)", 
     ylab="Average Global Intensity")

lines(sort(weekday_day_avg$minutes), predict(fit_poly_wd_day, 
                                             newdata=data.frame(minutes=sort(weekday_day_avg$minutes))), col="blue", lwd=2)
lines(sort(weekday_night_avg$minutes), predict(fit_poly_wd_night, 
                                               newdata=data.frame(minutes=sort(weekday_night_avg$minutes))), col="red", lwd=2)
lines(sort(weekend_day_avg$minutes), predict(fit_poly_we_day, 
                                             newdata=data.frame(minutes=sort(weekend_day_avg$minutes))), col="green", lwd=2)
lines(sort(weekend_night_avg$minutes), predict(fit_poly_we_night, 
                                               newdata=data.frame(minutes=sort(weekend_night_avg$minutes))), col="purple", lwd=2)

legend("topright", 
       legend=c("Weekday Day", "Weekday Night", "Weekend Day", "Weekend Night"),
       col=c("blue", "red", "green", "purple"),
       lwd=2)

dev.off()