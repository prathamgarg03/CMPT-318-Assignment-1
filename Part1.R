data <- read.table("Group_Assignment_Dataset.txt", header = TRUE, sep = "\t")

data$newDate <- as.POSIXlt(data$Date, format = "%d/%m/%Y") 
data$week <- strftime(data$newDate, format = "%W")
groupData <- subset(data, week == "04")



