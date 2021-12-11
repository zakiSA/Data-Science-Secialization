library(lubridate)
library(dplyr)

## Download dataset, unzip and read into RStudio using read.table
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl,destfile = "./electric_data",method="curl")
my_electricdata <- unzip("./electric_data")
my_data <- read.table(my_electricdata,sep = ";",header = TRUE,stringsAsFactors = FALSE)
##Convert Date column to class Date using dmy() from lubridate
my_data$Date <- dmy(my_data$Date)
##To convert my_data into a tibble using tbl_df()
my_table <- tbl_df(my_data)
## To select only rows where Date is 2007-02-01 and 2007-02-02 using filter() from dplyr
my_rows <-filter(my_table, Date=="2007-02-01"|Date=="2007-02-02")
## Convert my_rows into data frame
my_df <- as.data.frame(my_rows)
##Make new variable combining date and time using paste()
my_df$newdatetime <- paste(my_df$Date,my_df$Time)
##Convert newdatetime to datetime class using strptime() from lubridate
my_df$newdatetime <- strptime(my_df$newdatetime,"%Y-%m-%d %H:%M:%S")
##Select desired rows from the data frame
my_df <- select(my_df,Date:Sub_metering_3,newdatetime)
##Create new column weekday by extracting weekday value from newdatetime
my_df$weekday <- wday(my_df$newdatetime,label=TRUE)

##Plotting graph
## Plot weekdays but without any data 
plot(my_df$weekday)
##Plot newdatetime vs Submetering on the same graph
plot(my_df$newdatetime,my_df$Sub_metering_1,type="l",xlab=" ",ylab = "Energy sub metering")
lines(my_df$newdatetime,my_df$Sub_metering_2,col="red")
lines(my_df$newdatetime,my_df$Sub_metering_3,col="blue")
##Add legend to top right corner of graph
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("black","red","blue"))

##Plot graph to PNG file
dev.copy(png,file="plot3.png",width=480,height=480)
dev.off()
