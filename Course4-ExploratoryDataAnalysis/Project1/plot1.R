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
##Convert my_rows back into data frame using as.data.frame()    
        my_df <- as.data.frame(my_rows)

##Plot histogram with Global Acitive power column
##Convert Global_active _power to numeric
        my_df$Global_active_power <-as.numeric(my_df$Global_active_power)
        hist(my_df$Global_active_power,col="red",main = "Global Active Power",
             xlab ="Global Active Power (kilowatts)")
##Plot graph to PNG file
       dev.copy(png,file="plot1.png",width=480,height=480)
       dev.off()
       

    
        
        