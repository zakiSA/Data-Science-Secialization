#Course4 - Exploratory Data Analysis Course Project 2

library(curl)
library(dplyr)
library(ggplot2)



fileUrl <-("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip")
download.file(fileUrl,destfile = "./epa_data",method="curl")
my_epadata <- unzip("./epa_data")
##Read NEI data using readRDS()
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


#For Plot1.png

##Create NEI tbl_df usiing tbl_df() from dplyr
#NEI_table <- tbl_df(NEI) # Deprecated use tibble
NEI_table <- tibble(NEI)
##Group NEI data by year
by_year <- group_by(NEI_table,year)
##Summarize Emission data
emission_sum <- summarize(by_year,sum=sum(Emissions))
##Create data frame
em_df <- as.data.frame(emission_sum)
colnames(em_df) <- c("Year","Total.Emissions")
##Plot Emission data using base plotting
with(em_df,plot(Year,Total.Emissions))
points(em_df$Year,em_df$Total.Emissions,pch=19)
title(main="US Total PM2.5 Emissions 1999 to 2008")
lines(em_df$Year,em_df$Total.Emissions)

##Plot graph to PNG file
dev.copy(png,file="plot1.png",width=480,height=480)
dev.off()


#For Plot2.png
##SUbset NEI for data from Baltimore using fips code 24510
my_rows <- subset(NEI,fips=="24510")
##Create tbl_df using tbl_df() from dplyr
my_table <- tbl_df(my_rows)
##Group data by year
ba_data <- group_by(my_table,year)
##Calculate sum of Emissions
ba_sum <- summarize(ba_data,sum=sum(Emissions))
##Create data frame
ba_df <- as.data.frame(ba_sum)
colnames(ba_df) <- c("Year","Total.Emissions")
##Plot Baltimore Emission data using base plotting
with(ba_df,plot(Year,Total.Emissions))
points(ba_df$Year,ba_df$Total.Emissions,pch=19)
title(main="Baltimore City:Total PM2.5 Emissions 1999 to 2008")
lines(ba_df$Year,ba_df$Total.Emissions)

##Plot graph to PNG file
dev.copy(png,file="plot2.png",width=480,height=480)
dev.off()


#Plot 3

my_scctbl <- tbl_df(SCC)
## Convertinr SCC column into charachter
my_scctbl$SCC <- as.character(my_scctbl$SCC)
##Selecting columns from SCC
my_cols <- select(my_scctbl,SCC:Data.Category)
##Merging my_cols and my_tbl based on SCC
mrg_tbl <- merge(my_cols,my_table,by="SCC")
##group data by Data.Ctegory and year
my_data <- group_by(mrg_tbl,Data.Category,year)
##Calculate the sum of Emissions for each group
my_sum <- summarize(my_data,sum=sum(Emissions))
##Create data frame
my_df <- as.data.frame(my_sum)
my_df <- filter(my_df,Data.Category=="Nonpoint"|
                    Data.Category=="Nonroad"|Data.Category=="Onroad"|
                    Data.Category=="Point")
##Plot data using ggplot2 function qplot
qplot(year,sum,data=my_df,color=Data.Category,geom =c("point","path"),
      main="Baltimore 1999-2008:PM2.5 Emissions from different types of Sources")


##Plot graph to PNG file
dev.copy(png,file="plot3.png",width=480,height=480)
dev.off()


# Plot 4

##Create NEI tbl_df
my_table <- tbl_df(NEI)
##Creating SCC tbl_df
my_scctbl <- tbl_df(SCC)
##Selecting columns from my_scctbl
my_scctbl <- select(my_scctbl,SCC:Short.Name)
##Merging my_cols and my_tbl based on SCC
mrg_tbl <- merge(my_scctbl,my_table,by="SCC")
##Select rows of my_scctbl where the source is coal based
coal_rows <- mrg_tbl[grep("Comb(.*?)Coal",mrg_tbl$Short.Name), ]
my_group <- group_by(coal_rows,year)
##Calculate total emisiions for each group
my_sum <- summarize(my_group,sum=sum(Emissions))
##Create data frame
my_df <- as.data.frame(my_sum)
##Plot Coal data using ggplot2 ggplot function
qplot(year,sum,data=my_df,geom =c("point","path"),
      main="US PM2.5 Emissions: Coal Combustion Emissions 1999-2008")

##Plot graph to PNG file
dev.copy(png,file="plot4.png",width=480,height=480)
dev.off()


# plot 5

##Creating SCC tbl_df
my_scctbl <- tbl_df(SCC)
## Convertinr SCC column into charachter
#my_scctbl$SCC <- as.character(my_scctbl$SCC)
##Selecting columns from SCC
my_cols <- select(my_scctbl,SCC:Short.Name)
##Merging my_cols and my_tbl based on SCC
mrg_tbl <- merge(my_cols,my_table,by="SCC")
##Select rows that show emissions from Motor Vehicle Sources
motorvehicle_rows <- mrg_tbl[grep("Highway Veh",mrg_tbl$Short.Name), ]
##Group by year
my_data <- group_by(motorvehicle_rows,year)
##Sum the Emissions by year
my_sum <- summarize(my_data,sum=sum(Emissions))
##Create data frame
my_df <- as.data.frame(my_sum)
##Plot Motor vehicle emission data for Baltimore using qplot
qplot(year,sum,data=my_df,geom =c("point","path"),
      main="Baltimore PM2.5 Emissions 1999-2008: Motor Vehicles")

##Plot graph to PNG file
dev.copy(png,file="plot5.png",width=480,height=480)
dev.off()


# Plot 6

##Creating SCC tbl_df
my_scctbl <- tbl_df(SCC)
##Selecting columns from SCC
my_cols <- select(my_scctbl,SCC:Short.Name)
##Merging my_cols and my_table based by SCC
mrg_tbl <- merge(my_cols,my_table,by="SCC")
##Select rows for Baltimore City and LA county
my_rows <- subset(mrg_tbl,fips=="24510"|fips=="06037")
##Select rows that show emissions from Motor Vehicle Sources
motorvehicle_rows <- my_rows[grep("Highway Veh",my_rows$Short.Name), ]
##Group data based on fips and year
my_group <- group_by(motorvehicle_rows,fips,year)
##Calculate sum of emissions for each group
my_sum <- summarize(my_group,sum=sum(Emissions))
##Create data frame
my_df <- as.data.frame(my_sum)
##Plot data using ggplot2 function qplot
qplot(year,sum,data=my_df,color=fips,geom =c("point","path"),
      main="Baltimore & LA PM2.5 Emissions 1999-2008: Motor Vehicles")


##Plot graph to PNG file
dev.copy(png,file="plot6.png",width=480,height=480)
dev.off()