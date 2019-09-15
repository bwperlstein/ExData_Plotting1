plot3 <- function() {
        link <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        plotnm <- "plot3.png"
        initial <- "C:/Users/berna/Documents/R"
        subdir <- "Exploratory_Data_Analysis"
        textf <- "household_power_consumption.txt"
        label <- "Energy sub metering"
        
        library(lubridate)
        library(readr)
        library(stringr)
        library(dplyr)
        
        if(!file.exists(subdir)) {
                dir.create(subdir)
        }
        
        tempf <- tempfile()
        download.file(link, tempf)
        full_table <- read.table(unz(tempf, textf), sep = ";", na.strings = "?", colClasses = "factor", header = TRUE)
        unlink(tempf)

        extract <- with(full_table, dmy(Date) == "2007/02/01" | dmy(Date) == "2007/02/02")
        extract_table <- as_tibble(full_table[extract, ])
        extract_table$Global_active_power <-
            as.numeric(as.character(extract_table$Global_active_power)) ## Convert kilowatts field to numeric
        extract_table$Sub_metering_1 <-
                as.numeric(as.character(extract_table$Sub_metering_1)) ## Convert Submetering fields to numeric
        extract_table$Sub_metering_2 <-
                as.numeric(as.character(extract_table$Sub_metering_2))  
        extract_table$Sub_metering_3 <-
                as.numeric(as.character(extract_table$Sub_metering_3))
                
        ## Post extracted data for comparison with original data
        setwd(paste(initial, subdir, sep = "/"))
        con2 <- file(textf, open = "w")
        write.table(extract_table, con2, quote = FALSE, sep = ";", row.names = FALSE)
        close(con2)
        
        ## Change date format for use by R functions
        extract_table <- mutate(extract_table, Date_yyyy_mm_dd = dmy(as.character(Date))) %>% 
                select(Date, Date_yyyy_mm_dd, everything())             ## Add column in date format and
        extract_table <- select(extract_table, -1)                      ## remove original date field

        ## Plot lines for Sub metering 1, 2 & 3, adding a legend
        dev <- png(plotnm)
        with(extract_table,{
               plot(as_datetime(paste(as.character(Date_yyyy_mm_dd),
                                as.character(Time), sep = " ")), Sub_metering_1,
                                type = "l" , xlab = "", ylab = label, col = "black", lwd = 1)  
               lines(as_datetime(paste(as.character(Date_yyyy_mm_dd),
                                as.character(Time), sep = " ")), Sub_metering_2,
                                col = "red", lwd = 1)
               lines(as_datetime(paste(as.character(Date_yyyy_mm_dd),
                                as.character(Time), sep = " ")), Sub_metering_3,
                                col = "blue", lwd = 1)
               legend("topright", legend = c(names(extract_table)[7], names(extract_table)[8], names(extract_table)[9]), 
                       lty = c(1, 1, 1), col = c("black", "red", "blue"),
                       text.col = c("black", "red", "blue"), border = 'black')
        })
        dev.off()
        
}