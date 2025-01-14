plot4 <- function() {
        link <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        plotnm <- "plot4.png"
        initial <- "C:/Users/berna/Documents/R"
        subdir <- "Exploratory_Data_Analysis"
        textf <- "household_power_consumption.txt"
        label <- "Energy sub metering"
        lab_ext <- "(kilowatt)"
        label2 <- "datetime"
        
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
            as.numeric(as.character(extract_table$Global_active_power)) ## Convert active kilowatts field to numeric
        extract_table$Voltage <-
                as.numeric(as.character(extract_table$Voltage)) ## Convert voltage field to numeric              
        extract_table$Sub_metering_1 <-
                as.numeric(as.character(extract_table$Sub_metering_1)) ## Convert Submetering fields to numeric
        extract_table$Sub_metering_2 <-
                as.numeric(as.character(extract_table$Sub_metering_2))  
        extract_table$Sub_metering_3 <-
                as.numeric(as.character(extract_table$Sub_metering_3))
        extract_table$Global_reactive_power <-
                as.numeric(as.character(extract_table$Global_reactive_power)) ## Convert reactive kilowatts field to numeric
        
        ## Post extracted data for comparison with original data
        setwd(paste(initial, subdir, sep = "/"))
        con2 <- file(textf, open = "w")
        write.table(extract_table, con2, quote = FALSE, sep = ";", row.names = FALSE)
        close(con2)
        
        ## Change date format for use by R functions
        extract_table <- mutate(extract_table, Date_yyyy_mm_dd = dmy(as.character(Date))) %>% 
                select(Date, Date_yyyy_mm_dd, everything())             ## Add column in date format and
        extract_table <- select(extract_table, -1)                      ## remove original date field

        label1 <- names(extract_table)[3]                                ## Use global active power in titel
        label1 <- gsub("_", " ", label1)                                  ## Replace underscores with spaces in label1
        label1 <- str_to_title(label1)                                    ## Convert to title case
        label1 <- paste(label1,lab_ext, sep = " ")                        ## Add kilowatts to x-label
        
        ## Plot lines for Sub metering 1, 2 & 3, adding a legend
        dev <- png(plotnm)
        par(mfrow = c(2, 2))
        with(extract_table, {
               plot(as_datetime(
                        paste(as.character(Date_yyyy_mm_dd), as.character(Time), sep = " ")),
                        Global_active_power, type = "l", xlab = "", ylab = label1)
                
                plot(as_datetime(
                        paste(as.character(Date_yyyy_mm_dd), as.character(Time), sep = " ")),
                     Voltage, type = "l", xlab = label2)
                
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
               
               plot(as_datetime(
                       paste(as.character(Date_yyyy_mm_dd), as.character(Time), sep = " ")),
                    Global_reactive_power, type = "l", xlab = label2)
        })
        dev.off()
        
}