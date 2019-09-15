plot2 <- function() {
        link <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        plotnm <- "plot2.png"
        initial <- "C:/Users/berna/Documents/R"
        subdir <- "Exploratory_Data_Analysis"
        textf <- "household_power_consumption.txt"
        lab_ext <- "(kilowatt)"
        
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
                
        ## Post extracted data for comparison with original data
        setwd(paste(initial, subdir, sep = "/"))
        con2 <- file(textf, open = "w")
        write.table(extract_table, con2, quote = FALSE, sep = ";", row.names = FALSE)
        close(con2)
        
        ## Set y-label
        label <- names(extract_table)[3]                                ## Use global active power in y-label
        label <- gsub("_", " ", label)                                  ## Replace underscores with spaces in y-label
        label <- str_to_title(label)                                    ## Convert label to title case
        label <- paste(label,lab_ext, sep = " ")                        ## Add kilowatts to y-label
        
        ## Change date format for use by R functions
        extract_table <- mutate(extract_table, Date_yyyy_mm_dd = dmy(as.character(Date))) %>% 
                select(Date, Date_yyyy_mm_dd, everything())             ## Add column in date format and
        extract_table <- select(extract_table, -1)                      ## remove original date field

        dev <- png(plotnm)
        with(extract_table, plot(as_datetime(
                paste(as.character(Date_yyyy_mm_dd),
                      as.character(Time), sep = " ")),
                            Global_active_power, type = "l", xlab = "", ylab = label))
        dev.off()
        
}