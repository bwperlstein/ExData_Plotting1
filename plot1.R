plot1 <- function() {
        link <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        plotnm <- "plot1.png"
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
        
        title <- names(extract_table)[3]                                ## Use global active power in titel
        title <- gsub("_", " ", title)                                  ## Replace underscores with spaces in title
        title <- str_to_title(title)                                    ## Convert to title case
        label <- paste(title,lab_ext, sep = " ")                        ## Add kilowatts to x-label
                
        ## Post extracted data for comparison with original data
        setwd(paste(initial, subdir, sep = "/"))
        con2 <- file(textf, open = "w")
        write.table(extract_table, con2, quote = FALSE, sep = ";", row.names = FALSE)
        close(con2)
        
        ## Plot histogram of global active power to .png file
        dev <- png(plotnm)
        hist(extract_table$Global_active_power, xlab = label, main =  title, col = "red")
        dev.off()
        
}