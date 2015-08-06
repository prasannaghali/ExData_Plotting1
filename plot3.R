#######################################################################################
#
# @file: plot3.R
# @author: pghali@gmail.com
# @date: 08/06/2015
#
# @function: plot3
# @intent: This function perform an analysis of the variation of household energy usage
# over a two day period. In particular, this function provides a time-series plot of three
# different submeter readings over a user-specified range of dates
# frequency of "Global Active Power".
# @parameter: A character vector containing the range of dates over which the analysis
# is to be performed. The first and second elements of the vector represent the initial
# and final dates over which the analysis is to be performed.
# Dates are represented as "dd/mm/yyyy".
# @return: None.
# @side effects: Create a file "plot3.png" containing the time-series plot of three submeter
# readings over a user-specified range of dates.
#
#######################################################################################
plot3 <- function(date_range=c("1/2/2007", "2/2/2007")) {
  hpc <- preparedata(date_range)
  
  # open PNG graphics device for time-series line plots of three different submeter
  # readings over a user-specified range of dates
  png("plot3.png")
  plot(hpc$Date,
       hpc$Sub_metering_1,
       xaxt="n",
       xlab="",
       ylab="Energy sub metering",
       col="black",
       type="l")
  axis.POSIXct(1,
               at=seq(min(hpc$Date), max(hpc$Date)+(3600*24),by="day"),
               format="%a")
  lines(hpc$Date, hpc$Sub_metering_2, col="red")
  lines(hpc$Date, hpc$Sub_metering_3, col="blue")
  legend("topright",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         lty=c(1,1,1),
         col=c("black", "red", "blue"))
  dev.off()
}

#######################################################################################
#
# @file: plot3.R
# @author: pghali@gmail.com
# @date: 08/06/2015
#
# @function: preparedata
# @intent: This is a helper function that gets and cleans the dataset prior to performing
# an analysis of the variation of household energy usage.
# First, if the zipped file containing data set file(s) is not present in the current
# working directory, the zipped file is downloaded and the data file(s) are extracted
# into the current working directory.
# Second, a subset of lines in the data file are extracted into a data frame.
# Third, the "Date" and "Time" columns are concatenated into a POSIXlt Date/Time format.
# Fourth, the unnecessary "Time" column is removed from the data frame.
# parameter: A character vector containing the range of dates over which the analysis
# is to be performed. The first and second elements of the vector represent the initial
# and final dates over which the analysis is to be performed.
# Dates are represented as "dd/mm/yyyy".
# @return: Data frame containing clean data set that is ready for analysis.
# @side effects: None.
#
#######################################################################################
preparedata <- function(date_range) {
  # if data file doesn't exist, then we may have to either download the zipped file
  # (containing the data file) or just unzip the (existing) zipped file
  datafile_name <- "household_power_consumption.txt"
  if (!file.exists(datafile_name)) {
    # since data file doesn't exist, we may have to download the zipped file ...
    zippedfile_name <- "exdata-data-household_power_consumption.zip"
    if (!file.exists(zippedfile_name)) {
      # download the zipped file using the link in the course Project page ...
      file_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
      setInternet2(use=TRUE) # for Windows 8 - otherwise cannot download from https URL's
      download.file(file_url, zippedfile_name)
    }
    # unzip the file to extract data file ...
    unzip(zippedfile_name)
  }
  
  # first - determine column names and column classes ...
  tmp_df <- read.table(datafile_name,
                       header = TRUE,
                       nrows = 2,
                       sep=";",
                       stringsAsFactors=FALSE,
                       na.strings="?")
  col_classes <- sapply(tmp_df, class)
  col_names <- names(tmp_df)
  
  # second - determine number of lines to skip and number of lines to read ...
  date_time_range <- c(paste(date_range[1], "00:00:00", sep=";"),
                       paste(date_range[2], "23:59:00", sep=";"))
  line_range <- grep(pattern=paste("^(",date_time_range[1],"|",date_time_range[2],")",sep=""),
                     readLines(datafile_name))
  # now, load the required data set as a dataframe ...
  hpc <- read.table(datafile_name,
                    header=FALSE,
                    skip=ifelse(line_range[1]>1,line_range[1]-1,0),
                    nrows=line_range[2]-line_range[1]+1,
                    sep=";",
                    colClasses=col_classes,
                    na.strings="?")
  names(hpc) <- col_names
  
  # combine date and time into POSIXlt format
  hpc$Date <- paste(hpc$Date, hpc$Time)
  # note: date retrieved from data set is in the form "dd/mm/yyyy"
  hpc$Date <- strptime(hpc$Date, format="%d/%m/%Y %H:%M:%S")
  # remove hpc$Time
  return(hpc[,!(names(hpc) %in% c("Time"))]) 
}
