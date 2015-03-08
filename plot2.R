plot2 <- function()
{
  #Read the UCI data set into a data frame variable and ensure the data set was read.
  #Stop the program if the data set was not read in.
  data <- read.table("household_power_consumption.txt",header=TRUE,sep=";",stringsAsFactors=FALSE)
  if(is.null(data))
  {
    stop("The dataset household_power_consumption.txt is not in the working directory. 
        Ensure the data set is in the working directory and then re-run the program.")
  }
  
  #Subset the data to include only February 1 and 2, 2007 obervations
  data2 <- data[data$Date=="1/2/2007" | data$Date=="2/2/2007",]
  
  #Delete the original data set to clear up memory
  rm(data)
  
  #Review data set for "?'s" to set them to NA
  for (i in seq_along(ncol(data2)))
  {
    data2[[i]][data2[[i]]=="?"] <- NA  
  }

  #Keep only the complete cases
  data2 <- data2[complete.cases(data2),]
  
  #Combine date and time fields for complete time field, and format the resulting DateTime field
  data2$DateTime <- paste(data2$Date, data2$Time)
  data2$DateTime <- strptime(data2$DateTime, "%d/%m/%Y %H:%M:%S")
  
  
  #Open plot device
  png("plot2.png",height=480,width=480)

  #Plot the data 
  plot(data2$DateTime,data2$Global_active_power,type="l",ylab="Global Active Power (kilowatts)",xlab="")
  
  #Close device
  dev.off()
  
  #Print completion message
  wd <- getwd()
  message(paste("The PNG file is saved to", wd))
  
}