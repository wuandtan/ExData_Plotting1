plot1 <- function(fn = "plot1.png") 
  #this function is to generate the histogram of "Global active power" on 1/2/2007 and 2/2/2007
  #input: the file name of the image (with .png extension)
  #output: the image.
{

  #read the file
  t=read.table("household_power_consumption.txt", sep = ";")
  #find the corresponding dates
  data <- t[t$V1=="1/2/2007" | t$V1=="2/2/2007" ,]
  #get the column
  Global_active_power <- data$V3
  #from factor to numeric
  Global_active_power <- as.numeric(levels(Global_active_power))[Global_active_power]
  png(file = fn)
  #plot the histogram
  hist(Global_active_power, main = "Global Active Power", xlab = "Global active power (kilowatts)", ylab = "Frequency", col= "red")
  dev.off()
}
