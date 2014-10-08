plot2 <- function(fn = "plot2.png") 
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
  #plot the graph
  
  plot(Global_active_power,type = "l", ylab = "Global Active Power (kilowatts)", xlab = "", xaxt = 'n')
  ticks = c(1, length(data$V1[data$V1=="1/2/2007"])+1, length(data$V1)+1);
  lab = c("Thu", "Fri","Sat")
  axis(side = 1, at = ticks, labels = lab)
  dev.off()
}
