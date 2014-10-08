plot4 <- function(fn = "plot4.png") 
  #this function is to generate 4 subplots on 1/2/2007 and 2/2/2007.
  #subplots include (a) curve of global active power, (b) voltage, (c) energy submetering (d) global_reactive_power
  #input: the file name of the image (with .png extension)
  #output: the image.
{
  
  #read the file
  t=read.table("household_power_consumption.txt", sep = ";")
  #find the corresponding dates
  data <- t[t$V1=="1/2/2007" | t$V1=="2/2/2007" ,]
  
  
  
  png(file = fn)
  #plot the graph
  par(mfrow=c(2,2))
  
  #subplot 1,1
  Global_active_power <- data$V3
  #from factor to numeric
  Global_active_power <- as.numeric(levels(Global_active_power))[Global_active_power]
  plot(Global_active_power,type = "l", ylab = "Global Active Power (kilowatts)", xlab = "", xaxt = 'n')
  ticks = c(1, length(data$V1[data$V1=="1/2/2007"])+1, length(data$V1)+1);
  lab = c("Thu", "Fri","Sat")
  axis(side = 1, at = ticks, labels = lab)
  
  
  #subplot 1,2
  voltage = as.numeric(levels(data$V5))[data$V5]
  plot(voltage,type = "l",lty=1, ylab = "Voltage",xlab = "datetime", xaxt = 'n')
  lab = c("Thu", "Fri","Sat")
  axis(side = 1, at = ticks, labels = lab)
  
  
  #subplot 2,1
  #get the column
  sub_metering_1 <- data$V7
  sub_metering_2 <- data$V8
  sub_metering_3 <- data$V9
  #from factor to numeric
  sub_metering_1 <- as.numeric(levels(sub_metering_1))[sub_metering_1]
  sub_metering_2 <- as.numeric(levels(sub_metering_2))[sub_metering_2]
  sub_metering_3 <- as.numeric(levels(sub_metering_3))[sub_metering_3]
  matplot(cbind(sub_metering_1,sub_metering_2,sub_metering_3),type = "l",lty=1, col = c("black","red","blue"), ylab = "Energy sub metering", xlab = "", xaxt = 'n')
  ticks = c(1, length(data$V1[data$V1=="1/2/2007"])+1, length(data$V1)+1);
  lab = c("Thu", "Fri","Sat")
  axis(side = 1, at = ticks, labels = lab)
  legend("topright",c(as.character(t$V7[1]),as.character(t$V8[1]),as.character(t$V9[1])), pch = "___", bty = 'n',  col = c("black","red","blue"))
  
  #subplot 2,2
  
  global_reactive_power = as.numeric(levels(data$V4))[data$V4]
  plot(global_reactive_power,type = "l",lty=1, ylab = "Global_reactive_power",xlab = "datetime", xaxt = 'n')
  lab = c("Thu", "Fri","Sat")
  axis(side = 1, at = ticks, labels = lab)
  
  dev.off()
}
