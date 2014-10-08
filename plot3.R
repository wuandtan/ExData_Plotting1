plot3 <- function(fn = "plot3.png") 
  #this function is to generate the three sub meterings on 1/2/2007 and 2/2/2007
  #input: the file name of the image (with .png extension)
  #output: the image.
{
  
  #read the file
  t=read.table("household_power_consumption.txt", sep = ";")
  #find the corresponding dates
  data <- t[t$V1=="1/2/2007" | t$V1=="2/2/2007" ,]
  #get the column
  sub_metering_1 <- data$V7
  sub_metering_2 <- data$V8
  sub_metering_3 <- data$V9
  #from factor to numeric
  sub_metering_1 <- as.numeric(levels(sub_metering_1))[sub_metering_1]
  sub_metering_2 <- as.numeric(levels(sub_metering_2))[sub_metering_2]
  sub_metering_3 <- as.numeric(levels(sub_metering_3))[sub_metering_3]
  
  
  png(file = fn)
  #plot the graph
  matplot(cbind(sub_metering_1,sub_metering_2,sub_metering_3),type = "l",lty=1, col = c("black","red","blue"), ylab = "Energy sub metering", xlab = "", xaxt = 'n')
  ticks = c(1, length(data$V1[data$V1=="1/2/2007"])+1, length(data$V1)+1);
  lab = c("Thu", "Fri","Sat")
  axis(side = 1, at = ticks, labels = lab)
  legend("topright",c(as.character(t$V7[1]),as.character(t$V8[1]),as.character(t$V9[1])), pch = "___", col = c("black","red","blue"))
  dev.off()
}
