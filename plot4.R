## THis function reads the House hold power consumption data and
## generates the 4 graphs for Global Active Power consumption, Voltage, Energy Submetering 
## and Global Reactive Power. Finally put all 4 graphs in one screen. 
plot4 <- function () {
    
    power_data <- read.table("household_power_consumption.txt",header = TRUE, sep = ";",
                             stringsAsFactors = FALSE)
    
    power_plot <- subset(power_data,(as.Date(Date,"%d/%m/%Y") >= as.Date("01/02/2007","%d/%m/%Y")
                                       & as.Date(Date,"%d/%m/%Y") <= as.Date("02/02/2007","%d/%m/%Y")),
                                    select = c(Date,Time,Global_active_power,Global_reactive_power,
                                               Voltage,Sub_metering_1,Sub_metering_2,Sub_metering_3))
    
    power_plot_final <- within(power_plot, { timestamp=strptime(paste(Date, Time),"%d/%m/%Y %H:%M:%S")
                            as.numeric(Global_active_power)
                            as.numeric(Global_reactive_power)
                            as.numeric(Voltage)
                            as.numeric(Sub_metering_1)
                            as.numeric(Sub_metering_2)
                            as.numeric(Sub_metering_3)
                            rm(Date,Time)
    })
    
    par(mfrow = c(2,2))
    
    with (power_plot_final, {
          plot(timestamp, Global_active_power,type="l",
                                 ylab = "Global Active Power",xlab="",cex.lab=.7 )
          plot(timestamp, Voltage,type="l", ylab = "Voltage",xlab="datetime",cex.lab=.7 )
        
          plot(timestamp, Sub_metering_1,type="l", ylab = "Energy sub metering",xlab="",cex.lab=.7)
                lines(timestamp, Sub_metering_2, col = "red")
                lines(timestamp, Sub_metering_3, col = "blue")
                legend("topright",lwd =1,col=c("black","red","blue"),
                legend = c("Sub_metering_1","Sub_metering_1","Sub_metering_3"),text.font = 3,cex=.5 )
    
         plot(timestamp, Global_reactive_power,type="l",
                                ylab = "Global Ractive Power",xlab="datetime",cex.lab=.7 )
    })
    dev.copy(png,file="plot4.png")
    dev.off()
    
}
