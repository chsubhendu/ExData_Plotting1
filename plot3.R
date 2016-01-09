## THis function reads the House hold power consumption data and
## generates the overlaying line charts for Energy Sub metering
plot3 <- function () {
    
    power_data <- read.table("household_power_consumption.txt",header = TRUE, sep = ";",
                             stringsAsFactors = FALSE)
    
    power_plot <- subset(power_data,(as.Date(Date,"%d/%m/%Y") >= as.Date("01/02/2007","%d/%m/%Y")
                        & as.Date(Date,"%d/%m/%Y") <= as.Date("02/02/2007","%d/%m/%Y")),
                         select = c(Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3))
    power_plot_final <- within(power_plot, { timestamp=strptime(paste(Date, Time),
                            "%d/%m/%Y %H:%M:%S")
                            as.numeric(Sub_metering_1)
                            as.numeric(Sub_metering_2)
                            as.numeric(Sub_metering_3)
                            rm(Date,Time)
                            })
    
    with (power_plot_final, plot(timestamp, Sub_metering_1,type="l",
                                 ylab = "Energy sub metering",xlab=""))
    with (power_plot_final, lines(timestamp, Sub_metering_2, col = "red"))
    with (power_plot_final, lines(timestamp, Sub_metering_3, col = "blue"))
    legend("topright",lwd =1,col=c("black","red","blue"),
           legend = c("Sub_metering_1","Sub_metering_1","Sub_metering_3") )
    dev.copy(png,file="plot3.png")
    dev.off()
    
}
