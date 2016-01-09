## THis function reads the House hold power consumption data and
## generates the Global Active Power consumption line chart over 3 days
plot2 <- function () {
    
    power_data <- read.table("household_power_consumption.txt",header = TRUE, sep = ";",
                             stringsAsFactors = FALSE)
    
    power_plot <- subset(power_data,(as.Date(Date,"%d/%m/%Y") >= as.Date("01/02/2007","%d/%m/%Y")
                        & as.Date(Date,"%d/%m/%Y") <= as.Date("02/02/2007","%d/%m/%Y")),
                         select = c(Date,Time,Global_active_power))
    power_plot_final <- within(power_plot, { timestamp=strptime(paste(Date, Time),
                            "%d/%m/%Y %H:%M:%S")
                            as.numeric(Global_active_power)
                            rm(Date,Time)
                            })
    
    active_power <- as.numeric(power_plot$Global_active_power)
    with (power_plot_final, plot(timestamp, Global_active_power,type="l",
                                 ylab = "Global Active Power (killowatts)",xlab="" ))
    
    dev.copy(png,file="plot2.png")
    dev.off()
    
}
