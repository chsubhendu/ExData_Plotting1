## THis function reads the House hold power consumption data and
## generates the histogram plot for Global Active Power consumption
plot1 <- function () {
    
    power_data <- read.table("household_power_consumption.txt",header = TRUE, sep = ";",
                             stringsAsFactors = FALSE)
    
    power_plot <- subset(power_data,(as.Date(Date,"%d/%m/%Y") >= as.Date("01/02/2007","%d/%m/%Y")
                        & as.Date(Date,"%d/%m/%Y") <= as.Date("02/02/2007","%d/%m/%Y")),
                         select = c(Date,Time,Global_active_power))
    active_power <- as.numeric(power_plot$Global_active_power)
    hist(active_power,col="red",main="Global Active Power",
         xlab="Global Active Power (killowatts)")
    dev.copy(png,file="plot1.png")
    dev.off()
    
}
