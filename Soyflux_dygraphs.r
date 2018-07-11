library(xts)
library(dygraphs)
library(plotly)
library(openair)
#test

#----------------------------------------------------------------------------------------
## Data display selection

# for the last 30 days, 48*30 = 1440 data points, for last week 48*7 = 336 data points
selection.month <- 1440

#----------------------------------------------------------------------------------------
## Import .dat file 

input.path            <- "C:/Users/Mike/Dropbox/PhD/Research/Part I/DataAnalysis/Soyflux-IP_SoyfluxMT.dat"

# Output paths for html files
Public.battery        <- "C:/Users/Mike/Dropbox/Public/Capuaba_Battery.html"
Public.precipitation  <- "C:/Users/Mike/Dropbox/Public/Capuaba_Precipitation.html"
Public.temperature    <- "C:/Users/Mike/Dropbox/Public/Capuaba_Temperature.html"
Public.VWC            <- "C:/Users/Mike/Dropbox/Public/Capuaba_VWC.html"
Public.G              <- "C:/Users/Mike/Dropbox/Public/Capuaba_G.html"
Public.Radiation      <- "C:/Users/Mike/Dropbox/Public/Capuaba_Rad.html"

# Import .dat files from CR1000 measurements at the Soyflux site
Station <- read.table(input.path, header=TRUE, sep=",", na.strings="NAN", dec=".", strip.white=TRUE, skip=4)
colnames(Station) <- c("timestamp", "record", "BattV", "PTempt", "year", 
                       "jday", "hours", "minutes", "Hukse_shf_Avg", "Hukse_shf_mV_Avg", 
                       "Hukse_shf_cal", "EC1", "EC2", "EC3", "EC4", "GS3T1", "GS3T2", "GS3T3",
                       "GS3T4", "VWC1", "VWC2", "VWC3", "VWC4", "WP1", "WP2", "WP3", "WP4", 
                       "MPST1", "MPST2", "MPST3", "MPST4", "TEM1", "TEM2", "TEM3", "TEM4", "TEM5", 
                       "TEM6", "TEM7", "TEM8", "TEM9", "Rn", "PAR", "Rs", "NA1", "NA2", "Wd", "Ws", "Tair", 
                       "RH", "Pa", "Precip", "Hail", "PRI", "D570", "U570", "D531", "U531", 
                       "Ind1", "Ind2", "NDVI", "D650", "U650", "D810", "U810", "Ind3", "Ind4", 
                       "TCanopy")

#----------------------------------------------------------------------------------------
## Format dates

Station$timestamp <- as.POSIXct(Station$timestamp, "%Y-%m-%d %H:%M", tz="GMT")       
Station$date <- as.Date(Station$timestamp, "%Y-%m-%d %H:%M:%S", tz="GMT")

# Latest week data selection
start <- as.Date(tail(Station$timestamp, 336)[1], format = "%Y-%m-%d")
end   <- as.Date(tail(Station$timestamp, 1), format = "%Y-%m-%d")

#----------------------------------------------------------------------------------------
## Obtain daily precipitation

Station.daily <- timeAverage(Station, avg.time = "day", statistic = "mean")
Precip <- data.frame(Station.daily$date, Station.daily$Precip*48)
colnames(Precip) <- c("Station.daily.date", "Station.daily.Precip")

# Organize battery voltage data - not shared online
T1 <- data.frame(Station$timestamp, Station$BattV)
colnames(T1) <- c("timestamp", "BattV")
rownames(T1) <- T1[,1]
T1$timestamp <- NULL
T1 <- as.xts(T1)
indexTZ(T1) <- "GMT"   # all CR1000 measurements are GMT

# Organize precipitation data - to be shared online
T2 <- data.frame(tail(Station.daily$date, 30), 
                 signif(tail(Precip$Station.daily.Precip, 30), digits = 2),
                 signif(tail(Station.daily$RH, 30), digits = 2))
colnames(T2) <- c("date", "P", "UR")
rownames(T2) <- T2[,1] 
T2$date <- NULL

# Organize temperature measurements - to be shared online
T3 <- data.frame(tail(Station$timestamp, selection.month), 
                 signif(tail(Station$Tair, selection.month), digits = 2),
                 signif(tail(Station$GS3T1, selection.month), digits = 2))
                 colnames(T3) <- c("timestamp", "ar", "solo")
rownames(T3) <- T3[,1]
T3$timestamp <- NULL
T3 <- as.xts(T3)
indexTZ(T3) <- "GMT"

# Organize soil water content - not to be shared online
T4 <- data.frame(Station$timestamp, Station$VWC1, Station$VWC2, Station$VWC3, Station$VWC4,
                 Station$WP1, Station$WP2, Station$WP3, Station$WP4)
colnames(T4) <- c("timestamp", "VWC5cm", "VWC10cm", "VWC30cm", "VWC60cm",
                  "WP5cm", "WP10cm", "WP30cm", "WP60cm")
rownames(T4) <- T4[,1]
T4$timestamp <- NULL
T4 <- as.xts(T4)
indexTZ(T4) <- "GMT"

# Organize soil heat flux measuremets - not to be shared online
T5 <- data.frame(Station$timestamp, Station$Hukse_shf_Avg, 
                 Station$TEM1, Station$TEM2, Station$TEM3, Station$TEM4)
colnames(T5) <- c("timestamp", "Main", "TEM1", "TEM2", "TEM3", "TEM4")
rownames(T5) <- T5[,1]
T5$timestamp <- NULL
T5 <- as.xts(T5)
indexTZ(T5) <- "GMT"

# Organize radiation measurements - not to be shared online
T6 <- data.frame(Station$timestamp, 
                 signif(Station$Rs, digits = 3), 
                 signif(Station$PAR, digits = 4), 
                 signif(Station$Rn, digits = 3))
colnames(T6) <- c("timestamp", "Rs", "PAR", "Rn")
rownames(T6) <- T6[,1]
T6$timestamp <- NULL
T6 <- as.xts(T6)
indexTZ(T6) <- "GMT"

#----------------------------------------------------------------------------------------
## Plot dygraphs

# Plot battery voltage data
Battery <- dygraph(T1, main = "Battery Voltage at Capuaba farm") %>%
           dyAxis("y", label = "Volts") %>%
           dyOptions(fillGraph = TRUE, drawGrid = FALSE) %>%
           dyRangeSelector(dateWindow = c(start, end))

# Plot precipitation data
Precipitation <- dygraph(T2, main = "Precipitação (P) diária e Umidade Relativa (UR) do ar") %>%
                 dySeries("P", color = "blue") %>%
                 dyAxis("y", label = "P (mm/dia)", valueRange = c(0,100)) %>%
                 dyAxis("y2", label = "UR (m3/m3)", valueRange = c(0,100)) %>%
                 dySeries("UR", color = "black", axis = "y2") %>%
                 dyOptions(fillGraph = TRUE, drawGrid = FALSE) %>%
                 dyRangeSelector(fillColor = "blue", strokeColor = "blue",
                                 dateWindow = c(start, end))

# Plot air and soil temperatures
Temp <- dygraph(T3, main = "Temperatura") %>%
        dySeries("ar", drawPoints = TRUE, color = "red") %>%
        dySeries("solo", drawPoints = TRUE, color = "brown") %>%
        dyAxis("y", label = "graus Celsius") %>%
        dyRangeSelector(fillColor = "red", strokeColor = "brown", 
                        dateWindow = c(start, end))

# Plot soil volumetric water content and water potential at 5, 10, 30 and 60 cm depths
VWC <- dygraph(T4, main = "VWC and WP at Capuaba farm") %>%
       dySeries("VWC5cm", drawPoints = TRUE, color = "blue") %>%
       dySeries("VWC10cm", drawPoints = TRUE, color = "green") %>%
       dySeries("VWC30cm", drawPoints = TRUE, color = "brown") %>%
       dySeries("VWC60cm", drawPoints = TRUE, color = "black") %>%
       dySeries("WP5cm", drawPoints = TRUE, color = "blue", axis = "y2", strokeWidth = 5, strokePattern = "dashed") %>%
       dySeries("WP10cm", drawPoints = TRUE, color = "green", axis = "y2", strokeWidth = 5, strokePattern = "dashed") %>%
       dySeries("WP30cm", drawPoints = TRUE, color = "brown", axis = "y2", strokeWidth = 5, strokePattern = "dashed") %>%
       dySeries("WP60cm", drawPoints = TRUE, color = "black", axis = "y2", strokeWidth = 5, strokePattern = "dashed") %>%
       dyAxis("y", label = "m3/m3", valueRange = c(0,0.5)) %>%
       dyAxis("y2", label = "kPa", valueRange = c(-500,0)) %>%
       dyHighlight(highlightCircleSize = 2, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE) %>%
       dyRangeSelector(strokeColor = "black", dateWindow = c(start, end)) %>%
       dyLegend(width = 1000)

# Plot soil heat flux data from plates
Soil.heat <- dygraph(T5, main = "Soil heat flux at Capuaba farm") %>%
             dySeries("Main", drawPoints = TRUE, color = "black") %>%
             dySeries("TEM1", drawPoints = TRUE, color = "blue") %>%
             dySeries("TEM2", drawPoints = TRUE, color = "brown") %>%
             dySeries("TEM3", drawPoints = TRUE, color = "green") %>%
             dySeries("TEM4", drawPoints = TRUE, color = "red") %>%
             dyAxis("y", label = "G (W/m2)") %>%
             dyHighlight(highlightCircleSize = 2, highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE) %>%
             dyRangeSelector(strokeColor = "black", dateWindow = c(start, end)) %>%
             dyLegend(width = 600)

# Plot data from pyranometer, net radiometer and PAR sensor
Radiation <- dygraph(T6, main = "Soil heat flux at Capuaba farm") %>%
             dySeries("Rs", drawPoints = TRUE, color = "black") %>%
             dySeries("PAR", drawPoints = TRUE, color = "blue", axis = "y2") %>%
             dySeries("Rn", drawPoints = TRUE, color = "brown") %>%
             dyAxis("y", label = "Rs, Rn (W/m2)") %>%
             dyAxis("y2", label = "PAR (umol/m2s)") %>%
             dyHighlight(highlightCircleSize = 2, highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE) %>%
             dyRangeSelector(strokeColor = "black", dateWindow = c(start, end)) %>%
             dyLegend(width = 600)

#----------------------------------------------------------------------------------------
## Export to html on Public Dropbox folder

# ready graphs
htmlwidgets::saveWidget(as.widget(Battery), Public.battery)
htmlwidgets::saveWidget(as.widget(Precipitation), Public.precipitation)
htmlwidgets::saveWidget(as.widget(Temp), Public.temperature)
htmlwidgets::saveWidget(as.widget(VWC), Public.VWC)
htmlwidgets::saveWidget(as.widget(Soil.heat), Public.G)
htmlwidgets::saveWidget(as.widget(Radiation), Public.Radiation)

#----------------------------------------------------------------------------------------
## END
