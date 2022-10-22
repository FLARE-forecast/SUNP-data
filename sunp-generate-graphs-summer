library(lubridate)
library(dplyr)

download.file('https://github.com/FLARE-forecast/SUNP-data/raw/sunp-buoy-data/SUNP_buoy_met.csv', 'sunp-met.csv')
download.file('https://github.com/FLARE-forecast/SUNP-data/raw/sunp-buoy-data/SUNP_buoy_wq.csv', 'sunp-wq.csv')

#SUNP met data

sunpmetheader<-read.csv("sunp-met.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
sunpmetdata<-read.csv("sunp-met.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(sunpmetdata)<-names(sunpmetheader) #combine the names to deal with Campbell logger formatting

#Removes row if the TIMESTAMP column is blank
sunpmetdata <- sunpmetdata[complete.cases(sunpmetdata$TIMESTAMP),]

#Removes row if the RECORD column has an NA or blank
sunpmetdata=sunpmetdata[!(is.na(sunpmetdata$RECORD) | sunpmetdata$RECORD==""), ]

#for the time sequence we can use the same as from the FCR met staion 
end.time <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H:%M")), tzone = "Etc/GMT+5") #gives us current time with rounded minutes in EDT
start.time <- end.time - days(7) #to give us seven days of data for looking at changes
full_time <- seq(start.time, end.time, by = "min") #create sequence of dates from past 5 days to fill in data

obs4 <- array(NA,dim=c(length(full_time),17)) #create array that will be filled in with 10 columns
#commented all lines that are irrelevant for 2020 data, per change in data downloads
#met_timechange=max(which(sunpmetdata$TIMESTAMP=="2019-04-15 10:19:00")) #shows time point when met station was switched from GMT -4 to GMT -5
sunpmetdata$TIMESTAMP<-as.POSIXct(strptime(sunpmetdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned
#sunpmetdata$TIMESTAMP[c(1:met_timechange-1)]<-with_tz(force_tz(sunpmetdata$TIMESTAMP[c(1:met_timechange-1)],"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set
#sunpmetdata=sunpmetdata[-c(met_timechange-1),]

if (length(na.omit(sunpmetdata$TIMESTAMP[sunpmetdata$TIMESTAMP>start.time]))==0) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("sunpMetDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time, "and", end.time, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
} else {
  #merge instead of a for loop
  for(i in 1:length(full_time)){ #this loop looks for matching dates and extracts data from sunpmetdata file to obs4 array
    index = which(sunpmetdata$TIMESTAMP==full_time[i])
    if(length(index)>0){
      obs4[i,] <- unlist(sunpmetdata[index,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)])
    }
  }
  #obs4=merge(full_time,sunpmetdata, all.x=TRUE)#merge the data frame to get the last 7 days
  obs4<-as.data.frame(obs4) #make into DF
  colnames(obs4)<-names(sunpmetdata[index,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]) #get column names
  obs4$TIMESTAMP<-full_time #now have your array with a proper timedate stamp!
  
  pdf(paste0("sunpMetDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  par(mfrow=c(3,2))
  plot(obs4$TIMESTAMP,obs4$RECORD, main="RECORD", xlab="Time", ylab="Number", type='l')
  plot(obs4$TIMESTAMP,obs4$BattV, main="Battery", xlab="Time", ylab="Volts", type='l')
  plot(obs4$TIMESTAMP,obs4$AirTC, main="Air Temp", xlab="Time", ylab="degrees C", type='l')
  plot(obs4$TIMESTAMP,obs4$RH, main="Rel Hum", xlab="Time", ylab="%", type='l')
  # plot(obs4$TIMESTAMP,obs4$RH, main="Rain", xlab="Time", ylab="mm", type='l')
  plot(obs4$TIMESTAMP,obs4$WS_ms_Avg, main="Wind speed", xlab="Time", ylab="m/s",type='l')
  # plot(obs4$TIMESTAMP,obs4$, main="Shortwave Up", xlab="Time", ylab="W/m2",type='l')
  # plot(obs4$TIMESTAMP,obs4$SR01Dn_Avg, main="Shortwave Down", xlab="Time", ylab="W/m2",type='l')
  # plot(obs4$TIMESTAMP,obs4$IR01UpCo_Avg, main="Longwave Up", xlab="Time", ylab="W/m2",type='l')
  # plot(obs4$TIMESTAMP,obs4$IR01DnCo_Avg, main="Longwave Down", xlab="Time", ylab="W/m2",type='l')
  plot(obs4$TIMESTAMP,obs4$PAR_Den_Avg, main="PAR", xlab="Time", ylab="umol/s/m^2",type='l')
  dev.off() #file made!
}


sunpwaterheader<-read.csv("sunp-wq.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
sunpwaterdata<-read.csv("sunp-wq.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(sunpwaterdata)<-names(sunpwaterheader) #combine the names to deal with Campbell logger formatting


#Removes row if the TIMESTAMP column is blank
sunpwaterdata <- sunpwaterdata[complete.cases(sunpwaterdata$TIMESTAMP),]

#Removes row if the RECORD column has an NA or blank
sunpwaterdata=sunpwaterdata[!(is.na(sunpwaterdata$RECORD) | sunpwaterdata$RECORD==""), ]

#For the time sequence we can use the same as the FCR catwalk 
end.time1 <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H")), tzone = "Etc/GMT+5") #gives us current time with rounded hours in EDT
start.time1 <- end.time1 - days(7) #to give us seven days of data for looking at changes
full_time1 <- as.data.frame(seq(start.time1, end.time1, by = "10 min")) #create sequence of dates from past 5 days to fill in data
colnames(full_time1)=c("TIMESTAMP") #make it a data frame to merge to make obs5 later

#obs5 <- array(NA,dim=c(length(full_time1),41)) #create array that will be filled in with 41 columns (the entire size of the array)
#cat_timechange=max(which(sunpwaterdata$TIMESTAMP=="2019-04-15 10:00:00"))
sunpwaterdata$TIMESTAMP<-as.POSIXct(strptime(sunpwaterdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned
#sunpwaterdata$TIMESTAMP[c(1:cat_timechange-1)]<-with_tz(force_tz(sunpwaterdata$TIMESTAMP[c(1:cat_timechange-1)],"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set

if (length(na.omit(sunpwaterdata$TIMESTAMP[sunpwaterdata$TIMESTAMP>start.time1]))==0) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("sunpWaterQualityDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time1, "and", end.time1, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
} else {
  #I would end up with NAs for all of the data values
  #for(j in 5:39){
  #sunpwaterdata[,j]<-as.numeric(levels(sunpwaterdata[,j]))[sunpwaterdata[,j]]#need to set all columns to numeric values
  
  #}
  #for(i in 1:length(full_time1)){ #this loop looks for matching dates and extracts data from metdata file to obs array
  #index = which(sunpwaterdata$TIMESTAMP==full_time1[427])
  #if(length(index)>0){
  #obs5[n,] <- unlist(sunpwaterdata[index,c(1:41)])
  #}
  #}
  
  obs5=merge(full_time1,sunpwaterdata, all.x = TRUE)#merge the data frame to get the last 7 days
  #obs5<-as.data.frame(obs5) #make into DF
  #obs5[,1] <- full_time1 #now have your array with a proper timedate stamp!
  #colnames(obs5)<-names(sunpwaterdata[index,c(1:41)]) #get column names
  
  pdf(paste0("sunpWaterQualityDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  par(mfrow=c(3,2))
  
  plot(obs5$TIMESTAMP,obs5$RECORD, main="Campbell Logger Record", xlab="Time", ylab="Number", type='l')
  plot(obs5$TIMESTAMP,obs5$BattV, main="Campbell Logger Battery", xlab="Time", ylab="Volts", type='l')
  #Going to add these back in when the EXos are in 
  #added y limits so the axises would show up when the are no data
  plot(obs5$TIMESTAMP,obs5$EXO_battery, main="EXO Battery", xlab="Time", ylab="Volts", type='l',lwd=1.5,  ylim=c(2,8)) # previously c(-0.5, 15)
  
  plot(obs5$TIMESTAMP, obs5$Radio_bat, main = "Radio Battery", xlab = "Time", ylab = "Volts", type = "l", lwd = 1.5, ylim = c(10,16))

  plot(obs5$TIMESTAMP,obs5$EXO_cablepower, main="EXO Cable Power", xlab="Time", ylab="Volts", type='l',lwd=1.5, ylim=c(10,15))

  plot(obs5$TIMESTAMP,obs5$EXO_depth, main="EXO Depth", xlab="Time", ylab="Meters", type='l', ylim=c(0,3))

  
  
  plot(obs5$TIMESTAMP,obs5$EXO_pressure, main="Sonde Pressure", xlab="Time", ylab="psi", type='l', ylim=c(-1,10))

  
  plot(obs5$TIMESTAMP,obs5$doobs, main="DO", xlab="Time", ylab="mg/L", type='l', col="medium sea green", lwd=1.5, ylim = c(min(obs5$doobs, obs5$doobs_1, na.rm = TRUE) - 1, max(obs5$doobs, obs5$doobs_1, na.rm = TRUE) + 1))
  points(obs5$TIMESTAMP, obs5$doobs_1, col = "magenta", lwd = 1.5, type = "l")
  legend("topleft", c("EXO DO 1m", "DO 10m"), text.col = c("magenta", "medium sea green"), x.intersp=0.001)

  plot(obs5$TIMESTAMP,obs5$dosat, main="DO % saturation", xlab="Time", ylab="% saturation", type='l', col="medium sea green", lwd=1.5, ylim = c(min(obs5$dosat, obs5$dosat_1, na.rm = TRUE) - 5, max(obs5$dosat, obs5$dosat_1, na.rm = TRUE) + 5))
  points(obs5$TIMESTAMP, obs5$dosat_1, col = "magenta", lwd = 1.5, type = "l")
  legend("topleft", c("EXO DO 1m", "DO 10m"), text.col = c("magenta", "medium sea green"), x.intersp=0.001)
  
  
  plot(obs5$TIMESTAMP,obs5$Cond, main="Cond, SpCond @ 1.0m", xlab="Time", ylab="uS/cm", type='l', col="magenta", lwd=1.5, ylim=c(min(obs5$Cond, obs5$SpCond, na.rm = TRUE) - 1, max(obs5$Cond, obs5$SpCond, na.rm = TRUE) + 1))
  points(obs5$TIMESTAMP, obs5$SpCond, col="black", type='l', lwd=1.5)
  legend("topleft", c("SPCond 1m", "Cond 1m"), text.col=c("black", "magenta"), x.intersp=0.001)
  
  plot(obs5$TIMESTAMP, obs5$TDS, main = "TDS @ 1.0m", col="DarkOrange1", xlab = "Time", ylab = "mg/L", type="l", lwd=1.5, ylim = c(0,0.2))
  
  #
  plot(obs5$TIMESTAMP,obs5$Chla_1, main="Chla, Phyco @ 1.0m", xlab="Time", ylab="ug/L", type='l', col="green", lwd=1.5, ylim=c(min(obs5$Chla_1, obs5$BGAPC_1, na.rm = TRUE) -0.5, max(obs5$Chla_1, obs5$BGAPC_1, na.rm = TRUE) + 0.5))
  points(obs5$TIMESTAMP, obs5$BGAPC_1, col="blue", type='l', lwd=1.5)
  legend("topleft", c("Chla 1m", "Phyco 1m"), text.col=c("green", "blue"), x.intersp=0.001)

  plot(obs5$TIMESTAMP, obs5$fDOM_QSU_1, main = "fDOM @ 1.0m", xlab = "Time", ylab = "QSU", col="firebrick4", type='l', lwd=1.5, ylim = c(min(obs5$fDOM_QSU_1, na.rm = TRUE) - 1, max(obs5$fDOM_QSU_1, na.rm = TRUE) + 1))
  
  
  par(mfrow=c(1,1))
  par(oma=c(1,1,1,4))
  plot(obs5$TIMESTAMP,obs5$wtr_surface, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="firebrick4", lwd=1.5, ylim=c(0, 35))
  points(obs5$TIMESTAMP, obs5$wtr_1, col="firebrick1", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_2, col="DarkOrange1", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_3, col="gold", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_4, col="greenyellow", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_5, col="medium sea green", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_6, col="sea green", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_7, col="DeepSkyBlue4", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_8, col="blue2", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_9, col="blue4", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_10, col="darkslateblue", type='l', lwd=1.5)
  par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("right",c("0.1m","1m", "2m", "3m", "4m", "5m", "6m", "7m", "8m","9m","10m"),
         text.col=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                    "DeepSkyBlue4", "blue2", "blue4", "darkslateblue"), 
         cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
  
  dev.off() #file made!
}


