# Plotting scripts for SUNP Buoy Figures
# Author: Vahid, Whitney Woelmer, and Adrienne Breef-Pilz

# Edits:
# 14 Oct 24- uncommented out the if statement for when the buoy is in the harbor. 

library(lubridate)
library(dplyr)

#setwd("~/Dropbox/scc_figs/")

download.file('https://raw.githubusercontent.com/FLARE-forecast/SUNP-data/sunp-buoy-data/CR6_SUNP_SUNP_buoy_met.csv', 'sunp-met.csv')
download.file('https://raw.githubusercontent.com/FLARE-forecast/SUNP-data/sunp-buoy-data/CR6_SUNP_SUNP_buoy_wq.csv', 'sunp-wq.csv')

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
full_time <- as.data.frame(seq(start.time, end.time, by = "min")) #create sequence of dates from past 5 days to fill in data
colnames(full_time)=c("TIMESTAMP") #make it a data frame to merge to make obs4 later


#obs4 <- array(NA,dim=c(length(full_time),17)) #create array that will be filled in with 10 columns
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
  
  
  
  obs4 <- dplyr::left_join(full_time,sunpmetdata, keep = F)#merge the data frame to get the last 7 days
  # for(i in 1:length(full_time)){ #this loop looks for matching dates and extracts data from sunpmetdata file to obs4 array
  #   index = which(sunpmetdata$TIMESTAMP==full_time[i])
  #   if(length(index)>0){
  #     obs4[i,] <- unlist(sunpmetdata[index,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)])
  #   }
  
  #obs4<-merge(full_time,sunpmetdata, all.x=TRUE)#merge the data frame to get the last 7 days
  #obs4<-as.data.frame(obs4) #make into DF
  #colnames(obs4)<-names(sunpmetdata[index,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]) #get column names
  #obs4$TIMESTAMP<-full_time #now have your array with a proper timedate stamp!
  
  pdf(paste0("sunpMetDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  par(mfrow=c(3,2))
  plot(obs4$TIMESTAMP,obs4$RECORD, main="RECORD", xlab="Time", ylab="Number", type='l')
  plot(obs4$TIMESTAMP,obs4$BattV, main="Battery", xlab="Time", ylab="Volts", type='l')
  plot(obs4$TIMESTAMP,obs4$AirTC, main="Air Temp", xlab="Time", ylab="degrees C", type='l')
  plot(obs4$TIMESTAMP,obs4$RH, main="Rel Hum", xlab="Time", ylab="%", type='l')
  plot(obs4$TIMESTAMP, obs4$Press, main="Barometric Pressure", xlab="Time", ylab="mbar", type="l")
  plot(obs4$TIMESTAMP,obs4$Rel_WS_ms_Avg, main="Wind speed", xlab="Time", ylab="m/s",type='l')
  plot(obs4$TIMESTAMP, obs4$Rel_WindDir, main="Wind Direction", xlab="Time", ylab="degrees", type="l")

  # only print the Net raiodometer plots if the sensors are plugged in. 
  if(obs4[9950,"Incoming_SW_Avg"]!="NAN" & obs4[9950, "Outgoing_LW_Avg"]!="NAN" & obs4[2000, "Incoming_SW_Avg"]!="NAN" & obs4[2000,"Outgoing_LW_Avg"]!="NAN" ){
  plot(obs4$TIMESTAMP,obs4$Incoming_SW_Avg, main="Shortwave Up", xlab="Time", ylab="W/m2",type='l')
  plot(obs4$TIMESTAMP,obs4$Outgoing_SW_Avg, main="Shortwave Down", xlab="Time", ylab="W/m2",type='l')
  plot(obs4$TIMESTAMP,obs4$Incoming_LW_Avg, main="Longwave Up", xlab="Time", ylab="W/m2",type='l')
  plot(obs4$TIMESTAMP,obs4$Outgoing_LW_Avg, main="Longwave Down", xlab="Time", ylab="W/m2",type='l')
  }
  plot(obs4$TIMESTAMP,obs4$PAR_Den_Avg, main="PAR", xlab="Time", ylab="umol/s/m^2",type='l')
  dev.off() #file made!
}

####### Water Quality Data from the EXO and thermistors#####

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
  
  obs5 <- dplyr::left_join(full_time1,sunpwaterdata, keep = F)#merge the data frame to get the last 7 days
  # make everything numeric
  obs5[2:54] <- sapply(obs5[2:54],as.numeric)
  #obs5<-as.data.frame(obs5) #make into DF
  #obs5[,1] <- full_time1 #now have your array with a proper timedate stamp!
  #colnames(obs5)<-names(sunpwaterdata[index,c(1:41)]) #get column names 
  # }  moved to the end so the graphs are also in the else part of the statement
  
  ### If statement to change plots when plot is in the Harbor or out in the lake#####
  
  pdf(paste0("sunpWaterQualityDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  par(mfrow=c(3,2))
  
  # If the EXO and the temp string is out of the water then the buoy must me in the Harbor so print only the Mid RDO plots
  
  if(obs5[1000,"wtr_surface"]=="NaN" & obs5[1000, "EXO_wtr_shallow"]=="NaN" & obs5[800, "EXO_wtr_deep"]=="NaN" & obs5[800,"wtr_surface"]=="NaN" & obs5[800, "EXO_wtr_shallow"]=="NaN" & obs5[800, "EXO_wtr_deep"]=="NaN"){

    plot(obs5$TIMESTAMP,obs5$RECORD, main="Campbell Logger Record", xlab="Time", ylab="Number", type='l')
    plot(obs5$TIMESTAMP,obs5$BattV, main="Campbell Logger Battery", xlab="Time", ylab="Volts", type='l')
    #Going to add these back in when the EXos are in
    #added y limits so the axises would show up when the are no data
    #plot(obs5$TIMESTAMP, obs5$Radio_bat, main = "Radio Battery", xlab = "Time", ylab = "Volts", type = "l", lwd = 1.5, ylim = c(10,16))


    plot(obs5$TIMESTAMP,obs5$doobs_mid, main="DO (mg/L) in Harbor", xlab="Time", ylab="mg/L", type='l', col="blue", lwd=1.5)

    plot(obs5$TIMESTAMP,obs5$dosat_mid, main="DO (% saturation) in Harbor", xlab="Time", ylab="% saturation", type='l', col="blue")

    plot(obs5$TIMESTAMP,obs5$dotemp_mid, main="Water Temperature in Harbor", xlab="Time", ylab="Temperature (°C)", type='l', col="red")

    dev.off() #file made!

    # Else the Buoy is out in the Lake and all the plots
  }else{
  plot(obs5$TIMESTAMP,obs5$RECORD, main="Campbell Logger Record", xlab="Time", ylab="Number", type='l')
  plot(obs5$TIMESTAMP,obs5$BattV, main="Campbell Logger Battery", xlab="Time", ylab="Volts", type='l')
  #Going to add these back in when the EXos are in 
  #added y limits so the axises would show up when the are no data
  plot(obs5$TIMESTAMP,obs5$EXO_battery_shallow, main="EXO Battery", xlab="Time", ylab="Volts", type='l',lwd=1.5,  ylim=c(2,8)) # previously c(-0.5, 15)
  points(obs5$TIMESTAMP, obs5$EXO_battery_deep, col="red", lwd =1.5, type="l")
  legend("topleft", c("EXO Battery Shallow", "EXO Battery Deep"), text.col = c("black", "red"), x.intersp=0.001)
  #plot(obs5$TIMESTAMP, obs5$Radio_bat, main = "Radio Battery", xlab = "Time", ylab = "Volts", type = "l", lwd = 1.5, ylim = c(10,16))
  
  # wiper 
  plot(obs5$TIMESTAMP, obs5$EXO_wiper_shallow, main="EXO wiper voltage", xlab="Time", ylab="volts", type ="l")
  points(obs5$TIMESTAMP, obs5$EXO_wiper_deep, col="red", type="l")
  legend("topleft", c("EXO 1m", "EXO 8m"), text.col = c("black", "red"), x.intersp=0.001)
  
  
  plot(obs5$TIMESTAMP,obs5$EXO_cablepower_shallow, main="EXO Cable Power", xlab="Time", ylab="Volts", type='l',lwd=1.5, ylim=c(10,15))
  points(obs5$TIMESTAMP, obs5$EXO_cablepower_deep, col="red", type = "l", lwd =1.5)
  legend("topleft", c("EXO 1m", "EXO 8m"), text.col = c("black", "red"), x.intersp=0.001)
  
  plot(obs5$TIMESTAMP,obs5$EXO_depth_deep, main="Depth for Deep EXO", xlab="Time", ylab="Meters", type='l')
  
  #plot(obs5$TIMESTAMP,obs5$EXO_pressure_deep, main="Sonde Pressure for Deep EXO", xlab="Time", ylab="psi", type='l')
  
  # Temperature
  plot(obs5$TIMESTAMP,obs5$EXO_wtr_shallow, main="Water temp of sondes", xlab="Time", ylab="degrees C", type='l', col="medium sea green", lwd=1.5, ylim = c(min(obs5$EXO_wtr_deep,na.rm = TRUE) - 1, max(obs5$EXO_wtr_shallow,na.rm = TRUE) + 5))
  points(obs5$TIMESTAMP, obs5$dotemp_mid, col="black", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$EXO_wtr_deep, col="magenta", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$dotemp_deep, col="blue4", type='l', lwd=1.5)
  legend("topleft", c("1m EXO", "5m DO","8m EXO", "10m DO"), text.col=c("medium sea green", "black", "magenta","blue4"), x.intersp=0.001)
  
  # DO Plots
  plot(obs5$TIMESTAMP,obs5$EXO_doobs_shallow, main="DO", xlab="Time", ylab="mg/L", type='l', col="medium sea green", lwd=1.5, ylim = c(min(obs5$doobs_mid, obs5$doobs_deep, obs5$EXO_doobs_shallow,na.rm = TRUE) - 1, max(obs5$doobs_mid, obs5$doobs_deep, obs5$EXO_doobs_shallow,na.rm = TRUE) + 5))
  points(obs5$TIMESTAMP, obs5$doobs_mid, col = "magenta", lwd = 1.5, type = "l")
  points(obs5$TIMESTAMP, obs5$EXO_doobs_deep, col = "black", lwd = 1.5, type = "l")
  points(obs5$TIMESTAMP, obs5$doobs_deep, col="darkgray", lwd = 1.5, type="l")
  legend("topleft", c("EXO DO 1m", "DO mid ~5m", "EXO DO 8m",  "DO 10m"), text.col = c("medium sea green", "magenta", "black", "darkgray"), x.intersp=0.001)
  
  plot(obs5$TIMESTAMP,obs5$EXO_dosat_shallow, main="DO % saturation", xlab="Time", ylab="% saturation", type='l', col="medium sea green", lwd=1.5, ylim = c(min(obs5$dosat_mid, obs5$dosat_deep,obs5$EXO_dosat_shallow, na.rm = TRUE) - 5, max(obs5$dosat_mid, obs5$dosat_deep, obs5$EXO_dosat_shallow, na.rm = TRUE) + 45))
  points(obs5$TIMESTAMP, obs5$dosat_mid, col = "magenta", lwd = 1.5, type = "l")
  points(obs5$TIMESTAMP, obs5$EXO_dosat_deep, col = "black", lwd = 1.5, type = "l")
  points(obs5$TIMESTAMP, obs5$dosat_deep, col="darkgray", lwd = 1.5, type="l")
  legend("topleft", c("EXO DO 1m", "DO mid", "EXO DO 8m",  "DO 10m"), text.col = c("medium sea green", "magenta", "black", "darkgray"), x.intersp=0.001)
  
  # Conductivity
  plot(obs5$TIMESTAMP,obs5$EXO_Cond_shallow, main="Cond, SpCond and TDS", xlab="Time", ylab="uS/cm", type='l', col="red", lwd=1.5, ylim=c(min(obs5$EXO_Cond_deep, obs5$EXO_SpCond_shallow, na.rm = TRUE) - 1, max(obs5$EXO_Cond_shallow, obs5$EXO_SpCond_deep, na.rm = TRUE) + 55))
  points(obs5$TIMESTAMP, obs5$EXO_Cond_deep, col="magenta", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$EXO_SpCond_shallow, col="black", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$EXO_SpCond_deep, col="gray", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$EXO_fDOM_QSU_shallow, col="firebrick4", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$EXO_fDOM_QSU_deep, col="darkorange2", type='l', lwd=1.5)
  legend("topleft", c("Cond 1m","Cond 8m" ,"SpCond 1m", "SpCond 8m", "fDOM 1m", "fDOM 8m"), 
         text.col=c("red","magenta","black","gray", "firebrick4", "darkorange2"), x.intersp=0.001)
  
  
  #chla
  plot(obs5$TIMESTAMP,obs5$EXO_Chla_shallow, main="Chla, Phyco @ 1.0m", xlab="Time", ylab="ug/L", type='l', col="green", lwd=1.5, ylim=c(min(obs5$EXO_Chla_shallow, obs5$EXO_BGAPC_shallow, na.rm = TRUE) -0.5, max(obs5$EXO_Chla_shallow, obs5$EXO_BGAPC_shallow, na.rm = TRUE) + 2))
  points(obs5$TIMESTAMP, obs5$EXO_BGAPC_shallow, col="blue", type='l', lwd=1.5)
  
  legend("topleft", c("Chla 1m", "Phyco 1m"), text.col=c("green", "blue"), x.intersp=0.001)
  
  
  #turbidity
  plot(obs5$TIMESTAMP,obs5$EXO_Turbidity_NTU, main="Turbidity and TDS", xlab="Time", ylab="ug/L", type='l')
  points(obs5$TIMESTAMP, obs5$EXO_TDS_shallow, col="orange", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$EXO_TDS_deep, col="darkorange1", type='l', lwd=1.5)
  legend("topleft", c("Turbidity 8m","TDS 1m" ,"TDS 8m"), 
         text.col=c("black","orange", "darkorange1"), x.intersp=0.001)
  
  # Temperature
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
}  
