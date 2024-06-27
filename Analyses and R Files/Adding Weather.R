
library(sf)
library(openmeteo)
library(lubridate)
rm(list=ls())
setwd("C:/Users/John Crockett/Documents/URI")
GPS1_21=st_read("Data from Field Work/2021 Data/Tracks/GPS1/GPS1_withLC.shp")
GPS2_21=st_read("Data from Field Work/2021 Data/Tracks/GPS2/GPS2_withLC.shp")
GPS3_21=st_read("Data from Field Work/2021 Data/Tracks/GPS3/GPS3_withLC.shp")
GPS4_21=st_read("Data from Field Work/2021 Data/Tracks/GPS4/GPS4_withLC.shp")
GPS5_21=st_read("Data from Field Work/2021 Data/Tracks/GPS5/GPS5_withLC.shp")
GPS2_22=st_read("Data from Field Work/2022 Data/Tracks/GPS 2/GPS2_withLC.shp")
GPS4_22=st_read("Data from Field Work/2022 Data/Tracks/GPS 4/GPS4_withLC.shp")
GPS5_22=st_read("Data from Field Work/2022 Data/Tracks/GPS 5/GPS5_withLC.shp")
GPS1_23=st_read("Data from Field Work/2023 Data/Tracks/GPS 1/GPS1_withLC.shp")
GPS2_23=st_read("Data from Field Work/2023 Data/Tracks/GPS 2/GPS2_withLC.shp")
GPS3_23=st_read("Data from Field Work/2023 Data/Tracks/GPS 3/GPS3_withLC.shp")
GPSPhone=st_read("Data from Field Work/Phone Tracks/GPSPhone_withLC.shp")

GPS1_All=rbind(GPS1_21,GPS1_23)
GPS2_All=rbind(GPS2_21,GPS2_22,GPS2_23)
GPS3_All=rbind(GPS3_21,GPS3_23)
GPS4_All=rbind(GPS4_21,GPS4_22)
GPS5_All=rbind(GPS5_21,GPS5_22)

FieldData21=read.csv(file = "Analyses and R Files/FieldDataWithLC2021.csv")
FieldData22=read.csv(file = "Analyses and R Files/FieldDataWithLC2022.csv")
FieldData23=read.csv(file = "Analyses and R Files/FieldDataWithLC2023.csv")
FieldAll=rbind(FieldData22,
               FieldData21[,match(colnames(FieldData22),colnames(FieldData21))],
               FieldData23[,match(colnames(FieldData22),colnames(FieldData23))])

FieldAll$GPS[is.na(FieldAll$GPS)]="none"
FieldAll$lat=FieldAll$lon=NA
list_gps=list(GPS1=GPS1_All,GPS2=GPS2_All,
              GPS3=GPS3_All,GPS4=GPS4_All,
              GPS5=GPS5_All,GPSPhone=GPSPhone)
namevec=c(1,2,3,4,5)
for(i in 1:length(FieldAll$X)){
  for(v in 1:5){
     if(FieldAll$GPS[i]==namevec[v]){
    FieldAll$lon[i]=ifelse(is.na(min(list_gps[[v]]$time[list_gps[[v]]$time>FieldAll$Start.Time[i]])),NA,
                                 unlist(list_gps[[v]]$geometry[list_gps[[v]]$time==min(list_gps[[v]]$time[list_gps[[v]]$time>FieldAll$Start.Time[i]])])[1])
    FieldAll$lat[i]=ifelse(is.na(min(list_gps[[v]]$time[list_gps[[v]]$time>FieldAll$Start.Time[i]])),NA,
                                 unlist(list_gps[[v]]$geometry[list_gps[[v]]$time==min(list_gps[[v]]$time[list_gps[[v]]$time>FieldAll$Start.Time[i]])])[2])
    }
  }
     if(FieldAll$GPS[i]=="phone"|FieldAll$GPS[i]=="Phone"){
       FieldAll$lon[i]=ifelse(is.na(min(GPSPhone$time[GPSPhone$time>FieldAll$Start.Time[i]])),NA,
                              unlist(GPSPhone$geometry[GPSPhone$time==min(GPSPhone$time[GPSPhone$time>FieldAll$Start.Time[i]])])[1])
  FieldAll$lat[i]=ifelse(is.na(min(GPSPhone$time[GPSPhone$time>FieldAll$Start.Time[i]])),NA,
    unlist(GPSPhone$geometry[GPSPhone$time==min(GPSPhone$time[GPSPhone$time>FieldAll$Start.Time[i]])])[2])
  }
}
hour(round_date(as.POSIXct(FieldAll$Start.Time[1]),"hours"))
as.data.frame(weather_history(location=TestPoints[1,],start =GPS1_21$datetime[1],
                end = GPS1_21$datetime[5],hourly = "temperature_2m"))
hour(round_date(ymd_hms(paste0(substring(GPS1_21$time[1],first=0,last = 10),
               substring(GPS1_21$time[1],first = 12,last=19))),unit = "hours"))
FieldAll$Temp=FieldAll$Precip=FieldAll$cloudcover= NA
for(i in 1:length(FieldAll$X)){
  if(!is.na(FieldAll$lat[i])){
    tempweather=as.data.frame(weather_history(
                       location=c(FieldAll[i,25],FieldAll[i,24]),
                       start =FieldAll$Date[i],
                       end = FieldAll$Date[i],
                       hourly = c("temperature_2m","cloudcover"),daily = "precipitation_sum"))
    
    FieldAll$Temp[i]=tempweather[hour(round_date(as.POSIXct(FieldAll$Start.Time[i]),"hours"))+1,4]
    FieldAll$cloudcover[i]=tempweather[hour(round_date(as.POSIXct(FieldAll$Start.Time[i]),"hours"))+1,5]
    FieldAll$Precip[i]=tempweather[1,3]
  }
  print(i/length(FieldAll$X))
}
FieldAll=FieldAll[,1:28]
write.csv(FieldAll,file="Analyses and R Files/FieldDataWithLC_Weather.csv")
