library(plotKML)
library(dplyr)
library(lubridate)
library(readxl)
library(data.table)
getwd()
###Clearing all leftover vars
rm(list=ls())
###Setting working directory so R knows to look at this season's data
#setwd("C:/Users/John Crockett/Documents/URI/Data from Field Work/Summer 2021")
setwd("C:/Users/John Crockett/Documents/URI/Data from Field Work/2022 Data")
###Importing the field data and getting R to recognize the time and date as such
#FieldData=data.frame(read_xlsx("Summer 2021 All watersheds.xlsx"))
FieldData=data.frame(read_xlsx("2022 Field Data_withSummer.xlsx"))

FieldData$Newtime=ymd_hms(paste0(FieldData$Date,substring(FieldData$Time,first = 12)))

###Reading in the data from all GPS units used in the season
###Some seasons will have more of these
#GPS1=read.csv(file = "Tracks/GPS1/GPS1.csv")
GPS2=read.csv(file = "Tracks/GPS2/GPS2.csv")
#GPS3=read.csv(file = "Tracks/GPS3/GPS3.csv")
GPS4=read.csv(file = "Tracks/GPS4/GPS4.csv")
GPS5=read.csv(file = "Tracks/GPS5/GPS5.csv")

###If the GPS was a phone, we will ignore it for now
FieldData$GPS[is.na(FieldData$GPS)]=0

###Getting GPS Dates+Times into the same format
GPS1$datetime=ymd_hms(GPS1$datetime)
GPS2$datetime=ymd_hms(GPS2$datetime)
GPS3$datetime=ymd_hms(GPS3$datetime)
GPS4$datetime=ymd_hms(GPS4$datetime)
GPS5$datetime=ymd_hms(GPS5$datetime)

GPS1$datetime=round_date(GPS1$datetime, unit = "minute")
GPS2$datetime=round_date(GPS2$datetime, unit = "minute")
GPS3$datetime=round_date(GPS3$datetime, unit = "minute")
GPS4$datetime=round_date(GPS4$datetime, unit = "minute")
GPS5$datetime=round_date(GPS5$datetime, unit = "minute")
FieldData$Newtime=round_date(FieldData$Newtime,unit="minute")

###GPS time readings are off by 5 hrs (winter) or 4 hrs (summer)
hour(GPS1$datetime)=hour(GPS1$datetime)-5
hour(GPS2$datetime)=hour(GPS2$datetime)-5
hour(GPS3$datetime)=hour(GPS3$datetime)-5
hour(GPS4$datetime)=hour(GPS4$datetime)-5
hour(GPS5$datetime)=hour(GPS5$datetime)-5

###Subset the data by which GPS was used
Field1=FieldData[which(FieldData$GPS==1),]
Field2=FieldData[which(FieldData$GPS==2),]
Field3=FieldData[which(FieldData$GPS==3),]
Field4=FieldData[which(FieldData$GPS==4),]
Field5=FieldData[which(FieldData$GPS==5),]

###Now that our times for the GPS locations are to the nearest
###   minute, keep only the first one to avoid having too many
GPS1.1=GPS1[!duplicated(GPS1$datetime),]
GPS2.1=GPS2[!duplicated(GPS2$datetime),]
GPS3.1=GPS3[!duplicated(GPS3$datetime),]
GPS4.1=GPS4[!duplicated(GPS4$datetime),]
GPS5.1=GPS5[!duplicated(GPS5$datetime),]

####Merge the locations to the observations by time
merge1=merge.data.frame(x=Field1,y=GPS1.1,by.x="Newtime", by.y="datetime",all.x = TRUE)
merge2=merge.data.frame(x=Field2,y=GPS2.1,by.x="Newtime", by.y="datetime",all.x = TRUE)
merge3=merge.data.frame(x=Field3,y=GPS3.1,by.x="Newtime", by.y="datetime",all.x = TRUE)
merge4=merge.data.frame(x=Field4,y=GPS4.1,by.x="Newtime", by.y="datetime",all.x = TRUE)
merge5=merge.data.frame(x=Field5,y=GPS5.1,by.x="Newtime", by.y="datetime",all.x = TRUE)


FieldNew=rbind(merge1,merge2,merge3,merge4,merge5)
FieldNew=data.frame(FieldNew)
getwd()
write.csv(x=FieldNew,file="FieldDataWithCoords.csv")



######
#Merging Summer and Winter
#####
setwd("C:/Users/John Crockett/Documents/URI/Data from Field Work")
winter=read.csv("Winter 2020 2021/FieldDataWithCoordsWinter2021.csv")
summer=read.csv("Summer 2021/FieldDataWithCoords.csv")
winter=winter[,-(c(21,19,18,17,16))]
summer=summer[,-c(20,19,18,16)]
both=rbind(winter,summer)
both=both[which(both$Spp=="Beaver"|both$Spp=="beaver"|both$Spp=="beaver?"),]
library(sf)
both=both[which(is.na(both$lon)==F),]
bothpoints=st_as_sf(both,coords = c("lon","lat"))
st_write(obj=bothpoints,dsn = "FieldDataWithCoords2021.shp",append=F)
unique(both$Date)
unique(both$Site.ID[which(is.na(both$lon))])
st_crs(bothpoints)
