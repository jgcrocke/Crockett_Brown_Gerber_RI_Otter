# This will be where we join the tracks to the landcover layer
#1. First we want to import the tracks, which we can do by modifying the 
#     "add coords to data" script.
#2. Then we limit the tracks to the time we said we were surveying
#3. Then we want to import the landcover from 2020.
#4. Then I guess calculate how much of the track is within each landcover
#     class, given that it is in a site. Use as a new observer-varying 
#     covariate in the big model. Hopefully it all functions!
library(plotKML)
library(dplyr)
library(lubridate)
library(readxl)
library(data.table)
library(sf)
rm(list=ls())
setwd("C:/Users/John Crockett/Documents/URI")
#Load in Field Data
FieldData=data.frame(read_xlsx("Data from Field Work/2023 Data/2023 Data.xlsx"))
FieldData=FieldData[,-(20:25)]
#####
#GPS1
#####
#####
#Load Points,merge landcover
#GPS1_23=read.csv("Data from Field Work/2023 Data/Tracks/GPS 1/GPS1.csv")
#GPS1_23=st_as_sf(GPS1_23,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPS1_23)=st_crs(LC2020)
#mat2=st_intersects(GPS1_23,LC2020)
#GPS1_23$LC2020=NA
#for(i in 1:length(mat2)){
#  GPS1_23$LC2020[i]=LC2020$Descr_2020[mat2[[i]]]
#}
##Keep one point per minute, given that points have landcover
#GPS1_23=GPS1_23[!is.na(GPS1_23$LC2020),]
#GPS1_23$datetime=round_date(as_datetime(GPS1_23$datetime), unit = "minute")
#GPS1_23=GPS1_23[!duplicated(GPS1_23$datetime),]
#####
#Save the GPS layer again so you don't have to re-do the hard part
#st_write(GPS1_23,dsn = "Data from Field Work/2023 Data/Tracks/GPS 1/GPS1_withLC.shp",append = F)
GPS1_23=st_read("Data from Field Work/2023 Data/Tracks/GPS 1/GPS1_withLC.shp")
#####
#rm(mat2)
#rm(LC2020)
FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2023-03-12"])=tz(FieldData$End.time[FieldData$Date<"2023-03-12"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2023-03-12"])=tz(FieldData$End.time[FieldData$Date>"2023-03-12"])="EDT"
#FieldData$End.time[256:261]=FieldData$End.time[262]
GPS1_23$time=as_datetime(GPS1_23$time)
GPS1_23$time[GPS1_23$datetime<"2023-03-12"]=GPS1_23$time[GPS1_23$datetime<"2023-03-12"]-hours(5)
GPS1_23$time[GPS1_23$datetime>"2023-03-12"]=GPS1_23$time[GPS1_23$datetime>"2023-03-12"]-hours(4)

#tz(GPS1_23$time[GPS1_23$datetime<"2023-03-12"])="EST";tz(GPS1_23$time[GPS1_23$datetime>"2023-03-12"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2,3,5)]))),]
unqGPSSites=uniqsites[uniqsites$GPS==1,]
unqGPSSites$flag=NA
for(i in 1:length(unqGPSSites$Site.ID)){
  unqGPSSites$flag[i]=length(GPS1_23$LC2020[
    GPS1_23$time>unqGPSSites$Start.Time[i]&
      GPS1_23$time<unqGPSSites$End.time[i]])
}
unqGPSSites[unqGPSSites$flag==0,c(2,3,5,7,8,16)]
#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
#Double check which landcovers you need to account for
unique(GPS1_23$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPS1_23$LC2020[(GPS1_23$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPS1_23$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPS1_23$LC2020=="Mixed Forest")&
                                         GPS1_23$time<unqGPSSites$End.time[i]&
                                         GPS1_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_23$LC2020[GPS1_23$time<unqGPSSites$End.time[i]&
                            GPS1_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPS1_23$LC2020[GPS1_23$LC2020=="Wetland"&
                                         GPS1_23$time<unqGPSSites$End.time[i]&
                                         GPS1_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_23$LC2020[GPS1_23$time<unqGPSSites$End.time[i]&
                            GPS1_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPS1_23$LC2020[(GPS1_23$LC2020=="Developed Recreation (all recreation)"|
                                         GPS1_23$LC2020=="High Density Residential (<1/8 acre lots)"|
                                         GPS1_23$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPS1_23$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPS1_23$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPS1_23$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPS1_23$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPS1_23$LC2020=="Railroads (and associated facilities)"                 |
                                         GPS1_23$LC2020=="Commercial (sale of products and services)"            |
                                         GPS1_23$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPS1_23$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPS1_23$LC2020=="Vacant Land"                                           |
                                         GPS1_23$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPS1_23$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPS1_23$LC2020=="Mines, Quarries and Gravel Pits")&
                                        GPS1_23$time<unqGPSSites$End.time[i]&
                                        GPS1_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_23$LC2020[GPS1_23$time<unqGPSSites$End.time[i]&
                            GPS1_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPS1_23$LC2020[GPS1_23$LC2020=="Water"&
                                         GPS1_23$time<unqGPSSites$End.time[i]&
                                         GPS1_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_23$LC2020[GPS1_23$time<unqGPSSites$End.time[i]&
                            GPS1_23$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPS1_23$LC2020[(GPS1_23$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPS1_23$LC2020=="Cemeteries"|
                                           GPS1_23$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPS1_23$LC2020=="Transitional Areas (urban open)"|
                                           GPS1_23$LC2020=="Cropland (tillable)")&
                                          GPS1_23$time<unqGPSSites$End.time[i]&
                                          GPS1_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_23$LC2020[GPS1_23$time<unqGPSSites$End.time[i]&
                            GPS1_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPS1_23$LC2020[(GPS1_23$LC2020=="Power Lines (100' or more width)"|
                                        GPS1_23$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                       GPS1_23$time<unqGPSSites$End.time[i]&
                                       GPS1_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_23$LC2020[GPS1_23$time<unqGPSSites$End.time[i]&
                            GPS1_23$time>unqGPSSites$Start.Time[i]])
}
#Add the new LC data to the field data
FieldData$Forest=FieldData$Shrub=FieldData$UpGrass=FieldData$Built=
 FieldData$Wetland=FieldData$Water=NA
for(i in 1:length(LCDF$Site)){
  FieldData$Forest[FieldData$GPS==1&
                     FieldData$Site.ID==LCDF$Site[i]&
                     FieldData$Date==LCDF$Date[i]]=LCDF$Forest[i]
   FieldData$Shrub[FieldData$GPS==1&
                     FieldData$Site.ID==LCDF$Site[i]&
                     FieldData$Date==LCDF$Date[i]]=LCDF$Shrub[i]
  FieldData$Wetland[FieldData$GPS==1&
                     FieldData$Site.ID==LCDF$Site[i]&
                     FieldData$Date==LCDF$Date[i]]=LCDF$Wetland[i]
  FieldData$Built[FieldData$GPS==1&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Built[i]
  FieldData$UpGrass[FieldData$GPS==1&
                      FieldData$Site.ID==LCDF$Site[i]&
                      FieldData$Date==LCDF$Date[i]]=LCDF$UpGrass[i]
  FieldData$Water[FieldData$GPS==1&
                     FieldData$Site.ID==LCDF$Site[i]&
                     FieldData$Date==LCDF$Date[i]]=LCDF$Water[i]
}

#LCDF[LCDF$Date<"2023-03-12"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
#LCDF[LCDF$Date>"2023-03-12"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
#####
#GPS2
#####
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
#####
#rm(mat2)
#rm(GPS1_23)
#GPS2_23=read.csv("Data from Field Work/2023 Data/Tracks/GPS 2/GPS2.csv")
#GPS2_23=st_as_sf(GPS2_23,coords=c(2,3))
###Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPS2_23)=st_crs(LC2020)
#mat2=st_intersects(GPS2_23,LC2020)
#GPS2_23$LC2020=NA
#for(i in 1:length(mat2)){
#  GPS2_23$LC2020[i]=LC2020$Descr_2020[mat2[[i]]]
#}
##Keep one point per minute, given that points have landcover
#GPS2_23=GPS2_23[!is.na(GPS2_23$LC2020),]
#GPS2_23$datetime=round_date(as_datetime(GPS2_23$datetime), unit = "minute")
#GPS2_23=GPS2_23[!duplicated(GPS2_23$datetime),]
#####
##Save the GPS layer again so you don't have to re-do the hard part
######
#st_write(GPS2_23,dsn = "Data from Field Work/2023 Data/Tracks/GPS 2/GPS2_withLC.shp",append = F)
#####
GPS2_23=st_read("Data from Field Work/2023 Data/Tracks/GPS 2/GPS2_withLC.shp")
#rm(mat2)
#rm(LC2020)
#FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
#FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2023-03-12"])=tz(FieldData$End.time[FieldData$Date<"2023-03-12"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2023-03-12"])=tz(FieldData$End.time[FieldData$Date>"2023-03-12"])="EDT"
#FieldData$End.time[256:261]=FieldData$End.time[262]
GPS2_23$time=as_datetime(GPS2_23$time)
GPS2_23$time[GPS2_23$datetime<"2023-03-12"]=GPS2_23$time[GPS2_23$datetime<"2023-03-12"]-hours(5)
GPS2_23$time[GPS2_23$datetime>"2023-03-12"]=GPS2_23$time[GPS2_23$datetime>"2023-03-12"]-hours(4)

#tz(GPS2_23$time[GPS2_23$datetime<"2023-03-12"])="EST";tz(GPS2_23$time[GPS2_23$datetime>"2023-03-12"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2,3,5)]))),]
unqGPSSites=uniqsites[uniqsites$GPS==2,]
unqGPSSites$flag=NA
for(i in 1:length(unqGPSSites$Site.ID)){
  unqGPSSites$flag[i]=length(GPS2_23$LC2020[
    GPS2_23$time>unqGPSSites$Start.Time[i]&
      GPS2_23$time<unqGPSSites$End.time[i]])
}
unqGPSSites[unqGPSSites$flag==0,c(2,3,5,7,8,16)]

#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
unique(GPS2_23$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPS2_23$LC2020[(GPS2_23$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPS2_23$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPS2_23$LC2020=="Mixed Forest")&
                                         GPS2_23$time<unqGPSSites$End.time[i]&
                                         GPS2_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_23$LC2020[GPS2_23$time<unqGPSSites$End.time[i]&
                            GPS2_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPS2_23$LC2020[GPS2_23$LC2020=="Wetland"&
                                          GPS2_23$time<unqGPSSites$End.time[i]&
                                          GPS2_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_23$LC2020[GPS2_23$time<unqGPSSites$End.time[i]&
                            GPS2_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPS2_23$LC2020[(GPS2_23$LC2020=="Developed Recreation (all recreation)"|
                                         GPS2_23$LC2020=="High Density Residential (<1/8 acre lots)"|
                                         GPS2_23$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPS2_23$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPS2_23$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPS2_23$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPS2_23$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPS2_23$LC2020=="Railroads (and associated facilities)"                 |
                                         GPS2_23$LC2020=="Commercial (sale of products and services)"            |
                                         GPS2_23$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPS2_23$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPS2_23$LC2020=="Vacant Land"                                           |
                                         GPS2_23$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPS2_23$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPS2_23$LC2020=="Mines, Quarries and Gravel Pits"|
                                         GPS2_23$LC2020=="Other Transportation (terminals, docks, etc.)")&
                                        GPS2_23$time<unqGPSSites$End.time[i]&
                                        GPS2_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_23$LC2020[GPS2_23$time<unqGPSSites$End.time[i]&
                            GPS2_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPS2_23$LC2020[GPS2_23$LC2020=="Water"&
                                        GPS2_23$time<unqGPSSites$End.time[i]&
                                        GPS2_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_23$LC2020[GPS2_23$time<unqGPSSites$End.time[i]&
                            GPS2_23$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPS2_23$LC2020[(GPS2_23$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPS2_23$LC2020=="Cemeteries"|
                                           GPS2_23$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPS2_23$LC2020=="Transitional Areas (urban open)"|
                                           GPS2_23$LC2020=="Cropland (tillable)")&
                                          GPS2_23$time<unqGPSSites$End.time[i]&
                                          GPS2_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_23$LC2020[GPS2_23$time<unqGPSSites$End.time[i]&
                            GPS2_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPS2_23$LC2020[(GPS2_23$LC2020=="Power Lines (100' or more width)"|
                                         GPS2_23$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPS2_23$time<unqGPSSites$End.time[i]&
                                        GPS2_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_23$LC2020[GPS2_23$time<unqGPSSites$End.time[i]&
                            GPS2_23$time>unqGPSSites$Start.Time[i]])
}
#Add the new LC data to the field data
#FieldData$Forest=FieldData$Shrub=FieldData$UpGrass=FieldData$Built=
#  FieldData$Wetland=FieldData$Water=NA
for(i in 1:length(LCDF$Site)){
  FieldData$Forest[FieldData$GPS==2&
                     FieldData$Site.ID==LCDF$Site[i]&
                     FieldData$Date==LCDF$Date[i]]=LCDF$Forest[i]
  FieldData$Shrub[FieldData$GPS==2&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Shrub[i]
  FieldData$Wetland[FieldData$GPS==2&
                      FieldData$Site.ID==LCDF$Site[i]&
                      FieldData$Date==LCDF$Date[i]]=LCDF$Wetland[i]
  FieldData$Built[FieldData$GPS==2&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Built[i]
  FieldData$UpGrass[FieldData$GPS==2&
                      FieldData$Site.ID==LCDF$Site[i]&
                      FieldData$Date==LCDF$Date[i]]=LCDF$UpGrass[i]
  FieldData$Water[FieldData$GPS==2&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Water[i]
}
LCDF[LCDF$Date<"2023-03-12"&is.na(LCDF$Water)==1,c(1,2,4)]
LCDF[LCDF$Date>"2023-03-12"&is.na(LCDF$Water)==1,c(1,2,4)]


#####
#GPS3
#####
#####
#Load Points, merge landcover
#####
#rm(GPS2_23)
#rm(mat2)
#GPS3_23=read.csv("Data from Field Work/2023 Data/Tracks/GPS 3/GPS3.csv")
#GPS3_23=st_as_sf(GPS3_23,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPS3_23)=st_crs(LC2020)
#mat2=st_intersects(GPS3_23,LC2020)
#GPS3_23$LC2020=NA
#for(i in 1:length(mat2)){
#  GPS3_23$LC2020[i]=LC2020$Descr_2020[mat2[[i]]]
#}
##Keep one point per minute, given that points have landcover
#GPS3_23=GPS3_23[!is.na(GPS3_23$LC2020),]
#GPS3_23$datetime=round_date(as_datetime(GPS3_23$datetime), unit = "minute")
#GPS3_23=GPS3_23[!duplicated(GPS3_23$datetime),]
#####
##Save the GPS layer again so you don't have to re-do the hard part
######
#st_write(GPS3_23,dsn = "Data from Field Work/2023 Data/Tracks/GPS 3/GPS3_withLC.shp",append = F)
GPS3_23=st_read("Data from Field Work/2023 Data/Tracks/GPS 3/GPS3_withLC.shp")
#####
#rm(mat2)
#rm(LC2020)
#FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
#FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2023-03-12"])=tz(FieldData$End.time[FieldData$Date<"2023-03-12"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2023-03-12"])=tz(FieldData$End.time[FieldData$Date>"2023-03-12"])="EDT"
#FieldData$End.time[256:261]=FieldData$End.time[262]
GPS3_23$time=as_datetime(GPS3_23$time)
GPS3_23$time[GPS3_23$datetime<"2023-03-12"]=GPS3_23$time[GPS3_23$datetime<"2023-03-12"]-hours(5)
GPS3_23$time[GPS3_23$datetime>"2023-03-12"]=GPS3_23$time[GPS3_23$datetime>"2023-03-12"]-hours(4)

#tz(GPS3_23$time[GPS3_23$datetime<"2023-03-12"])="EST";tz(GPS3_23$time[GPS3_23$datetime>"2023-03-12"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2,3,5)]))),]
unqGPSSites=uniqsites[uniqsites$GPS==3,]
unqGPSSites$flag=NA
for(i in 1:length(unqGPSSites$Site.ID)){
  unqGPSSites$flag[i]=length(GPS3_23$LC2020[
    GPS3_23$time>unqGPSSites$Start.Time[i]&
      GPS3_23$time<unqGPSSites$End.time[i]])
}
unqGPSSites[unqGPSSites$flag==0,c(2,3,5,7,8,16)]

#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
unique(GPS3_23$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPS3_23$LC2020[(GPS3_23$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPS3_23$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPS3_23$LC2020=="Mixed Forest")&
                                         GPS3_23$time<unqGPSSites$End.time[i]&
                                         GPS3_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_23$LC2020[GPS3_23$time<unqGPSSites$End.time[i]&
                            GPS3_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPS3_23$LC2020[GPS3_23$LC2020=="Wetland"&
                                          GPS3_23$time<unqGPSSites$End.time[i]&
                                          GPS3_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_23$LC2020[GPS3_23$time<unqGPSSites$End.time[i]&
                            GPS3_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPS3_23$LC2020[(GPS3_23$LC2020=="Developed Recreation (all recreation)"|
                                         GPS3_23$LC2020=="High Density Residential (<1/8 acre lots)"|
                                         GPS3_23$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPS3_23$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPS3_23$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPS3_23$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPS3_23$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPS3_23$LC2020=="Railroads (and associated facilities)"                 |
                                         GPS3_23$LC2020=="Commercial (sale of products and services)"            |
                                         GPS3_23$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPS3_23$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPS3_23$LC2020=="Vacant Land"                                           |
                                         GPS3_23$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPS3_23$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPS3_23$LC2020=="Mines, Quarries and Gravel Pits"|
                                         GPS3_23$LC2020=="Other Transportation (terminals, docks, etc.)")&
                                        GPS3_23$time<unqGPSSites$End.time[i]&
                                        GPS3_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_23$LC2020[GPS3_23$time<unqGPSSites$End.time[i]&
                            GPS3_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPS3_23$LC2020[GPS3_23$LC2020=="Water"&
                                        GPS3_23$time<unqGPSSites$End.time[i]&
                                        GPS3_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_23$LC2020[GPS3_23$time<unqGPSSites$End.time[i]&
                            GPS3_23$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPS3_23$LC2020[(GPS3_23$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPS3_23$LC2020=="Cemeteries"|
                                           GPS3_23$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPS3_23$LC2020=="Transitional Areas (urban open)"|
                                           GPS3_23$LC2020=="Cropland (tillable)")&
                                          GPS3_23$time<unqGPSSites$End.time[i]&
                                          GPS3_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_23$LC2020[GPS3_23$time<unqGPSSites$End.time[i]&
                            GPS3_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPS3_23$LC2020[(GPS3_23$LC2020=="Power Lines (100' or more width)"|
                                         GPS3_23$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPS3_23$time<unqGPSSites$End.time[i]&
                                        GPS3_23$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_23$LC2020[GPS3_23$time<unqGPSSites$End.time[i]&
                            GPS3_23$time>unqGPSSites$Start.Time[i]])
}
#Add the new LC data to the field data
#FieldData$Forest=FieldData$Shrub=FieldData$UpGrass=FieldData$Built=
#  FieldData$Wetland=FieldData$Water=NA
for(i in 1:length(LCDF$Site)){
  FieldData$Forest[FieldData$GPS==3&
                     FieldData$Site.ID==LCDF$Site[i]&
                     FieldData$Date==LCDF$Date[i]]=LCDF$Forest[i]
  FieldData$Shrub[FieldData$GPS==3&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Shrub[i]
  FieldData$Wetland[FieldData$GPS==3&
                      FieldData$Site.ID==LCDF$Site[i]&
                      FieldData$Date==LCDF$Date[i]]=LCDF$Wetland[i]
  FieldData$Built[FieldData$GPS==3&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Built[i]
  FieldData$UpGrass[FieldData$GPS==3&
                      FieldData$Site.ID==LCDF$Site[i]&
                      FieldData$Date==LCDF$Date[i]]=LCDF$UpGrass[i]
  FieldData$Water[FieldData$GPS==3&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Water[i]
}
LCDF[LCDF$Date<"2023-03-12"&is.na(LCDF$Water)==1,c(1,2,4)]
LCDF[LCDF$Date>"2023-03-12"&is.na(LCDF$Water)==1,c(1,2,4)]

#####
#####
#GPS Phone
#####
#####
#Load Points, merge landcover
#####
#rm(GPSPhone_23)
#rm(mat2)
#GPSPhone_23=read.csv("Data from Field Work/Phone Tracks/GPSPhone.csv")
#GPSPhone_23=st_as_sf(GPSPhone_23,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2022.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#GPSPhone_23=GPSPhone_23[GPSPhone_23$datetime>"2023-01-01",]
#st_crs(GPSPhone_23)=st_crs(LC2020)
#mat2=st_intersects(GPSPhone_23,LC2020)
#GPSPhone_23$LC2020=NA
#for(i in 1:length(mat2)){
#  GPSPhone_23$LC2020[i]=ifelse(length(mat2[[i]])>0,
#                               LC2020$Descr_2020[mat2[[i]]],
#                               NA)
#}
##Keep one point per minute, given that points have landcover
#GPSPhone_23=GPSPhone_23[!is.na(GPSPhone_23$LC2020),]
#GPSPhone_23$datetime=round_date(as_datetime(GPSPhone_23$datetime), unit = "minute")
#GPSPhone_23=GPSPhone_23[!duplicated(GPSPhone_23$datetime),]
#####
##Save the GPS layer again so you don't have to re-do the hard part
######
#st_write(GPSPhone_23,dsn = "Data from Field Work/Phone Tracks/GPSPhone_withLC.shp",append = F)
GPSPhone_23=st_read("Data from Field Work/Phone Tracks/GPSPhone_withLC.shp")
GPSPhone_23$time=as_datetime(GPSPhone_23$time)
GPSPhone_23$time[GPSPhone_23$datetime<"2023-03-12"]=GPSPhone_23$time[GPSPhone_23$datetime<"2023-03-12"]-hours(5)
GPSPhone_23$time[GPSPhone_23$datetime>"2023-03-12"]=GPSPhone_23$time[GPSPhone_23$datetime>"2023-03-12"]-hours(4)
uniqsites$GPS[uniqsites$GPS=="Phone"]="phone"
unqGPSSites=uniqsites[uniqsites$GPS=="phone",]
#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
unique(GPSPhone_23$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPSPhone_23$LC2020[(GPSPhone_23$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPSPhone_23$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPSPhone_23$LC2020=="Mixed Forest")&
                                         GPSPhone_23$time<unqGPSSites$End.time[i]&
                                         GPSPhone_23$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_23$LC2020[GPSPhone_23$time<unqGPSSites$End.time[i]&
                            GPSPhone_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPSPhone_23$LC2020[GPSPhone_23$LC2020=="Wetland"&
                                          GPSPhone_23$time<unqGPSSites$End.time[i]&
                                          GPSPhone_23$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_23$LC2020[GPSPhone_23$time<unqGPSSites$End.time[i]&
                            GPSPhone_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPSPhone_23$LC2020[(GPSPhone_23$LC2020=="Developed Recreation (all recreation)"|
                                         GPSPhone_23$LC2020=="High Density Residential (<1/8 acre lots)"|
                                         GPSPhone_23$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPSPhone_23$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPSPhone_23$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPSPhone_23$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPSPhone_23$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPSPhone_23$LC2020=="Railroads (and associated facilities)"                 |
                                         GPSPhone_23$LC2020=="Commercial (sale of products and services)"            |
                                         GPSPhone_23$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPSPhone_23$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPSPhone_23$LC2020=="Vacant Land"                                           |
                                         GPSPhone_23$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPSPhone_23$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPSPhone_23$LC2020=="Mines, Quarries and Gravel Pits"|
                                         GPSPhone_23$LC2020=="Other Transportation (terminals, docks, etc.)")&
                                        GPSPhone_23$time<unqGPSSites$End.time[i]&
                                        GPSPhone_23$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_23$LC2020[GPSPhone_23$time<unqGPSSites$End.time[i]&
                            GPSPhone_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPSPhone_23$LC2020[GPSPhone_23$LC2020=="Water"&
                                        GPSPhone_23$time<unqGPSSites$End.time[i]&
                                        GPSPhone_23$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_23$LC2020[GPSPhone_23$time<unqGPSSites$End.time[i]&
                            GPSPhone_23$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPSPhone_23$LC2020[(GPSPhone_23$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPSPhone_23$LC2020=="Cemeteries"|
                                           GPSPhone_23$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPSPhone_23$LC2020=="Transitional Areas (urban open)"|
                                           GPSPhone_23$LC2020=="Cropland (tillable)")&
                                          GPSPhone_23$time<unqGPSSites$End.time[i]&
                                          GPSPhone_23$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_23$LC2020[GPSPhone_23$time<unqGPSSites$End.time[i]&
                            GPSPhone_23$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPSPhone_23$LC2020[(GPSPhone_23$LC2020=="Power Lines (100' or more width)"|
                                         GPSPhone_23$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPSPhone_23$time<unqGPSSites$End.time[i]&
                                        GPSPhone_23$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_23$LC2020[GPSPhone_23$time<unqGPSSites$End.time[i]&
                            GPSPhone_23$time>unqGPSSites$Start.Time[i]])
}
#Add the new LC data to the field data
#FieldData$Forest=FieldData$Shrub=FieldData$UpGrass=FieldData$Built=
#  FieldData$Wetland=FieldData$Water=NA
for(i in 1:length(LCDF$Site)){
  FieldData$Forest[FieldData$GPS=="phone"&
                     FieldData$Site.ID==LCDF$Site[i]&
                     FieldData$Date==LCDF$Date[i]]=LCDF$Forest[i]
  FieldData$Shrub[FieldData$GPS=="phone"&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Shrub[i]
  FieldData$Wetland[FieldData$GPS=="phone"&
                      FieldData$Site.ID==LCDF$Site[i]&
                      FieldData$Date==LCDF$Date[i]]=LCDF$Wetland[i]
  FieldData$Built[FieldData$GPS=="phone"&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Built[i]
  FieldData$UpGrass[FieldData$GPS=="phone"&
                      FieldData$Site.ID==LCDF$Site[i]&
                      FieldData$Date==LCDF$Date[i]]=LCDF$UpGrass[i]
  FieldData$Water[FieldData$GPS=="phone"&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Water[i]
}



#####
#Misc
#####
dim(FieldData[as.numeric(row.names(unique(
  FieldData[is.na(FieldData$Water),c(2:4)]))),c(2:4,16)])
write.csv(x = FieldData,file = "Analyses and R Files/FieldDataWithLC2023.csv",append = F)
