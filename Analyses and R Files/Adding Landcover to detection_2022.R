# This will be where we join the tracks to the landcover layer
#1. First we want to import the tracks, which we can do by modifying the 
#     "add coords to data" script.
#2. Then we need the shapes of the sites. we should be able to clip the 
#     tracks to the sites... right?
#3. Then we want to import the landcover. Eventually this should be the 2020
#     landcover data, but I guess for now we can use the old one.
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
FieldData=data.frame(read_xlsx("Data from Field Work/2022 Data/2022 Field Data_withSummer.xlsx"))
#####                                 #####
                              #GPS2#
#####                                 #####
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
#####
#GPS2_22=read.csv("Data from Field Work/2022 Data/Tracks/GPS 2/GPS2.csv")
#GPS2_22=st_as_sf(GPS2_22,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPS2_22)=st_crs(LC2020)
#mat2=st_intersects(GPS2_22,LC2020)
#GPS2_22$LC2020=NA
#for(i in 1:length(GPS2_22)){
#  GPS2_22$LC2020[i]=ifelse(length(LC2020$Descr_2020[mat2[[i]]])>0,
#  LC2020$Descr_2020[mat2[[i]]],NA)
#}
##Keep one point per minute, given that points have landcover
#GPS2_22=GPS2_22[!is.na(GPS2_22$LC2020),]
#GPS2_22$datetime=round_date(as_datetime(GPS2_22$datetime), unit = "minute")
#GPS2_22=GPS2_22[!duplicated(GPS2_22$datetime),]
#####
##Save the GPS layer again so you don't have to re-do the hard part
######
#st_write(GPS2_22,dsn = "Data from Field Work/2022 Data/Tracks/GPS 2/GPS2_withLC.shp",append = F)
#####
GPS2_22=st_read("Data from Field Work/2022 Data/Tracks/GPS 2/GPS2_withLC.shp")
#rm(mat2)
#rm(LC2020)
FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2022-03-13"])=tz(FieldData$End.time[FieldData$Date<"2022-03-13"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2022-03-13"])=tz(FieldData$End.time[FieldData$Date>"2022-03-13"])="EDT"
FieldData$End.time[256:261]=FieldData$End.time[262]
GPS2_22$time=as_datetime(GPS2_22$time)
GPS2_22$time[GPS2_22$datetime<"2022-03-13"]=GPS2_22$time[GPS2_22$datetime<"2022-03-13"]-hours(5)
GPS2_22$time[GPS2_22$datetime>"2022-03-13"]=GPS2_22$time[GPS2_22$datetime>"2022-03-13"]-hours(4)

#tz(GPS2_22$time[GPS2_22$datetime<"2022-03-13"])="EST";tz(GPS2_22$time[GPS2_22$datetime>"2022-03-13"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2:4)]))),]
unqGPSSites=uniqsites[uniqsites$GPS==2,]
#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
#Double check which landcovers you need to account for
unique(GPS2_22$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPS2_22$LC2020[(GPS2_22$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPS2_22$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPS2_22$LC2020=="Mixed Forest"|
                                          GPS2_22$LC2020=="Orchards, Groves, Nurseries")&
                                         GPS2_22$time<unqGPSSites$End.time[i]&
                                         GPS2_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_22$LC2020[GPS2_22$time<unqGPSSites$End.time[i]&
                            GPS2_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPS2_22$LC2020[GPS2_22$LC2020=="Wetland"&
                                          GPS2_22$time<unqGPSSites$End.time[i]&
                                          GPS2_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_22$LC2020[GPS2_22$time<unqGPSSites$End.time[i]&
                            GPS2_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPS2_22$LC2020[(GPS2_22$LC2020=="Developed Recreation (all recreation)"|
                                         GPS2_22$LC2020=="High Density Residential (<1/8 acre lots)"|
                                         GPS2_22$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPS2_22$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPS2_22$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPS2_22$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPS2_22$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPS2_22$LC2020=="Railroads (and associated facilities)"                 |
                                         GPS2_22$LC2020=="Commercial (sale of products and services)"            |
                                         GPS2_22$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPS2_22$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPS2_22$LC2020=="Vacant Land"                                           |
                                         GPS2_22$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPS2_22$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPS2_22$LC2020=="Mines, Quarries and Gravel Pits"|
                                         GPS2_22$LC2020=="Other Transportation (terminals, docks, etc.)")&
                                        GPS2_22$time<unqGPSSites$End.time[i]&
                                        GPS2_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_22$LC2020[GPS2_22$time<unqGPSSites$End.time[i]&
                            GPS2_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPS2_22$LC2020[GPS2_22$LC2020=="Water"&
                                        GPS2_22$time<unqGPSSites$End.time[i]&
                                        GPS2_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_22$LC2020[GPS2_22$time<unqGPSSites$End.time[i]&
                            GPS2_22$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPS2_22$LC2020[(GPS2_22$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPS2_22$LC2020=="Cemeteries"|
                                           GPS2_22$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPS2_22$LC2020=="Transitional Areas (urban open)"|
                                           GPS2_22$LC2020=="Cropland (tillable)")&
                                          GPS2_22$time<unqGPSSites$End.time[i]&
                                          GPS2_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_22$LC2020[GPS2_22$time<unqGPSSites$End.time[i]&
                            GPS2_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPS2_22$LC2020[(GPS2_22$LC2020=="Power Lines (100' or more width)"|
                                         GPS2_22$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPS2_22$time<unqGPSSites$End.time[i]&
                                        GPS2_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_22$LC2020[GPS2_22$time<unqGPSSites$End.time[i]&
                            GPS2_22$time>unqGPSSites$Start.Time[i]])
}
#Add the new LC data to the field data
FieldData$Forest=FieldData$Shrub=FieldData$UpGrass=FieldData$Built=
  FieldData$Wetland=FieldData$Water=NA
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
LCDF[LCDF$Date<"2022-03-13"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
LCDF[LCDF$Date>"2022-03-13"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]

#####
                                  #GPS4
#####
#####
#Load Points,merge landcover
#rm(FieldGPS)
#rm(GPS2_22)
#rm(mat2)
#rm(unqGPSSites)
#GPS4_22=read.csv("Data from Field Work/2022 Data/Tracks/GPS 4/GPS4.csv")
#GPS4_22=st_as_sf(GPS4_22,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPS4_22)=st_crs(LC2020)
#mat2=st_intersects(GPS4_22,LC2020)
#GPS4_22$LC2020=NA
#for(i in 1:length(GPS4_22$LC2020)){
#  GPS4_22$LC2020[i]=ifelse(length(LC2020$Descr_2020[mat2[[i]]])>0,
#                           LC2020$Descr_2020[mat2[[i]]],NA)
#}
##Keep one point per minute, given that points have landcover
#GPS4_22=GPS4_22[!is.na(GPS4_22$LC2020),]
#GPS4_22$datetime=round_date(as_datetime(GPS4_22$datetime), unit = "minute")
#GPS4_22=GPS4_22[!duplicated(GPS4_22$datetime),]
#######
###Save the GPS layer again so you don't have to re-do the hard part
#st_write(GPS4_22,dsn = "Data from Field Work/2022 Data/Tracks/GPS 4/GPS4_withLC.shp",append = F)
GPS4_22=st_read("Data from Field Work/2022 Data/Tracks/GPS 4/GPS4_withLC.shp")
#####
#rm(mat2)
#rm(LC2020)
#FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
#FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2022-03-13"])=tz(FieldData$End.time[FieldData$Date<"2022-03-13"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2022-03-13"])=tz(FieldData$End.time[FieldData$Date>"2022-03-13"])="EDT"
#FieldData$End.time[256:261]=FieldData$End.time[262]
GPS4_22$time=as_datetime(GPS4_22$time)
GPS4_22$time[GPS4_22$datetime<"2022-03-13"]=GPS4_22$time[GPS4_22$datetime<"2022-03-13"]-hours(5)
GPS4_22$time[GPS4_22$datetime>"2022-03-13"]=GPS4_22$time[GPS4_22$datetime>"2022-03-13"]-hours(4)

#tz(GPS4_22$time[GPS4_22$datetime<"2022-03-13"])="EST";tz(GPS4_22$time[GPS4_22$datetime>"2022-03-13"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2:4)]))),]
unqGPSSites=uniqsites[uniqsites$GPS==4,]
#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
#Double check which landcovers you need to account for
#names(GPS4_22)=c(names(GPS4_22)[1:4],"LC2020","geometry")
unique(GPS4_22$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPS4_22$LC2020[(GPS4_22$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPS4_22$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPS4_22$LC2020=="Mixed Forest"|
                                          GPS4_22$LC2020=="Orchards, Groves, Nurseries")&
                                         GPS4_22$time<unqGPSSites$End.time[i]&
                                         GPS4_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_22$LC2020[GPS4_22$time<unqGPSSites$End.time[i]&
                            GPS4_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPS4_22$LC2020[GPS4_22$LC2020=="Wetland"&
                                          GPS4_22$time<unqGPSSites$End.time[i]&
                                          GPS4_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_22$LC2020[GPS4_22$time<unqGPSSites$End.time[i]&
                            GPS4_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPS4_22$LC2020[(GPS4_22$LC2020=="Developed Recreation (all recreation)"|
                                         GPS4_22$LC2020=="High Density Residential (<1/8 acre lots)"|
                                         GPS4_22$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPS4_22$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPS4_22$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPS4_22$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPS4_22$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPS4_22$LC2020=="Railroads (and associated facilities)"                 |
                                         GPS4_22$LC2020=="Commercial (sale of products and services)"            |
                                         GPS4_22$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPS4_22$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPS4_22$LC2020=="Vacant Land"                                           |
                                         GPS4_22$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPS4_22$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPS4_22$LC2020=="Mines, Quarries and Gravel Pits"|
                                         GPS4_22$LC2020=="Other Transportation (terminals, docks, etc.)")&
                                        GPS4_22$time<unqGPSSites$End.time[i]&
                                        GPS4_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_22$LC2020[GPS4_22$time<unqGPSSites$End.time[i]&
                            GPS4_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPS4_22$LC2020[GPS4_22$LC2020=="Water"&
                                        GPS4_22$time<unqGPSSites$End.time[i]&
                                        GPS4_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_22$LC2020[GPS4_22$time<unqGPSSites$End.time[i]&
                            GPS4_22$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPS4_22$LC2020[(GPS4_22$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPS4_22$LC2020=="Cemeteries"|
                                           GPS4_22$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPS4_22$LC2020=="Transitional Areas (urban open)"|
                                           GPS4_22$LC2020=="Cropland (tillable)")&
                                          GPS4_22$time<unqGPSSites$End.time[i]&
                                          GPS4_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_22$LC2020[GPS4_22$time<unqGPSSites$End.time[i]&
                            GPS4_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPS4_22$LC2020[(GPS4_22$LC2020=="Power Lines (100' or more width)"|
                                         GPS4_22$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPS4_22$time<unqGPSSites$End.time[i]&
                                        GPS4_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_22$LC2020[GPS4_22$time<unqGPSSites$End.time[i]&
                            GPS4_22$time>unqGPSSites$Start.Time[i]])
}
#Add the new LC data to the field data
#FieldData$Forest=FieldData$FreshW=FieldData$UpGrass=FieldData$Built=
 # FieldData$Salt=FieldData$FSwamp=FieldData$SSwamp=FieldData$EmMarsh=NA
for(i in 1:length(LCDF$Site)){
  FieldData$Forest[FieldData$GPS==4&
                     FieldData$Site.ID==LCDF$Site[i]&
                     FieldData$Date==LCDF$Date[i]]=LCDF$Forest[i]
  FieldData$Shrub[FieldData$GPS==4&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Shrub[i]
  FieldData$Wetland[FieldData$GPS==4&
                      FieldData$Site.ID==LCDF$Site[i]&
                      FieldData$Date==LCDF$Date[i]]=LCDF$Wetland[i]
  FieldData$Built[FieldData$GPS==4&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Built[i]
  FieldData$UpGrass[FieldData$GPS==4&
                      FieldData$Site.ID==LCDF$Site[i]&
                      FieldData$Date==LCDF$Date[i]]=LCDF$UpGrass[i]
  FieldData$Water[FieldData$GPS==4&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Water[i]
}

LCDF[LCDF$Date<"2022-03-13"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
LCDF[LCDF$Date>"2022-03-13"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
#####
                                    #GPS5
#####
#####
#Load Points, merge landcover
#####
#rm(mat2)
#GPS5_22=read.csv("Data from Field Work/2022 Data/Tracks/GPS 5/GPS5.csv")
#GPS5_22=st_as_sf(GPS5_22,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPS5_22)=st_crs(LC2020)
#mat2=st_intersects(GPS5_22,LC2020)
#GPS5_22$LC2020=NA
#for(i in 1:length(GPS5_22$time)){
#  GPS5_22$LC2020[i]=ifelse(length(LC2020$Descr_2020[mat2[[i]]])>0,
#                           LC2020$Descr_2020[mat2[[i]]],NA)
#}
##Keep one point per minute, given that points have landcover
#GPS5_22=GPS5_22[!is.na(GPS5_22$LC2020),]
#GPS5_22$datetime=round_date(as_datetime(GPS5_22$datetime), unit = "minute")
#GPS5_22=GPS5_22[!duplicated(GPS5_22$datetime),]
######
##Save the GPS layer again so you don't have to re-do the hard part
#####
#st_write(GPS5_22,dsn = "Data from Field Work/2022 Data/Tracks/GPS 5/GPS5_withLC.shp",append = F)
GPS5_22=st_read("Data from Field Work/2022 Data/Tracks/GPS 5/GPS5_withLC.shp")
#####
#rm(mat2)
#rm(LC2020)
#FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
#FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2022-03-13"])=tz(FieldData$End.time[FieldData$Date<"2022-03-13"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2022-03-13"])=tz(FieldData$End.time[FieldData$Date>"2022-03-13"])="EDT"
#FieldData$End.time[256:261]=FieldData$End.time[262]
GPS5_22$time=as_datetime(GPS5_22$time)
GPS5_22$time[GPS5_22$datetime<"2022-03-13"]=GPS5_22$time[GPS5_22$datetime<"2022-03-13"]-hours(5)
GPS5_22$time[GPS5_22$datetime>"2022-03-13"]=GPS5_22$time[GPS5_22$datetime>"2022-03-13"]-hours(4)

#tz(GPS5_22$time[GPS5_22$datetime<"2022-03-13"])="EST";tz(GPS5_22$time[GPS5_22$datetime>"2022-03-13"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2:4)]))),]
unqGPSSites=uniqsites[uniqsites$GPS==5,]
#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
#Double check which landcovers you need to account for
unique(GPS5_22$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPS5_22$LC2020[(GPS5_22$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPS5_22$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPS5_22$LC2020=="Mixed Forest"|
                                          GPS5_22$LC2020=="Orchards, Groves, Nurseries")&
                                         GPS5_22$time<unqGPSSites$End.time[i]&
                                         GPS5_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_22$LC2020[GPS5_22$time<unqGPSSites$End.time[i]&
                            GPS5_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPS5_22$LC2020[GPS5_22$LC2020=="Wetland"&
                                          GPS5_22$time<unqGPSSites$End.time[i]&
                                          GPS5_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_22$LC2020[GPS5_22$time<unqGPSSites$End.time[i]&
                            GPS5_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPS5_22$LC2020[(GPS5_22$LC2020=="Developed Recreation (all recreation)"|
                                         GPS5_22$LC2020=="High Density Residential (<1/8 acre lots)"|
                                         GPS5_22$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPS5_22$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPS5_22$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPS5_22$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPS5_22$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPS5_22$LC2020=="Railroads (and associated facilities)"                 |
                                         GPS5_22$LC2020=="Commercial (sale of products and services)"            |
                                         GPS5_22$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPS5_22$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPS5_22$LC2020=="Vacant Land"                                           |
                                         GPS5_22$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPS5_22$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPS5_22$LC2020=="Mines, Quarries and Gravel Pits"|
                                         GPS5_22$LC2020=="Other Transportation (terminals, docks, etc.)")&
                                        GPS5_22$time<unqGPSSites$End.time[i]&
                                        GPS5_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_22$LC2020[GPS5_22$time<unqGPSSites$End.time[i]&
                            GPS5_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPS5_22$LC2020[GPS5_22$LC2020=="Water"&
                                        GPS5_22$time<unqGPSSites$End.time[i]&
                                        GPS5_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_22$LC2020[GPS5_22$time<unqGPSSites$End.time[i]&
                            GPS5_22$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPS5_22$LC2020[(GPS5_22$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPS5_22$LC2020=="Cemeteries"|
                                           GPS5_22$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPS5_22$LC2020=="Transitional Areas (urban open)"|
                                           GPS5_22$LC2020=="Cropland (tillable)")&
                                          GPS5_22$time<unqGPSSites$End.time[i]&
                                          GPS5_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_22$LC2020[GPS5_22$time<unqGPSSites$End.time[i]&
                            GPS5_22$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPS5_22$LC2020[(GPS5_22$LC2020=="Power Lines (100' or more width)"|
                                         GPS5_22$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPS5_22$time<unqGPSSites$End.time[i]&
                                        GPS5_22$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_22$LC2020[GPS5_22$time<unqGPSSites$End.time[i]&
                            GPS5_22$time>unqGPSSites$Start.Time[i]])
}
#Add the new LC data to the field data
#FieldData$Forest=FieldData$FreshW=FieldData$UpGrass=FieldData$Built=
# FieldData$Salt=FieldData$FSwamp=FieldData$SSwamp=FieldData$EmMarsh=NA
for(i in 1:length(LCDF$Site)){
  FieldData$Forest[FieldData$GPS==5&
                     FieldData$Site.ID==LCDF$Site[i]&
                     FieldData$Date==LCDF$Date[i]]=LCDF$Forest[i]
  FieldData$Shrub[FieldData$GPS==5&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Shrub[i]
  FieldData$Wetland[FieldData$GPS==5&
                      FieldData$Site.ID==LCDF$Site[i]&
                      FieldData$Date==LCDF$Date[i]]=LCDF$Wetland[i]
  FieldData$Built[FieldData$GPS==5&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Built[i]
  FieldData$UpGrass[FieldData$GPS==5&
                      FieldData$Site.ID==LCDF$Site[i]&
                      FieldData$Date==LCDF$Date[i]]=LCDF$UpGrass[i]
  FieldData$Water[FieldData$GPS==5&
                    FieldData$Site.ID==LCDF$Site[i]&
                    FieldData$Date==LCDF$Date[i]]=LCDF$Water[i]
}

colnames(FieldData)
FieldData=FieldData[,-c(12,16,19)]
write.csv(FieldData,file = "Analyses and R Files/FieldDataWithLC2022.csv")
unique(c(GPS2_22$LC2020,GPS4_22$LC2020,GPS5_22$LC2020))
