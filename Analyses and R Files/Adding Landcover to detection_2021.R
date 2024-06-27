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
Winter21=read_xlsx("Data from Field Work/2021 Data/All Watersheds Winter 2020 2021.xlsx")
Summer21=read_xlsx("Data from Field Work/2021 Data/Summer 2021 All watersheds.xlsx")
FieldData=data.frame(rbind(Winter21,Summer21))
colnames(FieldData)[c(2,6,7)]=c("Site.ID","Start.Time","End.time")
#####
                                  #GPS1
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
#####
#GPS1_21=read.csv("Data from Field Work/2021 Data/Tracks/GPS1/GPS1.csv")
#GPS1_21=st_as_sf(GPS1_21,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPS1_21)=st_crs(LC2020)
#mat2=st_intersects(GPS1_21,LC2020)
#GPS1_21$LC2020=NA
#for(i in 1:length(GPS1_21$X)){
#  GPS1_21$LC2020[i]=ifelse(length(LC2020$Descr_2020[mat2[[i]]])>0,
#  LC2020$Descr_2020[mat2[[i]]],NA)
#}
##Keep one point per minute, given that points have landcover
#GPS1_21=GPS1_21[!is.na(GPS1_21$LC2020),]
#GPS1_21$datetime=round_date(as_datetime(GPS1_21$datetime), unit = "minute")
#GPS1_21=GPS1_21[!duplicated(GPS1_21$datetime),]
#####
##Save the GPS layer again so you don't have to re-do the hard part
######
#st_write(GPS1_21,dsn = "Data from Field Work/2021 Data/Tracks/GPS1/GPS1_withLC.shp",append = F)
#####
GPS1_21=st_read("Data from Field Work/2021 Data/Tracks/GPS1/GPS1_withLC.shp")
#rm(mat2)
#rm(LC2020)
FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2021-03-14"])=tz(FieldData$End.time[FieldData$Date<"2021-03-14"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2021-03-14"])=tz(FieldData$End.time[FieldData$Date>"2021-03-14"])="EDT"
which(is.na(FieldData$Start.Time))
which(is.na(FieldData$End.time))
which(FieldData$Date==FieldData$Date[c(106,229)]&
        FieldData$Site.ID==FieldData$Site.ID[c(106,229)])
FieldData[c(106,107,109,111,113,229,230,228),c(2:4,6,7)]
FieldData$End.time[106]=FieldData$End.time[107]
FieldData$Start.Time[106]=FieldData$Start.Time[107]

GPS1_21$time=as_datetime(GPS1_21$time)
GPS1_21$time[GPS1_21$datetime<"2021-03-14"]=GPS1_21$time[GPS1_21$datetime<"2021-03-14"]-hours(5)
GPS1_21$time[GPS1_21$datetime>"2021-03-14"]=GPS1_21$time[GPS1_21$datetime>"2021-03-14"]-hours(4)

#tz(GPS1_21$time[GPS1_21$datetime<"2021-03-14"])="EST";tz(GPS1_21$time[GPS1_21$datetime>"2021-03-14"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2:4)]))),]
unqGPSSites=uniqsites[uniqsites$GPS==1,]
#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
#Double check which landcovers you need to account for
unique(GPS1_21$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPS1_21$LC2020[(GPS1_21$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPS1_21$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPS1_21$LC2020=="Mixed Forest"|
                                          GPS1_21$LC2020=="Orchards, Groves, Nurseries")&
                                         GPS1_21$time<unqGPSSites$End.time[i]&
                                         GPS1_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_21$LC2020[GPS1_21$time<unqGPSSites$End.time[i]&
                            GPS1_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPS1_21$LC2020[GPS1_21$LC2020=="Wetland"&
                                          GPS1_21$time<unqGPSSites$End.time[i]&
                                          GPS1_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_21$LC2020[GPS1_21$time<unqGPSSites$End.time[i]&
                            GPS1_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPS1_21$LC2020[(GPS1_21$LC2020=="Airports (and associated facilities)"|
                                         GPS1_21$LC2020=="Commercial (sale of products and services)"            |
                                         GPS1_21$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPS1_21$LC2020=="Commercial/Residential Mixed"                          |
                                         GPS1_21$LC2020=="Confined Feeding Operations"                           |
                                         GPS1_21$LC2020=="Developed Recreation (all recreation)"                 |
                                         GPS1_21$LC2020=="Ground-mounted Solar Energy Systems"                   |
                                         GPS1_21$LC2020=="High Density Residential (<1/8 acre lots)"             |
                                         GPS1_21$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPS1_21$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPS1_21$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPS1_21$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPS1_21$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPS1_21$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPS1_21$LC2020=="Mines, Quarries and Gravel Pits"                       |
                                         GPS1_21$LC2020=="Other Transportation (terminals, docks, etc.)"         |
                                         GPS1_21$LC2020=="Railroads (and associated facilities)"                 |
                                         GPS1_21$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPS1_21$LC2020=="Vacant Land"                                           |
                                         GPS1_21$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPS1_21$LC2020=="Water and Sewage Treatment"                            |
                                         GPS1_21$LC2020=="Wind Energy Systems")&
                                        GPS1_21$time<unqGPSSites$End.time[i]&
                                        GPS1_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_21$LC2020[GPS1_21$time<unqGPSSites$End.time[i]&
                            GPS1_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPS1_21$LC2020[GPS1_21$LC2020=="Water"&
                                        GPS1_21$time<unqGPSSites$End.time[i]&
                                        GPS1_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_21$LC2020[GPS1_21$time<unqGPSSites$End.time[i]&
                            GPS1_21$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPS1_21$LC2020[(GPS1_21$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPS1_21$LC2020=="Cemeteries"|
                                           GPS1_21$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPS1_21$LC2020=="Transitional Areas (urban open)"|
                                           GPS1_21$LC2020=="Cropland (tillable)")&
                                          GPS1_21$time<unqGPSSites$End.time[i]&
                                          GPS1_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_21$LC2020[GPS1_21$time<unqGPSSites$End.time[i]&
                            GPS1_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPS1_21$LC2020[(GPS1_21$LC2020=="Power Lines (100' or more width)"|
                                         GPS1_21$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPS1_21$time<unqGPSSites$End.time[i]&
                                        GPS1_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS1_21$LC2020[GPS1_21$time<unqGPSSites$End.time[i]&
                            GPS1_21$time>unqGPSSites$Start.Time[i]])
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

LCDF[LCDF$Date<"2021-03-14"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
LCDF[LCDF$Date>"2021-03-14"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]


#####
                                #GPS2
#####
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
#####
#GPS2_21=read.csv("Data from Field Work/2021 Data/Tracks/GPS2/GPS2.csv")
#GPS2_21=st_as_sf(GPS2_21,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPS2_21)=st_crs(LC2020)
#mat2=st_intersects(GPS2_21,LC2020)
#GPS2_21$LC2020=NA
#for(i in 1:length(GPS2_21$X)){
#  GPS2_21$LC2020[i]=ifelse(length(LC2020$Descr_2020[mat2[[i]]])>0,
#  LC2020$Descr_2020[mat2[[i]]],NA)
#}
##Keep one point per minute, given that points have landcover
#GPS2_21=GPS2_21[!is.na(GPS2_21$LC2020),]
#GPS2_21$datetime=round_date(as_datetime(GPS2_21$datetime), unit = "minute")
#GPS2_21=GPS2_21[!duplicated(GPS2_21$datetime),]
#####
##Save the GPS layer again so you don't have to re-do the hard part
######
#st_write(GPS2_21,dsn = "Data from Field Work/2021 Data/Tracks/GPS2/GPS2_withLC.shp",append = F)
#####
GPS2_21=st_read("Data from Field Work/2021 Data/Tracks/GPS2/GPS2_withLC.shp")
#rm(mat2)
#rm(LC2020)
#FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
#FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2021-03-14"])=tz(FieldData$End.time[FieldData$Date<"2021-03-14"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2021-03-14"])=tz(FieldData$End.time[FieldData$Date>"2021-03-14"])="EDT"
#FieldData$End.time[256:261]=FieldData$End.time[262]
GPS2_21$time=as_datetime(GPS2_21$time)
GPS2_21$time[GPS2_21$datetime<"2021-03-14"]=GPS2_21$time[GPS2_21$datetime<"2021-03-14"]-hours(5)
GPS2_21$time[GPS2_21$datetime>"2021-03-14"]=GPS2_21$time[GPS2_21$datetime>"2021-03-14"]-hours(4)

#tz(GPS2_21$time[GPS2_21$datetime<"2021-03-14"])="EST";tz(GPS2_21$time[GPS2_21$datetime>"2021-03-14"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2:4)]))),]
unqGPSSites=uniqsites[uniqsites$GPS==2,]
#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
#Double check which landcovers you need to account for
unique(GPS2_21$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPS2_21$LC2020[(GPS2_21$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPS2_21$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPS2_21$LC2020=="Mixed Forest"|
                                          GPS2_21$LC2020=="Orchards, Groves, Nurseries")&
                                         GPS2_21$time<unqGPSSites$End.time[i]&
                                         GPS2_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_21$LC2020[GPS2_21$time<unqGPSSites$End.time[i]&
                            GPS2_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPS2_21$LC2020[GPS2_21$LC2020=="Wetland"&
                                          GPS2_21$time<unqGPSSites$End.time[i]&
                                          GPS2_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_21$LC2020[GPS2_21$time<unqGPSSites$End.time[i]&
                            GPS2_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPS2_21$LC2020[(GPS2_21$LC2020=="Airports (and associated facilities)"|
                                         GPS2_21$LC2020=="Commercial (sale of products and services)"            |
                                         GPS2_21$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPS2_21$LC2020=="Commercial/Residential Mixed"                          |
                                         GPS2_21$LC2020=="Confined Feeding Operations"                           |
                                         GPS2_21$LC2020=="Developed Recreation (all recreation)"                 |
                                         GPS2_21$LC2020=="Ground-mounted Solar Energy Systems"                   |
                                         GPS2_21$LC2020=="High Density Residential (<1/8 acre lots)"             |
                                         GPS2_21$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPS2_21$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPS2_21$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPS2_21$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPS2_21$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPS2_21$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPS2_21$LC2020=="Mines, Quarries and Gravel Pits"                       |
                                         GPS2_21$LC2020=="Other Transportation (terminals, docks, etc.)"         |
                                         GPS2_21$LC2020=="Railroads (and associated facilities)"                 |
                                         GPS2_21$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPS2_21$LC2020=="Vacant Land"                                           |
                                         GPS2_21$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPS2_21$LC2020=="Water and Sewage Treatment"                            |
                                         GPS2_21$LC2020=="Wind Energy Systems")&
                                        GPS2_21$time<unqGPSSites$End.time[i]&
                                        GPS2_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_21$LC2020[GPS2_21$time<unqGPSSites$End.time[i]&
                            GPS2_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPS2_21$LC2020[GPS2_21$LC2020=="Water"&
                                        GPS2_21$time<unqGPSSites$End.time[i]&
                                        GPS2_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_21$LC2020[GPS2_21$time<unqGPSSites$End.time[i]&
                            GPS2_21$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPS2_21$LC2020[(GPS2_21$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPS2_21$LC2020=="Cemeteries"|
                                           GPS2_21$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPS2_21$LC2020=="Transitional Areas (urban open)"|
                                           GPS2_21$LC2020=="Cropland (tillable)")&
                                          GPS2_21$time<unqGPSSites$End.time[i]&
                                          GPS2_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_21$LC2020[GPS2_21$time<unqGPSSites$End.time[i]&
                            GPS2_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPS2_21$LC2020[(GPS2_21$LC2020=="Power Lines (100' or more width)"|
                                         GPS2_21$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPS2_21$time<unqGPSSites$End.time[i]&
                                        GPS2_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS2_21$LC2020[GPS2_21$time<unqGPSSites$End.time[i]&
                            GPS2_21$time>unqGPSSites$Start.Time[i]])
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

LCDF[LCDF$Date<"2021-03-14"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
LCDF[LCDF$Date>"2021-03-14"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]

#####
                                    #GPS3
#####
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
#####
#GPS3_21=read.csv("Data from Field Work/2021 Data/Tracks/GPS3/GPS3.csv")
#GPS3_21=st_as_sf(GPS3_21,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPS3_21)=st_crs(LC2020)
#mat2=st_intersects(GPS3_21,LC2020)
#GPS3_21$LC2020=NA
#for(i in 1:length(GPS3_21$X)){
#  GPS3_21$LC2020[i]=ifelse(length(LC2020$Descr_2020[mat2[[i]]])>0,
#  LC2020$Descr_2020[mat2[[i]]],NA)
#}
##Keep one point per minute, given that points have landcover
#GPS3_21=GPS3_21[!is.na(GPS3_21$LC2020),]
#GPS3_21$datetime=round_date(as_datetime(GPS3_21$datetime), unit = "minute")
#GPS3_21=GPS3_21[!duplicated(GPS3_21$datetime),]
#####
##Save the GPS layer again so you don't have to re-do the hard part
######
#st_write(GPS3_21,dsn = "Data from Field Work/2021 Data/Tracks/GPS3/GPS3_withLC.shp",append = F)
#####
GPS3_21=st_read("Data from Field Work/2021 Data/Tracks/GPS3/GPS3_withLC.shp")
#rm(mat2)
#rm(LC2020)
#FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
#FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2021-03-14"])=tz(FieldData$End.time[FieldData$Date<"2021-03-14"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2021-03-14"])=tz(FieldData$End.time[FieldData$Date>"2021-03-14"])="EDT"
#FieldData$End.time[256:261]=FieldData$End.time[262]
GPS3_21$time=as_datetime(GPS3_21$time)
GPS3_21$time[GPS3_21$datetime<"2021-03-14"]=GPS3_21$time[GPS3_21$datetime<"2021-03-14"]-hours(5)
GPS3_21$time[GPS3_21$datetime>"2021-03-14"]=GPS3_21$time[GPS3_21$datetime>"2021-03-14"]-hours(4)

#tz(GPS3_21$time[GPS3_21$datetime<"2021-03-14"])="EST";tz(GPS3_21$time[GPS3_21$datetime>"2021-03-14"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2:4)]))),]
unqGPSSites=uniqsites[uniqsites$GPS==3,]
#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
#Double check which landcovers you need to account for
unique(GPS3_21$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPS3_21$LC2020[(GPS3_21$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPS3_21$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPS3_21$LC2020=="Mixed Forest"|
                                          GPS3_21$LC2020=="Orchards, Groves, Nurseries")&
                                         GPS3_21$time<unqGPSSites$End.time[i]&
                                         GPS3_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_21$LC2020[GPS3_21$time<unqGPSSites$End.time[i]&
                            GPS3_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPS3_21$LC2020[GPS3_21$LC2020=="Wetland"&
                                          GPS3_21$time<unqGPSSites$End.time[i]&
                                          GPS3_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_21$LC2020[GPS3_21$time<unqGPSSites$End.time[i]&
                            GPS3_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPS3_21$LC2020[(GPS3_21$LC2020=="Airports (and associated facilities)"|
                                         GPS3_21$LC2020=="Commercial (sale of products and services)"            |
                                         GPS3_21$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPS3_21$LC2020=="Commercial/Residential Mixed"                          |
                                         GPS3_21$LC2020=="Confined Feeding Operations"                           |
                                         GPS3_21$LC2020=="Developed Recreation (all recreation)"                 |
                                         GPS3_21$LC2020=="Ground-mounted Solar Energy Systems"                   |
                                         GPS3_21$LC2020=="High Density Residential (<1/8 acre lots)"             |
                                         GPS3_21$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPS3_21$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPS3_21$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPS3_21$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPS3_21$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPS3_21$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPS3_21$LC2020=="Mines, Quarries and Gravel Pits"                       |
                                         GPS3_21$LC2020=="Other Transportation (terminals, docks, etc.)"         |
                                         GPS3_21$LC2020=="Railroads (and associated facilities)"                 |
                                         GPS3_21$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPS3_21$LC2020=="Vacant Land"                                           |
                                         GPS3_21$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPS3_21$LC2020=="Water and Sewage Treatment"                            |
                                         GPS3_21$LC2020=="Wind Energy Systems")&
                                        GPS3_21$time<unqGPSSites$End.time[i]&
                                        GPS3_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_21$LC2020[GPS3_21$time<unqGPSSites$End.time[i]&
                            GPS3_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPS3_21$LC2020[GPS3_21$LC2020=="Water"&
                                        GPS3_21$time<unqGPSSites$End.time[i]&
                                        GPS3_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_21$LC2020[GPS3_21$time<unqGPSSites$End.time[i]&
                            GPS3_21$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPS3_21$LC2020[(GPS3_21$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPS3_21$LC2020=="Cemeteries"|
                                           GPS3_21$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPS3_21$LC2020=="Transitional Areas (urban open)"|
                                           GPS3_21$LC2020=="Cropland (tillable)")&
                                          GPS3_21$time<unqGPSSites$End.time[i]&
                                          GPS3_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_21$LC2020[GPS3_21$time<unqGPSSites$End.time[i]&
                            GPS3_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPS3_21$LC2020[(GPS3_21$LC2020=="Power Lines (100' or more width)"|
                                         GPS3_21$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPS3_21$time<unqGPSSites$End.time[i]&
                                        GPS3_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS3_21$LC2020[GPS3_21$time<unqGPSSites$End.time[i]&
                            GPS3_21$time>unqGPSSites$Start.Time[i]])
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

LCDF[LCDF$Date<"2021-03-14"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
LCDF[LCDF$Date>"2021-03-14"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]

#####
                                #GPS4
#####
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
####
#GPS4_21=read.csv("Data from Field Work/2021 Data/Tracks/GPS4/GPS4.csv")
#GPS4_21=st_as_sf(GPS4_21,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPS4_21)=st_crs(LC2020)
#mat2=st_intersects(GPS4_21,LC2020)
#GPS4_21$LC2020=NA
#for(i in 1:length(GPS4_21$X)){
#  GPS4_21$LC2020[i]=ifelse(length(LC2020$Descr_2020[mat2[[i]]])>0,
#  LC2020$Descr_2020[mat2[[i]]],NA)
#}
##Keep one point per minute, given that points have landcover
#GPS4_21=GPS4_21[!is.na(GPS4_21$LC2020),]
#GPS4_21$datetime=round_date(as_datetime(GPS4_21$datetime), unit = "minute")
#GPS4_21=GPS4_21[!duplicated(GPS4_21$datetime),]
#####
##Save the GPS layer again so you don't have to re-do the hard part
######
#st_write(GPS4_21,dsn = "Data from Field Work/2021 Data/Tracks/GPS4/GPS4_withLC.shp",append = F)
#####
GPS4_21=st_read("Data from Field Work/2021 Data/Tracks/GPS4/GPS4_withLC.shp")
#rm(mat2)
#rm(LC2020)
#FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
#FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2021-03-14"])=tz(FieldData$End.time[FieldData$Date<"2021-03-14"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2021-03-14"])=tz(FieldData$End.time[FieldData$Date>"2021-03-14"])="EDT"
#FieldData$End.time[256:261]=FieldData$End.time[262]
GPS4_21$time=as_datetime(GPS4_21$time)
GPS4_21$time[GPS4_21$datetime<"2021-03-14"]=GPS4_21$time[GPS4_21$datetime<"2021-03-14"]-hours(5)
GPS4_21$time[GPS4_21$datetime>"2021-03-14"]=GPS4_21$time[GPS4_21$datetime>"2021-03-14"]-hours(4)

#tz(GPS4_21$time[GPS4_21$datetime<"2021-03-14"])="EST";tz(GPS4_21$time[GPS4_21$datetime>"2021-03-14"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2:4)]))),]
unqGPSSites=uniqsites[uniqsites$GPS==4,]
#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
#Double check which landcovers you need to account for
unique(GPS4_21$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPS4_21$LC2020[(GPS4_21$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPS4_21$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPS4_21$LC2020=="Mixed Forest"|
                                          GPS4_21$LC2020=="Orchards, Groves, Nurseries")&
                                         GPS4_21$time<unqGPSSites$End.time[i]&
                                         GPS4_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_21$LC2020[GPS4_21$time<unqGPSSites$End.time[i]&
                            GPS4_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPS4_21$LC2020[GPS4_21$LC2020=="Wetland"&
                                          GPS4_21$time<unqGPSSites$End.time[i]&
                                          GPS4_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_21$LC2020[GPS4_21$time<unqGPSSites$End.time[i]&
                            GPS4_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPS4_21$LC2020[(GPS4_21$LC2020=="Airports (and associated facilities)"|
                                         GPS4_21$LC2020=="Commercial (sale of products and services)"            |
                                         GPS4_21$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPS4_21$LC2020=="Commercial/Residential Mixed"                          |
                                         GPS4_21$LC2020=="Confined Feeding Operations"                           |
                                         GPS4_21$LC2020=="Developed Recreation (all recreation)"                 |
                                         GPS4_21$LC2020=="Ground-mounted Solar Energy Systems"                   |
                                         GPS4_21$LC2020=="High Density Residential (<1/8 acre lots)"             |
                                         GPS4_21$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPS4_21$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPS4_21$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPS4_21$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPS4_21$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPS4_21$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPS4_21$LC2020=="Mines, Quarries and Gravel Pits"                       |
                                         GPS4_21$LC2020=="Other Transportation (terminals, docks, etc.)"         |
                                         GPS4_21$LC2020=="Railroads (and associated facilities)"                 |
                                         GPS4_21$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPS4_21$LC2020=="Vacant Land"                                           |
                                         GPS4_21$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPS4_21$LC2020=="Water and Sewage Treatment"                            |
                                         GPS4_21$LC2020=="Wind Energy Systems")&
                                        GPS4_21$time<unqGPSSites$End.time[i]&
                                        GPS4_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_21$LC2020[GPS4_21$time<unqGPSSites$End.time[i]&
                            GPS4_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPS4_21$LC2020[GPS4_21$LC2020=="Water"&
                                        GPS4_21$time<unqGPSSites$End.time[i]&
                                        GPS4_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_21$LC2020[GPS4_21$time<unqGPSSites$End.time[i]&
                            GPS4_21$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPS4_21$LC2020[(GPS4_21$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPS4_21$LC2020=="Cemeteries"|
                                           GPS4_21$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPS4_21$LC2020=="Transitional Areas (urban open)"|
                                           GPS4_21$LC2020=="Cropland (tillable)")&
                                          GPS4_21$time<unqGPSSites$End.time[i]&
                                          GPS4_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_21$LC2020[GPS4_21$time<unqGPSSites$End.time[i]&
                            GPS4_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPS4_21$LC2020[(GPS4_21$LC2020=="Power Lines (100' or more width)"|
                                         GPS4_21$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPS4_21$time<unqGPSSites$End.time[i]&
                                        GPS4_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS4_21$LC2020[GPS4_21$time<unqGPSSites$End.time[i]&
                            GPS4_21$time>unqGPSSites$Start.Time[i]])
}
#Add the new LC data to the field data
#FieldData$Forest=FieldData$Shrub=FieldData$UpGrass=FieldData$Built=
#  FieldData$Wetland=FieldData$Water=NA
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

LCDF[LCDF$Date<"2021-03-14"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
LCDF[LCDF$Date>"2021-03-14"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
#####
                                #GPS5
#####
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
#####
#GPS5_21=read.csv("Data from Field Work/2021 Data/Tracks/GPS5/GPS5.csv")
#GPS5_21=st_as_sf(GPS5_21,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPS5_21)=st_crs(LC2020)
#mat2=st_intersects(GPS5_21,LC2020)
#GPS5_21$LC2020=NA
#for(i in 1:length(GPS5_21$X)){
#  GPS5_21$LC2020[i]=ifelse(length(LC2020$Descr_2020[mat2[[i]]])>0,
#  LC2020$Descr_2020[mat2[[i]]],NA)
#}
##Keep one point per minute, given that points have landcover
#GPS5_21=GPS5_21[!is.na(GPS5_21$LC2020),]
#GPS5_21$datetime=round_date(as_datetime(GPS5_21$datetime), unit = "minute")
#GPS5_21=GPS5_21[!duplicated(GPS5_21$datetime),]
#####
##Save the GPS layer again so you don't have to re-do the hard part
######
#st_write(GPS5_21,dsn = "Data from Field Work/2021 Data/Tracks/GPS5/GPS5_withLC.shp",append = F)
#####
GPS5_21=st_read("Data from Field Work/2021 Data/Tracks/GPS5/GPS5_withLC.shp")
#rm(mat2)
#rm(LC2020)
#FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
#FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2021-03-14"])=tz(FieldData$End.time[FieldData$Date<"2021-03-14"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2021-03-14"])=tz(FieldData$End.time[FieldData$Date>"2021-03-14"])="EDT"
#FieldData$End.time[256:261]=FieldData$End.time[262]
GPS5_21$time=as_datetime(GPS5_21$time)
GPS5_21$time[GPS5_21$datetime<"2021-03-14"]=GPS5_21$time[GPS5_21$datetime<"2021-03-14"]-hours(5)
GPS5_21$time[GPS5_21$datetime>"2021-03-14"]=GPS5_21$time[GPS5_21$datetime>"2021-03-14"]-hours(4)

#tz(GPS5_21$time[GPS5_21$datetime<"2021-03-14"])="EST";tz(GPS5_21$time[GPS5_21$datetime>"2021-03-14"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2:4)]))),]
unqGPSSites=uniqsites[uniqsites$GPS==5,]
#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
#Double check which landcovers you need to account for
unique(GPS5_21$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPS5_21$LC2020[(GPS5_21$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPS5_21$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPS5_21$LC2020=="Mixed Forest"|
                                          GPS5_21$LC2020=="Orchards, Groves, Nurseries")&
                                         GPS5_21$time<unqGPSSites$End.time[i]&
                                         GPS5_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_21$LC2020[GPS5_21$time<unqGPSSites$End.time[i]&
                            GPS5_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPS5_21$LC2020[GPS5_21$LC2020=="Wetland"&
                                          GPS5_21$time<unqGPSSites$End.time[i]&
                                          GPS5_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_21$LC2020[GPS5_21$time<unqGPSSites$End.time[i]&
                            GPS5_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPS5_21$LC2020[(GPS5_21$LC2020=="Airports (and associated facilities)"|
                                         GPS5_21$LC2020=="Commercial (sale of products and services)"            |
                                         GPS5_21$LC2020=="Commercial/Industrial Mixed"                           |
                                         GPS5_21$LC2020=="Commercial/Residential Mixed"                          |
                                         GPS5_21$LC2020=="Confined Feeding Operations"                           |
                                         GPS5_21$LC2020=="Developed Recreation (all recreation)"                 |
                                         GPS5_21$LC2020=="Ground-mounted Solar Energy Systems"                   |
                                         GPS5_21$LC2020=="High Density Residential (<1/8 acre lots)"             |
                                         GPS5_21$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                         GPS5_21$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                         GPS5_21$LC2020=="Low Density Residential (>2 acre lots)"                |
                                         GPS5_21$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                         GPS5_21$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                         GPS5_21$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                         GPS5_21$LC2020=="Mines, Quarries and Gravel Pits"                       |
                                         GPS5_21$LC2020=="Other Transportation (terminals, docks, etc.)"         |
                                         GPS5_21$LC2020=="Railroads (and associated facilities)"                 |
                                         GPS5_21$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                         GPS5_21$LC2020=="Vacant Land"                                           |
                                         GPS5_21$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                         GPS5_21$LC2020=="Water and Sewage Treatment"                            |
                                         GPS5_21$LC2020=="Wind Energy Systems")&
                                        GPS5_21$time<unqGPSSites$End.time[i]&
                                        GPS5_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_21$LC2020[GPS5_21$time<unqGPSSites$End.time[i]&
                            GPS5_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPS5_21$LC2020[GPS5_21$LC2020=="Water"&
                                        GPS5_21$time<unqGPSSites$End.time[i]&
                                        GPS5_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_21$LC2020[GPS5_21$time<unqGPSSites$End.time[i]&
                            GPS5_21$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPS5_21$LC2020[(GPS5_21$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPS5_21$LC2020=="Cemeteries"|
                                           GPS5_21$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPS5_21$LC2020=="Transitional Areas (urban open)"|
                                           GPS5_21$LC2020=="Cropland (tillable)")&
                                          GPS5_21$time<unqGPSSites$End.time[i]&
                                          GPS5_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_21$LC2020[GPS5_21$time<unqGPSSites$End.time[i]&
                            GPS5_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPS5_21$LC2020[(GPS5_21$LC2020=="Power Lines (100' or more width)"|
                                         GPS5_21$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPS5_21$time<unqGPSSites$End.time[i]&
                                        GPS5_21$time>unqGPSSites$Start.Time[i]])/
    length(GPS5_21$LC2020[GPS5_21$time<unqGPSSites$End.time[i]&
                            GPS5_21$time>unqGPSSites$Start.Time[i]])
}
#Add the new LC data to the field data
#FieldData$Forest=FieldData$Shrub=FieldData$UpGrass=FieldData$Built=
#  FieldData$Wetland=FieldData$Water=NA
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

LCDF[LCDF$Date<"2021-03-14"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
LCDF[LCDF$Date>"2021-03-14"&is.na(LCDF$EmMarsh)==1,c(1,2,4)]
#####
FieldData[FieldData$UpGrass=="NaN"&(FieldData$Site.ID==61|
                                      FieldData$Site.ID==65),c(2:4,14)]

#####
# Now let's see if phone stuff will fill in any gaps
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
#####
#GPSPhone=read.csv("Data from Field Work/Phone Tracks/GPSPhone.csv")
#GPSPhone=st_as_sf(GPSPhone,coords=c(2,3))
##Load landcover
#LC2020=st_read(dsn="GIS Data/Ecological Communities RIGIS/Reprojected2020.shp")
##Find which points are in which landcover polygons
#LC2020=st_make_valid(LC2020)
#st_crs(GPSPhone)=st_crs(LC2020)
#mat2=st_intersects(GPSPhone,LC2020)
#GPSPhone$LC2020=NA
#for(i in 1:length(GPSPhone$X)){
#  GPSPhone$LC2020[i]=ifelse(length(LC2020$Descr_2020[mat2[[i]]])>0,
#  LC2020$Descr_2020[mat2[[i]]],NA)
#}
##Keep one point per minute, given that points have landcover
#GPSPhone=GPSPhone[!is.na(GPSPhone$LC2020),]
#GPSPhone$datetime=round_date(as_datetime(GPSPhone$datetime), unit = "minute")
#GPSPhone=GPSPhone[!duplicated(GPSPhone$datetime),]
######
###Save the GPS layer again so you don't have to re-do the hard part
#######
#st_write(GPSPhone,dsn = "Data from Field Work/Phone Tracks/GPSPhone_withLC.shp",append = F)
#####
GPSPhone=st_read("Data from Field Work/Phone Tracks/GPSPhone_withLC.shp")
GPSPhone_21=GPSPhone[GPSPhone$datetime<"2021-09-01",]
#rm(mat2)
#rm(LC2020)
#FieldData$Start.Time=ymd_hms(paste0(FieldData$Date,substring(FieldData$Start.Time,first = 12)))
#FieldData$End.time=ymd_hms(paste0(FieldData$Date,substring(FieldData$End.time,first = 12)))
#tz(FieldData$Start.Time[FieldData$Date<"2021-03-14"])=tz(FieldData$End.time[FieldData$Date<"2021-03-14"])="EST"
#tz(FieldData$Start.Time[FieldData$Date>"2021-03-14"])=tz(FieldData$End.time[FieldData$Date>"2021-03-14"])="EDT"
#FieldData$End.time[256:261]=FieldData$End.time[262]
GPSPhone_21$time=as_datetime(GPSPhone_21$time)
GPSPhone_21$time[GPSPhone_21$datetime<"2021-03-14"]=GPSPhone_21$time[GPSPhone_21$datetime<"2021-03-14"]-hours(5)
GPSPhone_21$time[GPSPhone_21$datetime>"2021-03-14"]=GPSPhone_21$time[GPSPhone_21$datetime>"2021-03-14"]-hours(4)

#tz(GPSPhone_21$time[GPSPhone_21$datetime<"2021-03-14"])="EST";tz(GPSPhone_21$time[GPSPhone_21$datetime>"2021-03-14"])="EDT"
#This next bit finds site and day combos that used the GPS we're 
#     interested in. Could probably be done more elegantly but w/e
uniqsites=FieldData[as.numeric(row.names(unique(FieldData[,c(2:4)]))),]
unqGPSSites=uniqsites[uniqsites$GPS=="phone",]
#Make a new data frame to hold the landcover
LCDF=data.frame(Site=unqGPSSites$Site.ID,Date=unqGPSSites$Date)
#Double check which landcovers you need to account for
unique(GPSPhone_21$LC2020)
LCDF$Forest=LCDF$Water=LCDF$UpGrass=LCDF$Built=
  LCDF$Wetland=LCDF$Shrub=NA
###Big loop to put the proportion of points with each landcover
#       class into the corresponding row in the data frame you made
for(i in 1:length(unqGPSSites$Site)){
  LCDF$Forest[i]=length(GPSPhone_21$LC2020[(GPSPhone_21$LC2020=="Deciduous Forest (>80% hardwood)"|
                                          GPSPhone_21$LC2020=="Softwood Forest (>80% softwood)"|
                                          GPSPhone_21$LC2020=="Mixed Forest"|
                                          GPSPhone_21$LC2020=="Orchards, Groves, Nurseries")&
                                         GPSPhone_21$time<unqGPSSites$End.time[i]&
                                         GPSPhone_21$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_21$LC2020[GPSPhone_21$time<unqGPSSites$End.time[i]&
                            GPSPhone_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Wetland[i]=length(GPSPhone_21$LC2020[GPSPhone_21$LC2020=="Wetland"&
                                          GPSPhone_21$time<unqGPSSites$End.time[i]&
                                          GPSPhone_21$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_21$LC2020[GPSPhone_21$time<unqGPSSites$End.time[i]&
                            GPSPhone_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Built[i]=length(GPSPhone_21$LC2020[(GPSPhone_21$LC2020=="Airports (and associated facilities)"|
                                             GPSPhone_21$LC2020=="Commercial (sale of products and services)"            |
                                             GPSPhone_21$LC2020=="Commercial/Industrial Mixed"                           |
                                             GPSPhone_21$LC2020=="Commercial/Residential Mixed"                          |
                                             GPSPhone_21$LC2020=="Confined Feeding Operations"                           |
                                             GPSPhone_21$LC2020=="Developed Recreation (all recreation)"                 |
                                             GPSPhone_21$LC2020=="Ground-mounted Solar Energy Systems"                   |
                                             GPSPhone_21$LC2020=="High Density Residential (<1/8 acre lots)"             |
                                             GPSPhone_21$LC2020=="Industrial (manufacturing, design, assembly, etc.)"    |
                                             GPSPhone_21$LC2020=="Institutional (schools, hospitals, churches, etc.)"    |
                                             GPSPhone_21$LC2020=="Low Density Residential (>2 acre lots)"                |
                                             GPSPhone_21$LC2020=="Medium Density Residential (1 to 1/4 acre lots)"       |
                                             GPSPhone_21$LC2020=="Medium High Density Residential (1/4 to 1/8 acre lots)"|
                                             GPSPhone_21$LC2020=="Medium Low Density Residential (1 to 2 acre lots)"     |
                                             GPSPhone_21$LC2020=="Mines, Quarries and Gravel Pits"                       |
                                             GPSPhone_21$LC2020=="Other Transportation (terminals, docks, etc.)"         |
                                             GPSPhone_21$LC2020=="Railroads (and associated facilities)"                 |
                                             GPSPhone_21$LC2020=="Roads (divided highways >200' plus related facilities)"|
                                             GPSPhone_21$LC2020=="Vacant Land"                                           |
                                             GPSPhone_21$LC2020=="Waste Disposal (landfills, junkyards, etc.)"           |
                                             GPSPhone_21$LC2020=="Water and Sewage Treatment"                            |
                                             GPSPhone_21$LC2020=="Wind Energy Systems")&
                                        GPSPhone_21$time<unqGPSSites$End.time[i]&
                                        GPSPhone_21$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_21$LC2020[GPSPhone_21$time<unqGPSSites$End.time[i]&
                            GPSPhone_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Water[i]=length(GPSPhone_21$LC2020[GPSPhone_21$LC2020=="Water"&
                                        GPSPhone_21$time<unqGPSSites$End.time[i]&
                                        GPSPhone_21$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_21$LC2020[GPSPhone_21$time<unqGPSSites$End.time[i]&
                            GPSPhone_21$time>unqGPSSites$Start.Time[i]])
  LCDF$UpGrass[i]=length(GPSPhone_21$LC2020[(GPSPhone_21$LC2020=="Pasture (agricultural not suitable for tillage)"|
                                           GPSPhone_21$LC2020=="Cemeteries"|
                                           GPSPhone_21$LC2020=="Idle Agriculture (abandoned fields and orchards)"|
                                           GPSPhone_21$LC2020=="Transitional Areas (urban open)"|
                                           GPSPhone_21$LC2020=="Cropland (tillable)")&
                                          GPSPhone_21$time<unqGPSSites$End.time[i]&
                                          GPSPhone_21$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_21$LC2020[GPSPhone_21$time<unqGPSSites$End.time[i]&
                            GPSPhone_21$time>unqGPSSites$Start.Time[i]])
  LCDF$Shrub[i]=length(GPSPhone_21$LC2020[(GPSPhone_21$LC2020=="Power Lines (100' or more width)"|
                                         GPSPhone_21$LC2020=="Brushland (shrub and brush areas, reforestation)")&
                                        GPSPhone_21$time<unqGPSSites$End.time[i]&
                                        GPSPhone_21$time>unqGPSSites$Start.Time[i]])/
    length(GPSPhone_21$LC2020[GPSPhone_21$time<unqGPSSites$End.time[i]&
                            GPSPhone_21$time>unqGPSSites$Start.Time[i]])
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

checklist=unique(FieldData[is.na(FieldData$UpGrass)==T,2:4])
colnames(FieldData)
FieldData[row.names(unique(FieldData[is.na(FieldData$UpGrass)==T,2:4])),c(2:4,15,20)]
write.csv(FieldData,file = "Analyses and R Files/FieldDataWithLC2021.csv")

