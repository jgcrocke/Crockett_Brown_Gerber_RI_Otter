#
#             This file will bring several older, messier, files
#             together into one spot. Here, we will extract coordinates
#             from the GPS files, add landcover (both site and observer
#             level) to observations, and add weather
#####
#         Step zero: packages
#         I'm not 100% sure all these packages are necessary,
#         but I also don't think they will break each other
#####
library(plotKML)
library(dplyr)
library(lubridate)
library(readxl)
library(data.table)
library(sf)
library(openmeteo)

#####
#         Step One: Extracting Tracks
#         This has not been re-run, but the file paths have been
#         updated to the Github versions.
#####
#Clearing all leftover vars
rm(list=ls())
library(this.path)
setwd(here())
setwd('..')
##Pointing R towards the right folder. From here on in you could run this##
##on a loop but I didn't want to bother. Something like for (z in 1:5){  ##
##  all of step one but using paste0 and assign to get GPS[z] to be  ##
##  legible}

#Setting up a dummy dataframe
GPS1=data.frame(lon=0,lat=0,ele=0,time="2021-01-13T16:08:30Z")
#make a list of the files in the folder so we can loop over them. 
##NOTE: if you haven't made a csv in this folder yet, the loop should start at
## 1 instead of 2. Also note that the amt subtracted from the length
##  may vary based on whether there is a shapefile present.
files=list.files()
##Loops over all the files
for(x in 1:(length(files))){
  ##Each GPX file when extracted is a list of lists. There is an "overlist" of 
  ##one element, and then a list of some number of other lists. 
  tracktemp1=readGPX(files[x])
  ##Get rid of the "overlist"
  tracktemp2=unlist(tracktemp1$tracks,recursive = F)
  ##Stores a temporary variable n which is how many lists are in the list
  n=length(tracktemp2)
  for(i in 1:n){
    ##looping over every list in the list, make each list a dataframe and
    tempdf=data.frame(tracktemp2[i])
    ##Standardize naming
    colnames(tempdf)=c("lon","lat","ele","time")
    tempdf$ele=as.numeric(tempdf$ele)
    ##Append the dataframe from this list to the bottom of the big dataframe
    GPS1=bind_rows(GPS1,tempdf)
  }
}
##Get the T and Z nonsense out of the time column and make sure it's stored as
## date and time
GPS1$datetime=ymd_hms(paste(substring(GPS1$time,first=1,last=10),
                            substring(GPS1$time,first=12,last=20)))
##Drop the dummy first row from the big dataframe
GPS1=GPS1[-1,]
##Output a csv
write.csv(x = GPS1,file = "C:/Users/John Crockett/Documents/GitHub/URI_SemAqMammals_Muskrat/Data from Field Work/2023 Data/Tracks/GPS 1/GPS1.csv",append = F)

#####
#Extracting tracks: Phone tracks
#####
#Clearing all leftover vars
rm(list=ls())
##Pointing R towards the right folder. From here on in you could run this##
##on a loop but I didn't want to bother. Something like for (z in 1:5){  ##
##  all the stuff below but using paste0 and assign to get GPS[z] to be  ##
##  legible}
setwd("C:/Users/John Crockett/Documents/GitHub/URI_SemAqMammals_Muskrat/Data from Field Work")
##Setting up a dummy dataframe
GPSPhone=data.frame(lon=0,lat=0,ele=0,time="2021-01-13T16:08:30Z")
#make a list of the files in the folder so we can loop over them. 
##NOTE: if you haven't made a csv in this folder yet, the loop should start at
## 1 instead of 2. Also note that the amt subtracted from the length
##  may vary based on whether there is a shapefile present.
unlsted=unlist(unlist(readGPX("Phone Tracks/field-data-april-to-july-23.gpx"),
                      recursive=F),recursive=F)
unlsted2=unlist(unlist(readGPX("Phone Tracks/field-data-thru-april-23.gpx"),
                       recursive=F),recursive=F)
##Loops over all the files
n=length(unlsted)
for(i in 1:n){
  ##looping over every list in the list, make each list a dataframe and
  tempdf=data.frame(unlsted[i])
  ##Standardize naming
  colnames(tempdf)=c("lon","lat","ele","time")
  tempdf$ele=as.numeric(tempdf$ele)
  ##Append the dataframe from this list to the bottom of the big dataframe
  GPSPhone=bind_rows(GPSPhone,tempdf)
}
n=length(unlsted2)
for(i in 1:n){
  ##looping over every list in the list, make each list a dataframe and
  tempdf=data.frame(unlsted2[i])
  ##Standardize naming
  colnames(tempdf)=c("lon","lat","ele","time")
  tempdf$ele=as.numeric(tempdf$ele)
  ##Append the dataframe from this list to the bottom of the big dataframe
  GPSPhone=bind_rows(GPSPhone,tempdf)
}
##Get the T and Z nonsense out of the time column and make sure it's stored as
## date and time
GPSPhone$datetime=ymd_hms(paste(substring(GPSPhone$time,first=1,last=10),
                                substring(GPSPhone$time,first=12,last=20)))
##Drop the dummy first row from the big dataframe
GPSPhone=GPSPhone[-1,]
##Output a csv
write.csv(x = GPSPhone,file = "C:/Users/John Crockett/Documents/GitHub/URI_SemAqMammals_Muskrat/Data from Field Work/Phone Tracks/GPSPhone.csv",append = F)
#####
#         Step Two: Adding landcover to observations
#         Again, this section has not been run (because the 
#         shapefile is too large to push to github), but would
#         theoretically work if the file fit.
#####
# 2021 Data
#####
rm(list=ls())
setwd("C:/Users/John Crockett/Documents/GitHub/URI_SemAqMammals_Muskrat")
#Load in Field Data
Winter21=read_xlsx("Data from Field Work/2021 Data/All Watersheds Winter 2020 2021.xlsx")
Summer21=read_xlsx("Data from Field Work/2021 Data/Summer 2021 All watersheds.xlsx")
FieldData=data.frame(rbind(Winter21,Summer21))
colnames(FieldData)[c(2,6,7)]=c("Site.ID","Start.Time","End.time")
####
#GPS1
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
####
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
####
##Save the GPS layer again so you don't have to re-do the hard part
####
#st_write(GPS1_21,dsn = "Data from Field Work/2021 Data/Tracks/GPS1/GPS1_withLC.shp",append = F)
####
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


####
#GPS2
####
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
####
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
####
##Save the GPS layer again so you don't have to re-do the hard part
####
#st_write(GPS2_21,dsn = "Data from Field Work/2021 Data/Tracks/GPS2/GPS2_withLC.shp",append = F)
####
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

####
#GPS3
####
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
####
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
####
##Save the GPS layer again so you don't have to re-do the hard part
####
#st_write(GPS3_21,dsn = "Data from Field Work/2021 Data/Tracks/GPS3/GPS3_withLC.shp",append = F)
####
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

####
#GPS4
####
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
####
##Save the GPS layer again so you don't have to re-do the hard part
####
#st_write(GPS4_21,dsn = "Data from Field Work/2021 Data/Tracks/GPS4/GPS4_withLC.shp",append = F)
####
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
####
#GPS5
####
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
####
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
####
##Save the GPS layer again so you don't have to re-do the hard part
####
#st_write(GPS5_21,dsn = "Data from Field Work/2021 Data/Tracks/GPS5/GPS5_withLC.shp",append = F)
####
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
####
FieldData[FieldData$UpGrass=="NaN"&(FieldData$Site.ID==61|
                                      FieldData$Site.ID==65),c(2:4,14)]

####
# Now let's see if phone stuff will fill in any gaps
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
####
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
####
###Save the GPS layer again so you don't have to re-do the hard part
####
#st_write(GPSPhone,dsn = "Data from Field Work/Phone Tracks/GPSPhone_withLC.shp",append = F)
####
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

#####
# 2022 Data
#####
#Load in Field Data
FieldData=data.frame(read_xlsx("Data from Field Work/2022 Data/2022 Field Data_withSummer.xlsx"))
####                                 
#GPS2#
####                                
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
####
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
####
##Save the GPS layer again so you don't have to re-do the hard part
####
#st_write(GPS2_22,dsn = "Data from Field Work/2022 Data/Tracks/GPS 2/GPS2_withLC.shp",append = F)
###
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

####
#GPS4
####
####
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
####
###Save the GPS layer again so you don't have to re-do the hard part
#st_write(GPS4_22,dsn = "Data from Field Work/2022 Data/Tracks/GPS 4/GPS4_withLC.shp",append = F)
GPS4_22=st_read("Data from Field Work/2022 Data/Tracks/GPS 4/GPS4_withLC.shp")
####
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
####
#GPS5
####
####
#Load Points, merge landcover
####
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
####
##Save the GPS layer again so you don't have to re-do the hard part
####
#st_write(GPS5_22,dsn = "Data from Field Work/2022 Data/Tracks/GPS 5/GPS5_withLC.shp",append = F)
GPS5_22=st_read("Data from Field Work/2022 Data/Tracks/GPS 5/GPS5_withLC.shp")
####
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

#####
# 2023 Data
#####
#Load in Field Data
FieldData=data.frame(read_xlsx("Data from Field Work/2023 Data/2023 Data.xlsx"))
FieldData=FieldData[,-(20:25)]
####
#GPS1
####
####
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
####
#Save the GPS layer again so you don't have to re-do the hard part
#st_write(GPS1_23,dsn = "Data from Field Work/2023 Data/Tracks/GPS 1/GPS1_withLC.shp",append = F)
GPS1_23=st_read("Data from Field Work/2023 Data/Tracks/GPS 1/GPS1_withLC.shp")
####
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
####
#GPS2
####
#Load Points, merge landcover
#Commented out until new GPS needs to be introduced
####
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
####
##Save the GPS layer again so you don't have to re-do the hard part
####
#st_write(GPS2_23,dsn = "Data from Field Work/2023 Data/Tracks/GPS 2/GPS2_withLC.shp",append = F)
###
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


####
#GPS3
####
####
#Load Points, merge landcover
####
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
####
##Save the GPS layer again so you don't have to re-do the hard part
####
#st_write(GPS3_23,dsn = "Data from Field Work/2023 Data/Tracks/GPS 3/GPS3_withLC.shp",append = F)
GPS3_23=st_read("Data from Field Work/2023 Data/Tracks/GPS 3/GPS3_withLC.shp")
####
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

####
####
#GPS Phone
####
####
#Load Points, merge landcover
####
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
####
##Save the GPS layer again so you don't have to re-do the hard part
####
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
write.csv(x = FieldData,file = "Analyses and R Files/FieldDataWithLC2023.csv",append = F)
####
#####
#         Step Three: Adding weather to observations
#         This should run fine (all files are in place), but
#         took a while the last time I ran it; not clear if this
#         was a slow loop problem or if the open meteo API is slow.
#         It could also have been a connection issue.
#####
rm(list=ls())
setwd("C:/Users/John Crockett/Documents/GitHub/URI_SemAqMammals_Muskrat")
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

#####
#         Step Four: Converting observations into TTD data
#####
###Import the Data, fix some time issues, 
#   keep one site.id*watershed*observer*date*spp*signtype combo (the one
#   with the shortest time to detection)
#####
rm(list=ls())
setwd("C:/Users/John Crockett/Documents/GitHub/URI_SemAqMammals_Muskrat")
FieldAll=read.csv(file ="Analyses and R Files/FieldDataWithLC_Weather.csv")
###Just keep the species
FieldAll=FieldAll[,-1]
FieldAll$Start.Time=as.POSIXct(FieldAll$Start.Time)
FieldAll$End.time=as.POSIXct(FieldAll$End.time)
FieldAll$Time[!is.na(as.numeric(FieldAll$Fresh))]=FieldAll$Fresh
tryvec=which(!is.na(as.numeric(FieldAll$Time)-1))
FieldAll$Time[tryvec]=paste(FieldAll$Date[tryvec],
                            paste(floor(as.numeric(FieldAll$Time[tryvec])*24),
                                  round(60*(as.numeric(FieldAll$Time[tryvec])*24-
                                              floor(as.numeric(FieldAll$Time[tryvec])*24)),
                                        digits = 2),
                                  "00",sep=":"),sep=" ")
FieldAll$Time[!is.na(FieldAll$Time)&
                is.na(ymd_hms(FieldAll$Time))]=paste(
                  FieldAll$Date[!is.na(FieldAll$Time)&
                                  is.na(ymd_hms(FieldAll$Time))]," ",
                  FieldAll$Time[!is.na(FieldAll$Time)&
                                  is.na(ymd_hms(FieldAll$Time))],":00",
                  sep="")
FieldAll$Time[!is.na(FieldAll$Time)]=paste(
  FieldAll$Date[!is.na(FieldAll$Time)],
  substring(FieldAll$Time[!is.na(FieldAll$Time)],first = 12),
  sep=" "
)
FieldAll$Day=as.numeric(difftime(ymd(FieldAll$Date),
                                 floor_date(ymd(FieldAll$Date),
                                            unit = "year"),
                                 units="days"))
FieldAll$Time=ymd_hms(FieldAll$Time,tz= "America/New_York")

FieldAll$Time[which(hour(FieldAll$Time)<7)]=
  FieldAll$Time[which(hour(FieldAll$Time)<7)]+hours(12)
FieldAll[which(FieldAll$Time<FieldAll$Start.Time),c(3:4,7,12,29)]
minute(FieldAll$Time[1984])=46
FieldAll$TTD=as.numeric(difftime(FieldAll$Time,FieldAll$Start.Time,
                           units = "mins"))

FieldAll$YMax=as.numeric(difftime(FieldAll$End.time,FieldAll$Start.Time,
                                  units = "mins"))

which(FieldAll$TTD>FieldAll$YMax)
sppvec=unique(FieldAll$Spp)
FieldAll$Spp[FieldAll$Spp=="otter"|
               FieldAll$Spp=="otter, maybe racoon"]="Otter"
FieldAll$Spp[FieldAll$Spp=="muskrat"]="Muskrat"
FieldAll$Spp[FieldAll$Spp=="beaver"]="Beaver"
FieldAll$Spp[FieldAll$Spp=="N/A"|
               FieldAll$Spp=="n/a"|
               FieldAll$Spp=="no habitat"]=NA
FieldAll$Spp[FieldAll$Spp!="Otter"&
               FieldAll$Spp!="Beaver"&
               FieldAll$Spp!="Muskrat"&
               !is.na(FieldAll$Spp)]="Other"
#Fix the sign types for muskrats
unique(FieldAll$Sign.Type[FieldAll$Spp=="Muskrat"])
FieldAll$Sign.Type[FieldAll$Spp=="Muskrat"&(FieldAll$Sign.Type=="Scat"|
    FieldAll$Sign.Type=="scat fresh"|FieldAll$Sign.Type=="scat old"|
    FieldAll$Sign.Type=="pile of scat"|FieldAll$Sign.Type=="skat")]="scat"
FieldAll$Sign.Type[FieldAll$Spp=="Muskrat"&(FieldAll$Sign.Type=="Track"|
    FieldAll$Sign.Type=="tracks")]="track"
FieldAll$Sign.Type[FieldAll$Spp=="Muskrat"&(
    FieldAll$Sign.Type=="chewed cattail"|
    FieldAll$Sign.Type=="stripped veg"|FieldAll$Sign.Type=="chewed grass"|
    FieldAll$Sign.Type=="chewed bark")]="chewed veg"
FieldAll$Sign.Type[FieldAll$Spp=="Muskrat"&FieldAll$Sign.Type=="jaw bone"]="corpse"
FieldAll$Sign.Type[FieldAll$Spp=="Muskrat"&(
  FieldAll$Sign.Type=="muskrat"|
  FieldAll$Sign.Type=="visual encounter")]="visual observation"
FieldAll$Sign.Type[FieldAll$Spp=="Muskrat"&
                     FieldAll$Sign.Type=="bank den"]="den"
FieldAll$Sign.Type[FieldAll$Spp=="Muskrat"&
                     FieldAll$Sign.Type=="lodge?"]="lodge"
FieldAll$Sign.Type[FieldAll$Spp=="Muskrat"&
                     FieldAll$Sign.Type=="feeding platform"]="platform"
#fix the sign types for beavers
unique(FieldAll$Sign.Type[FieldAll$Spp=="Beaver"])
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  regexpr("lodge",FieldAll$Sign.Type,ignore.case = T)>0]="Lodge"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  regexpr("stick",FieldAll$Sign.Type,ignore.case = T)>0]="chewed stick"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  regexpr("tree",FieldAll$Sign.Type,ignore.case = T)>0]="chewed tree"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  regexpr("Stump",FieldAll$Sign.Type,ignore.case = T)>0]="chewed tree"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  regexpr("log",FieldAll$Sign.Type,ignore.case = T)>0]="chewed tree"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  regexpr("dam",FieldAll$Sign.Type,ignore.case = T)>0]="dam"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  regexpr("ound",FieldAll$Sign.Type,ignore.case = T)>0]="castor mound"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  regexpr("foot",FieldAll$Sign.Type,ignore.case = T)>0]="track"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  regexpr("smell",FieldAll$Sign.Type,ignore.case = T)>0]="castor smell"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&(
        FieldAll$Sign.Type=="caster"|
          FieldAll$Sign.Type=="Caster")]="castor mound"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&(
  FieldAll$Sign.Type=="stripped veg"|
  FieldAll$Sign.Type=="chewed root"|FieldAll$Sign.Type=="chewed shrub"|
  FieldAll$Sign.Type=="chewed veg and stripped pine"|
  FieldAll$Sign.Type=="chewed bark")]="chewed veg"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&(
  FieldAll$Sign.Type=="chewed branch"|
    FieldAll$Sign.Type=="chewed tsump")]="chewed tree"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  FieldAll$Sign.Type=="feed bed"]="food cache"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  FieldAll$Sign.Type=="visual encounter"]="visual observation"
FieldAll$Sign.Type[FieldAll$Spp=="Beaver"&
  FieldAll$Sign.Type=="?"]="chewed tree"

#We're suspicious of non-latrine signs for otters and want to ensure 
# similarity to CB's data
FieldAll$Sign.Type[FieldAll$Spp=="Otter"&
               (FieldAll$Sign.Type=="latrine"|
               FieldAll$Sign.Type=="scat"|
               FieldAll$Sign.Type=="Scat")]="Latrine"
FieldAll$TTD[FieldAll$Spp=="Otter"&
               FieldAll$Sign.Type!="Latrine"]=NA
unique(FieldAll$Fresh)
match(c("Y","y","v"),unique(FieldAll$Fresh))
FieldAll$Fresh[which(!is.na(FieldAll$Fresh)&FieldAll$Fresh!="y"&
                       FieldAll$Fresh!="Y"&
                       FieldAll$Fresh!="v")]="N"
colvec=c(2,3:5,9:11,30,31)
ncolvec=c("Watershed","Site.ID","Date","Observer","Spp","Sign.Type","Fresh",
          "TTD","YMax")

FieldAll$TTD[is.na(FieldAll$TTD)]=-99
FieldAll$New=0
for(z in 1:6){
  tmp=as.matrix(unique(FieldAll[,colvec[z]]))
  for(i in 1:length(tmp)){
    tmp2=which(FieldAll[,colvec[z]]==tmp[i])
    FieldAll$New[tmp2]=FieldAll$New[tmp2]+i*10^(2*(z-1))
  }
}
tmp3=unique(FieldAll$New)
tmp4=vector(length = length(tmp3))
for(i in 1:length(tmp3)){
  tmp4[i]=which(FieldAll$New==tmp3[i]&
                  FieldAll$TTD==min(FieldAll$TTD[FieldAll$New==tmp3[i]]))
}
FieldAll=FieldAll[tmp4,c(2:5,7:11,16:23,26:31)]
FieldAll$TTD[FieldAll$TTD==-99]=NA
write.csv(FieldAll,file="TTDAllYrs.csv")

#####
# Adding site varying covariates
#####
Pawc=st_read("Site Shapefiles/PawcatuckQkmAll.shp")
BIS=st_read("Site Shapefiles/BISQkm.shp")
Quin=st_read("Site Shapefiles/QuinebaugQkm.shp")
Narr=st_read("Site Shapefiles/SurveyedSites2022.shp")
Woon=st_read("Site Shapefiles/WoonMossSelectedQkm.shp")
Black1=st_read("Site Shapefiles/BlackstoneSelectedQkm.shp")
Black2=st_read("Site Shapefiles/BlackstoneNewSelection2.shp")
colnames(Black2)[c(7,9,11,13,15,17,19,21,23,25,29,31,32,34,36,38:40)]=colnames(Black1)
Black2=Black2[,match(colnames(Black1),colnames(Black2))]
Black=rbind(Black1,Black2)
FieldAll$BForest=FieldAll$BSalt=FieldAll$BImp=
  FieldAll$BSwamp=FieldAll$BFresh=FieldAll$BStrm=0
unique(FieldAll$Watershed)

for(i in 1:length(FieldAll$Site.ID)){
  if(FieldAll$Watershed[i]=="BIS"|
     FieldAll$Watershed[i]=="Block Island Sound"){
    FieldAll$BForest[i]=
      BIS$Floodpla_1[match(FieldAll$Site.ID[i],BIS$NewID)]+
      BIS$Forested_1[match(FieldAll$Site.ID[i],BIS$NewID)]
    FieldAll$BSalt[i]=
      BIS$Salt.Wat_1[match(FieldAll$Site.ID[i],BIS$NewID)]+
      BIS$Salt.Mar_1[match(FieldAll$Site.ID[i],BIS$NewID)]+
      BIS$Intertid_1[match(FieldAll$Site.ID[i],BIS$NewID)]+
      BIS$Tidal.Cr_1[match(FieldAll$Site.ID[i],BIS$NewID)]+
      BIS$Tidal.Ri_1[match(FieldAll$Site.ID[i],BIS$NewID)]
    FieldAll$BSwamp[i]=
      BIS$Modified_1[match(FieldAll$Site.ID[i],BIS$NewID)]+
      BIS$Emergent_1[match(FieldAll$Site.ID[i],BIS$NewID)]+
      BIS$Shrub.Sw_1[match(FieldAll$Site.ID[i],BIS$NewID)]+
      BIS$Peatland_1[match(FieldAll$Site.ID[i],BIS$NewID)]
    FieldAll$BFresh[i]=
      BIS$Fresh.Wa_1[match(FieldAll$Site.ID[i],BIS$NewID)]
    FieldAll$BImp[i]=
      BIS$Impervio_1[match(FieldAll$Site.ID[i],BIS$NewID)]
    FieldAll$BStrm[i]=
      BIS$StrmOrder_[match(FieldAll$Site.ID[i],BIS$NewID)]}
  if(FieldAll$Watershed[i]=="Pawcatuck"){
    FieldAll$BForest[i]=
      Pawc$Floodpla_1[match(FieldAll$Site.ID[i],Pawc$NewID)]+
      Pawc$Forested_1[match(FieldAll$Site.ID[i],Pawc$NewID)]
    FieldAll$BSalt[i]=
      Pawc$Salt.Wat_1[match(FieldAll$Site.ID[i],Pawc$NewID)]+
      Pawc$Salt.Mar_1[match(FieldAll$Site.ID[i],Pawc$NewID)]+
      Pawc$Intertid_1[match(FieldAll$Site.ID[i],Pawc$NewID)]+
      Pawc$Tidal.Cr_1[match(FieldAll$Site.ID[i],Pawc$NewID)]+
      Pawc$Tidal.Ri_1[match(FieldAll$Site.ID[i],Pawc$NewID)]
    FieldAll$BSwamp[i]=
      Pawc$Modified_1[match(FieldAll$Site.ID[i],Pawc$NewID)]+
      Pawc$Emergent_1[match(FieldAll$Site.ID[i],Pawc$NewID)]+
      Pawc$Shrub.Sw_1[match(FieldAll$Site.ID[i],Pawc$NewID)]+
      Pawc$Peatland_1[match(FieldAll$Site.ID[i],Pawc$NewID)]
    FieldAll$BFresh[i]=
      Pawc$Fresh.Wa_1[match(FieldAll$Site.ID[i],Pawc$NewID)]
    FieldAll$BImp[i]=
      Pawc$Impervio_1[match(FieldAll$Site.ID[i],Pawc$NewID)]
    FieldAll$BStrm[i]=
      Pawc$StrmOrder_[match(FieldAll$Site.ID[i],Pawc$NewID)]}
  if(FieldAll$Watershed[i]=="Narragansett Bay"|
     FieldAll$Watershed[i]=="Pawtuxet"){
    FieldAll$BForest[i]=
      Narr$Fldpl_1[match(FieldAll$Site.ID[i],Narr$NewID)]+
      Narr$Frstd_1[match(FieldAll$Site.ID[i],Narr$NewID)]
    FieldAll$BSalt[i]=
      Narr$Slt_W_1[match(FieldAll$Site.ID[i],Narr$NewID)]+
      Narr$Slt_M_1[match(FieldAll$Site.ID[i],Narr$NewID)]+
      Narr$Intrt_1[match(FieldAll$Site.ID[i],Narr$NewID)]+
      Narr$Tdl_C_1[match(FieldAll$Site.ID[i],Narr$NewID)]+
      Narr$Tdl_R_1[match(FieldAll$Site.ID[i],Narr$NewID)]
    FieldAll$BSwamp[i]=
      Narr$Modfd_1[match(FieldAll$Site.ID[i],Narr$NewID)]+
      Narr$Emrgn_1[match(FieldAll$Site.ID[i],Narr$NewID)]+
      Narr$Shr_S_1[match(FieldAll$Site.ID[i],Narr$NewID)]+
      Narr$Ptlnd_1[match(FieldAll$Site.ID[i],Narr$NewID)]
    FieldAll$BFresh[i]=
      Narr$Frs_W_1[match(FieldAll$Site.ID[i],Narr$NewID)]
    FieldAll$BImp[i]=
      Narr$Imprv_1[match(FieldAll$Site.ID[i],Narr$NewID)]
    FieldAll$BStrm[i]=
      Narr$StrmOr_[match(FieldAll$Site.ID[i],Narr$NewID)]}
  if(FieldAll$Watershed[i]=="Quinebaug"){
    FieldAll$BForest[i]=
      Quin$Floodpla_1[match(FieldAll$Site.ID[i],Quin$NewID)]+
      Quin$Forested_1[match(FieldAll$Site.ID[i],Quin$NewID)]
    FieldAll$BSalt[i]=
      Quin$Salt.Wat_1[match(FieldAll$Site.ID[i],Quin$NewID)]+
      Quin$Salt.Mar_1[match(FieldAll$Site.ID[i],Quin$NewID)]+
      Quin$Intertid_1[match(FieldAll$Site.ID[i],Quin$NewID)]+
      Quin$Tidal.Cr_1[match(FieldAll$Site.ID[i],Quin$NewID)]+
      Quin$Tidal.Ri_1[match(FieldAll$Site.ID[i],Quin$NewID)]
    FieldAll$BSwamp[i]=
      Quin$Modified_1[match(FieldAll$Site.ID[i],Quin$NewID)]+
      Quin$Emergent_1[match(FieldAll$Site.ID[i],Quin$NewID)]+
      Quin$Shrub.Sw_1[match(FieldAll$Site.ID[i],Quin$NewID)]+
      Quin$Peatland_1[match(FieldAll$Site.ID[i],Quin$NewID)]
    FieldAll$BFresh[i]=
      Quin$Fresh.Wa_1[match(FieldAll$Site.ID[i],Quin$NewID)]
    FieldAll$BImp[i]=
      Quin$Impervio_1[match(FieldAll$Site.ID[i],Quin$NewID)]
    FieldAll$BStrm[i]=
      Quin$StrmOrder_[match(FieldAll$Site.ID[i],Quin$NewID)]}
  if(FieldAll$Watershed[i]=="Blackstone"){
    FieldAll$BForest[i]=
      Black$FloodFor[match(FieldAll$Site.ID[i],Black$NewID)]+
      Black$ForSwamp[match(FieldAll$Site.ID[i],Black$NewID)]
    FieldAll$BSalt[i]=
      Black$SaltMarsh[match(FieldAll$Site.ID[i],Black$NewID)]+
      Black$Saltwat[match(FieldAll$Site.ID[i],Black$NewID)]+
      Black$Intertidal[match(FieldAll$Site.ID[i],Black$NewID)]+
      Black$TideCreek[match(FieldAll$Site.ID[i],Black$NewID)]+
      Black$TideRiv[match(FieldAll$Site.ID[i],Black$NewID)]
    FieldAll$BSwamp[i]=
      Black$ModMarsh[match(FieldAll$Site.ID[i],Black$NewID)]+
      Black$EmMarsh[match(FieldAll$Site.ID[i],Black$NewID)]+
      Black$ShrubSwamp[match(FieldAll$Site.ID[i],Black$NewID)]+
      Black$Peat[match(FieldAll$Site.ID[i],Black$NewID)]
    FieldAll$BFresh[i]=
      Black$FreshWat[match(FieldAll$Site.ID[i],Black$NewID)]
    FieldAll$BImp[i]=
      Black$Impervious[match(FieldAll$Site.ID[i],Black$NewID)]
    FieldAll$BStrm[i]=
      Black$StrmOrder[match(FieldAll$Site.ID[i],Black$NewID)]}
  if(FieldAll$Watershed[i]=="Woonasquatucket-Moshassuck"){
    FieldAll$BForest[i]=
      Woon$FloodFor[match(FieldAll$Site.ID[i],Woon$NewID)]+
      Woon$ForSwamp[match(FieldAll$Site.ID[i],Woon$NewID)]
    FieldAll$BSalt[i]=
      Woon$Saltwat[match(FieldAll$Site.ID[i],Woon$NewID)]+
      Woon$SaltMarsh[match(FieldAll$Site.ID[i],Woon$NewID)]+
      Woon$Intertidal[match(FieldAll$Site.ID[i],Woon$NewID)]+
      Woon$TideCreek[match(FieldAll$Site.ID[i],Woon$NewID)]+
      Woon$TideRiv[match(FieldAll$Site.ID[i],Woon$NewID)]
    FieldAll$BSwamp[i]=
      Woon$ModMarsh[match(FieldAll$Site.ID[i],Woon$NewID)]+
      Woon$EmMarsh[match(FieldAll$Site.ID[i],Woon$NewID)]+
      Woon$ShrubSwamp[match(FieldAll$Site.ID[i],Woon$NewID)]+
      Woon$Peat[match(FieldAll$Site.ID[i],Woon$NewID)]
    FieldAll$BFresh[i]=
      Woon$FreshWat[match(FieldAll$Site.ID[i],Woon$NewID)]
    FieldAll$BImp[i]=
      Woon$Impervious[match(FieldAll$Site.ID[i],Woon$NewID)]
    FieldAll$BStrm[i]=
      Woon$StrmOrder[match(FieldAll$Site.ID[i],Woon$NewID)]}
}

vec=unique(FieldAll[,c(1,2)])
write.csv(FieldAll,file="Analyses and R Files/TTDAllYrs.csv")

#####
#         Step Five: Cleaning up many, many, messy parts of the
#         data. The goal here is to have no NAs in covariates, bc
#         JAGS won't like it if we have NAs there. The basic
#         approach for NA values is:
#         a) use the other observer's weather and landcover from
#            the same site and date. Observers sometimes took
#            different paths, but usually had the same mode of
#            travel and went to similar places.
#         b) use the same observer's landcover from a different 
#            time (or another observer's landcover from a different
#            time). Less optimal, but should still work fairly well
#         c) estimate landcover using a simple model. Hopefully 
#            this is slightly better than just taking the mean;
#            I've seen suggestions to do this within the JAGS 
#            model but at this point there aren't many NAs left and
#            it doesn't seem worth it.
#         d) take the weather for the correct time from the centroid
#            of the watershed. 
setwd("C:/Users/John Crockett/Documents/GitHub/URI_SemAqMammals_Muskrat")
rm(list=ls())
TTDFrame=read.csv(file="Analyses and R Files/TTDAllYrs.csv")
#####
# Check which covariates that have NA values
#       Handy to re-run after fixing some parts
#####
names=colnames(TTDFrame)[c(2,5,10:19,22:28)]
vec=vector(length = length(names))
vec2=c(2,5,10:19,22:28)
for(i in 1:length(vec)){
  vec[i]=length(which(is.na(TTDFrame[,vec2[i]])))
}
names(vec)=names
vec
#####
# Cleaning up the watershed naming and fixing the strm order
#####
TTDFrame$Watershed[TTDFrame$Watershed=="Block Island Sound"]="BIS"
TTDFrame$BStrm[is.na(TTDFrame$BStrm)]=0
#Dropping one observation where nothing was observed (no GPS, 
#   no start or end time, no observations of spp)
TTDFrame=TTDFrame[-which(is.na(TTDFrame$Start.Time)&
                           is.na(TTDFrame$End.time)&
                           is.na(TTDFrame$Spp)&
                           is.na(TTDFrame$cloudcover)),]
#####
# Cleaning missing modes
#####
unique(TTDFrame$Mode)
TTDFrame$Mode[regexpr("kayak",TTDFrame$Mode,ignore.case = T)>0]="kayak"
TTDFrame$Mode[regexpr("foot",TTDFrame$Mode,ignore.case = T)>0]="foot"
TTDFrame$Mode[regexpr("walk",TTDFrame$Mode,ignore.case = T)>0]="foot"

vec=which(is.na(TTDFrame$Mode))
TTDFrame[TTDFrame$Site.ID==TTDFrame$Site.ID[vec[1]]&
           TTDFrame$Watershed==TTDFrame$Watershed[vec[1]]&
           TTDFrame$Date==TTDFrame$Date[vec[1]],c(2:5,12)]
TTDFrame[TTDFrame$Site.ID==TTDFrame$Site.ID[vec[2]]&
           TTDFrame$Watershed==TTDFrame$Watershed[vec[2]]&
           TTDFrame$Date==TTDFrame$Date[vec[2]],c(2:5,12)]
#confirmed both are foot by comparing to other observer and reviewing notes
TTDFrame$Mode[vec]="foot"
#####
# Check which covariates that have NA values
#       Handy to re-run after fixing some parts
#####
names=colnames(TTDFrame)[c(2,5,10:30)]
vec=vector(length = length(names))
vec2=c(2,5,10:30)
for(i in 1:length(vec)){
  vec[i]=length(which(is.na(TTDFrame[,vec2[i]])))
}
names(vec)=names
vec
#Now we're down to landcover from observers and weather
#####
# Cleaning missing Observer Land Cover
#####
#Finding which entries are missing landcover
vec=which(is.na(TTDFrame$Water))
# Checking that missing one landcover class means that all are missing
sum(is.na(TTDFrame[!is.na(TTDFrame$Water),12:16]))
sum(!is.na(TTDFrame[is.na(TTDFrame$Water),12:16]))
#Identifying missing landcovers that can be easily replaced by
#   the other observer on the same day I only used this to
#   look at thing and never involved biglist in other code, 
#   so we could delete this chunk no problem
biglist=list()
for (i in 1:length(vec)){
  biglist[[i]]=TTDFrame[TTDFrame$Site.ID==TTDFrame$Site.ID[vec[i]]&
                          TTDFrame$Watershed==TTDFrame$Watershed[vec[i]]&
                          TTDFrame$Date==TTDFrame$Date[vec[i]]&
                          TTDFrame$Observer!=TTDFrame$Observer[vec[i]],
                        c(2:5,13:18)]
}
#Replace ones where the other observer has landcover
for (v in 13:18){
  for (i in 1:length(vec)){
    tmp=TTDFrame[TTDFrame$Site.ID==TTDFrame$Site.ID[vec[i]]&
                   TTDFrame$Watershed==TTDFrame$Watershed[vec[i]]&
                   TTDFrame$Date==TTDFrame$Date[vec[i]],v]
    if(sum(is.finite(unique(tmp)))==1){
      TTDFrame[vec[i],v]=sum(unique(tmp),na.rm=T)
    }
  }
}
check=TTDFrame[vec,c(2:5,13:18)]
# Re-check which ones are missing landcover
vec=which(is.na(TTDFrame$Water))
# Check if some were visited a different time by the same observer,
#   usually the paths are similar enough
biglist=list()
for (i in 1:length(vec)){
  biglist[[i]]=TTDFrame[TTDFrame$Site.ID==TTDFrame$Site.ID[vec[i]]&
                          TTDFrame$Watershed==TTDFrame$Watershed[vec[i]]&
                          TTDFrame$Observer==TTDFrame$Observer[vec[i]],
                        c(2:5,13:18)]
}
#Replace ones where there is landcover from other date
for (v in 13:18){
  for (i in 1:length(vec)){
    tmp=TTDFrame[TTDFrame$Site.ID==TTDFrame$Site.ID[vec[i]]&
                   TTDFrame$Watershed==TTDFrame$Watershed[vec[i]]&
                   TTDFrame$Observer==TTDFrame$Observer[vec[i]],v]
    if(sum(is.finite(unique(tmp)))==1){
      TTDFrame[vec[i],v]=sum(unique(tmp),na.rm=T)
    }
  }
}
check=TTDFrame[vec,c(2:5,13:18)]
# Re-run the first; this will apply landcover from any observer
#   on any date where we have landcover to all observer and date
#   combinations that are missing landcover
vec=which(is.na(TTDFrame$Water))
for (v in 13:18){
  for (i in 1:length(vec)){
    tmp=TTDFrame[TTDFrame$Site.ID==TTDFrame$Site.ID[vec[i]]&
                   TTDFrame$Watershed==TTDFrame$Watershed[vec[i]]&
                   TTDFrame$Date==TTDFrame$Date[vec[i]],v]
    if(sum(is.finite(unique(tmp)))==1){
      TTDFrame[vec[i],v]=sum(unique(tmp),na.rm=T)
    }
  }
}
vec=which(is.na(TTDFrame$Water))
check=TTDFrame[vec,]

# For the remaining missing landcover, we will make a basic model
#   which will find the expected landcover given the site-level
#   covariate landcover values, the watershed, and the mode of travel
lcmodel1=lm(Water~BStrm+BFresh+BSwamp+BImp+BSalt+BForest+
              Mode,data = TTDFrame)
lcmodel2=lm(Wetland~BStrm+BFresh+BSwamp+BImp+BSalt+BForest+
              Mode,data = TTDFrame)
lcmodel3=lm(Built~BStrm+BFresh+BSwamp+BImp+BSalt+BForest+
              Mode,data = TTDFrame)
lcmodel4=lm(UpGrass~BStrm+BFresh+BSwamp+BImp+BSalt+BForest+
              Mode,data = TTDFrame)
lcmodel5=lm(Shrub~BStrm+BFresh+BSwamp+BImp+BSalt+BForest+
              Mode,data = TTDFrame)
lcmodel6=lm(Forest~BStrm+BFresh+BSwamp+BImp+BSalt+BForest+
              Mode,data = TTDFrame)
for(i in 1:length(vec)){
  tmp=as.vector(as.numeric(c(1,TTDFrame[vec[i],c(25:30)],
                             ifelse(TTDFrame$Mode[vec[i]]=="kayak",1,0))))
  TTDFrame$Water[vec[i]]  =lcmodel1$coefficients%*%tmp
  TTDFrame$Wetland[vec[i]]=lcmodel2$coefficients%*%tmp  
  TTDFrame$Built[vec[i]]  =lcmodel3$coefficients%*%tmp
  TTDFrame$UpGrass[vec[i]]=lcmodel4$coefficients%*%tmp  
  TTDFrame$Shrub[vec[i]]  =lcmodel5$coefficients%*%tmp
  TTDFrame$Forest[vec[i]] =lcmodel6$coefficients%*%tmp 
}
check=TTDFrame[vec,]
#####
# Check which covariates that have NA values
#       Handy to re-run after fixing some parts
#####
names=colnames(TTDFrame)[c(2,5,10:30)]
vec=vector(length = length(names))
vec2=c(2,5,10:30)
for(i in 1:length(vec)){
  vec[i]=length(which(is.na(TTDFrame[,vec2[i]])))
}
names(vec)=names
vec
# Still seeing some sites with missing wetland and forest values 
TTDFrame[is.na(TTDFrame$Wetland),]
TTDFrame[TTDFrame$Site.ID==2197,c(2:5,14,18)]
# Set the forest and wetland to be the same in the two visits in 
#     winter 2020-2021
TTDFrame[518:521,c(14,18)]=TTDFrame[526:529,c(14,18)]
#####
# Fixing weather problems
#####
vec=which(is.na(TTDFrame$Precip))
sum(is.na(TTDFrame$Precip[!is.na(TTDFrame$cloudcover)]))
sum(is.na(TTDFrame$Precip[!is.na(TTDFrame$Temp)]))
which(is.na(TTDFrame$Temp))
TTDFrame[vec,19:21]
#Identifying missing weather vars that can be easily replaced by
#   the other observer on the same day I only used this to
#   look at things and never involved biglist in other code, 
#   so we could delete this chunk no problem
biglist=list()
for (i in 1:length(vec)){
  biglist[[i]]=TTDFrame[TTDFrame$Site.ID==TTDFrame$Site.ID[vec[i]]&
                          TTDFrame$Watershed==TTDFrame$Watershed[vec[i]]&
                          TTDFrame$Date==TTDFrame$Date[vec[i]]&
                          TTDFrame$Observer!=TTDFrame$Observer[vec[i]],
                        c(2:5,19:21)]
}
# Once again there is ample opportunity to replace one observer's
#   weather with the other observer's. Assuming the start times 
#   don't round to different hours, this should be exactly right.
for (v in 19:21){
  for (i in 1:length(vec)){
    tmp=TTDFrame[TTDFrame$Site.ID==TTDFrame$Site.ID[vec[i]]&
                   TTDFrame$Watershed==TTDFrame$Watershed[vec[i]]&
                   TTDFrame$Date==TTDFrame$Date[vec[i]],v]
    if(sum(is.finite(unique(tmp)))==1){
      TTDFrame[vec[i],v]=sum(unique(tmp),na.rm=T)
    }
  }
}
check=TTDFrame[vec,c(2:5,19:21)]
# Now let's load in the shapefiles for the watersheds, and delete
#     some other objects that are now clutter.
rm(list = c("lcmodel1","lcmodel2","lcmodel3","lcmodel4","lcmodel5",
            "lcmodel6","biglist"))
wshed=st_read("Other Shapefiles/AllWatershedsUsed.shp")
#check which ones we need the centroid of
unique(TTDFrame$Watershed[is.na(TTDFrame$cloudcover)])
wshed_dissolve=wshed %>% group_by(Watershed) %>% summarize()
centrds=st_centroid(wshed_dissolve)
centrds$Watershed[centrds$Watershed=="Block Island Sound"]="BIS"
vec=which(is.na(TTDFrame$Precip))
#Find the position in the watersheds of centrds for the correct
#   watershed for each weather NA
weatherframe=TTDFrame[vec,]
weatherframe$WSHED=0
for(i in 1:length(vec)){
  weatherframe$WSHED[i]=which(centrds$Watershed==weatherframe$Watershed[i])
}
# Now we run a loop. Much faster than the previous weather loop
#   because there are 13 instead of thousands and because there
#   isn't an if statement inside it. Note that sf_coordinates()
#   supplies coordinates as (x,y) or (lon,lat) and weather_history()
#   is looking for lat long. So the rev is crucial tho you'll 
#   hopefully notice if all the temps are minus 20 C bc they are
#   from ~71.5 degrees south. 
for(i in 1:length(vec)){
    tempweather=as.data.frame(weather_history(
      location=rev(st_coordinates(centrds$geometry[weatherframe$WSHED[i]])),
      start =TTDFrame$Date[vec[i]],
      end = TTDFrame$Date[vec[i]],
      hourly = c("temperature_2m","cloudcover"),daily = "precipitation_sum"))
    
    TTDFrame$Temp[vec[i]]=tempweather[hour(round_date(as.POSIXct(TTDFrame$Start.Time[vec[i]]),"hours"))+1,4]
    TTDFrame$cloudcover[vec[i]]=tempweather[hour(round_date(as.POSIXct(TTDFrame$Start.Time[vec[i]]),"hours"))+1,5]
    TTDFrame$Precip[vec[i]]=tempweather[1,3]
  print(i/13)
}
#check that everything worked as expected. Note there is still 
# one site with an NA temp. It's not clear why. But we can set the
# two temps that are different from the rest from that day and 
# time as the same as the rest.
TTDFrame[vec,c(2:5,19:21)]

TTDFrame[TTDFrame$Site.ID==195,c(2:6,19)]
TTDFrame$Temp[TTDFrame$Site.ID==195&
           TTDFrame$Date=="2021-01-20"&
           (is.na(TTDFrame$Temp)|
           TTDFrame$Temp!=1.7)]=1.7
#Look again for missing covariates
names=colnames(TTDFrame)[c(2,5,10:30)]
vec=vector(length = length(names))
vec2=c(2,5,10:30)
for(i in 1:length(vec)){
  vec[i]=length(which(is.na(TTDFrame[,vec2[i]])))
}
names(vec)=names
vec
#There are none hooray.
write.csv(TTDFrame,"Analyses and R Files/TTDAllYrs_cleaned.csv")
