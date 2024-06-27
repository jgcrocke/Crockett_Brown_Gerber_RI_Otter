library(plotKML)
library(dplyr)
library(lubridate)
getwd()
###Clearing all leftover vars
rm(list=ls())
##Pointing R towards the right folder. Form here on in you could run this##
##on a loop but I didn't want to bother. Something like for (z in 1:5){  ##
##  all the stuff below but using paste0 and assign to get GPS[z] to be  ##
##  legible}
setwd("C:/Users/John Crockett/Documents/URI/Data from Field Work/2023 Data/Tracks/GPS 1")
##Setting up a dummy dataframe
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
write.csv(x = GPS1,file = "C:/Users/John Crockett/Documents/URI/Data from Field Work/2023 Data/Tracks/GPS 1/GPS1.csv",append = F)
write.csv(x=unique(date(GPS3$datetime)),file="C:/Users/John Crockett/Documents/URI/Data from Field Work/2023 Data/Tracks/GPS3Days.csv",append = F)

#####
#Above was for GPS, below is for phone tracks
library(plotKML)
library(dplyr)
library(lubridate)
getwd()
###Clearing all leftover vars
rm(list=ls())
##Pointing R towards the right folder. From here on in you could run this##
##on a loop but I didn't want to bother. Something like for (z in 1:5){  ##
##  all the stuff below but using paste0 and assign to get GPS[z] to be  ##
##  legible}
setwd("C:/Users/John Crockett/Documents/URI/Data from Field Work")
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
write.csv(x = GPSPhone,file = "C:/Users/John Crockett/Documents/URI/Data from Field Work/Phone Tracks/GPSPhone.csv",append = F)
