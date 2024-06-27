library(tidyr)
library(readxl)
library(sf)
library(lubridate)
setwd("C:/Users/John Crockett/Documents/URI")

#####
#2021 Only All Spp into TTD
#####
winter=read_xlsx("Data from Field Work/Winter 2020 2021/All Watersheds winter 2020 2021.xlsx")
summer=read_xlsx("Data from Field Work/Summer 2021/summer 2021 All Watersheds.xlsx")

#Combining winter and summer 2021 and keeping only relevant columns
summer=summer[,colnames(summer)[1:15]]
wnames=colnames(winter)[c(1:13,17,18)]
winter=winter[,wnames]
BothSeasons=rbind(winter,summer)
BothSeasons$SurveyTime=BothSeasons$`End time`- BothSeasons$`Start Time`
BothSeasons$Day=julian.POSIXt(BothSeasons$Date,origin=min(BothSeasons$Date))
BothSeasons$Day=as.numeric(BothSeasons$Day)
BothSeasons$SurveyTime=as.numeric(BothSeasons$SurveyTime)
BothSeasons$TTD=BothSeasons$Time-BothSeasons$`Start Time`
BothSeasons$TTD=as.numeric(BothSeasons$TTD)/60
BothSeasons=BothSeasons[,c("Watershed","Site ID","Observer","Spp","Day","SurveyTime","TTD")]

BothSeasons$Spp[is.na(BothSeasons$Spp)]=0
BothSeasons$Spp[BothSeasons$Spp=="Beaver"]="beaver"
BothSeasons$Spp[BothSeasons$Spp=="Muskrat"]="muskrat"
BothSeasons$Watershed[BothSeasons$Watershed=="BIS"]="Block Island Sound"
BothSeasons$Spp[BothSeasons$Spp!="beaver"&BothSeasons$Spp!="otter"&BothSeasons$Spp!="muskrat"&BothSeasons$Spp!=0]="other"
BothSeasons$Site.ID=BothSeasons$`Site ID`
BothSeasons$TTD[is.na(BothSeasons$TTD)]=-99
BothSeasons$New=0
for(z in 1:5){
  tmp=as.matrix(unique(BothSeasons[,z]))
  for(i in 1:length(tmp)){
    tmp2=which(BothSeasons[,z]==tmp[i])
    BothSeasons$New[tmp2]=BothSeasons$New[tmp2]+i*10^(2*(z-1))
  }
}
tmp3=unique(BothSeasons$New)
tmp4=vector(length = length(tmp3))
for(i in 1:length(tmp3)){
  tmp4[i]=which(BothSeasons$New==tmp3[i]&
                  BothSeasons$TTD==min(BothSeasons$TTD[BothSeasons$New==tmp3[i]]))
}
BothSeasons=BothSeasons[tmp4,c(1,8,3:7)]
BothSeasons$TTD[BothSeasons$TTD==-99]=NA

write.csv(BothSeasons,file="TTD21.csv")

#####
#2022 Only all Spp into TTD
#####
###Import the Data
rm(list=ls())
Both22=read_excel("Data from Field Work/2022 Data/2022 Field Data_withSummer.xlsx")
###Just keep the species
Both22$SurveyTime=Both22$`End time` - Both22$`Start Time`
Both22$FindTime=Both22$Time-Both22$`Start Time`
Both22$TTD=as.numeric(Both22$FindTime)/60
Both22$YMax=as.numeric(Both22$SurveyTime)
colnames(Both22)[2]="Site.ID"
Both22=Both22[,c("Watershed","Site.ID","Observer","Spp","Date","TTD","YMax")]
Both22$Day=as.numeric(difftime(Both22$Date,min(Both22$Date),units="days")+5)


Both22$TTD[is.na(Both22$TTD)]=-99
Both22$New=0
for(z in 1:5){
  tmp=as.matrix(unique(Both22[,z]))
  for(i in 1:length(tmp)){
    tmp2=which(Both22[,z]==tmp[i])
    Both22$New[tmp2]=Both22$New[tmp2]+i*10^(2*(z-1))
  }
}
tmp3=unique(Both22$New)
tmp4=vector(length = length(tmp3))
for(i in 1:length(tmp3)){
  tmp4[i]=which(Both22$New==tmp3[i]&
                  Both22$TTD==min(Both22$TTD[Both22$New==tmp3[i]]))
}
Both22=Both22[tmp4,c(1:4,8,6,7)]
Both22$TTD[Both22$TTD==-99]=NA
write.csv(Both22,file="TTD22.csv")








#####
#2023 Only all spp into TTD
#####
#####
###Import the Data
rm(list=ls())
FieldAll=read.csv(file ="Analyses and R Files/FieldDataWithLC_Weather.csv")
###Just keep the species
FieldAll=FieldAll[,-1]
FieldAll$Start.Time=as.POSIXct(FieldAll$Start.Time)
FieldAll$End.time=as.POSIXct(FieldAll$End.time)
FieldAll$SurveyTime=as.numeric(FieldAll$End.time- FieldAll$Start.Time)
FieldAll$NewTime=NA
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
FieldAll$Time=ymd_hms(FieldAll$Time)

FieldAll$FindTime=FieldAll$Time-FieldAll$Start.Time
FieldAll$TTD=as.numeric(FieldAll$FindTime)/60
FieldAll$YMax=as.numeric(FieldAll$SurveyTime)
FieldAll$Day=as.numeric(difftime(ymd(FieldAll$Date),
                                 floor_date(ymd(FieldAll$Date),
                                            unit = "year"),
                                 units="days"))

colvec=c(2,3,5,9,4,32,33)
ncolvec=c("Watershed","Site.ID","Observer","Spp","Date","TTD","YMax")

FieldAll$TTD[is.na(FieldAll$TTD)]=-99
FieldAll$New=0
for(z in 1:5){
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
FieldAll=FieldAll[tmp4,c(2:4,7:9,16:23,26:28,32:34)]
FieldAll$TTD[FieldAll$TTD==-99]=NA
write.csv(FieldAll,file="TTDAllYrs.csv")








