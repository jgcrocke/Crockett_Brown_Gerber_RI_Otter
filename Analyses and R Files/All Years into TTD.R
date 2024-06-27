library(tidyr)
library(readxl)
library(sf)
library(lubridate)
setwd("C:/Users/John Crockett/Documents/URI")
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
tryvec=which(!is.na(as.numeric(FieldAll$Time)))
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
neg=which(FieldAll$TTD<0)
miltime=which((hour(FieldAll$Start.Time[neg])-hour(FieldAll$Time[neg]))>10)
FieldAll$TTD[neg[miltime]]=FieldAll$TTD[neg[miltime]]+3600
FieldAll$YMax=as.numeric(FieldAll$SurveyTime)
FieldAll$Day=as.numeric(difftime(ymd(FieldAll$Date),
                                 floor_date(ymd(FieldAll$Date),
                                            unit = "year"),
                                 units="days"))

colvec=c(2,3,5,9,10,4,32,33)
ncolvec=c("Watershed","Site.ID","Observer","Spp","Sign.Type","Date","TTD","YMax")

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
colnames(FieldAll)
FieldAll=FieldAll[tmp4,c(2:5,9:10,16:23,26:28,32:34)]
FieldAll$TTD[FieldAll$TTD==-99]=NA
write.csv(FieldAll,file="TTDAllYrs.csv")

#####
# Adding site varying covariates
#####
Pawc=st_read("Fieldwork prep site selection/Pawcatuck/PawcatuckQkmAll.shp")
BIS=st_read("Fieldwork prep site selection/Block Island Sound/BISQkm.shp")
Quin=st_read("Fieldwork prep site selection/Quinebaug/QuinebaugQkm.shp")
Narr=st_read("Fieldwork prep site selection/SurveyedSites2022.shp")
Woon=st_read("GIS Data/2023 Sites/WoonMossSelectedQkm.shp")
Black1=st_read("GIS Data/2023 Sites/BlackstoneSelectedQkm.shp")
Black2=st_read("GIS Data/2023 Sites/BlackstoneNewSelection2.shp")
colnames(Black2)[c(7,9,11,13,15,17,19,21,23,25,29,31,32,34,36,38:40)]=colnames(Black1)
Black2=Black2[,match(colnames(Black1),colnames(Black2))]
Black=rbind(Black1,Black2)
FieldAll$BForest=FieldAll$BSalt=FieldAll$BImp=
  FieldAll$BSwamp=FieldAll$BFresh=FieldAll$BStrm=0
unique(FieldAll$Watershed)
match(FieldAll$Site.ID[FieldAll$Watershed=="Blackstone"],
      Black$NewID)

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
OCCCovs=FieldAll[rownames(vec),c(1,2,21:26)]
write.csv(OCCCovs,file="Analyses and R Files/OCCCovsAllYrs.csv")
write.csv(FieldAll,file="Analyses and R Files/TTDAllYrs.csv")

