#####
##This file is to set up the data for an integrated species
# distribution model (like in Koshkina et al. 2017). We will link
# presence/absence data for otters to latrine and roadkill data. 
#####
#####
#Packages
#####

library(sf)
library(lubridate)
library(openmeteo)
library(raster)
library(rgdal)
library(ggplot2)
library(rasterVis)
library(landscapemetrics)
library(exactextractr)
library(dplyr)
#####
#Basics
#####
setwd("C:/Users/John Crockett/Documents/GitHub/URI_SemAqMammals_Muskrat")

rm(list=ls())

#####
#Load in presence/absence data and join to surveyed sites
#####
TTDFrame=read.csv(file="Analyses and R Files/TTDAllYrs_cleaned.csv")
TTDFrame$TTD[TTDFrame$Spp!="Otter"]=NA
OtterTTD=TTDFrame[,-c(1,2)]
OtterTTD$Spp[OtterTTD$Spp!="Otter"]=NA
OtterTTD=OtterTTD[!duplicated(OtterTTD[,c(2:5)]),]
OtterTTD$km=NA
OtterTTD$Watershed[OtterTTD$Watershed=="BIS"]="Block Island Sound"
surveyed=st_read("Site Shapefiles/SurveyedSitesJoinedtoKm.shp")
km=st_read("Site Shapefiles/km_with_NLCD_Allyrs.shp")

for(i in 1:length(OtterTTD$Watershed)){
  OtterTTD$km[i]=surveyed$id[surveyed$Watershed==OtterTTD$Watershed[i]&
                               surveyed$Site_ID==OtterTTD$Site.ID[i]]
}
OtterTTD=OtterTTD[!is.na(OtterTTD$km),]
write.csv(OtterTTD,file = "OtterTTD_kmlink.csv")

###
# Don't need to re-run this section
#######
#fishboat=st_read("Other Shapefiles/fishBoatAcc12.shp")
#centrqkm=st_centroid(qkm[,"NewID"])
#fishboat=st_transform(fishboat,st_crs(centrqkm))
#centrqkm$distance=apply(st_distance(centrqkm,fishboat),1,min)
#qkm$boatd=centrqkm$distance

#####
# Below is some NLCD joining that we've already done,
# commented out
#####
#setwd(dir ="C:/Users/John Crockett/Documents/URI")
#
####
##Read in Data and reproject as needed
####
#NLCD_2019=raster("NRS 533/LandCoverData/NLCD_2019_Land_Cover_L48_20210604_KthBztjO2mB7n4VQtv3r.tiff")
#NLCD_2016=raster("NRS 533/LandCoverData/NLCD_2016_Land_Cover_L48_20210604_KthBztjO2mB7n4VQtv3r.tiff")
#NLCD_2013=raster("NRS 533/LandCoverData/NLCD_2013_Land_Cover_L48_20210604_KthBztjO2mB7n4VQtv3r.tiff")
#NLCD_2011=raster("NRS 533/LandCoverData/NLCD_2011_Land_Cover_L48_20210604_KthBztjO2mB7n4VQtv3r.tiff")
#NLCD_2008=raster("NRS 533/LandCoverData/NLCD_2008_Land_Cover_L48_20210604_KthBztjO2mB7n4VQtv3r.tiff")
#NLCD_2006=raster("NRS 533/LandCoverData/NLCD_2006_Land_Cover_L48_20210604_KthBztjO2mB7n4VQtv3r.tiff")
#NLCD_2004=raster("NRS 533/LandCoverData/NLCD_2004_Land_Cover_L48_20210604_KthBztjO2mB7n4VQtv3r.tiff")
#NLCD_2001=raster("NRS 533/LandCoverData/NLCD_2001_Land_Cover_L48_20210604_KthBztjO2mB7n4VQtv3r.tiff")
#rasterlist=list(NLCD_2001,NLCD_2004,NLCD_2006,NLCD_2008,
#                NLCD_2011,NLCD_2013,NLCD_2016,NLCD_2019)
#LCNames=c("N01","N04","N06","N08","N11","N13","N16","N18")
#sr <- crs(NLCD_2019)
##NLCD_2019=projectRaster(NLCD_2019,crs=sr)
#RIBounds=st_transform(km,crs=sr)
####
##Define a matrix for reclassifyng raster
####
#ClassMatrix=matrix(ncol=3,byrow=T,c(0,11,1,
#                                    11,25,5,
#                                    25,32,6,
#                                    32,43,4,
#                                    43,82,6,
#                                    82,90,3,
#                                    90,96,2))
#NamedMatrix=matrix(ncol=3,byrow=T,c(0,11,1,11,25,5,25,32,6,32,43,4,43,82,6,82,90,3,90,96,2))
#NamedMatrix=cbind(NamedMatrix,c("Open Water","Urban","Other","Forest","Other","Forested Wetland","Open Wetland"))
#colnames(NamedMatrix)=c("From","To","Becomes","Name")
##Getting the names right/keeping track
#order(NamedMatrix[,c(3,4)])
#name1=vector(length=6)
#for(i in 1:6){
#  name1[i]=NamedMatrix[which(as.numeric(NamedMatrix[,3])==i),4]
#}
##This little function will help us later shoutout stackoverflow
#sum_cover <- function(x){
#  list(x %>%
#         group_by(value) %>%
#         summarize(total_area = sum(coverage_area,na.rm = T)) %>%
#         mutate(proportion = total_area/sum(total_area,na.rm = T)))
#  
#}
##Setting up some columns for past landcover
#km$N19_Water=
#  km$N19_OWetland=km$N19_FWetland=km$N19_Forest=km$N19_Urban=
#  km$N16_Water=km$N16_OWetland=km$N16_FWetland=km$N16_Forest=km$N16_Urban=
#  km$N13_Water=km$N13_OWetland=km$N13_FWetland=km$N13_Forest=km$N13_Urban=
#  km$N11_Water=km$N11_OWetland=km$N11_FWetland=km$N11_Forest=km$N11_Urban=
#  km$N08_Water=km$N08_OWetland=km$N08_FWetland=km$N08_Forest=km$N08_Urban=
#  km$N06_Water=km$N06_OWetland=km$N06_FWetland=km$N06_Forest=km$N06_Urban=
#  km$N04_Water=km$N04_OWetland=km$N04_FWetland=km$N04_Forest=km$N04_Urban=
#  km$N01_Water=km$N01_OWetland=km$N01_FWetland=km$N01_Forest=km$N01_Urban=0
#newcolnames=vector(length = 40)
#for(i in 1:8){
#  newcolnames[(1+(i-1)*5):(5+(i-1)*5)]=
#    rev(colnames(km)[(8+(i-1)*5):(12+(i-1)*5)])
#}
#colnames(km)=c(colnames(km)[1:7],newcolnames)
#rec_rasterlist=list()
#for(z in 1:length(rasterlist)){
#  
#
####
##Crop the data to the watersheds of interest
####
#rasterlist[[z]] <- crop(rasterlist[[z]], extent(RIBounds))
#rasterlist[[z]] <- mask(rasterlist[[z]], RIBounds)
##Reclassify
#rec_rasterlist[[z]]=reclassify(x=rasterlist[[z]],rcl = ClassMatrix)
#
#
##extract the area of each raster cell covered by the plot and summarize
#extresults<- exact_extract(x=rec_rasterlist[[z]],
#                           y = km,coverage_area=T, 
#                           summarize_df = TRUE, fun = sum_cover)
#
#
##Filling in the new columns with the prop of each site covered by
##   each raster class
#for(i in 1:length(km$id)){
#  tempdf=as.data.frame(extresults[[i]])
#  for (v in 1:5){
#    if(sum(tempdf$value==v,na.rm = T)>0){
#      km[i,v+7+(5*(z-1))]=tempdf$proportion[tempdf$value==v&
#                                      !is.na(tempdf$value)]
#    }
#  }
#}
#}
#km$N01_Wet=km$N01_OWt+km$N01_FWt
#km$N04_Wet=km$N04_OWt+km$N04_FWt
#km$N06_Wet=km$N06_OWt+km$N06_FWt
#km$N08_Wet=km$N08_OWt+km$N08_FWt
#km$N11_Wet=km$N11_OWt+km$N11_FWt
#km$N13_Wet=km$N13_OWt+km$N13_FWt
#km$N16_Wet=km$N16_OWt+km$N16_FWt
#km$N19_Wet=km$N19_OWt+km$N19_FWt
#km=km[,-match(c("N01_OWt",
#          "N04_OWt",
#          "N06_OWt",
#          "N08_OWt",
#          "N11_OWt",
#          "N13_OWt",
#          "N16_OWt",
#          "N19_OWt",
#          "N01_FWt",
#          "N04_FWt",
#          "N06_FWt",
#          "N08_FWt",
#          "N11_FWt",
#          "N13_FWt",
#          "N16_FWt",
#          "N19_FWt"),colnames(km))]
#
#
#
#
#setwd("C:/Users/John Crockett/Documents/GitHub/URI_SemAqMammals_Muskrat")
#
#st_write(km,"Site Shapefiles/km_with_NLCD_Allyrs.shp",append = F)
#####
# Load in roads and traffic, add roads to Qkm
#####
setwd("C:/Users/John Crockett/Documents/GitHub/URI_SemAqMammals_Muskrat")

road.seg.k=st_read("Other Shapefiles/Roads_km_split.shp")
#traffic.seg.k=st_read("Other Shapefiles/Traffic_Km_Split.shp")
road.seg.k$HWY_LANE=road.seg.k$LANES
road.seg.k$HWY_LANE[road.seg.k$HWY_LANE==0]=NA
roads.major=road.seg.k[road.seg.k$F_SYSTEM<7,]
plot(roads.major["F_SYSTEM"])



km$roads=NA
km$hwylanes=NA
km$r1=km$r2=km$r3=km$r4=km$r5=km$r6=NA
for(i in 1:length(km$id)){
  if(!is.na(match(km$id[i],roads.major$id))){
    km$roads[i]=sum(st_length(roads.major[roads.major$id==km$id[i],]))
    km$r1[i]=sum(st_length(roads.major[roads.major$id==km$id[i]&roads.major$F_SYSTEM==1,]))
    km$r2[i]=sum(st_length(roads.major[roads.major$id==km$id[i]&roads.major$F_SYSTEM==2,]))
    km$r3[i]=sum(st_length(roads.major[roads.major$id==km$id[i]&roads.major$F_SYSTEM==3,]))
    km$r4[i]=sum(st_length(roads.major[roads.major$id==km$id[i]&roads.major$F_SYSTEM==4,]))
    km$r5[i]=sum(st_length(roads.major[roads.major$id==km$id[i]&roads.major$F_SYSTEM==5,]))
    km$r6[i]=sum(st_length(roads.major[roads.major$id==km$id[i]&roads.major$F_SYSTEM==6,]))
    km$hwylanes[i]=sum(st_length(roads.major[roads.major$id==km$id[i],])*roads.major$LANES[roads.major$id==km$id[i]])
  }
  if(i%%100==0){print(i/length(km$id))}
}
plot(km["roads"],border=NA)
km$logr1=ifelse(km$r1==0|is.na(log(km$r1)),0,log(km$r1))
km$logr2=ifelse(km$r2==0|is.na(log(km$r2)),0,log(km$r2))
km$logr3=ifelse(km$r3==0|is.na(log(km$r3)),0,log(km$r3))
km$logr4=ifelse(km$r4==0|is.na(log(km$r4)),0,log(km$r4))
km$logr5=ifelse(km$r5==0|is.na(log(km$r5)),0,log(km$r5))
km$logr6=ifelse(km$r6==0|is.na(log(km$r6)),0,log(km$r6))
km$hwylanes[is.na(km$hwylanes)]=0
km$loglanes=ifelse(!is.finite(log(km$hwylanes)),0,log(km$hwylanes))
km$logroads=log(km$roads)
plot(km[colnames(km)[41:47]],border=NA)
plot(km[colnames(km)[c(48:54)]],border=NA)

#####
# Let's see what else we can get out of the ponds/lakes and rivers/streams data
#####
pondlake=st_read("Other Shapefiles/HYDRO_Lakes_and_Ponds_24K_spf.shp")
streams=st_read("Other Shapefiles/HYDRO_Rivers_and_Streams_24K_spf.shp")
pondlake=st_transform(pondlake,crs = st_crs(km))
streams=st_transform(streams,crs = st_crs(km))
km$troutL=km$troutS=km$ColdL=km$ColdS=km$NNPlantsL=km$NNPlantsS=km$EffluentL=
  km$EffluentS=km$PCBL=km$PCBS=km$HgL=km$HgS=km$CaL=km$CaS=km$PbL=km$PbS=
  km$OtherMetalL=km$OtherMetalS=km$DO2L=km$DO2S=km$benthicS=km$PhosphoL=
  km$ChloroL=km$ChlorideL=km$pondsize=NA
km$PhosphoS=NA
mat=st_intersects(km,pondlake,sparse=T)
for(i in 1:length(mat)){
  temp=mat[[i]]
  if(length(temp)>0){
  km$troutL[i]=ifelse(is.finite(match("Y",pondlake$Trout_Stk[temp])),
                      ifelse(is.finite(match("N",pondlake$Trout_Stk[temp])),"B",
                             "Y"),"N")
  km$ColdL[i]=ifelse(is.finite(match("Warm",pondlake$ColdWarm[temp])),
                  ifelse(is.finite(match("Cold",pondlake$ColdWarm[temp])),"B",
                         "W"),
                  ifelse(is.finite(match("Cold",pondlake$ColdWarm[temp])),"C",
                  ifelse(is.finite(match("Unassessed",pondlake$ColdWarm[temp])),
                         "U",NA)))
  km$NNPlantsL[i]=max(ifelse(regexpr("AQUATIC PLANTS",pondlake$Impairment[temp])>0,1,0))
  km$EffluentL[i]=max(ifelse(
    regexpr("COLIFORM",pondlake$Impairment[temp])>0|
      regexpr("ENTEROCOCCUS",pondlake$Impairment[temp])>0,1,0))
  km$PCBL[i]=max(ifelse(regexpr("PCB",    pondlake$Impairment[temp])>0,1,0))
  km$HgL[i]= max(ifelse(regexpr("MERCURY",pondlake$Impairment[temp])>0,1,0))
  km$CaL[i]= max(ifelse(regexpr("CADMIUM",pondlake$Impairment[temp])>0,1,0))
  km$PbL[i]= max(ifelse(regexpr("LEAD",   pondlake$Impairment[temp])>0,1,0))
  km$OtherMetalL[i]=max(ifelse(regexpr("ALUMINUM",pondlake$Impairment[temp])>0|
                        regexpr("COPPER",pondlake$Impairment[temp])>0,1,0))
  km$DO2L[i]=     max(ifelse(regexpr("DISSOLVED OXYGEN",pondlake$Impairment[temp])>0,1,0))
  km$PhosphoL[i]= max(ifelse(regexpr("PHOSPHO",pondlake$Impairment[temp])>0,1,0))
  km$ChloroL[i]=  max(ifelse(regexpr("CHLORO",pondlake$Impairment[temp])>0,1,0))
  km$ChlorideL[i]=max(ifelse(regexpr("CHLORIDE",pondlake$Impairment[temp])>0,1,0))
  km$pondsize[i]= max(ifelse(regexpr(">= 10",pondlake$SizeClass[temp])>0,3,
               ifelse(regexpr("acre and",pondlake$SizeClass[temp])>0,2,1)))
  }
}
streams$Impairment[is.na(streams$Impairment)]="x"
mat=st_intersects(km,streams,sparse=T)
for(i in 1:length(mat)){
  temp=mat[[i]]
  if(length(temp)>0){
    km$troutS[i]=ifelse(is.finite(match("Y",streams$Trout_Stk[temp])),
                        ifelse(is.finite(match("N",streams$Trout_Stk[temp])),"B",
                               "Y"),"N")
    km$ColdS[i]=ifelse(is.finite(match("Warm",streams$ColdWarm[temp])),
                       ifelse(is.finite(match("Cold",streams$ColdWarm[temp])),"B",
                              "W"),
                       ifelse(is.finite(match("Cold",streams$ColdWarm[temp])),"C",
                              ifelse(is.finite(match("Unassessed",streams$ColdWarm[temp])),
                                     "U",NA)))
    km$NNPlantsS[i]=max(ifelse(regexpr("AQUATIC PLANTS",streams$Impairment[temp])>0,1,0),na.rm = T)
    km$EffluentS[i]=max(ifelse(
      regexpr("COLIFORM",streams$Impairment[temp])>0|
        regexpr("ENTEROCOCCUS",streams$Impairment[temp])>0,1,0),na.rm = T)
    km$PCBS[i]=max(ifelse(regexpr("PCB",    streams$Impairment[temp])>0,1,0),na.rm = T)
    km$HgS[i]= max(ifelse(regexpr("MERCURY",streams$Impairment[temp])>0,1,0),na.rm = T)
    km$CaS[i]= max(ifelse(regexpr("CADMIUM",streams$Impairment[temp])>0,1,0),na.rm = T)
    km$PbS[i]= max(ifelse(regexpr("LEAD",   streams$Impairment[temp])>0,1,0),na.rm = T)
    km$OtherMetalS[i]=max(ifelse(regexpr("ALUMINUM",streams$Impairment[temp])>0|
                                   regexpr("COPPER",streams$Impairment[temp])>0|
                                   regexpr("IRON",streams$Impairment[temp])>0|
                                   regexpr("ZINC",streams$Impairment[temp])>0,1,0),na.rm = T)
    km$DO2S[i]=     max(ifelse(regexpr("DISSOLVED OXYGEN",streams$Impairment[temp])>0,1,0),na.rm = T)
    km$benthicS[i]=max(ifelse(regexpr("BENTHIC",streams$Impairment[temp])>0,1,0),na.rm = T)
    km$PhosphoS[i]=max(ifelse(regexpr("PHOSPHO",streams$Impairment[temp])>0,1,0),na.rm = T)
  }
}

km$troutAll=km$ColdAll=km$NNPlantsAll=km$EffluentAll=
  km$PCBAll=km$HgAll=km$CaAll=km$PbAll=
  km$OtherMetalAll=km$DO2All=km$PhosphoAll=NA
for(i in 1:length(km$id)){
  km$troutAll[i]=ifelse(km$troutL[i]=="Y"|km$troutL[i]=="B"|
                          km$troutS[i]=="Y"|km$troutS[i]=="B",1,0)
  km$ColdAll[i]=ifelse(km$ColdL[i]=="B"|km$ColdS[i]=="B","B",ifelse(
    km$ColdS[i]=="W"|km$ColdL[i]=="W","W",ifelse(
      km$ColdL[i]=="C"|km$ColdS[i]=="C","C","U")))
  km$NNPlantsAll[i]=max(km$NNPlantsL[i],km$NNPlantsS[i])
  km$EffluentAll[i]=max(km$EffluentL[i],km$EffluentS[i])
  km$PCBAll[i]=max(km$PCBL[i],km$PCBS[i])
  km$HgAll[i]=max(km$HgL[i],km$HgS[i])
  km$PbAll[i]=max(km$PbL[i],km$PbS[i])
  km$CaAll[i]=max(km$CaL[i],km$CaS[i])
  km$OtherMetalAll[i]=max(km$OtherMetalL[i],km$OtherMetalS[i])
  km$DO2All[i]=max(km$DO2L[i],km$DO2S[i])
  km$PhosphoAll[i]=max(km$PhosphoL[i],km$PhosphoS[i])
}
for(i in 1:length(km$id)){
  for(k in c(55,62,63,65,83:93)){
    km[i,k]=ifelse(is.na(km[i,k]),0,km[i,k])
}
}
#km$logtraffic=ifelse(km$traffic==0,NA,log(km$traffic))
#plot(km[c("logroads","logtraffic")],border=NA)
#sqrt(var(km$logtraffic,na.rm = T))
#####
# Surveying was initially done in conjunction with beaver
#   surveys, so we will note the beaver surveys by watershed
#   and year
#####
beaverpoint=st_read("Other Shapefiles/BeaverPoints.shp")
LCDates=ymd(c("2001-06-01","2004-06-01","2006-06-01",
              "2008-06-01","2011-06-01","2013-06-01",
              "2016-06-01","2019-06-01"))
beaverpoint$year=year(beaverpoint$Date)
#for(i in 1:length(beaverpoint$Date)){
#  beaverpoint$LCD[i]=which.min(
#    abs(as.numeric(LCDates[]-beaverpoint$Date[i])))
#}
yearbyshed=matrix(0,nrow=length(unique(km$Watrshd)),
                  ncol=length(seq(1999:2023)))
for(i in 1:nrow(yearbyshed)){
  for(j in 1:ncol(yearbyshed)){
    yearbyshed[i,j]=ifelse(length(
      beaverpoint$Date[beaverpoint$year==1998+j&
           beaverpoint$Watershed==unique(km$Watrshd)[i]])>0,1,0)
           
  }
}
yearbyshed=as.data.frame(yearbyshed)
colnames(yearbyshed)=seq(1999:2023)

yearbyshed$Watershed=unique(km$Watrshd)

beaveffort=matrix(nrow=length(km$id),ncol=length(seq(1999:2023)))
for(i in 1:length(km$id)){
  temp=as.matrix(yearbyshed[
    which(km$Watrshd[i]==yearbyshed$Watershed),1:25])
  beaveffort[i,]=temp[1,]
}
beaveffort=as.data.frame(beaveffort)
colnames(beaveffort)=paste0("b",1999:2023)
write.csv(beaveffort,"BeaverEffort.csv")
latrines=st_read("Other Shapefiles/OttersChanged.shp")
latrines=st_transform(latrines,crs=st_crs(km))
latrines=latrines[!is.na(latrines$first),]

mat=st_intersects(latrines,km,sparse=T)
mat[sapply(mat,function(x)length(x)==0L)]=NA
for(i in 1:length(mat)){
  temp=ifelse(is.na(mat[[i]]),NA,min(mat[[i]][1]))
 mat[[i]]=temp[1]
}
u=unlist(mat,recursive = F)
latrines$kmu=km$id[u]

for(i in 1:length(latrines$first)){
  if(ifelse(is.na(latrines$last[i]),0,latrines$last[i]==latrines$first[i])){
  latrines$last[i]=NA
  }}
which(is.na(latrines$kmu))
latrines=latrines[!is.na(latrines$kmu),]
last.latrines=latrines[!is.na(latrines$last),c(5,18,20,23,22)]
latrines.new=latrines[,c(5,19,23,22)]
latrines.new$LCD=0
for(i in 1:length(latrines.new$first)){
  latrines.new$LCD[i]=which.min(
    abs(as.numeric(LCDates[]-latrines.new$first[i])))
}
st_write(latrines.new,"Other Shapefiles/latrines_in_km_clean.shp",append = F)
last.latrines=st_drop_geometry(last.latrines)
last.latrines$LCD=0
for(i in 1:length(last.latrines$last)){
  last.latrines$LCD[i]=which.min(
    abs(as.numeric(LCDates[]-last.latrines$last[i])))
}

write.csv(last.latrines,file = "Latrine.Resurveys.csv")
roadkill=st_read("Other Shapefiles/roadkill_clean.shp")
roadkill=st_make_valid(roadkill)

mat=st_intersects(roadkill,km,sparse = T)
mat[sapply(mat,function(x)length(x)==0L)]=NA
for(i in 1:length(mat)){
  temp=ifelse(is.na(mat[[i]]),NA,min(mat[[i]][1]))
  mat[[i]]=temp[1]
}
u=unlist(mat,recursive = F)
roadkill$kmu=km$id[u]
roadkill=roadkill[!is.na(roadkill$kmu),]
roadkill$LCD=0
for(i in 1:length(roadkill$ID)){
  roadkill$LCD[i]=which.min(
    abs(as.numeric(LCDates[]-roadkill$date_1[i])))
}
st_write(roadkill,"Other Shapefiles/roadkill_clean.shp",append = F)
match(roadkill$qkmu,latrines$qkmu)
qkm$nlat=qkm$nrk=NA
for(i in 1:length(qkm$id)){
  if(!is.na(match(qkm$id[i],roadkill$qkmu))){
    qkm$nrk[i]=length((match(qkm$id[i],roadkill$qkmu)))
  }
  if(!is.na(match(qkm$id[i],latrines$qkmu))){
    qkm$nlat[i]=length((match(qkm$id[i],latrines$qkmu)))
  }
}
km$roadpres=ifelse(is.na(km$roads),0,ifelse(km$roads>0,1,0))
km$trafpres=ifelse(is.na(km$logtraffic),0,ifelse(km$logtraffic>0,1,0))
km$strm2=ifelse(is.na(km$StrmOr_),0,ifelse(km$StrmOr_>1,1,0))
km=km[,c(1:40,48:55,62,63,65,83:95)]
#plot(km[colnames(km)[c(55,62,63,65,83:93)]],border=NA)
plot(km[colnames(km)[40:47]],border=NA)


km$FWY=km$Arterial=km$Collector=0
for(i in 1:2301){
  km$FWY[i]=ifelse(km$logr1[i]+km$logr2[i]==0,0,
                   log(ifelse(km$logr1[i]==0,0,exp(km$logr1[i]))+
                      ifelse(km$logr2[i]==0,0,exp(km$logr2[i]))))
  km$Arterial[i]=ifelse(km$logr3[i]+km$logr4[i]==0,0,
                        log(ifelse(km$logr3[i]==0,0,exp(km$logr3[i]))+
                       ifelse(km$logr4[i]==0,0,exp(km$logr4[i]))))
  km$Collector[i]=ifelse(km$logr5[i]+km$logr6[i]==0,0,
                         log(ifelse(km$logr5[i]==0,0,exp(km$logr5[i]))+
                        ifelse(km$logr6[i]==0,0,exp(km$logr6[i]))))
}


km$OtherMetalAll=as.vector(apply(st_drop_geometry(km[,c(56,58)]),1,max))
km$benthicS[!is.finite(km$benthicS)]=0

st_write(km,"Site Shapefiles/km_IDM.shp",append=F)
landcovs=st_drop_geometry(km[,-c(1:6)])
forest=landcovs[,which(regexpr("Frs",colnames(landcovs))>0)]
