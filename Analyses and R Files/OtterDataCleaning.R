library(this.path)
setwd(here())
setwd('..')

library(sf)
library(lubridate)
rm(list=ls())
km=st_read("Site Shapefiles/kmFinal.shp")
Pawc=st_read  ("Site Shapefiles/PawcatuckQkmAll.shp")
BIS=st_read   ("Site Shapefiles/BISQkm.shp")
Quin=st_read  ("Site Shapefiles/QuinebaugQkm.shp")
Narr=st_read  ("Site Shapefiles/SurveyedSites2022.shp")
Woon=st_read  ("Site Shapefiles/WoonMossSelectedQkm.shp")
Black1=st_read("Site Shapefiles/BlackstoneSelectedQkm.shp")
Black2=st_read("Site Shapefiles/BlackstoneNewSelection2.shp")
colnames(Black2)[c(7,9,11,13,15,17,19,21,23,25,29,31,32,34,36,38:40)]=colnames(Black1)
Black2=Black2[,match(colnames(Black1),colnames(Black2))]
Black=rbind(Black1,Black2)
fielddata=read.csv("Analyses and R Files/FieldDataWithLC_Weather.csv")
fielddata$Watershed[fielddata$Watershed=="BIS"]="Block Island Sound"
BIS=BIS[,c("Watershed","NewID","geometry")]
Black=Black[,c("Watershed","NewID","geometry")]
Pawc=  Pawc[,c("Watershed","NewID","geometry")]
Quin=  Quin[,c("Watershed","NewID","geometry")]
Narr=  Narr[,c("Wtrshd_y","NewID","geometry")]
Woon=  Woon[,c("Watershed","NewID","geometry")]
colnames(Narr)=c("Watershed","NewID","geometry")
allsheds=rbind(BIS,Black,Narr,Pawc,Quin,Woon)
plot(allsheds["Watershed"])
colnames(allsheds)=c("Watershed","Site.ID","geometry")
allsheds$Index=1:length(allsheds$Watershed)
vecmatch=vector(length = length(fielddata$X.1))
for(i in 1:length(fielddata$X.1)){
  vecmatch[i]=ifelse(!is.null(allsheds$Index[allsheds$Watershed==
                                            fielddata$Watershed[i]&
                                            allsheds$Site.ID==
                                            fielddata$Site.ID[i]]),
                     allsheds$Index[allsheds$Watershed==
                          fielddata$Watershed[i]&
                          allsheds$Site.ID==
                          fielddata$Site.ID[i]],
                     NA)
}
usedsheds=allsheds[unique(vecmatch),]
st_write(usedsheds,dsn="Site Shapefiles/AllSurveyedSitesAllYrs.shp",append = F)
surveyed=st_read("Site Shapefiles/SurveyedSitesJoinedtoKm.shp")
plot(surveyed["Watershed"],border=NA)
fielddata$kmID=NA
for(i in 1: length(fielddata$Watershed)){
  temp=surveyed$id[surveyed$Site_ID==fielddata$Site.ID[i]&
                                  surveyed$Watershed==fielddata$Watershed[i]]
  fielddata$kmID[i]=ifelse(sum(!is.na(temp))==0,NA,temp[!is.na(temp)])
  }
fielddata$kmID
fielddata[is.na(fielddata$kmID),3:4]
latrines=read_sf("Other Shapefiles/otter latrines in ri.shp")
latrines=latrines[-438,]
latrines$Month=latrines$Year=latrines$Day=latrines$first=latrines$last=NA
for(i in 1:length(latrines$DATE_FIRST)){
  latrines$Month[i]= ifelse(
    nchar(strsplit(latrines$DATE_FIRST[i],"/")[[1]][1])==2,
    strsplit(latrines$DATE_FIRST[i],"/")[[1]][1],
    paste0(0,strsplit(latrines$DATE_FIRST[i],"/")[[1]][1]))
  latrines$Day[i]= ifelse(
    nchar(strsplit(latrines$DATE_FIRST[i],"/")[[1]][2])==2,
    strsplit(latrines$DATE_FIRST[i],"/")[[1]][2],
    paste0(0,strsplit(latrines$DATE_FIRST[i],"/")[[1]][2]))
  latrines$Year[i]= ifelse(
    strsplit(latrines$DATE_FIRST[i],"/")[[1]][3]<21,
    paste0(20,strsplit(latrines$DATE_FIRST[i],"/")[[1]][3]),
    paste0(19,strsplit(latrines$DATE_FIRST[i],"/")[[1]][3]))
}
latrines$Month[252]=05;latrines$Year[252]=2002
latrines$first=ymd(paste(latrines$Year,latrines$Month,latrines$Day,
               sep="-"))
latrines$last=mdy(latrines$LAST_SURVE)
temp=which(nchar(latrines$LAST_SURVE)>8&!is.na(latrines$LAST_SURVE))
latrines$last[temp]=mdy(
  strsplit(latrines$LAST_SURVE[temp],", ")[[1]][2])

latrines$present_last=NA
unique(latrines$RESULTS_LA[!is.na(latrines$RESULTS_LA)])

unique(latrines$RESULTS_LA[regexpr("fresh",latrines$RESULTS_LA)>0&
                             !is.na(latrines$RESULTS_LA)&
                             regexpr("no",latrines$RESULTS_LA)>0])
unique(latrines$RESULTS_LA[!is.na(latrines$RESULTS_LA)&(
                             regexpr("no fresh",latrines$RESULTS_LA)>0|
                             regexpr("no recent",latrines$RESULTS_LA)>0|
                             regexpr("no activity",latrines$RESULTS_LA)>0|
                             regexpr("no new",latrines$RESULTS_LA)>0|
                             regexpr("no rec.",latrines$RESULTS_LA)>0)])



for(i in 1: length(latrines$ID)){
  if(!is.na(latrines$RESULTS_LA[i])&(
    regexpr("no fresh",latrines$RESULTS_LA[i])   <0|
    regexpr("no recent",latrines$RESULTS_LA[i])  <0|
    regexpr("no activity",latrines$RESULTS_LA[i])<0|
    regexpr("no new",latrines$RESULTS_LA[i])     <0|
    regexpr("no rec.",latrines$RESULTS_LA[i])    <0|
    regexpr("did not",latrines$RESULTS_LA[i])    <0)){
    latrines$present_last[i]=1
  }
  if(!is.na(latrines$RESULTS_LA[i])&(
    regexpr("no fresh",latrines$RESULTS_LA[i])>0|
    regexpr("no recent",latrines$RESULTS_LA[i])>0|
    regexpr("no activity",latrines$RESULTS_LA[i])>0|
    regexpr("no new",latrines$RESULTS_LA[i])>0|
    regexpr("no rec.",latrines$RESULTS_LA[i])>0)){
    latrines$present_last[i]=0
  }
} 
st_write(latrines,dsn="Other Shapefiles/OttersChanged.shp",append = F)
latrines=st_read("Other Shapefiles/OttersChanged.shp")
hist(c(latrines$first,latrines$last),breaks="months",freq=T)
latrines=st_transform(latrines,crs=st_crs(km))

km=st_transform(km,st_crs(latrines))
mat=st_intersects(latrines,km,sparse = T)
mat[sapply(mat,function(x)length(x)==0L)]=NA
u=unlist(mat,recursive = F)
latrines$u=u
km$npoints=km$ndet=0
for(i in 1:length(km$id)){
  if(!is.null(which(u==i))){
    km$npoints[i]=length(which(u==i))
    km$ndet[i]=length(which(u==i))+
      sum(ifelse(latrines$last[which(u==i)]-
            latrines$first[which(u==i)]>0,1,0),na.rm=T)
  }
}
mean(km$ndet)
plot(km[c("ndet","npoints")])
range(km$ndet)
usedgrids=unique(latrines$u)
grid.look=NULL
latrine.look=NULL
for (i in 1:length(usedgrids)){
  if(max(latrines$last[latrines$present_last==0&
                       latrines$u==i],na.rm = T)>
     max(latrines$first[latrines$u==i],
         latrines$last[latrines$present_last==1&
                       latrines$u==i],na.rm = T))
  {grid.look=c(grid.look,latrines$u[i])
    latrine.look=c(latrine.look,i)}
}
which(latrines$u==latrine.look[1])
latrines[c(which(latrines$u==latrine.look[1]),
           which(latrines$u==latrine.look[2]),
           which(latrines$u==latrine.look[3]),
           which(latrines$u==latrine.look[4]),
           which(latrines$u==latrine.look[5]),
           which(latrines$u==latrine.look[6])),c(2,16,15,20,21)]
plot(latrines["WATERSHED"],pch=16)
hist(latrines$first,breaks="years",freq=T,xlab = 1999:2020)
watersheds=read_sf("Other Shapefiles/AllWatershedsUsed.shp")
watersheds=st_transform(watersheds,st_crs(latrines))
mat2=st_intersects(latrines,watersheds,sparse = T)
mat2[sapply(mat2,function(x)length(x)==0L)]=NA
mat2=unlist(mat2,recursive = F)
latrines$watershed=NA
for(i in 1:length(latrines$ID)){
  latrines$watershed[i]=watersheds$Watershed[mat2[i]]
}
latrines$LCBINF=latrines$LCBINL=NA
lcbinvec=c(2001,2004,2006,2008,
           2011,2013,2016,2019)
lcbinvec=ym(paste(lcbinvec,"01",sep="-"))
lcbinvec2=c(2001,2004,2006,2008,
            2011,2013,2016,2019)
hasdate=which(!is.na(latrines$first))
for(i in 1:length(hasdate)){
  latrines$LCBINF[hasdate[i]]=lcbinvec2[which.min(
    abs(latrines$first[hasdate[i]]-lcbinvec))]
  latrines$LCBINL[hasdate[i]]=ifelse(is.na(
    latrines$last[hasdate[i]]),NA,
    lcbinvec2[which.min(abs(latrines$last[hasdate[i]]-lcbinvec))])
}
shedbyyear=matrix(nrow=length(unique(watersheds$Watershed)),
                  ncol=length(c(2001,2004,2006,2008,
                                2011,2013,2016,2019)))
for(i in 1:length(c(2001,2004,2006,2008,2011,2013,2016,2019))){
  for(j in 1:7){
    shedbyyear[j,i]=length(which(!is.na(latrines$first[
      latrines$watershed==unique(watersheds$Watershed)[j]&
        latrines$LCBINF==lcbinvec2[i]])))+
      length(which(!is.na(latrines$last[
          latrines$watershed==unique(watersheds$Watershed)[j]&
            latrines$LCBINL==lcbinvec2[i]&
            latrines$LCBINF!=latrines$LCBINL])))
  }
}
shedbyyear=data.frame(shedbyyear,
                      row.names = unique(watersheds$Watershed))
colnames(shedbyyear)=c(2001,2004,2006,2008,2011,2013,2016,2019)
plot(latrines[c("first","last")],pch=16)
#####
#Roadkill time
#####
roadkill=read_sf("Other Shapefiles/river otter roadkills in ri.shp")
roadkill=st_make_valid(roadkill)

for(i in 1:length(roadkill$DATE)){
  roadkill$Month[i]= ifelse(
    nchar(strsplit(roadkill$DATE[i],"/")[[1]][1])==2,
    strsplit(roadkill$DATE[i],"/")[[1]][1],
    paste0(0,strsplit(roadkill$DATE[i],"/")[[1]][1]))
  roadkill$Day[i]= ifelse(
    nchar(strsplit(roadkill$DATE[i],"/")[[1]][2])==2,
    strsplit(roadkill$DATE[i],"/")[[1]][2],
    paste0(0,strsplit(roadkill$DATE[i],"/")[[1]][2]))
  roadkill$Year[i]= strsplit(roadkill$DATE[i],"/")[[1]][3]
}
roadkill$date=ymd(paste(roadkill$Year,
                        roadkill$Month,
                        roadkill$Day,
                        sep = "-"))
plot(roadkill["date"],pch=16)
roadkill=st_transform(roadkill,st_crs(km))
mat=st_intersects(roadkill,km,sparse = T)
mat[sapply(mat,function(x)length(x)==0L)]=NA
u=unlist(mat,recursive = F)
roadkill$u=u
km$npoints=0
for(i in 1:length(km$id)){
  if(!is.null(which(u==i))){
    km$npoints[i]=length(which(u==i))
  }
}
plot(km["npoints"])
duplicated(roadkill$u)
hist(roadkill$date,
     breaks="year",freq=T)
plot(km["npoints"],reset=F,pal=c("white","yellow","orange"))
plot(roadkill["date"],pch=16,add=T)
roadkill=roadkill[!is.na(roadkill$u),]
st_write(roadkill,"Other Shapefiles/roadkill_clean.shp")
vec=vector()
for(i in 1:length(unique(roadkill$u))){
  temp=roadkill[roadkill$u==unique(roadkill$u)[i],]
  if(length(temp$ID)>1){
    if(length(unique(year(temp$date)))<length(year(temp$date))){
      vec=c(vec,i)
    }
  }
}
roadkill[which(roadkill$u==unique(roadkill$u)[vec[2]]),]
