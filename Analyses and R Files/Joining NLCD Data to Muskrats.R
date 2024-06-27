library(raster)
library(rgdal)
library(ggplot2)
library(sf)
library(rasterVis)
library(landscapemetrics)
library(exactextractr)
library(dplyr)
rm(list=ls())
setwd(dir ="C:/Users/John Crockett/Documents/URI")

#####
#Read in Data and reproject as needed
#####
RIBounds=st_read("GIS Data/Watershed Boundaries/AllWatershedsUsed.shp")


NLCD_2001=raster("NRS 533/LandCoverData/NLCD_2001_Land_Cover_L48_20210604_KthBztjO2mB7n4VQtv3r.tiff")

sr <- crs(NLCD_2001)
extent(RIBounds)
extent(NLCD_2001)
#NLCD_2001=projectRaster(NLCD_2001,crs=sr)
RIBounds=st_transform(RIBounds,crs=sr)
extent(RIBounds)
extent(NLCD_2001)
#####
#Define a matrix for reclassifyng raster
#####
ClassMatrix=matrix(ncol=3,byrow=T,c(0,11,1,
                                    11,25,9,
                                    25,32,6,
                                    32,43,8,
                                    43,52,4,
                                    52,80,5,
                                    80,82,7,
                                    82,90,3,
                                    90,96,2))
NamedMatrix=matrix(ncol=3,byrow=T,c(0,11,1,11,24,9,24,32,6,32,43,8,43,52,4,52,72,5,80,82,7,82,90,3,90,96,2))
NamedMatrix=cbind(NamedMatrix,c("Open Water","Urban","Barren Land","Forest","Shrub Scrub","Grassland","Agriculture","Forested Wetland","Open Wetland"))
colnames(NamedMatrix)=c("From","To","Becomes","Name")
unique(NLCD_2001)
#####
#Crop the data to the watersheds of interest
#####
NLCD_2001 <- crop(NLCD_2001, extent(RIBounds))
NLCD_2001 <- mask(NLCD_2001, RIBounds)
#Reclassify
r_NLCD_2001=reclassify(x=NLCD_2001,rcl = ClassMatrix)
unique(r_NLCD_2001)

NLCD_2019=raster("NRS 533/LandCoverData/NLCD_2019_Land_Cover_L48_20210604_KthBztjO2mB7n4VQtv3r.tiff")
NLCD_2019 <- crop(NLCD_2019, extent(RIBounds))
NLCD_2019 <- mask(NLCD_2019, RIBounds)
r_NLCD_2019=reclassify(x=NLCD_2019,rcl = ClassMatrix)
#####
# Reading in the sites used
#####
setwd("C:/Users/John Crockett/Documents/GitHub/URI_SemAqMammals_Muskrat")

surveyed=read_sf("Site Shapefiles/AllSurveyedSitesAllYrs.shp")
Qkm=read_sf("Site Shapefiles/QkmFinal.shp")
#Clean up Qkm
Qkm=Qkm[,c(1,9,11,13,15,17,19,21,23,25,27,29,31,32,34,38,39,40)]
#This little function will help us later shoutout stackoverflow
sum_cover <- function(x){
  list(x %>%
         group_by(value) %>%
         summarize(total_area = sum(coverage_area,na.rm = T)) %>%
         mutate(proportion = total_area/sum(total_area,na.rm = T)))
  
}

#extract the area of each raster cell covered by the plot and summarize
extresults<- exact_extract(x=r_NLCD_2001,
                           y = Qkm,coverage_area=T, 
                           summarize_df = TRUE, fun = sum_cover)
#Getting the names right/keeping track
order(NamedMatrix[,c(3,4)])
name1=vector(length=9)
for(i in 1:9){
  name1[i]=NamedMatrix[which(as.numeric(NamedMatrix[,3])==i),4]
}
#Setting up some columns for past landcover
Qkm$N01_Water=
  Qkm$N01_OWetland=
  Qkm$N01_FWetland=
  Qkm$N01_Shrub=
  Qkm$N01_Grass=
  Qkm$N01_Barren=
  Qkm$N01_Ag=
  Qkm$N01_Forest=
  Qkm$N01_Urban=0
#Filling in the new columns with the prop of each site covered by
#   each raster class
for(i in 1:17101){
  tempdf=as.data.frame(extresults[[i]])
  for (v in 1:9){
    if(sum(tempdf$value==v,na.rm = T)>0){
      Qkm[i,v+18]=tempdf$proportion[tempdf$value==v&
                                          !is.na(tempdf$value)]
    }
  }
}
#Set it up again with 2019
extresults2<- exact_extract(x=r_NLCD_2019,
                           y = Qkm,coverage_area=T, 
                           summarize_df = TRUE, fun = sum_cover)
# New columns again
Qkm$N19_Water=
  Qkm$N19_OWetland=
  Qkm$N19_FWetland=
  Qkm$N19_Shrub=
  Qkm$N19_Grass=
  Qkm$N19_Barren=
  Qkm$N19_Ag=
  Qkm$N19_Forest=
  Qkm$N19_Urban=0
#Filling in the new columns
for(i in 1:17101){
  tempdf=as.data.frame(extresults2[[i]])
  for (v in 1:9){
    if(sum(tempdf$value==v,na.rm = T)>0){
      Qkm[i,v+27]=tempdf$proportion[tempdf$value==v&
                                          !is.na(tempdf$value)]
    }
  }
}
#The above for loops messed up the order of column names, 
#   this corrects it
colnames(Qkm)=c(colnames(Qkm)[1:18],
                     colnames(Qkm)[27:19],
                     colnames(Qkm[36:27]))
#####
# Attaching this data to surveyed sites so we have the right
#   site ID names
#####
mat=st_equals(surveyed,Qkm)
mat=unlist(mat)
colnames(Qkm)
surveyed$N19_Water=Qkm$N19_Water[mat]
surveyed$N19_OWetland=Qkm$N19_OWetland[mat]
surveyed$N19_FWetland=Qkm$N19_FWetland[mat]
surveyed$N19_Shrub=Qkm$N19_Shrub[mat]
surveyed$N19_Grass=Qkm$N19_Grass[mat]
surveyed$N19_Barren=Qkm$N19_Barren[mat]
surveyed$N19_Ag=Qkm$N19_Ag[mat]
surveyed$N19_Forest=Qkm$N19_Forest[mat]
surveyed$N19_Urban=Qkm$N19_Urban[mat]
surveyed$N01_Water=Qkm$N01_Water[mat]
surveyed$N01_OWetland=Qkm$N01_OWetland[mat]
surveyed$N01_FWetland=Qkm$N01_FWetland[mat]
surveyed$N01_Shrub=Qkm$N01_Shrub[mat]
surveyed$N01_Grass=Qkm$N01_Grass[mat]
surveyed$N01_Barren=Qkm$N01_Barren[mat]
surveyed$N01_Ag=Qkm$N01_Ag[mat]
surveyed$N01_Forest=Qkm$N01_Forest[mat]
surveyed$N01_Urban=Qkm$N01_Urban[mat]
surveyed$StrmOrder=Qkm$StrmOrder_[mat]
surveyed$Salt=0
for(i in 1:277){
  surveyed$Salt[i]=ifelse(sum(Qkm$`Salt Mar_1`[mat[i]],
                              Qkm$`Salt Wat_1`[mat[i]],
                              Qkm$`Tidal Cr_1`[mat[i]],
                              Qkm$`Tidal Ri_1`[mat[i]],
                              Qkm$Intertid_1[mat[i]])>0,1,0)
}

st_write(surveyed,dsn = "Site Shapefiles/Surveyed_NLCD.shp",
         append = F)
st_write(Qkm,dsn = "Site Shapefiles/Qkm_NLCD.shp")

