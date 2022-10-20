#Making EPA version of CDL crop groupings
#Last edited KR McCaffrey Jan 2021

#import the packages needed
library(sp)
library(raster)
library(rgdal)

#set the county
co<-"San Joaquin"

#load the county shapefile
setwd("E:/Prob Crop Proj Final/Spatial Data/CA_Counties/")
counties<-readOGR(".","CA_Counties_TIGER2016")
crs(counties)
county<-subset(counties, counties$NAME==co)
rm(counties)

x<-CRS("+ proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
county<-spTransform(county, x)

#set the base working directory
setwd("E:/Prob Crop Proj Final/")

#UDL categories:
#Corn, Cotton, Rice, Other Grains, Soybeans, Other Row Crops, Vegetables and Ground Fruit
#Wheat, Alfalfa/Agricultural Grasses, Pasture/Rangeland, Other Crops, Other Orchards, Vineyards, and Citrus
#14

#The CDL has already been split for the county

#using 92 unique crop categories
wd<-paste0("./", co, "/CDL_Comb/")
wd2<-paste0("./",co,"/Annual_UDLs/")

#load in CDL raster as a template
temp<-raster(paste0("./", co, "/CDL_Comb/", co, "_2013_1_stack.tif"))

rast<-raster(ext=extent(temp), crs=x,
             resolution=c(30,30))

####Corn####
#1
for(k in c(2013:2017)){
  r<-raster(paste0(wd, co, "_", k, "_1_stack.tif"))
  r<-r[[1]] #just the first layer
  r[r>=1]<-1
  r<-resample(r, rast, method="ngb")
  r<-mask(r, county)
  writeRaster(r, paste0(wd2, "Corn_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####Cotton####
#2
for(k in c(2013:2017)){
  r<-raster(paste0(wd, co, "_", k, "_2_stack.tif"))
  r<-r[[1]] #just the first layer
  r[r>=1]<-1
  r<-resample(r, rast, method="ngb")
  r<-mask(r, county)
  writeRaster(r, paste0(wd2, "Cotton_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####Rice####
#3
for(k in c(2013:2017)){
  r<-raster(paste0(wd, co, "_", k, "_3_stack.tif"))
  r<-r[[1]] #just the first layer
  r[r>=1]<-1
  r<-resample(r, rast, method="ngb")
  r<-mask(r, county)
  writeRaster(r, paste0(wd2, "Rice_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####Soybeans####
#5
for(k in c(2013:2017)){
  r<-raster(paste0(wd, co, "_", k, "_5_stack.tif"))
  r<-r[[1]] #just the first layer
  r[r>=1]<-1
  r<-resample(r, rast, method="ngb")
  r<-mask(r, county)
  writeRaster(r, paste0(wd2, "Soybeans_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

#####Wheat####
#13, 14, 15
crops<-c(13, 14, 15)
rast #example raster
for(k in c(2013:2017)){
  crop_stack<-stack()#make an empty stack
  crop_stack<-stack(rast) #put the example extent raster in the stack
  for(i in crops){
    r<-raster(paste0(wd, co, "_",k,"_",i,"_stack.tif"))
    r<-r[[1]] #just the first layer
    r[r>=1]<-1 #presence pixels = 1
    crop_stack<-stack(crop_stack, r) #add to the stack
  }
  wheat<-sum(crop_stack, na.rm=T)
  wheat[wheat>=1]<-1
  wheat<-resample(wheat, rast, method="ngb")
  wheat<-mask(wheat, county)
  writeRaster(wheat, paste0(wd2, "Wheat_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####Vineyards####
#54
for(k in c(2013:2017)){
  r<-raster(paste0(wd,co, "_", k, "_54_stack.tif"))
  r<-r[[1]] #just the first layer
  r[r>=1]<-1
  r<-resample(r, rast, method="ngb")
  r<-mask(r, county)
  writeRaster(r, paste0(wd2, "Vineyards_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####Citrus####
#56, 69
crops<-c(56,69)
rast #example raster
for(k in c(2013:2017)){
  crop_stack<-stack()#make an empty stack
  crop_stack<-stack(rast) #put the example extent raster in the stack
  for(i in crops){
    r<-raster(paste0(wd, co, "_",k,"_",i,"_stack.tif"))
    r<-r[[1]] #just the first layer
    r[r>=1]<-1 #presence pixels = 1
    crop_stack<-stack(crop_stack, r) #add to the stack
  }
  citrus<-sum(crop_stack, na.rm=T)
  citrus[citrus>=1]<-1
  citrus<-resample(citrus, rast, method="ngb")
  citrus<-mask(citrus, county)
  writeRaster(citrus, paste0(wd2, "Citrus_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####Alfalfa and Ag Grasses####
#26, 49, 81
crops<-c(26, 49, 81)
rast
for(k in c(2013:2017)){
  crop_stack<-stack()#make an empty stack
  crop_stack<-stack(rast) #put the example extent raster in the stack
  for(i in crops){
    r<-raster(paste0(wd, co, "_",k,"_",i,"_stack.tif"))
    r<-r[[1]] #just the first layer
    r[r>=1]<-1 #presence pixels = 1
    crop_stack<-stack(crop_stack, r) #add to the stack
  }
  past<-sum(crop_stack, na.rm=T)
  past[past>=1]<-1
  past<-resample(past, rast, method="ngb")
  past<-mask(past, county)
  writeRaster(past, paste0(wd2, "AlfalfaAgGrass_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####PastureRangeland####
#27
for(k in c(2013:2017)){
  r<-raster(paste0(wd, co, "_", k, "_27_stack.tif"))
  r<-r[[1]] #just the first layer
  r[r>=1]<-1
  r<-resample(r, rast, method="ngb")
  r<-mask(r, county)
  writeRaster(r, paste0(wd2, "PastureRangeland_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####OtherGrains####
#4, 12, 16, 17, 18, 19, 20, 21, 22, 23, 24, 28, 29, 34, 62
crops<-c(4, 12, 16, 17, 18, 19, 20, 21, 22, 23, 24, 28, 29, 34, 62)
rast #example raster
for(k in c(2013:2017)){
  crop_stack<-stack()#make an empty stack
  crop_stack<-stack(rast) #put the example extent raster in the stack
  for(i in crops){
    r<-raster(paste0(wd, co, "_",k,"_",i,"_stack.tif"))
    r<-r[[1]] #just the first layer
    r[r>=1]<-1 #presence pixels = 1
    crop_stack<-stack(crop_stack, r) #add to the stack
  }
  grain<-sum(crop_stack, na.rm=T)
  grain[grain>=1]<-1
  grain<-resample(grain, rast, method="ngb")
  grain<-mask(grain, county)
  writeRaster(grain, paste0(wd2, "OtherGrains_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####Other Orchards####
#51, 52, 53, 55, 57, 58, 59, 60, 61, 67, 68, 72, 74, 75, 77, 80
crops<-c(51, 52, 53, 55, 57, 58, 59, 60, 61, 67, 68, 72, 74, 75, 77, 80)
rast #example raster
for(k in c(2013:2017)){
  crop_stack<-stack()#make an empty stack
  crop_stack<-stack(rast) #put the example extent raster in the stack
  for(i in crops){
    r<-raster(paste0(wd, co, "_",k,"_",i,"_stack.tif"))
    r<-r[[1]] #just the first layer
    r[r>=1]<-1 #presence pixels = 1
    crop_stack<-stack(crop_stack, r) #add to the stack
  }
  orch<-sum(crop_stack, na.rm=T)
  orch[orch>=1]<-1
  orch<-resample(orch, rast, method="ngb")
  orch<-mask(orch, county)
  writeRaster(orch, paste0(wd2, "OtherOrchards_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####Vegetables/Ground Fruit####
#9, 10, 11, 25, 31, 32, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 46, 63, 64, 65, 66, 70, 71, 73, 76, 78, 79, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92
crops<-c(9, 10, 11, 25, 31, 32, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 46, 63, 64, 65, 66, 70, 71, 73, 76, 78, 79, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92)
rast #example raster
for(k in c(2013:2017)){
  crop_stack<-stack()#make an empty stack
  crop_stack<-stack(rast) #put the example extent raster in the stack
  for(i in crops){
    r<-raster(paste0(wd, co, "_",k,"_",i,"_stack.tif"))
    r<-r[[1]] #just the first layer
    r[r>=1]<-1 #presence pixels = 1
    crop_stack<-stack(crop_stack, r) #add to the stack
  }
  veg<-sum(crop_stack, na.rm=T)
  veg[veg>=1]<-1
  veg<-resample(veg, rast, method="ngb")
  veg<-mask(veg, county)
  writeRaster(veg, paste0(wd2, "VegetablesFruit_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####Other Row Crops####
#6, 7, 8, 30, 45
crops<-c(6, 7, 8, 30, 45)
rast #example raster
for(k in c(2013:2017)){
  crop_stack<-stack()#make an empty stack
  crop_stack<-stack(rast) #put the example extent raster in the stack
  for(i in crops){
    r<-raster(paste0(wd, co, "_",k,"_",i,"_stack.tif"))
    r<-r[[1]] #just the first layer
    r[r>=1]<-1 #presence pixels = 1
    crop_stack<-stack(crop_stack, r) #add to the stack
  }
  row<-sum(crop_stack, na.rm=T)
  row[row>=1]<-1
  row<-resample(row, rast, method="ngb")
  row<-mask(row, county)
  writeRaster(row, paste0(wd2, "OtherRowCrops_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}

####Other Crops####
#33, 47, 48, 50
crops<-c(33, 47, 48, 50)
rast #example raster
for(k in c(2013:2017)){
  crop_stack<-stack()#make an empty stack
  crop_stack<-stack(rast) #put the example extent raster in the stack
  for(i in crops){
    r<-raster(paste0(wd, co, "_",k,"_",i,"_stack.tif"))
    r<-r[[1]] #just the first layer
    r[r>=1]<-1 #presence pixels = 1
    crop_stack<-stack(crop_stack, r) #add to the stack
  }
  other<-sum(crop_stack, na.rm=T)
  other[other>=1]<-1
  other<-resample(other, rast, method="ngb")
  other<-mask(other, county)
  writeRaster(other, paste0(wd2, "OtherCrops_UDL_",k,"_",co,".tif"), format="GTiff", overwrite=T)
}


#### put all 5 years together ####
wd3<-paste0("./",co,"/FiveYr_UDLs/")

#corn
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/Corn_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
corn<-sum(crop_stack, na.rm=T)
corn[corn>=1]<-1
corn<-mask(corn, county)
writeRaster(corn, paste0(wd3, "Corn_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#cotton
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/Cotton_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
cotton<-sum(crop_stack, na.rm=T)
cotton[cotton>=1]<-1
cotton<-mask(cotton, county)
writeRaster(cotton, paste0(wd3, "Cotton_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#rice
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/Rice_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
rice<-sum(crop_stack, na.rm=T)
rice[rice>=1]<-1
rice<-mask(rice, county)
writeRaster(rice, paste0(wd3, "Rice_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#soybeans
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/Soybeans_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
soy<-sum(crop_stack, na.rm=T)
soy[soy>=1]<-1
soy<-mask(soy, county)
writeRaster(soy, paste0(wd3, "Soybeans_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#wheat
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/Wheat_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
wheat<-sum(crop_stack, na.rm=T)
wheat[wheat>=1]<-1
wheat<-mask(wheat, county)
writeRaster(wheat, paste0(wd3, "Wheat_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#vineyards
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/Vineyards_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
vine<-sum(crop_stack, na.rm=T)
vine[vine>=1]<-1
vine<-mask(vine, county)
writeRaster(vine, paste0(wd3, "Vineyards_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#citrus
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/Citrus_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
citrus<-sum(crop_stack, na.rm=T)
citrus[citrus>=1]<-1
citrus<-mask(citrus, county)
writeRaster(citrus, paste0(wd3, "Citrus_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#Alfalfa/Ag Grasses
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./", co, "/Annual_UDLs/AlfalfaAgGrass_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
alfalfa<-sum(crop_stack, na.rm=T)
alfalfa[alfalfa>=1]<-1
alfalfa<-mask(alfalfa, county)
writeRaster(alfalfa, paste0(wd3, "AlfalfaAgGrass_UDL_5yr_", co, ".tif"), format="GTiff", overwrite=T)

#Pasture/rangeland
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/PastureRangeland_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
past<-sum(crop_stack, na.rm=T)
past[past>=1]<-1
past<-mask(past, county)
writeRaster(past, paste0(wd3, "PastureRangeland_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#Other Grains
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/OtherGrains_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
grain<-sum(crop_stack, na.rm=T)
grain[grain>=1]<-1
grain<-mask(grain, county)
writeRaster(grain, paste0(wd3, "OtherGrains_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#Other Orchards
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/OtherOrchards_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
orch<-sum(crop_stack, na.rm=T)
orch[orch>=1]<-1
orch<-mask(orch, county)
writeRaster(orch, paste0(wd3, "OtherOrchards_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#Vegetables/Ground Fruit
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/VegetablesFruit_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
veg<-sum(crop_stack, na.rm=T)
veg[veg>=1]<-1
veg<-mask(veg, county)
writeRaster(veg, paste0(wd3, "VegetablesFruit_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#Other Row Crops
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/OtherRowCrops_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
row<-sum(crop_stack, na.rm=T)
row[row>=1]<-1
row<-mask(row, county)
writeRaster(row, paste0(wd3, "OtherRowCrops_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#other crops
crop_stack<-stack()
crop_stack<-stack(rast)
for(k in c(2013:2017)){
  r<-raster(paste0("./",co,"/Annual_UDLs/OtherCrops_UDL_", k, "_", co, ".tif"))
  crop_stack<-stack(crop_stack, r)
}
other<-sum(crop_stack, na.rm=T)
other[other>=1]<-1
other<-mask(other, county)
writeRaster(other, paste0(wd3, "OtherCrops_UDL_5yr_",co,".tif"), format="GTiff", overwrite=T)

#### Buffering (according to Carbaryl BE 2021) uses the cultivated layer from the most recent CDL to constrain growth ####

#create a cultivated layer
citrus<-raster(paste0(wd2, "Citrus_UDL_2017_", co, ".tif"))
corn<-raster(paste0(wd2, "Corn_UDL_2017_", co, ".tif"))
cotton<-raster(paste0(wd2, "Cotton_UDL_2017_", co, ".tif"))
otherc<-raster(paste0(wd2, "OtherCrops_UDL_2017_", co, ".tif"))
grain<-raster(paste0(wd2, "OtherGrains_UDL_2017_", co, ".tif"))
orch<-raster(paste0(wd2, "OtherOrchards_UDL_2017_", co, ".tif"))
other<-raster(paste0(wd2, "OtherRowCrops_UDL_2017_", co, ".tif"))
alf<-raster(paste0(wd2, "AlfalfaAgGrass_UDL_2017_", co, ".tif"))
past<-raster(paste0(wd2, "PastureRangeland_UDL_2017_", co, ".tif"))
rice<-raster(paste0(wd2, "Rice_UDL_2017_", co, ".tif"))
soy<-raster(paste0(wd2, "Soybeans_UDL_2017_", co, ".tif"))
veg<-raster(paste0(wd2, "VegetablesFruit_UDL_2017_", co, ".tif"))
vine<-raster(paste0(wd2, "Vineyards_UDL_2017_", co, ".tif"))
wheat<-raster(paste0(wd2, "Wheat_UDL_2017_", co, ".tif"))

cultivated<-mosaic(citrus, corn, cotton, otherc, grain, orch, other, alf, past, rice, soy, veg, vine, wheat, fun=max, na.rm=T)
writeRaster(cultivated, paste0("./", co, "/Cultivated_Land_2017_", co, ".tif"), format="GTiff", overwrite=T)

#### Make annual cultivated layers ####

#load in CDL raster as a template
temp<-raster(paste0("./", co, "/CDL_Comb/", co, "_2013_1_stack.tif"))
rast<-raster(ext=extent(temp), crs=x,
             resolution=c(30,30))
#set up wds
wd3<-paste0("./",co,"/Annual_UDLs/")
wd4<-paste0("./",co,"/Cultivated_Annual/")

#
for(i in c(2013:2017)){
  citrus<-raster(paste0(wd3, "Citrus_UDL_", i, "_", co, ".tif"))
  corn<-raster(paste0(wd2, "Corn_UDL_", i, "_", co, ".tif"))
  cotton<-raster(paste0(wd2, "Cotton_UDL_", i, "_", co, ".tif"))
  otherc<-raster(paste0(wd2, "OtherCrops_UDL_", i, "_", co, ".tif"))
  grain<-raster(paste0(wd2, "OtherGrains_UDL_", i, "_",co, ".tif"))
  orch<-raster(paste0(wd2, "OtherOrchards_UDL_", i, "_", co, ".tif"))
  other<-raster(paste0(wd2, "OtherRowCrops_UDL_", i,"_", co, ".tif"))
  alf<-raster(paste0(wd2, "AlfalfaAgGrass_UDL_", i, "_", co, ".tif"))
  past<-raster(paste0(wd2, "PastureRangeland_UDL_", i, "_", co, ".tif"))
  rice<-raster(paste0(wd2, "Rice_UDL_", i, "_", co, ".tif"))
  soy<-raster(paste0(wd2, "Soybeans_UDL_", i, "_", co, ".tif"))
  veg<-raster(paste0(wd2, "VegetablesFruit_UDL_", i, "_", co, ".tif"))
  vine<-raster(paste0(wd2, "Vineyards_UDL_", i, "_", co, ".tif"))
  wheat<-raster(paste0(wd2, "Wheat_UDL_", i, "_", co, ".tif"))
  cultivated<-mosaic(citrus, corn, cotton, otherc, grain, orch, other, alf, 
                     past, rice, soy, veg, vine, wheat, fun=max, na.rm=T)
  writeRaster(cultivated, paste0(wd4, "Cultivated_Land_",i,".tif"), format="GTiff", overwrite=T)
  
}

#### compare to NASS acreage ####
#any individual crop records with (D) are changed to -1 for incorporation into UDL sums
#if any crop category records result in negative values, they are changed to 0
#buffer by one pixel layer at a time, within the cultivated crop layer (2017), to meet or exceed CoA acreage

wd3<-paste0("./",co,"/FiveYr_UDLs/")
cultivated<-raster(paste0("./", co, "/Cultivated_Land_2017_",co,".tif"))
Census<-read.csv(paste0("./", co, "/", co, "_Census_2012_UDL_Acreage.csv"))
wd4<-paste0("./",co, "/Final_UDLs/")

####Corn####
Corn<-raster(paste0(wd3, "Corn_UDL_5yr_", co, ".tif"))
Census # 97,570 
CornArea<-Corn*900
area<-cellStats(CornArea, sum, na.rm=T)
area * 0.000247105 #145,436.2
#good - meets or exceeds Ag Census value
writeRaster(Corn, paste0(wd4, "Corn_UDL_", co, "_Final.tif"), format="GTiff", overwrite=T)

####Cotton####
Cotton<-raster(paste0(wd3, "Cotton_UDL_5yr_", co, ".tif"))
Census #0
Cotton[Cotton>0]<-0
writeRaster(Cotton, paste0(wd4, "Cotton_UDL_", co, "_Final.tif"), format="GTiff", overwrite=T)

####Rice####
Rice<-raster(paste0(wd3, "Rice_UDL_5yr_", co, ".tif"))
Census #4,211
RiceArea<-Rice*900
area<-cellStats(RiceArea, sum, na.rm=T)
area * 0.000247105 #10,069.36
#good - meets or exceeds Ag Census value
writeRaster(Rice, paste0(wd4, "Rice_UDL_", co, "_Final.tif"), format="GTiff", overwrite=T)

####Soybeans####
Soy<-raster(paste0(wd3, "Soybeans_UDL_5yr_", co, ".tif"))
Census #0 Census acreage, set all raster values to 0
Soy[Soy>0]<-0
writeRaster(Soy, paste0(wd4, "Soybeans_UDL_",co, "_Final.tif"), format="GTiff", overwrite=T)

####Wheat####
Wheat<-raster(paste0(wd3, "Wheat_UDL_5yr_", co, ".tif"))
Census #27,752
WheatArea<-Wheat*900
area<-cellStats(WheatArea, sum, na.rm=T) 
area * 0.000247105 #109249.7
#good - meets or exceeds Ag Census value
writeRaster(Wheat, paste0(wd4, "Wheat_UDL_", co, "_Final.tif"), format="GTiff", overwrite=T)

####Vineyards####
Vine<-raster(paste0(wd3, "Vineyards_UDL_5yr_", co, ".tif"))
Census #106,753
VineArea<-Vine*900
area<-cellStats(VineArea, sum, na.rm=T)
area * 0.000247105 #162273.7
#good - meets or exceeds Ag Census value
writeRaster(Vine, paste0(wd4, "Vineyards_UDL_", co, "_Final.tif"), format="GTiff", overwrite=T)

####Citrus####
Citrus<-raster(paste0(wd3, "Citrus_UDL_5yr_", co, ".tif"))
Census #913
CitrusArea<-Citrus*900
area<-cellStats(CitrusArea, sum, na.rm=T)
area * 0.000247105 #2.668734
#buffer
Citrus2<-Citrus
Citrus2[Citrus2==0]<-NA
rbuff<-buffer(Citrus2, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #9.340569
#buffer again
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #23.57382
#buffer again
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #43.14453
#buffer again
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #64.7168
#buffer again
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #87.40104
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #108.9733
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 # 131.2128
#buffer again
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #155.4538
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #181.9187
#buffer again
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #210.1628
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #240.1861
#buffer again
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #272.6557
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #306.4596
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #342.9323
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #381.4066
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #420.7704
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #461.2462
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #502.6116
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #546.2009
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #592.4589
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #640.2738
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #689.6453
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #741.0185
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #793.726
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #847.9902
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 # 902.9217
#buffer again 
rbuff<-buffer(rbuff, width=30, doEdge=T) #buffer by 30 m - one pixel layer
rbuff<-mask(rbuff, cultivated, inverse=T, maskvalue=1, updateValue=NA)
marea<-rbuff*900
area<-cellStats(marea, sum, na.rm=T)
area*0.000247105 #958.7427
writeRaster(rbuff, paste0(wd4, "Citrus_UDL_", co, "_Final.tif"), format="GTiff", overwrite=T)

#### Alfalfa/Agricultural Grasses ####
AlfGrass<-raster(paste0(wd3, "AlfalfaAgGrass_UDL_5yr_", co, ".tif"))
Census #<0, no adjustment
writeRaster(AlfGrass, paste0(wd4, "AlfalfaAgGrass_UDL_", co, "_Final.tif"), format="GTiff", overwrite=T)

#### Pasture/Rangeland ####
Past<-raster(paste0(wd3, "PastureRangeland_UDL_5yr_", co, ".tif"))
Census #92,089 
PastArea<-Past*900
area<-cellStats(PastArea, sum, na.rm=T)
area * 0.000247105 # 234074.2
#good - meets or exceeds Ag Census value
writeRaster(Past, paste0(wd4, 'PastureRangeland_UDL_', co, "_Final.tif"), format="GTiff", overwrite=T)

####Other Grains####
Grain<-raster(paste0(wd3, "OtherGrains_UDL_5yr_", co, ".tif"))
Census #12,978
GrainArea<-Grain*900
area<-cellStats(GrainArea, sum, na.rm=T)
area * 0.000247105 #115309.1
#good - meets or exceeds Ag Census value
writeRaster(Grain, paste0(wd4, "OtherGrains_UDL_", co, "_Final.tif"), format="GTiff", overwrite=T)

####Other Orchards####
Orch<-raster(paste0(wd3, "OtherOrchards_UDL_5yr_", co, ".tif"))
Census #130,296
OrchArea<-Orch*900
area<-cellStats(OrchArea, sum, na.rm=T) #943443000
area * 0.000247105 #244173.6
#good - meets or exceeds Ag Census value
writeRaster(Orch, paste0(wd4, "OtherOrchards_UDL_", co, "_Final.tif"), format="GTiff", overwrite=T)

####Vegetables/Ground Fruit ####
Veg<-raster(paste0(wd3, "VegetablesFruit_UDL_5yr_", co, ".tif"))
Census # 64,092
VegArea<-Veg*900
area<-cellStats(VegArea, sum, na.rm=T)#62379900
area * 0.000247105 #123363.3
#good - meets or exceeds Ag Census value
writeRaster(Veg, paste0(wd4, "VegetablesFruit_UDL_", co, "_Final.tif"), format="GTiff", overwrite=T)

#### Other Row Crops ####
Row<-raster(paste0(wd3, "OtherRowCrops_UDL_5yr_", co, ".tif"))
Census #<0, no adjustment
writeRaster(Row, paste0(wd4,"OtherRowCrops_UDL_", co, "_Final.tif"), format="GTiff", overwrite=T)

#### Other Crops ####
Other<-raster(paste0(wd3, "OtherCrops_UDL_5yr_", co, ".tif"))
Census #19,042
OtherArea<-Other*900
area<-cellStats(OtherArea, sum, na.rm=T) #205049700
area * 0.000247105 #112589
#good - meets or exceeds Ag Census value
writeRaster(Other, paste0(wd4,"OtherCrops_UDL_",co,"_Final.tif"), format="GTiff", overwrite=T)

#### Acreage of UDL layers ####
# For Table 2
wd4<-paste0("./",co, "/Final_UDLs/")
citrus<-raster(paste0(wd4, "Citrus_UDL_", co, "_Final.tif"))
corn<-raster(paste0(wd4, "Corn_UDL_", co, "_Final.tif"))
cotton<-raster(paste0(wd4, "Cotton_UDL_", co, "_Final.tif"))
otherc<-raster(paste0(wd4, "OtherCrops_UDL_", co, "_Final.tif"))
grain<-raster(paste0(wd4, "OtherGrains_UDL_", co, "_Final.tif"))
orch<-raster(paste0(wd4, "OtherOrchards_UDL_", co, "_Final.tif"))
other<-raster(paste0(wd4, "OtherRowCrops_UDL_", co, "_Final.tif"))
alf<-raster(paste0(wd4, "AlfalfaAgGrass_UDL_", co, "_Final.tif"))
past<-raster(paste0(wd4, "PastureRangeland_UDL_", co, "_Final.tif"))
rice<-raster(paste0(wd4, "Rice_UDL_", co, "_Final.tif"))
soy<-raster(paste0(wd4, "Soybeans_UDL_", co, "_Final.tif"))
veg<-raster(paste0(wd4, "VegetablesFruit_UDL_", co, "_Final.tif"))
vine<-raster(paste0(wd4, "Vineyards_UDL_", co, "_Final.tif"))
wheat<-raster(paste0(wd4, "Wheat_UDL_", co, "_Final.tif"))

#citrus
citrusarea<-citrus*900
area<-cellStats(citrusarea, sum, na.rm=T)
area * 0.000247105 # 958.7427

#corn
cornarea<-corn*900
area<-cellStats(cornarea, sum, na.rm=T)
area * 0.000247105 # 145436.2

#cotton
cottonarea<-cotton*900
area<-cellStats(cottonarea, sum, na.rm=T)
area * 0.000247105 # 0 

#otherc
othercarea<-otherc*900
area<-cellStats(othercarea, sum, na.rm=T)
area * 0.000247105 # 112589

#grain
grainarea<-grain*900
area<-cellStats(grainarea, sum, na.rm=T)
area * 0.000247105 # 115309.1

#orch
orcharea<-orch*900
area<-cellStats(orcharea, sum, na.rm=T)
area * 0.000247105 # 244173.6

#other row crops
otherarea<-other*900
area<-cellStats(otherarea, sum, na.rm=T)
area * 0.000247105 # 1496.048

#alfalfa/ag grasses
alfarea<-alf*900
area<-cellStats(alfarea, sum, na.rm=T)
area * 0.000247105 # 149063.7

#pasture/rangeland
pastarea<-past*900
area<-cellStats(pastarea, sum, na.rm=T)
area * 0.000247105 # 234074.2

#rice
ricearea<-rice*900
area<-cellStats(ricearea, sum, na.rm=T)
area * 0.000247105 # 10069.36

#soybeans
soyarea<-soy*900
area<-cellStats(soyarea, sum, na.rm=T)
area * 0.000247105 # 0

#veg/ground fruit
vegarea<-veg*900
area<-cellStats(vegarea, sum, na.rm=T)
area * 0.000247105 # 123363.3

#vineyards
vinearea<-vine*900
area<-cellStats(vinearea, sum, na.rm=T)
area * 0.000247105 # 162273.7

#wheat
wheatarea<-wheat*900
area<-cellStats(wheatarea, sum, na.rm=T)
area * 0.000247105 # 109249.7
