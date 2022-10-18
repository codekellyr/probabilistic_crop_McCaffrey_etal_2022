#Getting crop layers needed for field method
#edited Dec 15 2021 KM

#import the packages needed
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(progress)

#get the counties in the study area

setwd("E:/Prob Crop Proj Final/Spatial Data/CA_Counties/")
counties<-readOGR(".","CA_Counties_TIGER2016")
county<-subset(counties, counties$NAME=="Madera" | 
                 counties$NAME=="Merced"|counties$NAME=="Sacramento"|
                 counties$NAME=="San Joaquin"|counties$NAME=="Stanislaus")

x<-CRS("+ proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# x2<-CRS("+init=epsg:4326")

county<-spTransform(county, x)

#set the base working directory
setwd("E:/Prob Crop Proj Final/")

#crop by crop. Combine all counties. Look at prob values/dist

#make a template raster
temp<-raster("./Bifenthrin_Probability_StudyArea_w_2012.tif")
rast<-raster(ext=extent(temp), crs=x, resolution=c(30, 30))

#Corn (1)
#Merced, Sacramento, San Joaquin, Stanislaus
counties<-c("Merced", "Sacramento", "San Joaquin", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_1prob_w.tif"))
  bigr<-resample(r, rast, method="ngb") #expand to all county area
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
corn<-do.call(mosaic, rlist)
#fill in any empty counties
corn[is.na(corn[])] <- 0 
corn<-mask(corn, county)
#save the result
writeRaster(corn, "./Final Crops/Corn_StudyArea.tif", format="GTiff", overwrite=T)

#Cotton (2)
#Madera, Merced
counties<-c("Madera","Merced")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_2prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x", "y")
rlist$fun<-max
rlist$na.rm<-TRUE
cotton<-do.call(mosaic, rlist)
#fill in any empty counties
cotton[is.na(cotton[])] <- 0 
cotton<-mask(cotton, county)
#save the result
writeRaster(cotton, "./Final Crops/Cotton_StudyArea.tif", format="GTiff", overwrite=T)

#Soybeans (5)
#Sacramento
counties<-c("Sacramento")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_5prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# soy<-do.call(mosaic, rlist)
#only one county, no mosaic
soy<-rlist[[1]]
#fill in any empty counties
soy[is.na(soy[])] <- 0 
soy<-mask(soy, county)
#save the result
writeRaster(soy, "./Final Crops/Soybeans_StudyArea.tif", format="GTiff", overwrite=T)

#Oats (18)
#Stanislaus
counties<-c("Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_18prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# oats<-do.call(mosaic, rlist)
#only one county, no mosaic
oats<-rlist[[1]]
#fill in any empty counties
oats[is.na(oats[])] <- 0 
oats<-mask(oats, county)
#save the result
writeRaster(oats, "./Final Crops/Oats_StudyArea.tif", format="GTiff", overwrite=T)

#Alfalfa (26)
#Sacramento
counties<-c("Sacramento")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_26prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# alfalfa<-do.call(mosaic, rlist)
#only one county, no mosaic
alfalfa<-rlist[[1]]
#fill in any empty counties
alfalfa[is.na(alfalfa[])] <- 0 
alfalfa<-mask(alfalfa, county)
#save the result
writeRaster(alfalfa, "./Final Crops/Alfalfa_StudyArea.tif", format="GTiff", overwrite=T)

#Dry Beans (31)
#Merced, Sacramento, San Joaquin, Stanislaus
counties<-c("Merced", "Sacramento", "San Joaquin", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_31prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
beans<-do.call(mosaic, rlist)
#fill in any empty counties
beans[is.na(beans[])] <- 0 
beans<-mask(beans, county)
#save the result
writeRaster(beans, "./Final Crops/DryBeans_StudyArea.tif", format="GTiff", overwrite=T)

#Potatoes (32)
#San Joaquin
counties<-c("San Joaquin")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_32prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# potato<-do.call(mosaic, rlist)
#only one county, no mosaic
potato<-rlist[[1]]
#fill in any empty counties
potato[is.na(potato[])] <- 0 
potato<-mask(potato, county)
#save the result
writeRaster(potato, "./Final Crops/Potatoes_StudyArea.tif", format="GTiff", overwrite=T)

#Sweet potatoes (35)
#Merced, Stanislaus
counties<-c("Merced", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_35prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
sweetpotato<-do.call(mosaic, rlist)
#fill in any empty counties
sweetpotato[is.na(sweetpotato[])] <- 0 
sweetpotato<-mask(sweetpotato, county)
#save the result
writeRaster(sweetpotato, "./Final Crops/Sweet potatoes_StudyArea.tif", format="GTiff", overwrite=T)

#Misc Vegs & Fruits (36)
#San Joaquin, Stanislaus
counties<-c("San Joaquin", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_36prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
misc<-do.call(mosaic, rlist)
#fill in any empty counties
misc[is.na(misc[])] <- 0 
misc<-mask(misc, county)
#save the result
writeRaster(misc, "./Final Crops/Misc Vegs Fruits_StudyArea.tif", format="GTiff", overwrite=T)

#Watermelons (37)
#Sacramento, San Joaquin, Stanislaus
counties<-c("Sacramento", "San Joaquin", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_37prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
watermelon<-do.call(mosaic, rlist)
#fill in any empty counties
watermelon[is.na(watermelon[])] <- 0 
watermelon<-mask(watermelon, county)
#save the result
writeRaster(watermelon, "./Final Crops/Watermelons_StudyArea.tif", format="GTiff", overwrite=T)

#Cucumbers (39)
#Merced, San Joaquin
counties<-c("Merced", "San Joaquin")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_39prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
cucumber<-do.call(mosaic, rlist)
#fill in any empty counties
cucumber[is.na(cucumber[])] <- 0 
cucumber<-mask(cucumber, county)
#save the result
writeRaster(cucumber, "./Final Crops/Cucumbers_StudyArea.tif", format="GTiff", overwrite=T)

#Tomatoes (43)
#Madera, Merced, Sacramento, San Joaquin, Stanislaus
counties<-c("Madera", "Merced", "Sacramento", "San Joaquin", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_43prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
tomato<-do.call(mosaic, rlist)
#fill in any empty counties
tomato[is.na(tomato[])] <- 0 
tomato<-mask(tomato, county)
#save the result
writeRaster(tomato, "./Final Crops/Tomatoes_StudyArea.tif", format="GTiff", overwrite=T)

#Fallow/Idle Cropland (50)
#Sacramento
counties<-c("Sacramento")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_50prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# fallow<-do.call(mosaic, rlist)
#only one county, no mosaic
fallow<-rlist[[1]]
#fill in any empty counties
fallow[is.na(fallow[])] <- 0 
fallow<-mask(fallow, county)
#save the result
writeRaster(fallow, "./Final Crops/Fallow Idle_StudyArea.tif", format="GTiff", overwrite=T)

#Grapes (54)
#Madera, Sacramento, San Joaquin, Stanislaus 
counties<-c("Madera", "Sacramento", "San Joaquin", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_54prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
grapes<-do.call(mosaic, rlist)
#fill in any empty counties
grapes[is.na(grapes[])] <- 0 
grapes<-mask(grapes, county)
#save the result
writeRaster(grapes, "./Final Crops/Grapes_StudyArea.tif", format="GTiff", overwrite=T)

#Pecans (57)
#Sanislaus
counties<-c("Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_57prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# pecans<-do.call(mosaic, rlist)
#only one county, no mosaic
pecans<-rlist[[1]]
#fill in any empty counties
pecans[is.na(pecans[])] <- 0 
pecans<-mask(pecans, county)
#save the result
writeRaster(pecans, "./Final Crops/Pecans_StudyArea.tif", format="GTiff", overwrite=T)

#Almonds (58)
#Madera, Merced, Sacramento, San Joaquin, Stanislaus
counties<-c("Madera","Merced","Sacramento","San Joaquin","Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_58prob_w.tif"))
  bigr<-resample(r, rast, method="ngb") #expand to all county area
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
almond<-do.call(mosaic, rlist)
#fill in any empty counties
almond[is.na(almond[])] <- 0 
almond<-mask(almond, county)
#save the result
writeRaster(almond, "./Final Crops/Almond_StudyArea.tif", format="GTiff", overwrite=T)

#Walnuts (59)
#Madera, Merced, Sacramento, San Joaquin, Stanislaus 
counties<-c("Madera", "Merced","Sacramento", "San Joaquin", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_59prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
walnuts<-do.call(mosaic, rlist)
#fill in any empty counties
walnuts[is.na(walnuts[])] <- 0 
walnuts<-mask(walnuts, county)
#save the result
writeRaster(walnuts, "./Final Crops/Walnuts_StudyArea.tif", format="GTiff", overwrite=T)

#Pears (60)
#San Joaquin
counties<-c("Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_60prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# pears<-do.call(mosaic, rlist)
#only one county, no mosaic
pears<-rlist[[1]]
#fill in any empty counties
pears[is.na(pears[])] <- 0 
pears<-mask(pears, county)
#save the result
writeRaster(pears, "./Final Crops/Pears_StudyArea.tif", format="GTiff", overwrite=T)

#Pistachios (61)
#Madera, Merced, San Joaquin, Stanislaus
counties<-c("Madera", "Merced", "San Joaquin", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./",co,"/Crop_Prob_Final/", co, "_61prob_W.tif"))
  bigr<-resample(r, rast, method="ngb") #expand to all county area
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
pistachio<-do.call(mosaic, rlist)
#fill in any empty counties
pistachio[is.na(pistachio[])] <- 0 
pistachio<-mask(pistachio, county)
#save the result
writeRaster(pistachio, "./Final Crops/Pistachio_StudyArea.tif", format="GTiff", overwrite=T)

#Cantaloupes (66)
#Merced, Sacramento, San Joaquin, Stanislaus
counties<-c("Merced", "Sacramento", "San Joaquin", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_66prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
cantaloupes<-do.call(mosaic, rlist)
#fill in any empty counties
cantaloupes[is.na(cantaloupes[])] <- 0 
cantaloupes<-mask(cantaloupes, county)
#save the result
writeRaster(cantaloupes, "./Final Crops/Cantaloupes_StudyArea.tif", format="GTiff", overwrite=T)

#Oranges (69)
#Madera
counties<-c("Madera")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_69prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# oranges<-do.call(mosaic, rlist)
#only one county, no mosaic
oranges<-rlist[[1]]
#fill in any empty counties
oranges[is.na(oranges[])] <- 0 
oranges<-mask(oranges, county)
#save the result
writeRaster(oranges, "./Final Crops/Oranges_StudyArea.tif", format="GTiff", overwrite=T)

#Honeydew Melons (70)
#Sacramento, San Joaquin, Stanislaus
counties<-c("Sacramento", "San Joaquin", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_70prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
honeydew<-do.call(mosaic, rlist)
#fill in any empty counties
honeydew[is.na(honeydew[])] <- 0 
honeydew<-mask(honeydew, county)
#save the result
writeRaster(honeydew, "./Final Crops/Honeydew Melons_StudyArea.tif", format="GTiff", overwrite=T)

#Peppers (73)
#Merced, San Joaquin  
counties<-c("Merced", "San Joaquin")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_73prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
peppers<-do.call(mosaic, rlist)
#fill in any empty counties
peppers[is.na(peppers[])] <- 0 
peppers<-mask(peppers, county)
#save the result
writeRaster(peppers, "./Final Crops/Peppers_StudyArea.tif", format="GTiff", overwrite=T)

#Pomegranates(74)
#Madera
counties<-c("Madera")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_74prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# pome-do.call(mosaic, rlist)
#only one county, no mosaic
pome<-rlist[[1]]
#fill in any empty counties
pome[is.na(pome[])] <- 0 
pome<-mask(pome, county)
#save the result
writeRaster(pome, "./Final Crops/Pomegranates_StudyArea.tif", format="GTiff", overwrite=T)

#Squash (79)
#Merced, Sacramento, San Joaquin
counties<-c("Merced", "Sacramento", "San Joaquin")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_79prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
squash<-do.call(mosaic, rlist)
#fill in any empty counties
squash[is.na(squash[])] <- 0 
squash<-mask(squash, county)
#save the result
writeRaster(squash, "./Final Crops/Squash_StudyArea.tif", format="GTiff", overwrite=T)

#Lettuce (82)
#Merced 
counties<-c("Merced")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_82prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# lettuce<-do.call(mosaic, rlist)
#only one county, no mosaic
lettuce<-rlist[[1]]
#fill in any empty counties
lettuce[is.na(lettuce[])] <- 0 
lettuce<-mask(lettuce, county)
#save the result
writeRaster(lettuce, "./Final Crops/Lettuce_StudyArea.tif", format="GTiff", overwrite=T)

#Pumpkins (83)
#Merced, Sacramento, San Joaquin, Stanislaus
counties<-c("Merced", "Sacramento", "San Joaquin", "Stanislaus")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_83prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
names(rlist)[1:2]<-c("x","y")
rlist$fun<-max
rlist$na.rm<-TRUE
pumpkins<-do.call(mosaic, rlist)
#fill in any empty counties
pumpkins[is.na(pumpkins[])] <- 0 
pumpkins<-mask(pumpkins, county)
#save the result
writeRaster(pumpkins, "./Final Crops/Pumpkins_StudyArea.tif", format="GTiff", overwrite=T)

#Blueberries (84)
#San Joaquin
counties<-c("San Joaquin")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_84prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# blue<-do.call(mosaic, rlist)
blue<-rlist[[1]]
#fill in any empty counties
blue[is.na(blue[])] <- 0 
blue<-mask(blue, county)
#save the result
writeRaster(blue, "./Final Crops/Lettuce_StudyArea.tif", format="GTiff", overwrite=T)

#Cabbage (85)
#San Joaquin
counties<-c("San Joaquin")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_85prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# cabbage<-do.call(mosaic, rlist)
cabbage<-rlist[[1]]
#fill in any empty counties
cabbage[is.na(cabbage[])] <- 0 
cabbage<-mask(cabbage, county)
#save the result
writeRaster(cabbage, "./Final Crops/Cabbage_StudyArea.tif", format="GTiff", overwrite=T)

#Eggplants (90)
#San Joaquin
counties<-c("San Joaquin")
rlist<-c()
for(co in counties){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_90prob_w.tif"))
  bigr<-resample(r, rast, method="ngb")
  rlist<-append(rlist, bigr)
}
#mosaic list
# names(rlist)[1:2]<-c("x","y")
# rlist$fun<-max
# rlist$na.rm<-TRUE
# eggplant<-do.call(mosaic, rlist)
eggplant<-rlist[[1]]
#fill in any empty counties
eggplant[is.na(eggplant[])] <- 0 
eggplant<-mask(eggplant, county)
#save the result
writeRaster(eggplant, "./Final Crops/Eggplants_StudyArea.tif", format="GTiff", overwrite=T)