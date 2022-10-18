# A script to produce the probabilistic and deterministic bifenthrin layers
# Last edited by KR McCaffrey Aug 2020

#Bifenthrin usage data was obtained from CA PUR database 

#import the packages needed
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(progress)

#set the county
co<-"San Joaquin"

#load the county shapefile
setwd("E:/Prob Crop Proj Final/Spatial Data/CA_Counties/")
counties<-readOGR(".","CA_Counties_TIGER2016")
crs(counties)
county<-subset(counties, counties$NAME==co)
rm(counties)

x<-CRS("+ proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
x2<-CRS("+init=epsg:4326")

county<-spTransform(county, x)
county2<-spTransform(county, x2)

#set the base working directory
setwd("E:/Prob Crop Proj Final/")

#Bifenthrin in San Joaquin:
#Almond, Dried beans, blueberry, brussels sprouts, 
#cabbage, cantaloupe, corn, cucumber, eggplant,
#grapes, melons, pear, peppers, pistachio, potato,
#pumpkin, squash, tomato, walnut, watermelons
##nursery use excluded (inc soil application preplant)

#Corresponding crop categories:
#Almonds (58), Dry Beans (31), Blueberries (84),
#Misc Vegs & Fruits (36),  Cabbage (85), Cantaloupes (66),
#Corn (1), Cucumbers (39), Eggplants (90),
#Grapes (54), Melons [Honeydew MElons (70), Watermelons (37)]
#Pears (60), Peppers (73), Pistachios (61), Potatoes (32),
#Pumpkins (83), Squash (79), Tomatoes (43),
#Walnuts (59), Watermelons [already inc]

#make a template raster
temp<-raster(paste0("./", co, "/CDL_Comb/", co, "_2013_1_stack.tif"))
rast<-raster(ext=extent(temp), crs=x,
             resolution=c(30,30))

# get the corresponding crop layers
crops<-c(1, 31, 32, 36, 37, 39, 43, 54, 58, 59, 60, 61, 66, 70, 73, 79, 83, 84, 85, 90)
crop_stack<-stack()
crop_stack<-stack(rast)
for(i in crops){
  r<-raster(paste0("./", co, "/Crop_Prob_Final/", co, "_",i,"prob_w.tif"))
  crop_stack<-stack(crop_stack, r)
}

Bifen_Prob<-calc(crop_stack, fun=function(p){1-prod(1-p, na.rm=T)}) #finds the probability that each pixel had a "Bifenthrin Crop" planted in it
Bifen_Prob2<-projectRaster(Bifen_Prob, crs=x2, method="ngb")

Bifen_Prob<-mask(Bifen_Prob, county)
Bifen_Prob2<-mask(Bifen_Prob2, county2)

plot(Bifen_Prob)
plot(Bifen_Prob2)

# writeRaster(Bifen_Prob, paste0("./",co,"/",co,"_Bifen_Prob_2012_w.tif"), format="GTiff", overwrite=T)
# writeRaster(Bifen_Prob2, paste0("./",co,"/", co, "_Bifen_Prob_2012_latlon_w.tif"), format="GTiff", overwrite=T)

#find the area
Bifen_Prob<-raster(paste0("./",co,"/",co,"_Bifen_Prob_2012_w.tif"))
area_pix<-Bifen_Prob
area_pix[area_pix>0]<-900 #set all cells equal to 900 m2
area_pix_prob<-area_pix*Bifen_Prob
Bifen_Area<-cellStats(area_pix_prob, sum, na.rm=T) #m2 
Bifen_Area
Bifen_Area_Acres<-Bifen_Area*0.000247105 #acres
Bifen_Area_Acres #probability-corrected area is 275766.8 acres

#total footprint area
Total_Area<-cellStats(area_pix, sum, na.rm=T) 
Total_Area 
Total_Area_Acres<-Total_Area*0.000247105
Total_Area_Acres #total footprint area is 502952.3

#A total of 9949.238556 lbs of Bifenthrin were applied to Ag Crops in Sacramento County in 2017
Prob_App_Rate<-9949.238556/Bifen_Area_Acres
Prob_App_Rate #0.03607845 lbs/acre

#rate based on total footprint
Total_Area_App_Rate<-9949.238556/Total_Area_Acres 
Total_Area_App_Rate # 0.01978168

#### Compare to UDL ####
#general categories: other orchards, vegetables and ground fruit,
#corn, vineyards

wd<-paste0("./",co,"/Final_UDLs/")
orch<-raster(paste0(wd, "OtherOrchards_UDL_", co, "_Final.tif"))
veg<-raster(paste0(wd, "VegetablesFruit_UDL_", co, "_Final.tif"))
corn<-raster(paste0(wd, "Corn_UDL_", co, "_Final.tif"))
vine<-raster(paste0(wd, "Vineyards_UDL_", co, "_Final.tif"))

Bifen_UDL<-mosaic(orch, veg, corn, vine, fun=max, na.rm=T)
Bifen_UDL2<-projectRaster(Bifen_UDL,crs=x2, method="ngb" )

# writeRaster(Bifen_UDL, paste0("./", co, "/Bifen_UDL_2012_", co, ".tif"), format="GTiff", overwrite=T)
# writeRaster(Bifen_UDL2, paste0("./", co, "/Bifen_UDL_2012_", co, "_latlon.tif"), format="GTiff", overwrite=T)

#find the area
Bifen_UDL<-raster(paste0("./", co, "/Bifen_UDL_2012_", co, ".tif"))
area_pix<-Bifen_UDL
area_pix[area_pix>0]<-900
UDL_Area<-cellStats(area_pix, sum, na.rm=T) 
UDL_Area #
UDL_Area_Acres<-UDL_Area*0.000247105 
UDL_Area_Acres #508653.1

#A total of 9949.238556 lbs of Bifenthrin were applied 
UDL_App_Rate<-9949.238556/UDL_Area_Acres
UDL_App_Rate #0.01955997 lbs/acre

#### Examine the Overlap ####

Bifen_Prob[Bifen_Prob==0]<-NA
Bifen_Prob[Bifen_Prob>0]<-1
plot(Bifen_Prob)

Bifen_UDL[Bifen_UDL==0]<-NA
Bifen_UDL[Bifen_UDL>0]<-1
plot(Bifen_UDL)

Overlap<-mask(Bifen_UDL, Bifen_Prob, maskvalue=1)
plot(Overlap)

Overlap[is.na(Overlap[])]<-0
Overlap<-mask(Overlap, county)
Overlap2<-projectRaster(Overlap, crs=x2, method="ngb")

plot(Overlap)
plot(Overlap2)

writeRaster(Overlap, paste0("./", co, "/Bifen_Overlap_2012_", co, "_w.tif"), format="GTiff", overwrite=T)
writeRaster(Overlap2, paste0("./", co, "/Bifen_Overlap_2012_", co, "_latlon_w.tif"), format="GTiff", overwrite=T)

# what is the added footprint area?
area_pix<-Overlap
area_pix[area_pix>0]<-900
Total_Area_Over<-cellStats(area_pix, sum, na.rm=T)
Total_Area_Over #
Total_Area_Over_Acres<-Total_Area_Over*0.000247105
Total_Area_Over_Acres #5700.861

#How much of the UDL is the OVerlap
Total_Area_Over_Acres/UDL_Area_Acres #0.01120776 - 1.12%
Bifen_Area_Acres/UDL_Area_Acres #0.542151 - 54.22%
Total_Area_Acres/UDL_Area_Acres #0.9887922 - 98.88%