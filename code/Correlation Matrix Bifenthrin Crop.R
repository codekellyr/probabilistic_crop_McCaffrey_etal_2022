# Correlation matrix of crop probability values to determine if crop occurence is correlated
#edited Dec 15 2021 KM

#import the packages needed
library(sp)
library(raster)
library(rgdal)
library(RcmdrMisc)
library(colorRamps)
library(RColorBrewer)
library(corrplot)



# library(dplyr)
# library(progress)
# library(Hmisc)


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

#load in brick of bifenthrin crops
rastlist<-list.files(path="E:/Prob Crop Proj Final/Final Crops/",
                     pattern=".tif$", all.files=T, full.names=T)
allrasters<-stack(rastlist)

#try random sample
sample<-data.frame(sampleRandom(allrasters, size=6000000, na.rm=T)) #sample 1/10th of data, but get rid of NA values
# write.csv(sample, "cor_test_sample.csv")
sample<-read.csv("cor_test_sample.csv")
sample<-sample[,-1]

#remove soybeans and eggplant, no area
sample<-sample[,-c(9, 24)]
names(sample)
colnames(sample)<-c("Alfalfa", "Almond", "Cabbage", "Cantaloupes", "Corn", "Cotton", "Cucumbers", "Dry Beans",
                    "Fallow_Idle Cropland", "Grapes", "Honeydew Melons", "Lettuce", "Misc Veg Fruit",
                    "Oats", "Oranges", "Pears", "Pecans", "Peppers", "Pistachio", "Pomegranates", 
                    "Potatoes", "Pumpkins", "Squash", "Sweet Potatoes", "Tomatoes", "Walnuts", "Watermelons")
test_cor<-rcorr.adjust(as.matrix(sample), type="pearson", use="complete.obs")

#change <.0001 to 0.0001
p_val<-test_cor$P
p_val<-gsub("<.0001", "0.0001", p_val)
p_val2<-matrix(as.numeric(p_val), ncol=ncol(p_val))

M<-test_cor$R$r
M
M[M==1]<-0

# tiff("bifenthrin_corr_update.tiff", width=8, height=8, res=300, units="in", compression="lzw")
corrplot(M, method="color", insig="pch", 
         addCoef.col="black", is.corr=F, cl.lim=c(-0.2, 0.2),
         type="lower", sig.level=0.05, diag=F, p.mat=as.matrix(p_val2),
         tl.col="black", addgrid.col="grey",
         tl.cex=0.8, cl.cex=0.6, cl.ratio=0.1, number.cex=0.6)
# dev.off()