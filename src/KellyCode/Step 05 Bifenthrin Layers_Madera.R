# A script to produce the probabilistic and deterministic bifenthrin layers
# Last edited by KR McCaffrey Jan 2021

#Bifenthrin usagae data was obtained from CA PUR database 

#import the packages needed
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(progress)

#set the county
co<-"Madera"

#load the county shapefile
setwd("E:/EPA Work/Prob Crop Proj Jan 2021 CoA2012/Spatial Data/CA_Counties/")
counties<-readOGR(".","CA_Counties_TIGER2016")
crs(counties)
county<-subset(counties, counties$NAME==co)
rm(counties)

x<-CRS("+ proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
x2<-CRS("+init=epsg:4326")

county<-spTransform(county, x)
county2<-spTransform(county, x2)

#set the base working directory
setwd("E:/EPA Work/Prob Crop Proj Jan 2021 CoA2012/")

#Bifenthrin in Madera:
#Almond, Cotton, Grapes, Orange, Pistachio, 
#Pomegranate, Tomato, and walnut

#Corresponding crop categories:
# Almonds (58), Cotton (2), Grapes (54), Oranges (69),
# Pistachios (61), Pomegranates (74), Tomatoes (43), 
#walnuts (59)

#make a template raster
temp<-raster(paste0("./", co, "/CDL_Comb/", co, "_2013_1_stack.tif"))
rast<-raster(ext=extent(temp), crs=x,
             resolution=c(30,30))

# get the corresponding crop layers
crops<-c(2, 43, 54, 58, 59, 61, 69, 74)
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
area_pix<-Bifen_Prob
area_pix[area_pix>0]<-900 #set all cells equal to 900 m2
area_pix_prob<-area_pix*Bifen_Prob
Bifen_Area<-cellStats(area_pix_prob, sum, na.rm=T) #m2 
Bifen_Area
Bifen_Area_Acres<-Bifen_Area*0.000247105 #acres
Bifen_Area_Acres #probability-corrected area is 187617.7 acres

#total footprint area
Total_Area<-cellStats(area_pix, sum, na.rm=T) 
Total_Area 
Total_Area_Acres<-Total_Area*0.000247105
Total_Area_Acres #total footprint area is 338533.4

#A total of 22329.91205 lbs of Bifenthrin were applied to Ag Crops in Madera County in 2017
Prob_App_Rate<-22329.91205/Bifen_Area_Acres
Prob_App_Rate #0.1190181 lbs/acre

#rate based on total footprint
Total_Area_App_Rate<-22329.91205/Total_Area_Acres 
Total_Area_App_Rate #0.06596074

#### Compare to UDL ####
#general categories: other orchards, cotton, 
#vineyards, citrus, veg/ground fruit
wd<-paste0("./",co,"/Final_UDLs/")
orch<-raster(paste0(wd, "OtherOrchards_UDL_", co, "_Final.tif"))
cotton<-raster(paste0(wd, "Cotton_UDL_", co, "_Final.tif"))
vine<-raster(paste0(wd, "Vineyards_UDL_", co, "_Final.tif"))
citrus<-raster(paste0(wd, "Citrus_UDL_", co, "_Final.tif"))
veg<-raster(paste0(wd, "VegetablesFruit_UDL_", co, "_Final.tif"))

Bifen_UDL<-mosaic(orch, cotton, vine, citrus, veg, fun=max, na.rm=T)
Bifen_UDL2<-projectRaster(Bifen_UDL,crs=x2, method="ngb" )

# writeRaster(Bifen_UDL, paste0("./", co, "/Bifen_UDL_2012_", co, ".tif"), format="GTiff", overwrite=T)
# writeRaster(Bifen_UDL2, paste0("./", co, "/Bifen_UDL_2012_", co, "_latlon.tif"), format="GTiff", overwrite=T)

#find the area
area_pix<-Bifen_UDL
area_pix[area_pix>0]<-900
UDL_Area<-cellStats(area_pix, sum, na.rm=T) 
UDL_Area 
UDL_Area_Acres<-UDL_Area*0.000247105 
UDL_Area_Acres #340621.2

#A total of 22329.91205 lbs of Bifenthrin were applied to Ag Crops in Madera County in 2017
UDL_App_Rate<-22329.91205/UDL_Area_Acres
UDL_App_Rate #0.06555644 lbs/acre

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

# writeRaster(Overlap, paste0("./", co, "/Bifen_Overlap_2012_", co, "_w.tif"), format="GTiff", overwrite=T)
# writeRaster(Overlap2, paste0("./", co, "/Bifen_Overlap_2012_", co, "_latlon_w.tif"), format="GTiff", overwrite=T)

# what is the added footprint area?
area_pix<-Overlap
area_pix[area_pix>0]<-900
Total_Area_Over<-cellStats(area_pix, sum, na.rm=T)
Total_Area_Over 
Total_Area_Over_Acres<-Total_Area_Over*0.000247105
Total_Area_Over_Acres #2087.84

#How much of the UDL is the OVerlap
Total_Area_Over_Acres/UDL_Area_Acres #0.006129506 - 0.61%
Bifen_Area_Acres/UDL_Area_Acres #0.5508105 - 55.08%
Total_Area_Acres/UDL_Area_Acres #0.9938705 - 99.39%

#### Images ####
#make room in the memory
#rm() #-insert objects
# 
# library(colorspace)
# cols<-rev(sequential_hcl(10))
# zeroCol<-cols[[1]]
# oneCol<-cols[[10]]
# 
# par(ps=12, cex=1, cex.main=1.2)
# setwd(paste0("./",co,"/"))
# tiff(paste0("Bifen_",co,"_Prob_w.tiff"), units="in", width=5, height=5, res=800, compression="lzw")
# plot(Bifen_Prob2, col=cols, zlim=c(0,1),axes=F, box=F, 
#      legend.args=list(text=as.expression(bquote("Probability"))))
# plot(county2, add=T)
# axis(1, labels=T, tick=T)
# axis(2, labels=T, tick=T)
# title(xlab="Longitude", ylab="Latitude", main="")
# dev.off()
# #rm(Bifen_Prob2) #clear some memory
# 
# par(ps=12, cex=1, cex.main=1.2)
# setwd(paste0("./",co,"/"))
# tiff(paste0("Bifen_", co, "_UDL.tiff"), units="in", width=5, height=5, res=800, compression="lzw")
# plot(Bifen_UDL2, col=c(zeroCol, oneCol), zlim=c(0,1), axes=F, box=F, legend=F)
# legend(-120.2, 37,
#        #"bottomright", 
#        #inset=c(-0.7,0), 
#        legend=c("Presence","Absence"), 
#        xpd=T, fill=c(oneCol,zeroCol), bty="n", cex=0.8)
# plot(county2, add=T)
# axis(1, labels=F, tick=T)
# axis(2, labels=T, tick=T)
# title(main="Bifenthrin Action Area", xlab="", ylab="")
# dev.off()
# 
# par(ps=12, cex=1, cex.main=1.2)
# setwd(paste0("./",co,"/"))
# tiff(paste0("Bifen_",co,"_Overlap_w.tiff"), units="in", width=5, height=5, res=800, compression="lzw")
# plot(Overlap2, col=c(zeroCol, oneCol), zlim=c(0,1), axes=F, box=F, legend=F)
# legend(-120.2, 37,
#        #"bottomright", 
#        #inset=c(-0.7,0), 
#        legend=c("Presence","Absence"), 
#        xpd=T, fill=c(oneCol,zeroCol), bty="n", cex=0.8)
# plot(county2, add=T)
# axis(1, labels=T, tick=T)
# axis(2, labels=T, tick=T)
# title(main="Additional Footprint Area", xlab="Longitude", ylab="Latitude")
# dev.off()
# 
# 
