# A script to produce the final study area layers
# Last edited by KR McCaffrey Jan 2021

#import the packages needed
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(progress)

#get the counties in the study area
setwd("E:/EPA Work/Prob Crop Proj Jan 2021 CoA2012/Spatial Data/CA_Counties/")
counties<-readOGR(".","CA_Counties_TIGER2016")
county<-subset(counties, counties$NAME=="Madera" | 
                 counties$NAME=="Merced"|counties$NAME=="Sacramento"|
                 counties$NAME=="San Joaquin"|counties$NAME=="Stanislaus")

x<-CRS("+ proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
x2<-CRS("+init=epsg:4326")

county<-spTransform(county, x)

#set the base working directory
setwd("E:/EPA Work/Prob Crop Proj Jan 2021 CoA2012/")

#Probability Raster
Mad<-raster("./Madera/Madera_Bifen_Prob_2012_w.tif")
Merc<-raster("./Merced/Merced_Bifen_Prob_2012_w.tif")
Sac<-raster("./Sacramento/Sacramento_Bifen_Prob_2012_w.tif")
SJ<-raster("./San Joaquin/San Joaquin_Bifen_Prob_2012_w.tif")
Stan<-raster("./Stanislaus/Stanislaus_Bifen_Prob_2012_w.tif")

Prob<-mosaic(Mad, Merc, Sac, SJ, Stan, fun=mean, na.rm=T)
Prob2<-projectRaster(Prob, crs=x2, method="ngb")

# writeRaster(Prob, "Bifenthrin_Probability_StudyArea_w_2012.tif", format="GTiff", overwrite=T)
# writeRaster(Prob2, "Bifenthrin_Probability_StudyArea_w_latlon_2012.tif", format="GTiff", overwrite=T)

#Deterministic Raster
Mad<-raster("./Madera/Bifen_UDL_2012_Madera.tif")
Merc<-raster("./Merced/Bifen_UDL_2012_Merced.tif")
Sac<-raster("./Sacramento/Bifen_UDL_2012_Sacramento.tif")
SJ<-raster("./San Joaquin/Bifen_UDL_2012_San Joaquin.tif")
Stan<-raster("./Stanislaus/Bifen_UDL_2012_Stanislaus.tif")

UDL<-mosaic(Mad, Merc, Sac, SJ, Stan, fun=max, na.rm=T)
UDL2<-projectRaster(UDL, crs=x2, method="ngb")

# writeRaster(UDL, "Bifenthrin_UDL_StudyArea_2012.tif", format="GTiff", overwrite=T)
# writeRaster(UDL2, "Bifenthrin_UDL_StudyArea_latlon_2012.tif", format="GTiff", overwrite=T)

#Overlap Raster
Prob[Prob==0]<-NA
Prob[Prob>0]<-1

UDL[UDL==0]<-NA
UDL[UDL>0]<-1

Overlap<-mask(UDL, Prob, maskvalue=1)
Overlap[is.na(Overlap[])]<-0
Overlap<-mask(Overlap, county)
Overlap2<-projectRaster(Overlap, crs=x2, method="ngb")

# writeRaster(Overlap, "Bifen_Overlap_2012.tif", format="GTiff", overwrite=T)
# writeRaster(Overlap2, "Bifen_Overlap_2012_latlon.tif", format="GTiff", overwrite=T)

#### Areas ####
Prob<-raster("Bifenthrin_Probability_StudyArea_w_2012.tif")
UDL<-raster("Bifenthrin_UDL_StudyArea_2012.tif")
Overlap<-raster("Bifen_Overlap_2012.tif")

area_pix<-Prob
area_pix[area_pix>0]<-900
area_m2<-cellStats(area_pix, sum, na.rm=T)
area_acre<-area_m2*0.000247105 
area_acre #1867159

area_pix<-area_pix*Prob
area_m2<-cellStats(area_pix, sum, na.rm=T)
area_acre<-area_m2*0.000247105 
area_acre #995866.1

area_pix<-UDL
area_pix[area_pix>0]<-900
area_m2<-cellStats(area_pix, sum, na.rm=T)
area_acre<-area_m2*0.000247105 
area_acre #2100145

area_pix<-Overlap
area_pix[area_pix>0]<-900
area_m2<-cellStats(area_pix, sum, na.rm=T)
area_acre<-area_m2*0.000247105 
area_acre #232985.8

#cultivated layer, annual

#2013
Madera<-raster("./Madera/Cultivated_Annual/Cultivated_Land_2013.tif")
Merced<-raster("./Merced/Cultivated_Annual/Cultivated_Land_2013.tif")
Sacramento<-raster("./Sacramento/Cultivated_Annual/Cultivated_Land_2013.tif")
SanJoaquin<-raster("./San Joaquin/Cultivated_Annual/Cultivated_Land_2013.tif")
Stanislaus<-raster("./Stanislaus/Cultivated_Annual/Cultivated_Land_2013.tif")

cult<-mosaic(Madera, Merced, Sacramento, SanJoaquin, Stanislaus, fun=max, na.rm=T)
cult2<-projectRaster(cult, crs=x2, method="ngb")

writeRaster(cult, "Cultivated_StudyArea_2013.tif", format="GTiff", overwrite=T)
writeRaster(cult2, "Cultivated_StudyArea_2013_latlon.tif", format="GTiff", overwrite=T)

#2014
Madera<-raster("./Madera/Cultivated_Annual/Cultivated_Land_2014.tif")
Merced<-raster("./Merced/Cultivated_Annual/Cultivated_Land_2014.tif")
Sacramento<-raster("./Sacramento/Cultivated_Annual/Cultivated_Land_2014.tif")
SanJoaquin<-raster("./San Joaquin/Cultivated_Annual/Cultivated_Land_2014.tif")
Stanislaus<-raster("./Stanislaus/Cultivated_Annual/Cultivated_Land_2014.tif")

cult<-mosaic(Madera, Merced, Sacramento, SanJoaquin, Stanislaus, fun=max, na.rm=T)
cult2<-projectRaster(cult, crs=x2, method="ngb")

writeRaster(cult, "Cultivated_StudyArea_2014.tif", format="GTiff", overwrite=T)
writeRaster(cult2, "Cultivated_StudyArea_2014_latlon.tif", format="GTiff", overwrite=T)

#2015
Madera<-raster("./Madera/Cultivated_Annual/Cultivated_Land_2015.tif")
Merced<-raster("./Merced/Cultivated_Annual/Cultivated_Land_2015.tif")
Sacramento<-raster("./Sacramento/Cultivated_Annual/Cultivated_Land_2015.tif")
SanJoaquin<-raster("./San Joaquin/Cultivated_Annual/Cultivated_Land_2015.tif")
Stanislaus<-raster("./Stanislaus/Cultivated_Annual/Cultivated_Land_2015.tif")

cult<-mosaic(Madera, Merced, Sacramento, SanJoaquin, Stanislaus, fun=max, na.rm=T)
cult2<-projectRaster(cult, crs=x2, method="ngb")

writeRaster(cult, "Cultivated_StudyArea_2015.tif", format="GTiff", overwrite=T)
writeRaster(cult2, "Cultivated_StudyArea_2015_latlon.if", format="GTiff", overwrite=T)

#2016
Madera<-raster("./Madera/Cultivated_Annual/Cultivated_Land_2016.tif")
Merced<-raster("./Merced/Cultivated_Annual/Cultivated_Land_2016.tif")
Sacramento<-raster("./Sacramento/Cultivated_Annual/Cultivated_Land_2016.tif")
SanJoaquin<-raster("./San Joaquin/Cultivated_Annual/Cultivated_Land_2016.tif")
Stanislaus<-raster("./Stanislaus/Cultivated_Annual/Cultivated_Land_2016.tif")

cult<-mosaic(Madera, Merced, Sacramento, SanJoaquin, Stanislaus, fun=max, na.rm=T)
cult2<-projectRaster(cult, crs=x2, method="ngb")

writeRaster(cult, "Cultivated_StudyArea_2016.tif", format="GTiff", overwrite=T)
writeRaster(cult2, "Cultivated_StudyArea_2016_latlon.if", format="GTiff", overwrite=T)

#2017
Madera<-raster("./Madera/Cultivated_Annual/Cultivated_Land_2017.tif")
Merced<-raster("./Merced/Cultivated_Annual/Cultivated_Land_2017.tif")
Sacramento<-raster("./Sacramento/Cultivated_Annual/Cultivated_Land_2017.tif")
SanJoaquin<-raster("./San Joaquin/Cultivated_Annual/Cultivated_Land_2017.tif")
Stanislaus<-raster("./Stanislaus/Cultivated_Annual/Cultivated_Land_2017.tif")

cult<-mosaic(Madera, Merced, Sacramento, SanJoaquin, Stanislaus, fun=max, na.rm=T)
cult2<-projectRaster(cult, crs=x2, method="ngb")

writeRaster(cult, "Cultivated_StudyArea_2017.tif", format="GTiff", overwrite=T)
writeRaster(cult2, "Cultivated_StudyArea_2017_latlon.tif", format="GTiff", overwrite=T)

## Area of each year

#2013
cult1<-raster("Cultivated_StudyArea_2013.tif")
area_pix<-cult1
area_pix[area_pix>0]<-900
area_m2<-cellStats(area_pix, sum, na.rm=T)
area_acre<-area_m2*0.000247105 
area_acre #3501450

#2014
cult2<-raster("Cultivated_StudyArea_2014.tif")
area_pix<-cult2
area_pix[area_pix>0]<-900
area_m2<-cellStats(area_pix, sum, na.rm=T)
area_acre<-area_m2*0.000247105 
area_acre #3532398

#2015
cult3<-raster("Cultivated_StudyArea_2015.tif")
area_pix<-cult3
area_pix[area_pix>0]<-900
area_m2<-cellStats(area_pix, sum, na.rm=T)
area_acre<-area_m2*0.000247105 
area_acre #3438037

#2016
cult4<-raster("Cultivated_StudyArea_2016.tif")
area_pix<-cult4
area_pix[area_pix>0]<-900
area_m2<-cellStats(area_pix, sum, na.rm=T)
area_acre<-area_m2*0.000247105 
area_acre #3432404

#2017
cult5<-raster("Cultivated_StudyArea_2017.tif")
area_pix<-cult5
area_pix[area_pix>0]<-900
area_m2<-cellStats(area_pix, sum, na.rm=T)
area_acre<-area_m2*0.000247105 
area_acre #3443683


#### by county ####

#madera
shp<-subset(county, county$NAME=="Madera")
for(i in c(2013:2017)){
  rast<-raster(paste0("Cultivated_StudyArea_", i, ".tif"))
  rast2<-mask(rast, shp)
  rast2[rast2>0]<-900
  area_m2<-cellStats(rast2, sum, na.rm=T)
  area_acre<-area_m2*0.000247105 
  print(area_acre)
}
#670037
#683958.9
#637485.8
#633128.7
#639845

#Merced
shp<-subset(county, county$NAME=="Merced")
for(i in c(2013:2017)){
  rast<-raster(paste0("Cultivated_StudyArea_", i, ".tif"))
  rast2<-mask(rast, shp)
  rast2[rast2>0]<-900
  area_m2<-cellStats(rast2, sum, na.rm=T)
  area_acre<-area_m2*0.000247105 
  print(area_acre)
}
#1013817
#1025061
#998073.6
#996107.2
#1000667

#Sacramento
shp<-subset(county, county$NAME=="Sacramento")
for(i in c(2013:2017)){
  rast<-raster(paste0("Cultivated_StudyArea_", i, ".tif"))
  rast2<-mask(rast, shp)
  rast2[rast2>0]<-900
  area_m2<-cellStats(rast2, sum, na.rm=T)
  area_acre<-area_m2*0.000247105 
  print(area_acre)
}
#388640.2
#394198.5
#387368.7
#389717.9
#390489.6

#San Joaquin
shp<-subset(county, county$NAME=="San Joaquin")
for(i in c(2013:2017)){
  rast<-raster(paste0("Cultivated_StudyArea_", i, ".tif"))
  rast2<-mask(rast, shp)
  rast2[rast2>0]<-900
  area_m2<-cellStats(rast2, sum, na.rm=T)
  area_acre<-area_m2*0.000247105 
  print(area_acre)
}
#733765.1
#737261.1
#734923.5
#734603.7
#734584.4

#Stanislaus
shp<-subset(county, county$NAME=="Stanislaus")
for(i in c(2013:2017)){
  rast<-raster(paste0("Cultivated_StudyArea_", i, ".tif"))
  rast2<-mask(rast, shp)
  rast2[rast2>0]<-900
  area_m2<-cellStats(rast2, sum, na.rm=T)
  area_acre<-area_m2*0.000247105 
  print(area_acre)
}
#695189.9
#691918.9
#680185.1
#678846.8
#678097.1


# 
# 
# 
# #### IMAGES ####
# 
# #Deterministic
# UDL<-raster("Bifenthrin_UDL_StudyArea.tif")
# area_pix<-UDL
# area_pix[area_pix>0]<-900
# area_m2<-cellStats(area_pix, sum, na.rm=T)
# area_acre<-area_m2*0.000247105 
# area_acre
# 
# # UDL2<-raster("Bifenthrin_UDL_StudyArea_latlon.tif")
# # 
# # county2<-spTransform(county, "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
# # 
# # wd<-"E:/EPA Work/Prob_Crop_Proj/MS_Images/"
# # setwd(wd)
# # 
# # #Deterministic
# # library(colorspace)
# # cols<-rev(heat_hcl(10))
# # zeroCol<-cols[1]
# # oneCol<-cols[10]
# # par(ps=12, cex=1, cex.main=1)
# # tiff("Bifen_UDL.tiff", units="in", width=6, height=6, res=800, compression="lzw")
# # plot(UDL2, col=c(zeroCol, oneCol), main="Deterministic Bifenthrin Action Area", 
# #      zlim=c(0,1), axes=F, box=F, legend=F,
# #      xlim=c(-122, -119), ylim=c(36.5, 39))
# # legend(-119.5, 37,
# #        legend=c("Presence","Absence"),
# #        xpd=T, fill=c(oneCol, zeroCol), bty="n", cex=0.8)
# # axis(1, labels=T, tick=T)
# # axis(2, labels=T, tick=T)
# # plot(county2, add=T)
# # text(coordinates(county2)[,1], coordinates(county2)[,2], 
# #      font=2, county$NAME, cex=0.6, ps=12, col="black", adj=0.4)
# # dev.off()
# 
# # #Probabilistic
# # setwd("E:/EPA Work/Prob_Crop_Proj")
# # Prob<-raster("Bifenthrin_Probability_StudyArea_w.tif")
# # Prob2<-raster("Bifenthrin_Probability_StudyArea_w_latlon.tif")
# # 
# # area_pix<-Prob
# # area_pix[area_pix>0]<-900
# # area_m2<-cellStats(area_pix, sum, na.rm=T)
# # area_acre<-area_m2*0.000247105 
# # area_acre #1866437
# # area_pix<-area_pix*Prob
# # area_m2<-cellStats(area_pix, sum, na.rm=T)
# # area_acre<-area_m2*0.000247105 
# # area_acre #1049646
# # 
# # setwd(wd)
# # par(ps=12, cex=1, cex.main=1)
# # tiff("Bifen_Prob.tiff", units="in", width=6, height=6, res=800, compression="lzw")
# # plot(Prob2, col=cols, zlim=c(0,1), axes=F, box=F,
# #      legend.args=list(text=as.expression(bquote("Probability"))),
# #      main="Probabilistic Bifenthrin Action Area")
# # axis(1, labels=T, tick=T)
# # axis(2, labels=T, tick=T)
# # plot(county2, add=T)
# # text(coordinates(county2)[,1], coordinates(county2)[,2], 
# #      county$NAME, cex=0.6, ps=12, col="black", adj=0.4, font=2)
# # dev.off()
# 
# 
# 
# setwd(wd)
# library(colorspace)
# cols<-rev(heat_hcl(10))
# zeroCol<-cols[1]
# oneCol<-cols[10]
# par(ps=12, cex=1, cex.main=1)
# tiff("Bifen_Overlap.tiff", units="in", width=6, height=6, res=800, compression="lzw")
# plot(Overlap2, col=c(zeroCol, oneCol), main="Additional Action Area", 
#      zlim=c(0,1), axes=F, box=F, legend=F,
#      xlim=c(-122, -119), ylim=c(36.5, 39))
# legend(-119.5, 37,
#        legend=c("Presence","Absence"),
#        xpd=T, fill=c(oneCol, zeroCol), bty="n", cex=0.8)
# axis(1, labels=T, tick=T)
# axis(2, labels=T, tick=T)
# plot(county2, add=T)
# text(coordinates(county2)[,1], coordinates(county2)[,2], 
#      font=2, county$NAME, cex=0.6, ps=12, col="black", adj=0.4)
# dev.off()