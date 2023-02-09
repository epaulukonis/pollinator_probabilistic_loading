### B. affinis analysis


### 02 STudy area

# Edited by E. Paulukonis Sept 2021

import_start_time <- Sys.time()
print("stepping into 01_studyarea_c.R")


BR<-read_sf(bombus_dir, layer = "RPBB_states")
BZ<-read_sf(bombus_dir, layer = "RPBB_Low_Potential_Zones_03172021")
fips<-read.csv(paste0(state_dir,"/state_codes.csv"))

#affinis_all<-read.csv(paste0(bombus_dir,"/bombus_ILL.csv"))
# affinis_all<-read.csv(paste0(bombus_dir,"/bombus_ILL.csv"))
# affinis_all$ID<-row.names(affinis_all)
# affinis_b<-affinis_all[affinis_all$Year <2000,]
# affinis_a<-affinis_all[affinis_all$Year >=2000,]

BZ<-st_transform(BZ, crs(CDL[[1]]))
#check
# plot(CDL[[1]][[1]])
# plot(BZ$geometry, add=T)


bombus_zones<-list()
# for(state in 1:length(CDL)){
#   state=1
#   cdl_by_state<-CDL[[state]]
#   cdl_masked = mask(cdl_by_state, BZ)
#   bombus_zones[state]<-cdl_masked
#   print(names(bombus_zones[state]))
#   
# }


saveRDS(bombus_zones, file=paste0(bombus_dir,"/bombus_CDL_zones.RData"))



namesones)<-names(CDL)
names(bombus_zones)=BR$STUSPS[match(names(bombus_zones),BR$STATEFP)]

for(state in 1:length(CDL)){
names(bombus_zones[[state]])<-paste0(names(bombus_zones[state]),"_",2008:2021)
}

#### kriging to consider later...----
#problem: many of the tutorials I've seen for kriging involve continuous data
#my continuous data isn't so great; it's more likely that some spots just recorded a single occurence even if there were multiple
# I have only ever looked at SV with residual data, and never done kriging before
#so I found a package/blog that talks about advantages of INLA, which allows for non-normal errors within presence/absence data


#variogram to get distance at which SP autocorrelation becomes negligible
# coordinates(affinis_a)<-~decimalLongitude+decimalLatitude #get coordinates
# coordinates(affinis_b)<-~decimalLongitude+decimalLatitude #get coordinates

# n.vgma <- variogram(log(individualCount) ~ decimalLatitude +decimalLongitude , affinis_a, width=0.1)
# n.fita <- fit.variogram(n.vgma, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
# plot(n.vgma, n.fita) 
# 
# n.vgmb <- variogram(log(individualCount) ~ decimalLatitude +decimalLongitude , affinis_b, width=0.1)
# n.fitb <- fit.variogram(n.vgmb, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)
# plot(n.vgmb, n.fitb) 


#2km as distance for now


#now, we need to define the points where we have measurements (counts), and the grid of interest
names(affinis_all)[5]<-"y"
names(affinis_all)[6]<-"x"

#all points that we have
plot_points <- affinis_all %>% as.data.frame %>%
  ggplot(aes(x,y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")
plot_points

#area of interest by state
BR_sp<-st_as_sf(BR)
plot(BR_sp["GEOID"], key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)


#let's create a grid across the study area
#first, re-project to the CDL

grid_list<-list()
for(state in 1:length(CDL)){
  state_shp<-BR[state,]
   proj4string(state_shp)=CRS("+init=epsg:4326") # set it to lat-long
   state_shp = spTransform(state_shp,crs(CDL[[1]])) #transform to match NC habitat
   # grid <- makegrid(state_shp, cellsize = 30)
   # grid_list[state]<-grid
   grid_list[state]<-state_shp
}


nc_points<-


logi_point_in_pol <- st_intersects(box, points, sparse = FALSE)
point_in_pol <- points[as.vector(logi_point_in_pol), ]

grid <- makegrid(grid_list[[1]], cellsize = 30) # cellsize in map units!
plot(grid)





#convert to spatial
coordinates(affinis_all)=~decimalLongitude+decimalLatitude #get coordinates
proj4string(affinis_all)=CRS("+init=epsg:4326") # set it to lat-long
affinis_all = spTransform(affinis_all,crs(raw_CDL_all[[1]][[1]])) #transform to match NC habitat


plot(affinis_b)
plot(affinis_a)







writeOGR(obj=study_area, dsn=paste0(state_dir,"/studyarea"), layer="StudyArea_bombus", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=suball, dsn=paste0(state_dir,"/studyarea"), layer="Counties_bombus", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=affinis_illa, dsn=paste0(state_dir,"/studyarea"), layer="locationsa_bombusesa", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=affinis_illb, dsn=paste0(state_dir,"/studyarea"), layer="locationsb_bombusesa", driver="ESRI Shapefile") # this is in geographical projection


#### GLRI study area
glri<-readOGR(paste0(state_dir,"/greatlakes_subbasins"), layer = "greatlakes_subbasins")
glri= spTransform(glri, crs(all_states)) #transform to match NC habitat

plot(study_area)
plot(glri,add=T, col='blue')

plot(glri)
plot(all_states,add=T)

#RPBB
rpbb<-readOGR(paste0(state_dir), layer = "RPBB_states")

codes<-as.data.frame(cbind(as.numeric(rpbb$STATEFP),(rpbb$STUSPS)))

write.csv(codes ,file='codes.csv', row.names=FALSE)


    