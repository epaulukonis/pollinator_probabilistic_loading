### B. affinis analysis


### 02 STudy area

# Edited by E. Paulukonis Sept 2021

import_start_time <- Sys.time()
print("stepping into 02_studyarea_c.R")


range<-read_sf(bombus_dir, layer = "RPBB_states") #states with RPBB
BZ<-read_sf(bombus_dir, layer = "RPBB_High_Potential_Zones_03172021") #habitat zones
fips<-read.csv(paste0(state_dir,"/state_codes.csv")) #state fips

affinis_all<-read.csv(paste0(bombus_dir,"/bombus_ILL.csv"))
affinis_all$ID<-row.names(affinis_all)
coordinates(affinis_all)=~decimalLongitude+decimalLatitude #get coordinates
proj4string(affinis_all)=CRS("+init=epsg:4326") 
affinis_all<-st_as_sf(affinis_all)

BP<-st_transform(affinis_all, crs(CDL[[1]])) #bombus points

#check
# plot(CDL[[1]][[1]])
# plot(BP$geometry, add=T)

BP_b<-st_buffer(BP, 500) #add a 500m buffer around the points to evaluate 

#check
# plot(BP_b$geometry[[1]])
# plot(BP$geometry[[1]], add=T)


#here is where we need to rename the data contained in the range of CDL data
names(CDL)=range$STUSPS[match(names(CDL),range$STATEFP)]


#this writes out all of the renamed CDLs so we can use them in the next script
# f=cdl_dir
# for(state in 1:length(CDL)){
# names(CDL[[state]])<-paste0(names(CDL[state]),"_",2008:2021)
# for(year in 1:14){
#   writeRaster(CDL[[state]][[year]], file.path(f, names(CDL[[state]][[year]])), format="GTiff", overwrite = TRUE)
# }
# 
# }









