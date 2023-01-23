### Probabilistic Crop Loading 

### 02 Extracttion of counties/area of interest

# Edited by E. Paulukonis Sept 2021

import_start_time <- Sys.time()
print("stepping into 01_studyarea_c.R")


all_states<-readOGR(state_dir, layer = "tl_2021_us_county") #read in states
#all_states<-spTransform(all_states, crs_bh) #reproject

ill<-all_states[all_states$STATEFP == "17",]
mi<-all_states[all_states$STATEFP == "26",]
wi<-all_states[all_states$STATEFP == "55",]


states<-c("17","26","55")
study_area<-all_states[all_states$STATEFP %in% states,]

sub_group<-c("DuPage","McHenry","Champaign","Waushara","Langlade","Rock", "Van Buren", "Oceana","Huron")
suball<-study_area[study_area$NAME %in% sub_group,]



affinis_ill<-read.csv(paste0(bombus_dir,"/bombus_ILL.csv"))
names(affinis_ill)

coordinates(affinis_ill)=~decimalLongitude+decimalLatitude #get coordinates
proj4string(affinis_ill)=CRS("+init=epsg:4326") # set it to lat-long
affinis_ill = spTransform(affinis_ill,crs(all_states)) #transform to match NC habitat

plot(study_area)
plot(affinis_ill, add=T)
plot(suball, add=T, col='red')


affinis_illb<-affinis_ill[affinis_ill$Year <2017,]
affinis_illa<-affinis_ill[affinis_ill$Year >=2017,]

plot(study_area)
plot(affinis_illb, add=T)
plot(suball, add=T, col='red')


plot(study_area)
plot(affinis_illa, add=T)
plot(suball, add=T, col='red')


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


    