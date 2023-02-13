### Probabilistic Crop Loading 

### 01 Extracttion of counties/area of interest

# Edited by E. Paulukonis Sept 2021

import_start_time <- Sys.time()
print("stepping into 01_studyarea_c.R")

# input state and species data
print(list.files(path=state_dir, all.files=TRUE, full.names=FALSE)) #state
print(list.files(path=bombus_dir, all.files=TRUE, full.names=FALSE)) #species


#rpbb_states<-readOGR(state_dir, layer = 'RPBB_states') #read in US counties

# get species potential zones
bomb_h <- readOGR(bombus_dir, layer = "RPBB_High_Potential_Zones_03172021")
# bomb_l <- readOGR(bombus_dir, layer = "RPBB_Low_Potential_Zones_03172021")
#h_range <- readOGR(bombus_dir, layer = "RPBB_US_range")
#rpbb_study<-readOGR(state_dir, layer = "IL_BNDY_County_Py") #read in states

#h_range<-spTransform(h_range, crs(bomb_h)) #reproject
#ill<-spTransform(rpbb_study, crs(bomb_h)) #reproject

# crs_bh<-crs(bomb_h)
# rm(bomb_h)

all_states<-readOGR(state_dir, layer = "tl_2021_us_county") #read in states
#all_states<-spTransform(all_states, crs_bh) #reproject

ill<-all_states[all_states$STATEFP == "17",]
mi<-all_states[all_states$STATEFP == "26",]
wi<-all_states[all_states$STATEFP == "55",]

rm(all_states)



#read in NLCD
print(list.files(path=paste0(nlcd_dir,"/NLCD/NLCD_F"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
nlcd_all<- file.path(paste0(nlcd_dir,"/NLCD/NLCD_F"), list.files(path=paste0(nlcd_dir,"/NLCD/NLCD_F"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
nlcd_all<-setNames(lapply(nlcd_all, raster), tools::file_path_sans_ext(basename(nlcd_all)))



#### various scraps ----

## you can take a look at some plots here: 
# plot(h_range, col='red')
# plot(states, add=T)
# plot(bomb_l, add=T)
# plot(bomb_h, add=T)
# plot(ill)
# plot(wi)
# plot(mi)


#study<-gIntersection(ill, h_range, byid=T, id=ill$COUNTY_NAM) #get intersection of counties and range here


#writeOGR(study, dsn=state_dir, layer="/study_area",driver="ESRI Shapefile") 

##get the roads
# roads<-readOGR(state_dir, layer = "tl_2011_17111_roads")
# roads<-spTransform(roads, crs(bomb_h))


# note that you may need to read out files to QGIS or other to look-up specific county
# if you're interested in a specific county, use code below:
# extract county
# co<-"PEORIA" #set county
# county<-state[state$COUNTY_NAM == co,]
# plot(county)
# plot(bomb_l, add=T)


 