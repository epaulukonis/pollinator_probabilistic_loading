### Probabilistic Crop Loading 

### 01 Extracttion of counties/area of interest

# Edited by E. Paulukonis June 2022

import_start_time <- Sys.time()
print("stepping into 01_studyarea.R")

# input state and species data
print(list.files(path=state_dir, all.files=TRUE, full.names=FALSE)) #state
print(list.files(path=bombus_dir, all.files=TRUE, full.names=FALSE)) #species


rpbb_states<-readOGR(state_dir, layer = 'RPBB_states') #read in US counties
rpbb_study<-readOGR(state_dir, layer = "IL_BNDY_County_Py") #read in states


# get species potential zones
bomb_h <- readOGR(bombus_dir, layer = "RPBB_High_Potential_Zones_03172021")
bomb_l <- readOGR(bombus_dir, layer = "RPBB_Low_Potential_Zones_03172021")
h_range <- readOGR(bombus_dir, layer = "RPBB_US_range") 

h_range<-spTransform(h_range, crs(bomb_h)) #reproject 
ill<-spTransform(rpbb_study, crs(bomb_h)) #reproject 
states<-spTransform(rpbb_states, crs(bomb_h)) #reproject 


## you can take a look at some plots here: 
# plot(h_range, col='red')
# plot(states, add=T)
# plot(bomb_l, add=T)
# plot(bomb_h, add=T)
# plot(ill, add=T)


study<-gIntersection(ill, h_range, byid=T, id=ill$COUNTY_NAM) #get intersection of counties and range here

#writeOGR(study, dsn=state_dir, layer="/study_area",driver="ESRI Shapefile") 
#plot(study)


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


 