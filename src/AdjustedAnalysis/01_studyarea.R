### Probabilistic Crop Loading 

### 01 Extracttion of counties/area of interest

# Edited by E. Paulukonis Sept 2021

import_start_time <- Sys.time()
print("stepping into 01_studyarea.R")

# input state and species data
print(list.files(path=state_dir, all.files=TRUE, full.names=FALSE)) #state
print(list.files(path=bombus_dir, all.files=TRUE, full.names=FALSE)) #species

state<-readOGR(state_dir, layer = "IL_BNDY_County_Py") #read in state
study<-readOGR(state_dir, layer = "Study_Counties") 
# plot(state)
# state #check crs of shapefile

# get species potential zones
bomb_h <- readOGR(bombus_dir, layer = "RPBB_High_Potential_Zones_03172021")
bomb_l <- readOGR(bombus_dir, layer = "RPBB_Low_Potential_Zones_03172021")
h_range <- readOGR(bombus_dir, layer = "RPBB_US_range") 

h_range<-spTransform(h_range, crs(bomb_h)) #reproject 
state<-spTransform(state, crs(bomb_h)) #reproject 
study<-spTransform(study, crs(bomb_h)) #reproject 
study<-aggregate(study)

# plot(h_range)
# plot(bomb_l, add=T)
# plot(bomb_h, add=T, col='red')

# plot(state, add=T)
# plot(study, add=T, col='blue')



# note that you may need to read out files to QGIS or other to look-up specific county

# if you're interested in a specific county, use code below:
# extract county
# co<-"PEORIA" #set county
# county<-state[state$COUNTY_NAM == co,]
# plot(county)
# plot(bomb_l, add=T)


