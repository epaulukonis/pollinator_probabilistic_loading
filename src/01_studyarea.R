### Probabilistic Crop Loading 

###01 Extracttion of counties/area of interest

#Edited by E. Paulukonis Sept 2021

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
# plot(bomb_l)
# plot(bomb_h, add=T, col='red')

state<-spTransform(state, crs(bomb_h)) #reproject 
study<-spTransform(study, crs(bomb_h)) #reproject 

plot(state)
plot(study, add=T, col='blue')

# plot(bomb_l, add=T, col='blue')
# plot(bomb_h, add=T, col='red')

# note that you may need to read out files to QGIS or other to look-up specific county

# extract county
# co<-"PEORIA" #set county
# county<-state[state$COUNTY_NAM == co,]
# plot(county)
# plot(bomb_l, add=T)


