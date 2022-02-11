### Probabilistic Crop Loading 

### 01 Extracttion of counties/area of interest

# Edited by E. Paulukonis Sept 2021

import_start_time <- Sys.time()
print("stepping into 01_studyarea.R")

# input state and species data
print(list.files(path=state_dir, all.files=TRUE, full.names=FALSE)) #state
print(list.files(path=bombus_dir, all.files=TRUE, full.names=FALSE)) #species


US<-readOGR(state_dir, layer = 'cb_2018_us_state_500k') #read in US counties
state<-readOGR(state_dir, layer = "IL_BNDY_County_Py") #read in state
# plot(state)
# state #check crs of shapefile

# get species potential zones
bomb_h <- readOGR(bombus_dir, layer = "RPBB_High_Potential_Zones_03172021")
bomb_l <- readOGR(bombus_dir, layer = "RPBB_Low_Potential_Zones_03172021")
h_range <- readOGR(bombus_dir, layer = "RPBB_US_range") 

h_range<-spTransform(h_range, crs(bomb_h)) #reproject 
state<-spTransform(state, crs(bomb_h)) #reproject 
us<-spTransform(US, crs(bomb_h)) #reproject 


plot(h_range, col='red')
plot(us, add=T)
plot(bomb_l, add=T)
plot(bomb_h, add=T)
plot(state, add=T)

us_s<-st_as_sf(us)
h_range_s<-st_as_sf(h_range)
study <- st_intersection(us_s, h_range_s)
plot(study)
study<-sf::as_Spatial(study)
# plot(study, add=T, col='blue')



# note that you may need to read out files to QGIS or other to look-up specific county

# if you're interested in a specific county, use code below:
# extract county
# co<-"PEORIA" #set county
# county<-state[state$COUNTY_NAM == co,]
# plot(county)
# plot(bomb_l, add=T)


