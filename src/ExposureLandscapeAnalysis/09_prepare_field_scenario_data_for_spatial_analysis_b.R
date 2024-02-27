### Probabilistic Crop Loading 

### 09 Spatial data formatting

# Edited by E. Paulukonis November 2023

### Read in simulated scenarios; we're going to focus on 2014 for this chapter. 
print(list.files(path=paste0(root_data_out, "/all_bombus/sampled_fields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
scenarios<- file.path(paste0(root_data_out, "/all_bombus/sampled_fields"), list.files(path=paste0(root_data_out, "/all_bombus/sampled_fields"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
scenarios<-setNames(lapply(scenarios, st_read), tools::file_path_sans_ext(basename(scenarios)))
scenarios<-scenarios[(mixedsort(as.character(names(scenarios))))]

#change column names to fix 'emerged date' as application date
colnamesog <- colnames(scenarios[[1]])
colnamesnew<-c("Compound","Commodity","Year","ApplicationType", "crop","area",  "id", "planteddates","applicationday", "AvgRt_k","geometry")
#colnamesnew<-c("Compond","Commdty","Year","ApplctT", "crop","area",  "id", "plntddt","applddt", "AvgRt_k","geometry")

#change names
scenarios<- lapply(scenarios, setNames, colnamesnew)

#let's get our primary data source
df<-scenarios[[14]] #get 2012
df<-df[!is.na(df$applicationday),] #remove all the unassigned compound/application types
#rm(scenarios)

# epsg <- rgdal::make_EPSG()
# i <- grep("France", epsg$note, ignore.case=TRUE)


# st_crs(df)
# df<-st_transform(df,4269) # transform to standard NAD83
# st_is_longlat(df) #check if in lat/long for area calculation

#head(st_coordinates(df))

### the first thing we want to do is extract the fields that are within the RPBB habitat zones. So we'll need to read thsoe in first. 
bomb_h <- st_read(bombus_dir, layer = "RPBB_High_Potential_Zones_03172021")
bomb_h <- st_transform(bomb_h, crs(df)) #reproject to match extent of DF


#let's clip the field scenario within the bombus affinis habitat
on_field_area<-st_intersection(df, bomb_h)

#remove smaller fields
on_field_area$shapearea<-as.numeric(st_area(on_field_area)) #should be in meters
on_field_area<-on_field_area[!on_field_area$shapearea < 62500,] #remove smaller fields that are less than 250m2 (1 quarter km squared)
#st_write(on_field_area, paste0(root_data_out, "/all_bombus/modified_sampled_fields/on_field_area.shp"), driver = "ESRI Shapefile")

#st_write(on_field_area,paste0(root_data_in,"/OutputData/on_field_hab.shp"),driver = "ESRI Shapefile")

#off-field buffer
off_field_area<-st_buffer(on_field_area,90)
off_field_area<-st_intersection(off_field_area, bomb_h)
#st_write(off_field_area, paste0(root_data_out, "/all_bombus/modified_sampled_fields/off_field_area.shp"), driver = "ESRI Shapefile")

# get habitat points 
habpnt<-st_read(paste0(root_data_in, "/MapData/"), layer = "habitatpointsf")
plot(habpnt$geometry)
  
 # st_read(paste0(bombus_dir, "/"), layer = "studypoints")
habpnt<-st_transform(habpnt, crs(df)) 



# #check they overlap
# plot(st_geometry(on_field_area))
# plot(st_geometry(st_centroid(habpnt)), pch = 3, col = 'red', add = TRUE)

#add buffer
buf_hab<-st_buffer(habpnt, 1000)
#st_write(buf_hab, paste0(root_data_out, "/buf_hab.shp"), driver = "ESRI Shapefile")

#get NLCD
nlcd<-raster(paste0(root_data_in, "/MapData/NLCD/Illinois/nlcd2013.tiff"))
crs(nlcd)<-crs(buf_hab)
habitat <- mask(crop(nlcd, buf_hab), buf_hab) #get nlcd within habitat
habitat<-rast(habitat)
#rm(nlcd)

# habitat[21:24,] <- 20
# habitat[3:9,] <- NA
# rc3[c(1:3,6:9),] <- NA



plot(habitat)
plot(st_geometry(on_field_area), add=T)


#### I'm not actually sure we'll need this yet, hence sticking it down here ----

#prepare the raster dataset that we'll convert the field scenario into
r.raster <- raster()
extent(r.raster) <- extent(scenario_clip)
crs(r.raster)<-crs(scenario_clip)
res(r.raster) <- 30

# rasterize
field_scenario <- rasterize(x = scenario_clip, y = r.raster, field = "id")
plot(field_Scenario)

# testy<-buffer(field_scenario,width=90)
# 
# testy <- ifel(is.na(field_scenario), 1, NA)
# bb <- !buffer(testy, width=90) 


writeRaster(testy, paste0(root_data_out,"/testing/raster_convert_buffer.tif"), overwrite=T)


###next, we need to get the NLCD raster habitat within the RPBB habitat zones, and reclassify NA to general crop
#here, we read in the non-crop NLCD rasters from the 3-county region
f<-paste0(root_data_out, "/all_NLCD/Illinois")
print(list.files(path=f, pattern='studyarea', all.files=TRUE, full.names=FALSE))
nlcd_ill<- file.path(f, list.files(path=f, pattern='studyarea', all.files=TRUE, full.names=FALSE))
nlcd_ill<-setNames(lapply(nlcd_ill, raster), tools::file_path_sans_ext(basename(nlcd_ill)))
habitat<-nlcd_ill[[5]] #get 2013

#let's get the NLCD habitat within the RPBB habitat zones 
habitat<-rast(habitat)
habitat<-classify(habitat, cbind(NA, 80)) #80 is both pasture and general crop
 crs(habitat)<-crs(bomb_h)
habitat <- mask(habitat, bomb_h) #get non-crop habtiat within the
#plot(habitat)
 















