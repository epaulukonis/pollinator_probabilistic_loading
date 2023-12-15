### Probabilistic Crop Loading 

### 09 Spatial data formatting

# Edited by E. Paulukonis November 2023

### Read in simulated scenarios; we're going to focus on 2021 for this chapter. 
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
df<-scenarios[[16]] #get 2014
df<-df[!is.na(df$applicationday),] #remove all the unassigned compound/application types

### the first thing we want to do is extract the fields that are within the RPBB habitat zones. So we'll need to read thsoe in first. 
bomb_h <- st_read(bombus_dir, layer = "RPBB_High_Potential_Zones_03172021")
bomb_h<- st_transform(bomb_h, crs(df)) #reproject to match extent of DF

#let's clip the field scenario within the bombus affinis habitat
on_field_area<-st_intersection(df, bomb_h)

#remove smaller fields
on_field_area$shapearea<-as.numeric(st_area(on_field_area))
on_field_area<-on_field_area[!on_field_area$shapearea < 62500,] #remove smaller fields that are less than 500m2
#st_write(on_field_area, paste0(root_data_out, "/all_bombus/modified_sampled_fields/testing_intersection_delete_after.shp"), driver = "ESRI Shapefile")

#off-field buffer
off_field_area<-st_buffer(on_field_area,90)
#st_write(off_field_area, paste0(root_data_out, "/all_bombus/modified_sampled_fields/testing_off_field_delete_after_f.shp"), driver = "ESRI Shapefile")







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
habitat<-nlcd_ill[[7]] #get 2019

#let's get the NLCD habitat within the RPBB habitat zones 
habitat<-rast(habitat)
habitat<-classify(habitat, cbind(NA, 80)) #80 is both pasture and general crop
habitat <- mask(habitat, bomb_h) #get non-crop habtiat within the
#plot(habitat)
 
 















