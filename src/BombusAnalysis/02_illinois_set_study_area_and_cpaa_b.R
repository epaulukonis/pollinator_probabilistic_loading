### Probabilistic Crop Loading 

### 02 set study area and CPAA thresholds

# Edited by E. Paulukonis March 2023

##### Get the threshold info for CPAA ----

all_states<-readOGR(state_dir, layer = "tl_2021_us_county") #read in states
study<-all_states[all_states$STATEFP == "17",]
rm(all_states)
sub_group<-c("McHenry","Boone","Winnebago") # pull out McHenry County
#sub_group<-c("Winnebago")
study_county<-study[study$NAME %in% sub_group,]
study_county<-spTransform(study_county, crs(cdl_data_ill_rec[[1]])) #reproject
study_area<-aggregate(study_county)


mask_crop<-function(x){
  r_list<-crop(x, study_area)
  mask(r_list, study_area)
}

cdl_set<-lapply(cdl_data_ill_rec, mask_crop) #crop and mask the fixed CDL to the counties, put in list
cdl_set_list<-stack(cdl_set) #represents single county stack


#reclassify all crop categories 
is_n <- c(0,1:256)
becomes_n <- c(NA,rep.int(1,256))
n<-cbind(is_n,becomes_n)
cdl_set_list_rec<-reclassify(cdl_set_list, n)



#here, we're creating the layer that will delineate 23 years of crop data 
#s0 = brick(cdl_set_list_rec)
s0=cdl_set_list_rec
coords = coordinates(s0)
s1 = as.data.frame(getValues(s0))
layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
layer.3 <- data.frame(coords, layer.3)
county_binned<-layer.3
#county_binned <- county_binned[!grepl("NANANANANANANANANANANANANANANANANANANANANANANA", county_binned$layer.3),] # remove pixels that have no crops in 23 years
county_binned$f<-gsub("NA", "", county_binned$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
county_binned$field<-as.numeric(county_binned$f) #turn f into fields as numeric integer
county_binned$n_years<-as.numeric(gsub(0, "", county_binned$field))
county_binned$bin<-floor(log10(county_binned$n_years)) + 1  #create column to count the number of years

#N pixels
county_binned$bin<-ifelse(county_binned$field == 0, 0, county_binned$bin)

#what's the mode of the bin #?
getmode  <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
getmode(county_binned$bin)
hist(county_binned$bin)

#convert to binary for mask
county_binned$bin_f<-1  #if you want binary layer
f<-paste0(root_data_out,'/all_thresh/Illinois/')
write.csv(county_binned, paste0(f,"3County19992021.csv"))


#convert to a raster for visualization
county_binned_og<-county_binned
coordinates(county_binned)<-~ x + y
gridded(county_binned)<-TRUE
county_binned<- raster(county_binned)
crs(county_binned) <- crs(cdl_data_ill_rec[[1]])
county_binned<-setValues(county_binned, county_binned_og$bin)
#plot(county_binned)

#terra::writeRaster(county_binned, paste0(root_data_out, "/winnebago_bin_layer.tif"), filetype = "GTiff", overwrite = TRUE)


#### get NLCD data read-in ----

f<-paste0(root_data_out, "/all_NLCD/Illinois")
print(list.files(path=f, pattern='studyarea', all.files=TRUE, full.names=FALSE))
nlcd_ill<- file.path(f, list.files(path=f, pattern='studyarea', all.files=TRUE, full.names=FALSE))
nlcd_ill<-setNames(lapply(nlcd_ill, raster), tools::file_path_sans_ext(basename(nlcd_ill)))


#read in NLCD
# print(list.files(path=paste0(nlcd_dir,"/NLCD/NLCD_F"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
# nlcd_all<- file.path(paste0(nlcd_dir,"/NLCD/NLCD_F"), list.files(path=paste0(nlcd_dir,"/NLCD/NLCD_F"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
# nlcd_all<-setNames(lapply(nlcd_all, raster), tools::file_path_sans_ext(basename(nlcd_all)))
# 
# 
# for(nlcd_layer in 1:length(nlcd_all)){
#   nlcd<-nlcd_all[[nlcd_layer]]
#   study_area<-spTransform(study_area, crs(nlcd)) #reproject to make it easier to clip
# 
#   nlcd<-crop(nlcd, study_area)
#   nlcdc<-projectRaster(nlcd, cdl_data_ill_rec[[1]], method='ngb',crs(cdl_data_ill_rec[[1]])) #make sure it's same projected as CDL
# 
#   nlcdc<-crop(nlcdc, study_area) #crop to co_r
#   nlcdc<-mask(nlcdc, study_area) #mask to co_r
#   nlcdc[nlcdc < 81 | nlcdc > 82]<-NA
# 
#   names(nlcdc)<-names(nlcd_all[nlcd_layer])
#   f<-paste0(root_data_out, "/all_NLCD/Illinois")
#   writeRaster(nlcdc, file.path(f, paste0("studyarea","_",names(nlcdc))), format="GTiff", overwrite = TRUE)
# 
# }
# 


#combine all NLCD years into 1 
# nlcd <- sprc(lapply(nlcd_ill, rast))
# nlcd_combine <- mosaic(nlcd)
# terra::writeRaster(nlcd_combine, file.path(f, paste0("studyarea","_all_nlcd")), format="GTiff", overwrite = TRUE)
# plot(nlcd_combine)


