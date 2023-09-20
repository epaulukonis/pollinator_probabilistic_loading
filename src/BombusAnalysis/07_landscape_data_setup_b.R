### Probabilistic Crop Loading 

### 07 landscape setup

# Edited by E. Paulukonis April 2023

f<-paste0(root_data_out, "/all_NLCD/Illinois/Habitat")
print(list.files(path=f, pattern='studyarea', all.files=TRUE, full.names=FALSE))
nlcd_ill<- file.path(f, list.files(path=f, pattern='studyarea', all.files=TRUE, full.names=FALSE))
nlcd_ill<-setNames(lapply(nlcd_ill, raster), tools::file_path_sans_ext(basename(nlcd_ill)))

plot(nlcd_ill[[1]])

# all_states<-readOGR(state_dir, layer = "tl_2021_us_county") #read in states
# study<-all_states[all_states$STATEFP == "17",]
# rm(all_states)
# sub_group<-c("McHenry","Boone","Winnebago") # pull out McHenry County
# #sub_group<-c("Winnebago")
# study_county<-study[study$NAME %in% sub_group,]
# study_county<-spTransform(study_county, crs(cdl_data_ill_rec[[1]])) #reproject
# study_area<-aggregate(study_county)

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
#   nlcdc[nlcdc == 81 | nlcdc == 82]<-NA
# 
#   names(nlcdc)<-names(nlcd_all[nlcd_layer])
#   f<-paste0(root_data_out, "/all_NLCD/Illinois/Habitat")
#   writeRaster(nlcdc, file.path(f, paste0("studyarea","_",names(nlcdc))), format="GTiff", overwrite = TRUE)
# 
# }

plot(nlcd_ill[[1]])
plot(field_data_f['totalloadingN_g'], add=T)

# test<-treated_fields[[1]]
# st_rasterize(test %>% dplyr::select(value, geometry))


r <- raster(field_data_f, res = 30)
r <- fasterize(field_data_f, r, field = "id", fun="first")

plot(r)

f<-paste0(root_data_out,"/testing")
writeRaster(r, file.path(f, paste0("/test_fasterize")), format="GTiff", overwrite = TRUE)



plot(field_data_f['totalloadingN_g'])

