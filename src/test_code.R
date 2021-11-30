

print(list.files(path=state_dir, all.files=TRUE, full.names=FALSE))
print(list.files(path=bombus_dir, all.files=TRUE, full.names=FALSE))
shape<-readOGR(state_dir, layer = "IL_BNDY_County_Py")
plot(shape)
shape

bomb_h <- readOGR(bombus_dir, layer = "RPBB_High_Potential_Zones_03172021")
bomb_l <- readOGR(bombus_dir, layer = "RPBB_Low_Potential_Zones_03172021")
plot(bomb_l)

shape <- rea
plot(bomb_h, add=T, col='red')

shape<-spTransform(shape, crs(bomb_l))

plot(shape)
plot(bomb_l, add=T, col='blue')
plot(bomb_h, add=T, col='red')


# start with peoria county
il.peoria<-shape[shape$COUNTY_NAM == "PEORIA",]
plot(il.peoria)
plot(bomb_l, add=T)


# scraps 02 for counties
# line 65
# when you get to the point where we'll do it for all counties, follow this:
# county_names<-state$COUNTY_NAM
# cdl_fin_co<-list()
# mask_crop_cdl<-function(cdl){
#   for (c in 1:length(county_names)){
#   county<-state[state$COUNTY_NAM == county_names[c],]
#   county_c<-crop(cdl, county)
#   cdl_fin_co[c]<-mask(county_c, county)
# }}
# you'll have a nested list of stacked rasters by county


# line 202
#when it's time to loop this over counties, try this:
# get_regional_crops<-function(county){
# out<-list()
# for (i in 1:length(county)){
#   out[[i]]<-sort(unique(values(county[[i]])))
# }}
# values_by_county<-lapply(cdl_fin_co_y, get_regional_crops)

#line 228 (function over all)
#when time comes to test over counties, try this out:
# foreach(county = length(cdl_fin_co_y)) %do% {
#   lapply(cdl_fin_co_y[[county]], reclassify_cdl, mc.cores=numCores)
# }



# scraps 02 general

#We need to combine double crops and adjust the codes for crops as needed
#note that this may be dependent on location; look to the compiled Accuracy data-set to learn more

#the new attribute codes for the CDL crops will depend on the region and county; therefore, we create code to semi-automatically reclassify
#old_crops<-codes
# new_code<-1:length(old_crops)
# old_non_dbl <- crop_list_fin[!grepl("Dbl", crop_list_fin$Cover_Type), ]
# dbl <- crop_list_fin[grepl("Dbl", crop_list_fin$Cover_Type), ]
# 

# for(y in 2008:2020){
#   for(c in codes){
#     rast<-stack(paste0(cdl_dir_rec, "/CDL", co, "_",y,"_",c,"_stack.tif"))
#     val<-df$new[df$old==c]
#     fl<-paste0(co,"_",2008,"_",val,"_stack.tif")
#     writeRaster(rast, paste(cdl_dir_adj, fl, sep="/"), format="GTiff", overwrite=T)
#   }
# }


# test_1<-stack(paste0(cdl_dir_rec, "/cdl", "_",2008,"_",1,"_stack.tif"))
# plot(test_1)
# 
# test_30<-stack(paste0(cdl_dir_rec, "/cdl", "_",2017,"_",30,"_stack.tif"))
# plot(test_30)


#Re-classify the rasters which needed recombination
#Dbl crop rasters are NOT mutually exclusive (e.g. dbl crop lettuce/durum wht is included in both lettuce and durum wheat)

#Corn: New=1, Old=1, 225, 226, 228, 237, 241
# for(k in 2013:2017){
#   layer1<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_1_stack.tif"))
#   layer2<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_225_stack.tif"))
#   layer3<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_226_stack.tif"))
#   layer4<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_228_stack.tif"))
#   layer5<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_237_stack.tif"))
#   layer6<-stack(paste0("./", co, "/CDL_Acc/", co, "_", k, "_241_stack.tif"))
#   pres<-max(layer1[[1]], layer2[[1]], layer3[[1]], layer4[[1]], layer5[[1]], layer6[[1]], na.rm=T)
#   acc<-max(layer1[[2]], layer2[[2]], layer3[[2]], layer4[[2]], layer5[[2]], layer6[[2]], na.rm=T)
#   err<-max(layer1[[3]], layer2[[3]], layer3[[3]], layer4[[3]], layer5[[3]], layer6[[3]], na.rm=T)
#   rast<-stack(pres, acc, err)
#   fl<-paste0(co,"_",k,"_1_stack.tif")
#   writeRaster(rast, paste(wd, fl, sep="/"), format="GTiff", overwrite=T)
# }


#test locally here: for Tom
#test out on a list of 2
# cdl_fin_co_t<-cdl_fin_co[10:11]
# reclassify_cdl<-function(cdl_data){
#   for(c in codes){
#     for(y in 2008:2009){
#       cdl <- cdl_data #get the CDL raster by year
#       values(cdl)[values(cdl)!=c]<-0 #set any values that are not crop to 0
#       acc<-as.matrix(cdl_acc%>%select("Attribute_Code",paste0(y))) #get the accuracy data for year y
#       err<-as.matrix(cdl_err%>%select("Attribute_Code",paste0(y))) #get the error data for year y
#       file_a<-reclassify(cdl, acc, right=NA) #reclassify the crop raster, so the cells are set to the crop's accuracy value
#       file_e<-reclassify(cdl, err, right=NA) #reclassify the crop raster, so the cells are set to the crop's error value
#       acc_stack<-stack(cdl, file_a, file_e) #make a stack
#       fl<-paste0(co, "_",y,"_",c,"_stack.tif") #set up the new file name, based on y and c
#       writeRaster(acc_stack,  paste(cdl_dir_rec,"/",fl, sep=""), format="GTiff", overwrite=T) #save the raster stack
#     }}}
# 
# 
# mclapply(cdl_fin_co_t, reclassify_cdl, mc.cores=numCores)
