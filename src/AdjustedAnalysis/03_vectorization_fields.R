### Probabilistic Crop Loading 

### 03 Vectorization of Fields

# Edited by E. Paulukonis Feb 2022
import_start_time <- Sys.time()
print("stepping into 03_vectorization_fields.R")


memory.limit(size=56000)
study$county_names<-names(study)  #add county names to the study area so we can run the crop function of the CDL to each county


#function to mask and crop raster cdl to each county
county_set_list<-list()
for (co in 1:length(study)){
  co_r<-study[study$county_names == study$county_names[co],]
  
  mask_crop<-function(x){
    r_list<-crop(x, co_r)
    mask(r_list, co_r)
  }
  
  county_set<-lapply(cdl_data_ill_rec[c(1:11)], mask_crop) #crop and mask the fixed CDL to the counties, put in list
  county_set_list[[co]]<-stack(county_set)
  
}


set1<-county_set_list[[1]] #let's get a single county stack, and reclassify it
m <- cbind(from = c(-Inf, 2, 6, 177), to = c(0, 4, 175,256), becomes = c(NA)) #keep corn, soy, grassland/pasture

for(layer in 1:11){
  set1[[layer]] <- reclassify(set1[[layer]], m, right=T)
}


plot(set1[[1]]) #check new reclassified raster
y<-set1 #this is a reclassified single county stack that we will use to test the concatenation process

#y <- stack(reclassify(set1, cbind(-Inf, 0, NA), right=TRUE))


s0 = brick(y)
coords = coordinates(s0) 
s1 = as.data.frame(getValues(s0))
layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
layer.3 <- data.frame(coords, layer.3)

testdf<-layer.3

testdf <- testdf[!grepl("NANANANANANANANANANANA", testdf$layer.3),] # remove pixels that have no crops in 11 years
unique(testl$layer.3)

testdf$f<-gsub("NA", "0", testdf$layer.3) #substitute 0 for NA; this will preserve order value, as 0s before integers collapse and zeros after stay
testdf$field<-as.numeric(testdf$f)

#test10<-testdf[testdf$field >= 10000000000,]
# plot(test10)

unique(testdf$layer.3) #look at unique values
unique(testdf$field)

#testl$layer.3 <- factor(testl$layer.3, levels = unique(testl$layer.3))


testl<-test10 #or test10
testl<-testl[,c(1:2,5)]
coordinates(testl) <- ~ x + y
gridded(testl) <- TRUE
testl<- raster(testl)

crs(testl) <- crs(set1)
plot(testl)



#first; let's remove isolated islands of corn, soy or grassland pixels that are likely not part of fields 
rc <- clump(testl, directions = 8) #this evaluates clumps of pixels (nearest neighbor =  8)
#plot(rc)
#writeRaster(rc, file.path(cdl_dir, "/rc"), format="GTiff", overwrite = TRUE)
# get frequency table    
f<-freq(rc)
# save frequency table as data frame
f<-as.data.frame(f)

# which rows of the data.frame are only represented by clumps = 1 pixels?
excludeID <- f$value[which(f$count <= 1)]
# make a new raster to be sieved
formaskSieve <- rc
# assign NA to all clumps whose IDs are found in excludeID
formaskSieve[rc %in% excludeID] <- NA

testls<-mask(testl, formaskSieve)
plot(testls)

#solution; let's do quantiles? So bin things according to class? Does that remove some pixels we don't want removed?
#q quantiles (83? 100?)
#idea is that for some pixel value 11155 and 11555, we assign the same value; what this does is simply tell us that 
#that field existed for a total of 6 years out of the 11, and 



writeRaster(set1[[11]], file.path(cdl_dir, "/cdl2009"), format="GTiff", overwrite = TRUE)
writeRaster(formaskSieve, file.path(cdl_dir, "/test_no_10"), format="GTiff", overwrite = TRUE)


##Issue: we have a lot of tiny pixels! How do we assign them to adjacent pixels?




