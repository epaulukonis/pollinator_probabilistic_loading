### Probabilistic Crop Loading 

### 03 Vectorization of Fields

# Edited by E. Paulukonis Feb 2022
import_start_time <- Sys.time()
print("stepping into 03_vectorization_fields.R")



study$county_names<-names(study)  #add county names to the study area so we can run the crop function of the CDL to each county

#function to mask and crop CDL to each county
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



set_list<-list()



#convert the layers to binary; we will use this first
is_n <- c(0,1,2:4,5,6:23,24,25:256)
becomes_n <- c(NA,1,rep.int(0,3),1,rep.int(0,18),1,rep.int(0,232))
n<-cbind(is_n,becomes_n)

#convert 1,5,24 (corn,soy,winter wheat) to 1,2,3, all other crop to 9
is <- c(0,1,2:4,5,6:23,24,25:256)
becomes <- c(NA,1,rep.int(9,3),2,rep.int(9,18),3,rep.int(9,232))
m<-cbind(is,becomes)




for (county in 1:length(county_set_list)){
  county_r<-county_set_list[[county]]

    for(layer in 1:nlayers(county_r)){
      county_r[[layer]] <- reclassify(county_r[[layer]], n)
    }
  set_list[[layer]]<-county_r

  #problem; individual layers work, but the loop doesn't; I think I am screwing up the process somehow
  #fix on Tuesday


}



unique(values(set_list_binary[[1]][[1]]))


y<-set_list[[1]] #get first county
s0 = brick(y)
coords = coordinates(s0) 
s1 = as.data.frame(getValues(s0))
layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
layer.3 <- data.frame(coords, layer.3)

testdf<-layer.3
testdf <- testdf[!grepl("NANANANANANANANANANANA", testdf$layer.3),] # remove pixels that have no crops in 11 years
unique(testdf$layer.3)
testdf$f<-gsub("NA", "", testdf$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
testdf$field<-as.numeric(testdf$f) #turn f into fields as numeric integer

testdf$bin<-floor(log10(testdf$field)) + 1  #create column to count the number of years


#Cumulative area raster
testl<-testdf
testl<-testl[,c(1:2,6)]
coordinates(testl) <- ~ x + y
gridded(testl) <- TRUE
testl<- raster(testl)
crs(testl) <- crs(cdl_data_ill_rec[[1]])
plot(testl)


sq<-as.data.frame(freq(testl)) 

cumulative<-sum(sq[1:11,2])



#solution; let's do quantiles? So bin things according to class? Does that remove some pixels we don't want removed?
#q quantiles (83? 100?)
#idea is that for some pixel value 11155 and 11555, we assign the same value; what this does is simply tell us that 
#that field existed for a total of 6 years out of the 11, and 



writeRaster(set1[[11]], file.path(cdl_dir, "/cdl2009"), format="GTiff", overwrite = TRUE)
writeRaster(testl, file.path(cdl_dir, "/test_"), format="GTiff", overwrite = TRUE)


##Issue: we have a lot of tiny pixels! How do we assign them to adjacent pixels?




