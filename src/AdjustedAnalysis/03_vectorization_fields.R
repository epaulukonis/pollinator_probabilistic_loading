### Probabilistic Crop Loading 

### 03 Vectorization of Fields

# Edited by E. Paulukonis Feb 2022
import_start_time <- Sys.time()
print("stepping into 03_vectorization_fields.R")


memory.limit(size=56000)
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
for (county in 1:length(county_set_list)){

county_r<-county_set_list[[county]]
m <- cbind(from = c(-Inf, 2, 6, 24), to = c(0, 4, 23,256), becomes = c(NA, NA, NA, NA)) #keep corn, soy, winter wheat; grassland/pasture is non-crop so do not include

    #reclassify 
    for(layer in 1:nlayers(county_r)){
      county_r[[layer]] <- reclassify(county_r[[layer]], m, right=T)}
      # set_list[[county]] <- county_r}
      
        list_freq<-list()
        for(f in 1:nlayers(county_r)){
        sq<-as.data.frame(freq(county_r[[f]]))
        sq[4,]<-ifelse(nrow(sq)==4,sq[4,],NA) #this ensures that all dataframes by year have the same # of rows
        sq[3,2]<-ifelse(is.na(sq[3,1]),NA,sq[3,2])
        list_freq[[f]]<-sq
        }
        
        #this should unlist the frequency set and put it in a data-frame that we can then use to evaluate 'core' crop area
        df_crop_areas<-do.call(cbind, lapply(list_freq, as.data.frame))
        value<-df_crop_areas[,1]
        df_crop_areas = df_crop_areas[,!names(df_crop_areas) == 'value']
        df_crop_areas$value<-value
        df_crop_areas<-df_crop_areas[1:3,c(12,1:11)]
        names(df_crop_areas)<-c("Crop",1999:2009) #make sure names align
        df_crop_areas[,2:12]<-df_crop_areas[,2:12]*900 #get area in meters
        df_crop_areas[is.na(df_crop_areas)] = 0
        df_crop_areas[4,2:12]<-colSums(df_crop_areas[,2:12])
        row.names(df_crop_areas)[4]<-'sum'
  
}

 





s0 = brick(y)
coords = coordinates(s0) 
s1 = as.data.frame(getValues(s0))
layer.3 = sapply(1:nrow(s1), function(x) paste0(s1[x, ], collapse = ''))
layer.3 <- data.frame(coords, layer.3)

testdf<-layer.3

testdf <- testdf[!grepl("NANANANANANANANANANANA", testdf$layer.3),] # remove pixels that have no crops in 11 years
unique(testdf$layer.3)
testdf$f<-gsub("NA", "", testdf$layer.3) #substitute "" for NA; this will preserve order value, this bins pixels by # years cropped (no differentiation between when)
testdf$field<-as.numeric(testdf$f)

#bins of digits; not so great, large swaths of fields 
testdf$bin<-floor(log10(testdf$field)) + 1




#one way to do this would be to bin by # in pattern 
#kmeans 
x<-testdf$field
testy<-kmeans(x, 100, iter.max = 10, nstart = 25)
testy$cluster
testdf$cluster<-testy$cluster

#bins of digits; not so great, large swaths of fields 
testdf$newcode<-floor(log10(testdf$field)) + 1

#quantiles; this doesn't make sense to me. I do understand how it's being binned in theory, not sure why the output is restricted in QGIS
quant<-as.data.frame(quantile(testdf$field, probs = seq(0, 1, 0.01))) 

#jenks breaks
getJenksBreaks(var, k, subset = NULL)



#Turn back into raster once sequence matching has been done 
testl<-testdf #or test10
testl<-testl[,c(1:2,6)]
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
writeRaster(testls, file.path(cdl_dir, "/test_kmeans_100"), format="GTiff", overwrite = TRUE)


##Issue: we have a lot of tiny pixels! How do we assign them to adjacent pixels?




