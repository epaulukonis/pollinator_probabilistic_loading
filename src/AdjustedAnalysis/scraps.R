#what we need to do:

#determine crops to which our pesticides of interest are applied (corn, ww, soy), and 'other'
#determine what the area is in a cumulative sense
#and then what the areas are when we ONLY use the N years designation




##### Total CDL area
#this section of the code outputs a total area by CDL pixel for each year, similar to table 2. 
# list_freq<-list()
# for(f in 1:nlayers(county_r)){
# sq<-as.data.frame(freq(county_r[[f]]))
# sq[4,]<-ifelse(nrow(sq)==4,sq[4,],NA) #this ensures that all dataframes by year have the same # of rows
# sq[3,2]<-ifelse(is.na(sq[3,1]),NA,sq[3,2])
# list_freq[[f]]<-sq
# }
# 
# #this should unlist the frequency set and put it in a data-frame that we can then use to evaluate 'core' crop area
# df_crop_areas<-do.call(cbind, lapply(list_freq, as.data.frame))
# value<-df_crop_areas[,1]
# df_crop_areas = df_crop_areas[,!names(df_crop_areas) == 'value']
# df_crop_areas$value<-value
# df_crop_areas<-df_crop_areas[1:3,c(12,1:11)]
# names(df_crop_areas)<-c("Crop",1999:2009) #make sure names align
# df_crop_areas[,2:12]<-df_crop_areas[,2:12]*900 #get area in meters
# df_crop_areas[is.na(df_crop_areas)] = 0
# df_crop_areas[4,2:12]<-colSums(df_crop_areas[,2:12])
# row.names(df_crop_areas)[4]<-'sum'
# 


#####Options/attempts at clustering
# #kmeans 
# x<-testdf$field
# testy<-kmeans(x, 100, iter.max = 10, nstart = 25)
# testy$cluster
# testdf$cluster<-testy$cluster
# 
# #bins of digits; not so great, large swaths of fields 
# testdf$newcode<-floor(log10(testdf$field)) + 1
# 
# #quantiles; this doesn't make sense to me. I do understand how it's being binned in theory, not sure why the output is restricted in QGIS
# quant<-as.data.frame(quantile(testdf$field, probs = seq(0, 1, 0.01))) 
# 
# #jenks breaks
# getJenksBreaks(var, k, subset = NULL)



#####This removes small singular pixels; at the moment, let's put this here while we evaluate the cumulative area
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



#is this correct? total possible response is n total crop pixels
#prob for 11 years 
success<-0:total_n
plot(success, dbinom(success, size=total_n, prob=.14),type='h')

