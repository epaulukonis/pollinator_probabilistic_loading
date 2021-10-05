### Probabilistic Crop Loading 

###01 Formatting CDL data

#Edited by E. Paulukonis Sept 2021

import_start_time <- Sys.time()
print("stepping into 02_formatting_cdl.R")

print(list.files(path=cdl_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
cdl_data <- file.path(cdl_dir, 
                               list.files(path=cdl_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))

cdl_data<-lapply(cdl_data, raster) #create list of cdl rasters 


#there are 3 different extents; 2020-2008, 2007-2006, and 2005-1999
# first expand extent 
ext<-extent(cdl_data[[22]])
cdl_data<-lapply(cdl_data, function(x) setExtent(x, ext))

#set parameters for memory to lower value
rasterOptions(memfrac=.3)

#sget 2005-1999
set_1<-cdl_data[c(1:7)]
#get 2006-2007
set_2<-cdl_data[c(8:9)]
#get rest
cdl_base<-cdl_data[c(10:22)]

r1<-cdl_data[[22]]
out_1<-lapply(set_1, function(file){
  projectRaster(file, r1, method='ngb',crs(r1))
})

out_2<-lapply(set_2, function(file){
  projectRaster(file, r1, method='ngb',crs(r1))
})

cdl_out_final<-stack(out_1, out_2, cdl_base)
pb<-progress_bar$new(total=560) #set up a progress bar
pb$tick(0) #start the progress bar at 0


writeRaster(cdl_out, filename = file.path(cdl_dir, "cdl_final.tif"), bylayer=TRUE, format="GTiff")

# cdl_stk<-file.path(cdl_dir, 
#                    list.files(path=cdl_dir, pattern='final.tif$', all.files=TRUE, full.names=FALSE))


#cdl_data[[1]]<-projectRaster(cdl_data[[1]], cdl_data[[22]], method='ngb', crs(cdl_data[[22]]))


print("read in cdl rasters for all years into a list")

