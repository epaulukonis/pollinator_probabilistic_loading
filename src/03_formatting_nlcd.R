### Probabilistic Crop Loading 

### 03 Formatting NLCD data

# Edited by E. Paulukonis Dec 2021


#### read in or create reclassified NLCD files: ----
#check that study area and state boundary plot properly
# state_d<-aggregate(state)
# plot(state_d)
# plot(study, add=T)

nlcd_past_filename<-paste0(nlcd_dir,"/NLCD_81_2001.tif") # check that the files haven't already been made
nlcd_filename<-paste0(nlcd_dir,"/nlcd_data_f.tif") 

if(file.exists(nlcd_filename)&&
   file.exists(nlcd_past_filename))
{
  #read in reclassed 81/82 nlcd data
  print('files do exist, read them in')
  print(list.files(path=nlcd_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  nlcd_data_rec <- file.path(nlcd_dir, list.files(path=nlcd_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
  nlcd_data_rec<-lapply(nlcd_data_rec, raster) #create list of cdl rasters 
  nlcd_81<-nlcd_data_rec[1:8]
  nlcd_82<-nlcd_data_rec[9:16]
  
  #read in stack of OG masked and cropped NLCD
  NLCD<-stack(paste0(nlcd_dir,"/nlcd_data_f.tif")) 
  years<-c(2001, 2004, 2006, 2008, 2011, 2013,2016, 2019)
  names(NLCD)<-years
  
}else{
print('files have not been made; make them')
print(list.files(path=nlcd_dir, pattern='.tiff$', all.files=TRUE, full.names=FALSE))
nlcd_data <- file.path(nlcd_dir, list.files(path=nlcd_dir, pattern='.tiff$', all.files=TRUE, full.names=FALSE))
nlcd_data<-lapply(nlcd_data, raster) #create list of cdl rasters 
nlcd_data_f<-lapply(nlcd_data, mask_crop) #use function from 02 to mask and crop
plot(nlcd_data_f[[1]]) #plot to make sure
NLCD<-stack(nlcd_data_f)
writeRaster(NLCD,paste0(nlcd_dir, "/nlcd_data_f.tif"), format="GTiff") # write masked and crop NLCD to raster stack

#create function to reclass and write to raster file; takes a minute
reclass_nlcd<-function(x)
{  
  name_x<-names(x)
  r_name<-substr(name_x, 6, nchar(name_x)-45)
  NLCD82<-x
  values(NLCD82)[values(NLCD82)!=82]<-0
  values(NLCD82)[values(NLCD82)==82]<-1
  NLCD81<-x
  values(NLCD81)[values(NLCD81)!=81]<-0
  values(NLCD81)[values(NLCD81)==81]<-1
  writeRaster(NLCD81, paste0(nlcd_dir,"/NLCD_81_",r_name,".tif"), format="GTiff", overwrite=T)
  writeRaster(NLCD82, paste0(nlcd_dir,"/NLCD_82_",r_name,".tif"), format="GTiff", overwrite=T)
}

#get output
lapply(nlcd_data, reclass_nlcd)
print(list.files(path=nlcd_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
nlcd_data_rec <- file.path(nlcd_dir, list.files(path=nlcd_dir, pattern='.tif$', all.files=TRUE, full.names=FALSE))
nlcd_data_rec<-lapply(nlcd_data_rec, raster) #create list of cdl rasters 
nlcd_81<-nlcd_data_rec[1:8]
nlcd_82<-nlcd_data_rec[9:16]
}

#### Read in and reclassify with the accuracy data ----
# split accuracy data to nlcd years as follows:
# 2001: 2001, 2004, 
# 2006: 2006, 2008
# 2011: 2011, 2013
# 2016: 2016, 2019

acc_data<-read.csv(paste0(nlcd_dir_acc, "/NLCD_Acc_Fin.csv")) #compiled accuracy data
acc_data_l<-split(acc_data, f = acc_data$Year)   #split by year

#### create conditional probability raster function
reclass_set <-function(n,y) {
  
    accuracy<-as.data.frame(acc_data_l[y]) #y is 1 of the 4 years for which accuracy data exists
    accuracy<-split(accuracy, f= accuracy[,2]) #split into list for each individual portion 
    
    #create loop to reclassify by each part of the accuracy dataset for this year
    nlcd_list<-list()
    for(i in 1:length(accuracy)){
      names_c<-c("crop","notcrop","notpast","past") # alphabetical
      
      df<-as.data.frame(accuracy[[i]])
      acc_mat<-as.matrix(df[1:16,3:5]) #create matrix for each crop/notcrop/pasture/notpasture section
      
      out<-reclassify(n, acc_mat, right=T) # n represents the index containing the NLCD raster to be reclassified
      names(out)<-paste0("NLCD_",names_c[i])
      nlcd_list[[i]]<-out #output as part of list
    }
    
    NLCD_crop_acc<-stack(nlcd_list[[1]], nlcd_list[[2]]) # crop, not crop
    NLCD_past_acc<-stack(nlcd_list[[4]], nlcd_list[[3]]) # pasture, not pasture
    y<-names(n)
    
    #write to stacked rasterfile
    writeRaster(NLCD_crop_acc, paste0(nlcd_dir,"/output/NLCD_crop_acc_stack_", y ,".tif"), format="GTiff", overwrite=T)
    writeRaster(NLCD_past_acc, paste0(nlcd_dir,"/output/NLCD_past_acc_stack_", y ,".tif"), format="GTiff", overwrite=T)
}


nlcd_acc_filename<-paste0(nlcd_dir,"/output/NLCD_crop_acc_stack_X2019.tif") #did we already run the analysis? 
if(file.exists(nlcd_acc_filename))
{
  print ('files exist, read them in')
  print(list.files(path=paste0(nlcd_dir,"/output"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  nlcd_data_acc <- file.path(paste0(nlcd_dir,"/output"), list.files(path=paste0(nlcd_dir,"/output"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
  nlcd_data_acc<-lapply(nlcd_data_acc, stack) #create stack of all reclassified accuracy NLCD
  
  }else{
    print('files do not exist, run function')
    for (i in 1:length(acc_data_l)){
      x = i*2
      reclass_set(NLCD[[x-1]], i)
      reclass_set(NLCD[[x]], i)
    }
    print(list.files(path=paste0(nlcd_dir,"/output"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
    nlcd_data_acc <- file.path(paste0(nlcd_dir,"/output"), list.files(path=paste0(nlcd_dir,"/output"), pattern='.tif$', all.files=TRUE, full.names=FALSE))
    nlcd_data_acc<-lapply(nlcd_data_acc, stack) #create lstack of all reclassified accuracy NLCD
}





###Find the area of 81 and 82 by year----

NLCD_Area <- data.frame(matrix(ncol = 2, nrow = 8))
colnames(NLCD_Area)<-c("year","area_total")


for (y in 1:length(nlcd_81)){
  
NLCD81<-nlcd_81[[y]]
NLCD82<-nlcd_82[[y]]

area81<-NLCD81*900
area81<-cellStats(area81, sum, na.rm=T)
area81<-area81*0.000247105

area82<-NLCD82*900
area82<-cellStats(area82, sum, na.rm=T)
area82<-area82*0.000247105

name_x<-names(NLCD81)
year<-substr(name_x, 9, nchar(name_x))

NLCD_Area[y,1] <- year
NLCD_Area[y,2]<-area81+area82
}

write.csv(NLCD_Area,paste0(nlcd_dir,"/NLCD_Area.csv"))


