### B. affinis analysis

### 01 Read CDL data

# Edited by E. Paulukonis Sept 2021

import_start_time <- Sys.time()
print("stepping into 01_read_cdl_b.R")

if(file.exists(paste0(cdl_dir_all,"/CDL/raw_CDL.RData"))){ 
 raw_CDL_all<- readRDS(file=paste0(cdl_dir_all,"/CDL/raw_CDL.RData"))
 
 CDL<-lapply(raw_CDL_all, stack)
 names(CDL)<-substring(names(CDL),5,6 )
 rm(raw_CDL_all)
  
  } else if (!file.exists(paste0(cdl_dir_all,"/CDL/raw_CDL.RData"))){
   common_path = "C:/Users/EPAULUKO/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/Annies-Crop-Downloader/zfiles/"
   primary_dirs = list.files(common_path);
   primary_dirs #all folders

    #this code inputs the file paths for all the individual unzipped tif files, puts them in big list
    inputdata<-list()
    for(file in 2:length(primary_dirs)) {
      inDir<-primary_dirs[file]
      cdl <- file.path(paste(common_path,inDir,sep = ""), list.files(path=paste(common_path,inDir,sep = ""), pattern='.tif$', all.files=TRUE, full.names=FALSE))
      cdl <- setNames(raster(cdl), tools::file_path_sans_ext(basename(cdl)))
        inputdata[[file]]<-cdl
      }

    #CT had to be done separately, got corrupted
    inDir<-primary_dirs[1]
    cdl <- file.path(paste(common_path,inDir,sep = ""), list.files(path=paste(common_path,inDir,sep = ""), pattern='.tif$', all.files=TRUE, full.names=FALSE))
    CT<- setNames(lapply(cdl, raster), tools::file_path_sans_ext(basename(cdl)))

    #combine CT and the rest, and change the names to represent FIPS code
    inputdata_f<-c(inputdata,CT)
    for(i in 1:length(inputdata_f)){
    names(inputdata_f)[i]<-paste0("CDL","_",as.character(substring(names(inputdata_f[[i]]),10,12 )))
    }
   
    names(inputdata_f)[393:406]<-"CDL_09"
    final_cdl_datasets<-split(inputdata_f,names(inputdata_f))
    saveRDS(final_cdl_datasets, file=paste0(cdl_dir,"/CDL/raw_CDL.RData"))
    
  }else{
      
      #make sure to run python script to get data from NASS, but CROPSCAPE has issues with 2020 data
      #this code unzips everything but be warned, you may need to manually download some zips
      root_cdl_dir <- 'C:/Users/EPAULUKO/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/Annies-Crop-Downloader'
      indat<-list.dirs(path = paste0(root_cdl_dir, "/data"), full.names = TRUE, recursive = T)
      indat<-indat[3:30] #subset to the folders containing the data 
      exDir<-paste0(root_cdl_dir,"/zfiles")
      for(folder in 1:length(indat)){
        inDir<-indat[folder]
      file_names <- list.files(inDir)
       walk( file_names, ~ unzip(zipfile = str_c(inDir,"/", .x), 
                                exdir = str_c(exDir,"/", .x)))
      }
      
       }
