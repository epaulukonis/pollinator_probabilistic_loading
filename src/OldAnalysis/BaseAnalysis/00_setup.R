### Probabilistic Crop Loading 

### 00 Setup of directories and file paths

# Edited by E. Paulukonis June 2022

#Install and load supporting libraries.
print("stepping into 00_setup.R")
print(Sys.info()[4])
R.Version()$version.string

# install.packages(c("ggplot2", "rgeos","sf","raster","dplyr","ggplot2","cowplot",
#                    "grid","foreign","progress","parallel","foreach", "gridExtra","stringr"))

library(sp)
library(sf)
library(stars)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(cowplot)
library(grid)
library(foreign)
library(progress)
library(parallel)
library(foreach)
library(gridExtra)
library(stringr)
library(smoothr)
library(exactextractr)

?freq
#library(TraMineR)
# library(GrpString)
# library(geosphere)
# library(ggridges)
# library(ghibli)
# library(tidyverse)
# library(progress)

who_is_running<-'eap'
#who_is_running<-'stp'
if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  root_dir <- file.path("c:", "git", "pollinator_probabilistic_loading")
}else if (Sys.info()[4]=="LZ26EPAULUKO"){
  root_dir <- 'C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/pollinator_probabilistic_loading'
}else{
  root_dir <- file.path("/work", "HONEYBEE", who_is_running, "pollinator_probabilistic_loading")
}
print(root_dir)

memory.limit(size=56000)

root_data_in <- file.path(root_dir, "data_in")
print(root_data_in)
root_data_out <- file.path(root_dir, "data_out")
print(root_data_out)
root_figures <- file.path(root_dir, "figures")
print(root_figures)
root_src <- file.path(root_dir, "src/AdjustedAnalysis")
print(root_src)

# set data directories, check to see what files are in each, and unzip if needed - uncomment if needed

# create function to unzip files if needed
# unzip_function<-function(file_exists, zip_file, directory){
#   if(!file_exists){unzip(zip_file, exdir=directory)}
# }

# nlcd
nlcd_dir = file.path(root_data_in, "MapData/NLCD")
print(nlcd_dir)
print(list.files(path=nlcd_dir, all.files=TRUE, full.names=FALSE))
nlcd_dir_acc = file.path(nlcd_dir, "accuracy")
print(nlcd_dir_acc)
print(list.files(path=nlcd_dir_acc, all.files=TRUE, full.names=FALSE))
# nlcd_zip<-list.files(path=nlcd_dir, pattern='.zip', all.files=TRUE, full.names=FALSE)
# print(nlcd_zip)
# zip_ex<- as.list(c(file.exists(file.path(nlcd_dir, nlcd_zip))))
# for (l in length(zip_ex)){
#   unzip_function(zip_ex[[l]], nlcd_zip, nlcd_dir)
# }
#
# cdl
cdl_dir = file.path(root_data_in, "MapData/CDL")
print(cdl_dir)
print(list.files(path=cdl_dir, all.files=TRUE, full.names=FALSE))


# field
field_dir = file.path(root_data_in, "MapData/Fields")
print(field_dir)
print(list.files(path=field_dir, all.files=TRUE, full.names=FALSE))



#Illinois specific
cdl_ill_dir= file.path(cdl_dir, "Illinois")
print(cdl_ill_dir)
print(list.files(path=cdl_ill_dir, all.files=TRUE, full.names=FALSE))

cdl_acc_dir = file.path(cdl_dir, "Illinois/cdl_acc")
print(cdl_acc_dir)
print(list.files(path=cdl_acc_dir, all.files=TRUE, full.names=FALSE))

cdl_dir_fin = file.path(cdl_dir, "Illinois/fixed_cdl")
print(cdl_dir_fin)
print(list.files(path=cdl_dir_fin, all.files=TRUE, full.names=FALSE))

cdl_dir_rec = file.path(cdl_dir, "Illinois/reclass_cdl/reclass_old_cdl")
print(cdl_dir_rec)
print(list.files(path=cdl_dir_rec, all.files=TRUE, full.names=FALSE))

cdl_dir_rec_n = file.path(cdl_dir, "Illinois/reclass_cdl/reclass_new_cdl")
print(cdl_dir_rec_n)
print(list.files(path=cdl_dir_rec_n, all.files=TRUE, full.names=FALSE))

cdl_dir_adj = file.path(cdl_dir, "Illinois/reclass_cdl/reclass_new_cdl/adjusted")
print(cdl_dir_adj)
print(list.files(path=cdl_dir_adj, all.files=TRUE, full.names=FALSE))


#Multi-state specific
cdl_ms_dir = file.path(cdl_dir, "Multi-State")
print(cdl_ms_dir)
print(list.files(path=cdl_ms_dir, all.files=TRUE, full.names=FALSE))


# cdl_zip<-list.files(path=cdl_dir, pattern='.zip', all.files=TRUE, full.names=FALSE)
# print(cdl_zip)
# zip_ex <- as.list(c(file.exists(file.path(cdl_dir, cdl_zip))))
# for (l in length(zip_ex)){
#   unzip_function(zip_ex[[l]], cdl_zip, cdl_dir)
# }
#
#
#coa
coa_dir = file.path(root_data_in, "CropData/CoA/all_CoA")
print(coa_dir)
print(list.files(path=coa_dir, all.files=TRUE, full.names=FALSE))

# coa_zip<-list.files(path=coa_dir, pattern='.zip', all.files=TRUE, full.names=FALSE)
# print(coa_zip)
# # zip_ex <- as.list(c(file.exists(file.path(coa_dir, coa_zip))))
# # for (l in length(zip_ex)){
# #   unzip_function(zip_ex[[l]], coa_zip, coa_dir)
# # }
# #
#
# caps
caps_dir = file.path(root_data_in, "CropData/CAPS")
print(caps_dir)
print(list.files(path=caps_dir, all.files=TRUE, full.names=FALSE))
# caps_zip<-list.files(path=caps_dir, pattern='.zip', all.files=TRUE, full.names=FALSE)
# print(caps_zip)
# # zip_ex <- as.list(c(file.exists(file.path(caps_dir, caps_zip))))
# # for (l in length(zip_ex)){
# #   unzip_function(zip_ex[[l]], caps_zip, caps_dir)
# # }
#
#
# pnsp
pnsp_dir = file.path(root_data_in, "PesticideData/PNSP")
print(pnsp_dir)
print(list.files(path=pnsp_dir, all.files=TRUE, full.names=FALSE))
# pnsp_zip<-list.files(path=pnsp_dir, pattern='.zip', all.files=TRUE, full.names=FALSE)
# print(pnsp_zip)
# # zip_ex <- as.list(c(file.exists(file.path(pnsp_dir, pnsp_zip))))
# # for (l in length(zip_ex)){
# #   unzip_function(zip_ex[[l]], pnsp_zip, pnsp_dir)
# # }
#
#
#
bombus_dir = file.path(root_data_in, "bombus")
print(bombus_dir)
print(list.files(path=bombus_dir, all.files=TRUE, full.names=FALSE))
# bombus_zip<-list.files(path=bombus_dir, pattern='.zip', all.files=TRUE, full.names=FALSE)
# print(bombus_zip)
# zip_ex <- as.list(c(file.exists(file.path(bombus_dir, bombus_zip))))
# for (l in length(zip_ex)){
#   unzip_function(zip_ex[[l]], bombus_zip, bombus_dir)
# }
#
state_dir = file.path(root_data_in, "MapData/state")
print(state_dir)
print(list.files(path=state_dir, all.files=TRUE, full.names=FALSE))
# state_zip<-list.files(path=state_dir, pattern='.zip', all.files=TRUE, full.names=FALSE)
# print(state_zip)
# zip_ex <- as.list(c(file.exists(file.path(state_dir, state_zip))))
# for (l in length(zip_ex)){
#   unzip_function(zip_ex[[l]], state_zip, state_dir)
# }



#source other files
# source(file.path(root_src, "01_studyarea.R"))
source(file.path(root_src, "02_formatting_cdl.R"))
source(file.path(root_src, "03_formatting_cdl.R"))
source(file.path(root_src, "04_CPAA_delineation.R"))


