### Probabilistic Crop Loading

### 00 Setup of directories and file paths

# Edited by E. Paulukonis October 2022

rm(list=ls())
gc()

#Install and load supporting libraries.
print("stepping into 00_setup.R")
print(Sys.info()[4])
# R.Version()$version.string
# # 
# install.packages(c("ggplot2","sf","raster","dplyr","ggplot2","cowplot",
#                    "grid","foreign","progress","parallel","foreach", "gridExtra","stringr", "stars", "rgdal","terra","smoothr",
#                    "exactextractr", "data.table", "gtools", "gstat", "fasterize", "purrr", "tidyverse", "ggridges"))


library(sf)
library(stars)
library(terra)
library(raster)
library(stars)
library(abind)
library(dplyr)
library(ggplot2)
library(cowplot)
library(foreign)
library(parallel)
library(stringr)
library(exactextractr)
library(data.table)
library(gtools)
library(gstat)
library(fasterize) 
library(purrr)
library(stringdist)
library(cowplot)
library(plotly)
library(ggpubr)
#library(rmapshaper)
library(tidyverse)
library(ggridges)
library(spData)
library(scales)


# install.packages("tmap", repos = c("https://r-tmap.r-universe.dev",
#                                  "https://cloud.r-project.org"))



#library(NCmisc) #this package lets me look at see what I'm using


who_is_running<-'eap'
#who_is_running<-'stp'
if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  root_dir <- file.path("c:", "git", "pollinator_probabilistic_loading")
}else if(Sys.info()[4]=="LZ26TPURUCKE-2"){
  root_dir <- file.path("c:", "Users", "tpurucke", "git", "pollinator_probabilistic_loading")
  PWC_dir <-  file.path("c:", "Users", "tpurucke", "git", "pollinator_probabilistic_loading", "data_in", "PWCdata", "pesticide runs")
}else if(Sys.info()[4]=="LZ26EPAULUKO-2"){
  root_dir <- 'C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/pollinator_probabilistic_loading'
  PWC_dir<-"C:/Users/EPAULUKO/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/apps/pwc_2.001/pesticide runs"
}else{
  root_dir <- file.path("/work", "HONEYBEE", who_is_running, "pollinator_probabilistic_loading")
}
print(root_dir)

# memory.limit() is no longert supported and handled by R internally
# memory.limit()
# memory.limit(size=56000)

root_data_in <- file.path(root_dir, "data_in")
print(root_data_in)
root_data_out <- file.path(root_dir, "data_out")
print(root_data_out)
root_figures <- file.path(root_dir, "figures")
print(root_figures)
root_figures_parameters <- file.path(root_dir, "figures", "parameters")
print(root_figures_parameters)
root_src <- file.path(root_dir, "src/ForagingAnalysis")
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

#All states
cdl_dir_all = file.path(root_data_in, "MapData/CDL/BombusAnalysis")
print(cdl_dir)
print(list.files(path=cdl_dir, all.files=TRUE, full.names=FALSE))

#Illinois
cdl_dir_adj = file.path(cdl_dir, "Illinois/reclass_cdl/reclass_new_cdl/adjusted")
print(cdl_dir_adj)
print(list.files(path=cdl_dir_adj, all.files=TRUE, full.names=FALSE))



#coa
coa_dir = file.path(root_data_in, "CropData/CoA")
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


pest_dir = file.path(root_data_in, "PesticideData")
print(pest_dir)
print(list.files(path=pest_dir, all.files=TRUE, full.names=FALSE))



#run  all code on hpc here
# source(file.path(root_src, "01_formatting_spatial_data_f.R"))
# source(file.path(root_src, "02a_onsite_model_f.R"))
# source(file.path(root_src, "02b_offsite_model_f.R"))
# source(file.path(root_src, "03_merging_data_f.R"))
# source(file.path(root_src, "04_rasterize_dataset_f.R"))


#this allows me to see which packages I actually need to run the line of scripts on the hpc
# list.functions.in.file(paste0(root_src, "/01_formatting_spatial_data_f.R"), alphabetic = TRUE)
# list.functions.in.file(paste0(root_src, "/02a_onsite_model_f.R"), alphabetic = TRUE)
# list.functions.in.file(paste0(root_src, "/02b_offsite_model_f.R"), alphabetic = TRUE)
# list.functions.in.file(paste0(root_src, "/03_merging_data_f.R"), alphabetic = TRUE)
# list.functions.in.file(paste0(root_src, "/04_rasterize_dataset_f.R"), alphabetic = TRUE)

