### Probabilistic Crop Loading 

###00 Setup of directories and file paths

#Edited by E. Paulukonis Sept 2021

#Install and load supporting libraries.
print("stepping into 00_setup.R")
print(Sys.info()[4])
R.Version()$version.string

library(sp)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(cowplot)
library(grid)
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
  root_dir <- 'C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/"pollinator_probabilistic_loading"'
}else{
  root_dir <- file.path("/work", "HONEYBEE", who_is_running, "pollinator_probabilistic_loading")
}
print(root_dir)

root_data_in <- file.path(root_dir, "data_in")
print(root_data_in)
root_data_out <- file.path(root_dir, "data_out")
print(root_data_out)
root_figures <- file.path(root_dir, "figures")
print(root_figures)
root_src <- file.path(root_dir, "src")
print(root_src)

# set data directories, check to see what files are in each, and unzip if needed - uncomment if needed

# create function to unzip files if needed
# unzip_function<-function(file_exists, zip_file, directory){
#   if(!file_exists){unzip(zip_file, exdir=directory)}
# }

# nlcd
nlcd_dir = file.path(root_data_in, "NLCD")
print(list.files(path=nlcd_dir, all.files=TRUE, full.names=FALSE))
# nlcd_zip<-list.files(path=nlcd_dir, pattern='.zip', all.files=TRUE, full.names=FALSE)
# print(nlcd_zip)
# zip_ex<- as.list(c(file.exists(file.path(nlcd_dir, nlcd_zip))))
# for (l in length(zip_ex)){
#   unzip_function(zip_ex[[l]], nlcd_zip, nlcd_dir)
# }
#
# cdl
cdl_dir = file.path(root_data_in, "CDL")
print(list.files(path=cdl_dir, all.files=TRUE, full.names=FALSE))
# cdl_zip<-list.files(path=cdl_dir, pattern='.zip', all.files=TRUE, full.names=FALSE)
# print(cdl_zip)
# zip_ex <- as.list(c(file.exists(file.path(cdl_dir, cdl_zip))))
# for (l in length(zip_ex)){
#   unzip_function(zip_ex[[l]], cdl_zip, cdl_dir)
# }
#
#
#coa
coa_dir = file.path(root_data_in, "CoA")
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
caps_dir = file.path(root_data_in, "CAPS")
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
pnsp_dir = file.path(root_data_in, "PNSP")
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
print(list.files(path=bombus_dir, all.files=TRUE, full.names=FALSE))
# bombus_zip<-list.files(path=bombus_dir, pattern='.zip', all.files=TRUE, full.names=FALSE)
# print(bombus_zip)
# zip_ex <- as.list(c(file.exists(file.path(bombus_dir, bombus_zip))))
# for (l in length(zip_ex)){
#   unzip_function(zip_ex[[l]], bombus_zip, bombus_dir)
# }
#
state_dir = file.path(root_data_in, "state")
print(list.files(path=state_dir, all.files=TRUE, full.names=FALSE))
# state_zip<-list.files(path=state_dir, pattern='.zip', all.files=TRUE, full.names=FALSE)
# print(state_zip)
# zip_ex <- as.list(c(file.exists(file.path(state_dir, state_zip))))
# for (l in length(zip_ex)){
#   unzip_function(zip_ex[[l]], state_zip, state_dir)
# }



#source other files
#source(file.path(root_src, "01a_nlcd_processing.R"))
#source(file.path(root_src, "01b_cdl_processing.R"))


