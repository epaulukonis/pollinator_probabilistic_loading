# install.packages("githubinstall")
# library(githubinstall)
# gh_search_packages('beecoSp')
# gh_install_packages("beecoSp")


library(sp)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(cowplot)
library(beecoSp)
lsf.str("package:beecoSp")


cdl_dir <- file.path("C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/pollinator_probabilistic_loading/data_in/MapData")
data_dir<-file.path("C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Paulukonis_Documents/manuscript_bombus_toxicity/douglas_materials")

cdl_data <- raster(paste0(cdl_dir,"/CDL/CDL_2019_17.tif"))

plot(cdl_data)



