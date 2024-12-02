

#### Calculate ratios of oral and contact exposures to assess 


beetox <-read.csv(paste0(pest_dir, "/BeeTox.csv"))
beetox$Compound<-toupper(beetox$Compound)

beetox$Ratio<-beetox$Oral_LD50_ug_bee/beetox$Contact_LD50_ug_bee
