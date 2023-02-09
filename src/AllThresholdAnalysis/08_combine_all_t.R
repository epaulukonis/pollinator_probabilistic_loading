### Probabilistic Crop Loading 

### 08 combine all

# Edited by E. Paulukonis January 2023


#### Sum Acreage Boxplots (Figure 1) ----
#original hyperparameters
final_Illinois<-do.call(rbind, list_of_dataI)
final_Illinois$County <- factor(final_Illinois$County , levels = c("Champaign", "McHenry", "DuPage"))
final_Illinois<-final_Illinois[!c(final_Illinois$County == 'McHenry' & final_Illinois$year==2021),] #remove
final_Illinois$State<-"Illinois"
final_Illinois$type<-"Large"

final_Michigan<-do.call(rbind, list_of_dataM)
final_Michigan$County <- factor(final_Michigan$County , levels = c("Huron", "Oceana", "VanBuren"))
final_Michigan$State<-"Michigan"
final_Michigan$type<-"Large"

final_Wisconsin<-do.call(rbind, list_of_dataW)
final_Wisconsin$County <- factor(final_Wisconsin$County , levels = c("Rock", "Waushara", "Langlade"))
final_Wisconsin$State<-"Wisconsin"
final_Wisconsin$type<-"Large"

#smaller hyperparameters
final_Illinoissf<-do.call(rbind, list_of_dataI_sf)
final_Illinoissf$County <- factor(final_Illinoissf$County , levels = c("Champaign", "McHenry", "DuPage"))
final_Illinoissf<-final_Illinoissf[!c(final_Illinoissf$County == 'McHenry' & final_Illinoissf$year==2021),] #remove
final_Illinoissf$State<-"Illinois"
final_Illinoissf$type<-"Small"

final_Michigansf<-do.call(rbind, list_of_dataM_sf)
final_Michigansf$County <- factor(final_Michigansf$County , levels = c("Huron", "Oceana", "VanBuren"))
final_Michigansf$State<-"Michigan"
final_Michigansf$type<-"Small"

final_Wisconsinsf<-do.call(rbind, list_of_dataW_sf)
final_Wisconsinsf$County <- factor(final_Wisconsinsf$County , levels = c("Rock", "Waushara", "Langlade"))
final_Wisconsinsf$State<-"Wisconsin"
final_Wisconsinsf$type<-"Small"



#combine here
final_all<-rbind(final_Illinois,final_Michigan,final_Wisconsin, final_Illinoissf,final_Michigansf,final_Wisconsinsf)
#saveRDS(final_all, file=paste0(root_data_out,"/final_all.RData"))


final_all$Label<-paste0(final_all$County," County, ",final_all$State)
final_all$Label<-ifelse(final_all$Label == "VanBuren County, Michigan, Large","Van Buren County, Michigan, Large", final_all$Label) #correct Van Buren
final_all$Label<-ifelse(final_all$Label == "VanBuren County, Michigan, Small","Van Buren County, Michigan, Small", final_all$Label) #correct Van Buren

#final_all$Label <- factor(final_all$Label , levels=unique(as.character(final_all$Label )) )
final_all <- transform(final_all, Label=reorder(Label, -sum_nass) ) 
nass_dat<-final_all[final_all$thresh ==1,]
nass_dat<-nass_dat %>% group_by(County, type) %>% mutate(avgnass =mean(sum_nass))%>% mutate(avgcdl = mean(sum_cdl))
#nass_dat$thresh<-c(1:14,1,2,5,10,1:12,14)
nass_dat$thresh<-rep(c(1,5,14,1,5,14,1,5,14),6)


#there a few VERY minor classes in the small data-set that make it over; the area is negligible for comparison purposes
#between the original and modified hyperparameter NASS/CDL totals, so we use the 'small' avg value. 

#what's the total percentage change across threshold? 
final_all <- final_all %>% group_by(thresh, County,type) %>% 
  mutate(avgfield =mean(sum_field))%>% 
  mutate(avgnass =mean(sum_nass)) %>%
  mutate(avgcdl =mean(sum_cdl))

percent<-final_all[final_all$year ==2008,]
percent<-percent %>% group_by(County, year, type) %>% 
  mutate(percentf = ((max(avgfield) - min (avgfield))/max(avgfield))*100) %>%
  mutate(percentn = ((max(avgnass) - max (avgfield))/max(avgnass))*100) %>%
  mutate(percentc = ((max(avgcdl) - max (avgfield))/max(avgcdl))*100)

percent[,13:15]<-round(percent[,13:15],1)
#percent<-percent %>% group_by(County) %>% mutate(sdf=100*(sd(avgfield)/mean(avgfield)) )


# percent<-percent %>%
#   group_by(County, type) %>%
#   mutate(heightf = min(avgfield)) %>%
#   # mutate(endf = min(avgfield)) %>%
#   # mutate(topf = max(avgfield)) %>%
#   #mutate(avgnass = ifelse(County %in% counties, max(avgnass) + .3 * sd(avgfield)), avgnass) %>%
#   mutate(heightn = avgnass ) %>%
#   # mutate(endn = min(avgnass)) %>%
#   # mutate(topn = max(avgnass)) %>%
#   # mutate(avgcdl = ifelse(County %in% counties, max(avgcdl) + .3 * sd(avgfield)), avgcdl) %>%
#   mutate(heightc = avgcdl)
# # mutate(endc = min(avgcdl)) %>%
# # mutate(topc = max(avgcdl))

percents<-percent[percent$type=="Small",]
percentl<-percent[percent$type=="Large",]

threshold_box<-
  ggplot(final_all, aes(x = as.factor(thresh), y = sum_field, color=interaction(type, thresh),group = interaction(type,thresh)))+
  geom_boxplot()+
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avgnass, group=1),size=1,color="black")+
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avgcdl, group=1),size=1,color="darkgrey")+
  geom_line(percents, mapping=aes(x=as.factor(thresh), y=avgfield, group=1),size=1, alpha=0.4, color=c("#8c510a"))+
  geom_line(percentl, mapping=aes(x=as.factor(thresh), y=avgfield, group=1),size=1, alpha=0.4, color=c("#01665e"))+
  geom_text(data=nass_dat[nass_dat$type == "Small",], mapping=aes(x = 15, y = avgnass, label = paste0("NASS")), size= 3, col='black', stat = "identity")+
  geom_text(data=nass_dat[nass_dat$type == "Small",], mapping=aes(x = 0.5, y = avgcdl, label = paste0("CDL")),  size= 3, col='darkgrey',  stat = "identity")+
  facet_wrap(.~Label, scales = "free_y")+
  scale_y_continuous(n.breaks=5,expand = expansion(mult = c(0, 0.1)))+
  scale_x_discrete(expand = expansion(add = c(2,2)))+
  xlab("Threshold") +
  ylab("Sum of Crop Acreages")+
  theme(panel.background = element_blank(),
        panel.spacing.x= unit(2.5, "lines"),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14, face='bold'),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14, face='bold'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")

threshold_box


### output Table 2
percent_out<-percent[percent$thresh == 1,]
percent_out<- percent_out[order(percent_out$type, -percent_out$avgnass), ]
write.csv(percent_out, paste0(root_figures, "/Manuscript/", "Table2.csv"))


## Crop Ratio Plot (Figure 2) ----
final_by_cropl<-rbind(final_Illinois_by_crop,final_Michigan_by_crop,final_Wisconsin_by_crop)
final_by_cropl$Hyperparameters<-'Large'
final_by_crops<-rbind(final_Illinois_by_cropsf,final_Michigan_by_cropsf, final_Wisconsin_by_cropsf)
final_by_crops$Hyperparameters<-'Small'

final_by_crop<-rbind(final_by_crops, final_by_cropl)

#levels(final_by_crop$Label)<- levels(final_all$Label)
final_by_crop<-final_by_crop%>%group_by(Label,Hyperparameters)%>%mutate(NASSacresm = mean(NASSacres))
final_by_crop<-na.omit(final_by_crop)

#this is to add an additional rank to sort the plot the same as the boxplot above
order<-as.data.frame(cbind(as.numeric(9:1) , c("Champaign County, Illinois", "Huron County, Michigan", "Rock County, Wisconsin",
                                               'McHenry County, Illinois',"Van Buren County, Michigan","Waushara County, Wisconsin",
                                               "Oceana County, Michigan", "Langlade County, Wisconsin", "DuPage County, Illinois")))
names(order)<-c("Rank","Label")
final_by_crop<-merge(final_by_crop,order,by="Label")
final_by_crop <- transform(final_by_crop, Label=reorder(Label, -as.numeric(Rank)) ) 

ratio_plotf<-ggplot(final_by_crop, aes(x=as.factor(Commodity), y=(log2(ratio)), fill=Hyperparameters)) +
  geom_boxplot()+
  facet_wrap(.~Label, scales = "free")+
  xlab("Crop") +
  ylab("Ratio Sum of Crop Acreage")+
  labs(title = "Ratio NASS Acres to Field Acres, By Crop")+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  # theme(legend.position = "none")
ratio_plotf


final_by_crop<- final_by_crop[order(final_by_crop$Hyperparameters, -final_by_crop$NASSacresm), ]
write.csv(final_by_crop, paste0(root_figures, "/Manuscript/", "Table3.csv"))
