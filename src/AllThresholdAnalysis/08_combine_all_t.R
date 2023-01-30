### Probabilistic Crop Loading 

### 09 hyperparameter runs

# Edited by E. Paulukonis January 2023

#to work on 1/27

#### Put together boxplots combining ALL; you'll need to run 07 first----
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
final_all$Label<-paste0(final_all$County," County, ",final_all$State)
final_all$Label<-ifelse(final_all$Label == "VanBuren County, Michigan, Large","Van Buren County, Michigan, Large", final_all$Label) #correct Van Buren
final_all$Label<-ifelse(final_all$Label == "VanBuren County, Michigan, Small","Van Buren County, Michigan, Small", final_all$Label) #correct Van Buren

#final_all$Label <- factor(final_all$Label , levels=unique(as.character(final_all$Label )) )
final_all <- transform(final_all, Label=reorder(Label, -sum_nass) ) 

nass_dat<-final_all[final_all$thresh ==1,]
nass_dat<-nass_dat %>% group_by(County) %>% mutate(avgnass =mean(sum_nass))%>% mutate(avgcdl = mean(sum_cdl))
#nass_dat$thresh<-c(1:14,1,2,5,10,1:12,14)
nass_dat$thresh<-rep(c(1,5,14,1,5,14,1,5,14),6)


t_box<-
  ggplot(final_all, aes(x = as.factor(thresh), y = sum_field, color=as.factor(thresh)))+
  geom_boxplot()+
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avgnass, group=1),size=1,color="black")+
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avgcdl, group=1),size=1,color="darkgrey")+
  facet_grid(rows = vars(Label), cols = vars(type),
  scales="free_y", switch = 'y')+
  #facet_wrap(.~Label, scales = "free_y")+
  scale_y_continuous(n.breaks=10,expand = expansion(mult = c(0, .1)))+
  xlab("Threshold") +
  ylab("Sum of Crop Acreages")+
  theme(legend.position = "none",axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

t_box






#what's the total percentage change across threshold? 
final_all <- final_all %>% group_by(thresh, County) %>% 
  mutate(avgfield =mean(sum_field))%>% 
  mutate(avgnass =mean(sum_nass)) %>%
  mutate(avgcdl =mean(sum_cdl))

percent<-final_all[final_all$year ==2008,]
percent<-percent %>% group_by(County, year) %>% 
  mutate(percentf = ((max(avgfield) - min (avgfield))/max(avgfield))*100) %>%
  mutate(percentn = ((max(avgnass) - max (avgfield))/max(avgnass))*100) %>%
  mutate(percentc = ((max(avgcdl) - max (avgfield))/max(avgcdl))*100)

percent[,13:15]<-round(percent[,13:15],1)
#percent<-percent %>% group_by(County) %>% mutate(sdf=100*(sd(avgfield)/mean(avgfield)) )


percent<-percent %>% 
  group_by(County) %>% 
  mutate(heightf = min(avgfield)) %>%
  # mutate(endf = min(avgfield)) %>%
  # mutate(topf = max(avgfield)) %>%
  #mutate(avgnass = ifelse(County %in% counties, max(avgnass) + .3 * sd(avgfield)), avgnass) %>%
  mutate(heightn = avgnass ) %>%
  # mutate(endn = min(avgnass)) %>%
  # mutate(topn = max(avgnass)) %>%
  # mutate(avgcdl = ifelse(County %in% counties, max(avgcdl) + .3 * sd(avgfield)), avgcdl) %>%
  mutate(heightc = avgcdl) 
# mutate(endc = min(avgcdl)) %>%
# mutate(topc = max(avgcdl)) 


fin_box<-t_box +
  geom_line(percent, mapping=aes(x=as.factor(thresh), y=avgfield, group=Label,),size=1, alpha=0.4, color="darkblue")+
  facet_grid(rows = vars(Label), cols = vars(type),
             scales="free_y", switch = 'y')+
  #facet_wrap(.~Label, scales = "free_y")+
  geom_text(percent, mapping=aes(x = 15.5, y = heightf, label = paste0(percentf, "%")), size= 4, col='darkblue', stat = "identity")+
  geom_text(percent, mapping=aes(x = 15, y = heightn, label = paste0(percentn, "%")), size= 4, col='black', stat = "identity")+
  geom_text(percent, mapping=aes(x = 15, y = heightc, label = paste0(percentc, "%")), size= 4, col='darkgrey',  stat = "identity")+
  coord_cartesian(xlim = c(1, 16), # This focuses the x-axis on the range of interest
                  clip = 'off') +  
  theme(panel.background = element_blank(),
        panel.spacing.x= unit(2.5, "lines"),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14, face='bold'),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14, face='bold'),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(expand = c(0.1,0))

fin_box


##do individual crop ratios here ----

final_by_crop<-rbind(final_Illinois_by_crop,final_Michigan_by_crop,final_Wisconsin_by_crop)
#levels(final_by_crop$Label)<- levels(final_all$Label)
final_by_crop<-final_by_crop%>%group_by(Label)%>%mutate(NASSacresm = mean(NASSacres))
final_by_crop<-na.omit(final_by_crop)

#this is to add an additional rank to sort the plot the same as the boxplot above
order<-as.data.frame(cbind(as.numeric(9:1) , c("Champaign County, Illinois", "Huron County, Michigan", "Rock County, Wisconsin",
                                               'McHenry County, Illinois',"Van Buren County, Michigan","Waushara County, Wisconsin",
                                               "Oceana County, Michigan", "Langlade County, Wisconsin", "DuPage County, Illinois")))
names(order)<-c("Rank","Label")
final_by_crop<-merge(final_by_crop,order,by="Label")
final_by_crop <- transform(final_by_crop, Label=reorder(Label, -as.numeric(Rank)) ) 

ratio_plotf<-ggplot(final_by_crop, aes(x=as.factor(Commodity), y=(log2(ratio)), fill=Commodity)) +
  geom_boxplot()+
  
  facet_wrap(.~Label, scales = "free")+
  xlab("Crop") +
  ylab("Ratio Sum of Crop Acreage")+
  labs(title = "Ratio NASS Acres to Field Acres, By Crop")+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position = "none")
ratio_plotf



