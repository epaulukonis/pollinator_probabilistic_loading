

# layer_by_year_crops$thresh<-names(year_list[layer])
#county_by_year_crops$County<-county_name
#names(list_of_year)<-years
#final_df_ratio<-do.call(rbind, list_of_year)


ratio_plot<-ggplot(all_data_t, aes(x=year, y=(sum_field), group= factor(thresh), fill=factor(thresh), colour=factor(thresh))) + 
  geom_point(mapping=aes( y=sum_nass), col='black') +
  geom_line(mapping=aes(y=sum_nass), col='black') +
  geom_point()+
  geom_line()+
  #geom_hline(yintercept=1, linetype='dotted', col = 'red')+
  # geom_label(data=total_ratio %>% filter(year==2021), aes(label = label), hjust = -.5, vjust=1.6,  
  #             label.padding = unit(0.35, "lines"), # Rectangle size around label
  #             label.size = 0.25,
  #             color = "black") +
  xlab("Year") +
  ylab("Acreages")+
  # scale_fill_manual(values = palette,breaks=c("Champaign1fin", "Champaign2fin", "Champaign3fin", "Champaign4fin",  "Champaign5fin",  "Champaign6fin",
  #                              "Champaign7fin",  "Champaign8fin", "Champaign9fin", "Champaign10fin", "Champaign11fin",  "Champaign12fin",
  #                              "Champaign13fin", "Champaign14fin"),
  #                     labels=c("1", "2", "3", "4",  "5",  "6",
  #                              "7",  "8", "9", "10", "11",  "12",
  #                              "13", "14"))+
  # scale_colour_manual(values = palette,breaks=c("Champaign1fin", "Champaign2fin", "Champaign3fin", "Champaign4fin",  "Champaign5fin",  "Champaign6fin",
  #                                             "Champaign7fin",  "Champaign8fin", "Champaign9fin", "Champaign10fin", "Champaign11fin",  "Champaign12fin",
  #                                             "Champaign13fin", "Champaign14fin"),
  #                   labels=c("1", "2", "3", "4",  "5",  "6",
  #                            "7",  "8", "9", "10", "11",  "12",
#                            "13", "14"))+
# scale_fill_manual(values = palette) +
# scale_color_manual(values=palette)+
scale_x_discrete(name ="Year", 
                 limits=c(2008:2021))+
  # expand_limits(y = 0)+
  # scale_y_continuous(breaks=c(0, max(all_data_t$sum_nass)))+
  
  labs(title = paste0(names(field_list_ill_f)[c]," County Total Field Acres to NASS Acres, by Threshold"))+
  guides(fill=guide_legend(title="Threshold"), colour=guide_legend(title="Threshold"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ratio_plot



#Plot showing the ratio of nass acres to field acres across years, by threshold, crop 
#test1<-test[test$thresh =="Champaign1_fin",]

# ratio_plot<-ggplot(test, aes(x=year, y=(ratio), group= Bin, fill=Bin, colour=Bin, shape=thresh)) + 
#   geom_point()+
#   xlab("Crop") + 
#   ylab("ratio")+
#   labs(title = "Ratio of Stuff")+
#   theme(panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"), 
#         axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
#         axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ratio_plot


#final_df_ratio$thresh <- factor(final_df_ratio$thresh, levels = c("virginica", "versicolor", "setosa"))

#install.packages('randomcoloR')



### Illinois
final_Ix<-tidyr::gather(final_I, "Category", "n", 3:5)
final_Ix$Category<-setNames(c('Field Acreage', 'CDL Acreage', 'NASS Acreage'), c("sum_field", "sum_cdl", "sum_nass"))[final_Ix$Category]
final_Ix$Category <- factor(final_Ix$Category , levels = c('Field Acreage', 'CDL Acreage', 'NASS Acreage'))

#let's set a variable to assign the unique color values 
setDT(final_Ix)[, Combo := .GRP, .(County, Category)]
final_Ix$Combo[final_Ix$Category == 'NASS Acreage']<-4
final_Ix$Combo[final_Ix$Category == 'CDL Acreage']<-5
final_Ix$Combo <- factor(final_Ix$Combo , levels = c(1,2,3,4,5))

ref1 = 4
ref2 = 5
myColors <- brewer.pal(length(levels(final_Ix$Combo)),"Set1")
names(myColors) <- levels(final_Ix$Combo)
myColors[grepl(ref1, names(myColors))] <- "black"
myColors[grepl(ref2, names(myColors))] <- "darkgrey"
colScale <- scale_colour_manual(labels=c("Champaign Fields","McHenry Fields","DuPage Fields","NASS","CDL"),name = "Acreages",values = myColors)

line_plotI<-ggplot(final_Ix, aes(x=year, y=log10(n),group=interaction(Category,County), color =Combo, shape=County)) + 
  geom_point()+
  geom_line()+
  colScale+
  scale_x_discrete(name ="Year", 
                   limits=c(2008:2021))+
  scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0.1, 0.1)))+
  labs(title = paste0("Illinois County Total Field Acres to NASS & CDL Acres by Year"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


line_plotI



### Michigan
final_Mx<-tidyr::gather(final_M, "Category", "n", 3:5)
final_Mx$Category<-setNames(c('Field Acreage', 'CDL Acreage', 'NASS Acreage'), c("sum_field", "sum_cdl", "sum_nass"))[final_Mx$Category]
final_Mx$Category <- factor(final_Mx$Category , levels = c('Field Acreage', 'CDL Acreage', 'NASS Acreage'))

#let's set a variable to assign the unique color values 
setDT(final_Mx)[, Combo := .GRP, .(County, Category)]
final_Mx$Combo[final_Mx$Category == 'NASS Acreage']<-4
final_Mx$Combo[final_Mx$Category == 'CDL Acreage']<-5
final_Mx$Combo <- factor(final_Mx$Combo , levels = c(1,2,3,4,5))

ref1 = 4
ref2 = 5
myColors <- brewer.pal(length(levels(final_Mx$Combo)),"Set1")
names(myColors) <- levels(final_Mx$Combo)
myColors[grepl(ref1, names(myColors))] <- "black"
myColors[grepl(ref2, names(myColors))] <- "darkgrey"
colScale <- scale_colour_manual(labels=c("Huron Fields","Oceana Fields","Van Buren Fields","NASS","CDL"), name = "Acreages",values = myColors)



line_plotM<-ggplot(final_Mx, aes(x=year, y=log10(n),group=interaction(Category,County), color =Combo, shape=County)) + 
  geom_point()+
  geom_line()+
  colScale+
  ylab("Log10(Sum Acreages)")+
  scale_x_discrete(name ="Year", 
                   limits=c(2008:2021))+
  scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0.1, 0.1)))+
  labs(title = paste0("Michigan County Total Field Acres to NASS & CDL Acres by Year"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


line_plotM


### Wisconsin
final_Wx<-tidyr::gather(final_W, "Category", "n", 3:5)
final_Wx$Category<-setNames(c('Field Acreage', 'CDL Acreage', 'NASS Acreage'), c("sum_field", "sum_cdl", "sum_nass"))[final_Wx$Category]
final_Wx$Category <- factor(final_Wx$Category , levels = c('Field Acreage', 'CDL Acreage', 'NASS Acreage'))

#let's set a variable to assign the unique color values 
setDT(final_Wx)[, Combo := .GRP, .(County, Category)]
final_Wx$Combo[final_Wx$Category == 'NASS Acreage']<-4
final_Wx$Combo[final_Wx$Category == 'CDL Acreage']<-5
final_Wx$Combo <- factor(final_Wx$Combo , levels = c(1,2,3,4,5))

ref1 = 4
ref2 = 5
myColors <- brewer.pal(length(levels(final_Wx$Combo)),"Set1")
names(myColors) <- levels(final_Wx$Combo)
myColors[grepl(ref1, names(myColors))] <- "black"
myColors[grepl(ref2, names(myColors))] <- "darkgrey"
colScale <- scale_colour_manual(labels=c("Rock Fields","Waushara Fields","Langlade Fields","NASS","CDL"), name = "Acreages",values = myColors)



line_plotW<-ggplot(final_Wx, aes(x=year, y=log10(n),group=interaction(Category,County), color =Combo, shape=County)) + 
  geom_point()+
  geom_line()+
  colScale+
  xlab("Year") +
  scale_x_discrete(name ="Year", 
                   limits=c(2008:2017))+
  scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0.1, 0.1)))+
  labs(title = paste0("Wisconsin County Total Field Acres to NASS & CDL Acres by Year"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


line_plotW





ggpubr::ggarrange(line_plotI, line_plotM, line_plotW, # list of plots
                  #labels = 1, # labels
                  #common.legend = T, # COMMON LEGEND
                  #legend = "right", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol = 1)  # number of rows

#Ill
#boxplots showing the specific acreage average via threshold compared to NASS average
final_Illinois<-do.call(rbind, list_of_dataI)
final_Illinois$County <- factor(final_Illinois$County , levels = c("Champaign", "McHenry", "DuPage"))
final_Illinois<-final_Illinois[!c(final_Illinois$County == 'McHenry' & final_Illinois$year==2021),] #remove outlier in McHenry
box<-ggplot(final_Illinois, aes(x = as.factor(thresh), y = sum_field, color=as.factor(thresh)))+
  geom_boxplot()+
  facet_wrap(.~County, scales = "free")
#geom_point(data = final_Illinois,aes(aes(x = year, y = sum_nass)),color = 'black')+
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
# theme(panel.background = element_blank(),
#       axis.line = element_line(colour = "black"),
#       axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
#       axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

box

nass_dat<-final_Illinois[final_Illinois$thresh ==1,]
nass_dat<-nass_dat %>% group_by(County) %>% mutate(avg =mean(sum_nass)) %>% mutate(avgcdl =mean(sum_cdl))
nass_dat$thresh<-c(1,5,14,1,5,14,1,5,14)
#nass_dat$thresh<-c(1:14,1,2,5,14,1:11,14)


ill_box<-box + 
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avg, group=1),size=1,color="black")+
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avgcdl, group=1),size=1,color="darkgrey")+
  facet_wrap(.~County, scales = "free_y")+
  scale_y_continuous(n.breaks=10,expand = expansion(mult = c(0, .1)))+
  xlab("Threshold") +
  ylab("Sum of Crop Acreages")+
  theme(legend.position = "none",axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

ill_box

##MI
#boxplots showing the specific acreage average via threshold compared to NASS average
final_Michigan<-do.call(rbind, list_of_dataM)
final_Michigan$County <- factor(final_Michigan$County , levels = c("Huron", "Oceana", "VanBuren"))
box<-ggplot(final_Michigan, aes(x = as.factor(thresh), y = sum_field, color=as.factor(thresh)))+
  geom_boxplot()+
  facet_wrap(.~County, scales = "free")
#geom_point(data = final_Illinois,aes(aes(x = year, y = sum_nass)),color = 'black')+
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
# theme(panel.background = element_blank(),
#       axis.line = element_line(colour = "black"),
#       axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
#       axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

box
nass_dat<-final_Michigan[final_Michigan$thresh ==1,]
nass_dat<-nass_dat %>% group_by(County) %>% mutate(avg =mean(sum_nass))%>% mutate(avgcdl =mean(sum_cdl))
nass_dat$thresh<-c(1,5,14,1,5,14,1,5,14)
#nass_dat$thresh<-c(1:14,1:14,1:7,8,9,14)

mi_box<-box + 
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avg, group=1),size=1,color="black")+
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avgcdl, group=1),size=1,color="darkgrey")+
  facet_wrap(.~County, scales = "free_y")+
  scale_y_continuous(n.breaks=10,expand = expansion(mult = c(0, .1)))+
  xlab("Threshold") +
  ylab("Sum of Crop Acreages")+
  theme(legend.position = "none",axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

mi_box

#what's the total percentage change across threshold? 
final_Michigan <- final_Michigan %>% group_by(thresh, County) %>% mutate(avgfield =mean(sum_field))
percent<-final_Michigan[final_Michigan$year ==2008,]
percent<-percent %>% group_by(County, year) %>% mutate(percent = ((max(avgfield) - min (avgfield))/max(avgfield))*100)
percent$Percent<-as.character(round(percent$percent,1))

percent<-percent %>% 
  group_by(County) %>% 
  mutate(height = min(avgfield) + .5 * sd(avgfield)) %>%
  mutate(end = min(avgfield)) %>%
  mutate(top = max(avgfield)) 
#percent shows the percent change in average field acreages over time by threshold

fin_boxm<-mi_box +
  #geom_line(percent, mapping=aes(x=as.factor(thresh), y=avgfield, group=County,),size=1, alpha=0.4, color="darkblue")+
  facet_wrap(.~County, scales = "free_y")+
  geom_segment(percent, mapping=aes(x=1, y=end, xend=1,yend=top), col='black')+
  geom_text(percent, mapping=aes(x = 2.5, y = height, label = paste0(Percent, "%")), size= 4, col='black', stat = "identity")
fin_boxm

##WI
#boxplots showing the specific acreage average via threshold compared to NASS average
final_Wisconsin<-do.call(rbind, list_of_dataW)
final_Wisconsin$County <- factor(final_Wisconsin$County , levels = c("Rock", "Waushara", "Langlade"))

box<-ggplot(final_Wisconsin, aes(x = as.factor(thresh), y = sum_field, color=as.factor(thresh)))+
  geom_boxplot()+
  facet_wrap(.~County, scales = "free")
#geom_point(data = final_Illinois,aes(aes(x = year, y = sum_nass)),color = 'black')+
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
# theme(panel.background = element_blank(),
#       axis.line = element_line(colour = "black"),
#       axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
#       axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
#       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

box
nass_dat<-final_Wisconsin[final_Wisconsin$thresh ==1,]
nass_dat<-nass_dat %>% group_by(County) %>% mutate(avg =mean(sum_nass))%>% mutate(avgcdl = mean(sum_cdl))
#nass_dat$thresh<-c(1:14,1,2,5,10,1:12,14)
nass_dat$thresh<-c(1,5,14,1,5,14,1,5,14)

wi_box<-box + 
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avg, group=1),size=1,color="black")+
  geom_line(nass_dat, mapping=aes(x=as.factor(thresh), y=avgcdl, group=1),size=1,color="darkgrey")+
  facet_wrap(.~County, scales = "free_y")+
  scale_y_continuous(n.breaks=10,expand = expansion(mult = c(0, .1)))+
  xlab("Threshold") +
  ylab("Sum of Crop Acreages")+
  theme(legend.position = "none",axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

wi_box

#what's the total percentage change across threshold? 
final_Wisconsin <- final_Wisconsin %>% group_by(thresh, County) %>% mutate(avgfield =mean(sum_field))
percent<-final_Wisconsin[final_Wisconsin$year ==2008,]
percent<-percent %>% group_by(County, year) %>% mutate(percent = ((max(avgfield) - min (avgfield))/max(avgfield))*100)
percent$Percent<-as.character(round(percent$percent,1))

percent<-percent %>% 
  group_by(County) %>% 
  mutate(height = min(avgfield) + .5 * sd(avgfield)) %>%
  mutate(end = min(avgfield)) %>%
  mutate(top = max(avgfield)) 
#percent shows the percent change in average field acreages over time by threshold

fin_boxw<-wi_box +
  #geom_line(percent, mapping=aes(x=as.factor(thresh), y=avgfield, group=County,),size=1, alpha=0.4, color="darkblue")+
  facet_wrap(.~County, scales = "free_y")+
  geom_segment(percent, mapping=aes(x=1, y=end, xend=1,yend=top), col='black')+
  geom_text(percent, mapping=aes(x = 2.5, y = height, label = paste0(Percent, "%")), size= 4, col='black', stat = "identity")
fin_boxw


### Combine OG Line Plots----
ggpubr::ggarrange(fin_boxi, fin_boxm, fin_boxw,# list of plots
                  #labels = 1, # labels
                  # common.legend = T, # COMMON LEGEND
                  # legend = "right", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol = 1)  # number of rows




ggpubr::ggarrange(ratio_plotI, ratio_plotM, ratio_plotW, # list of plots
                  labels = "AUTO", # labels
                  # common.legend = T, # COMMON LEGEND
                  # legend = "right", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol = 1)  # number of rows

