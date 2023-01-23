
#### Got it to work! single line plots for thresh 1 with all years
#here, I took the code from the OG line plots but changed the way the data is compiled by letting NASS = 0 at non-COA years
#then, I put everything together, plotted the lines for all years, and then took out any NASS years with 0
#I need to do this for the other two States and put them together, and also figure out how to get the legend to do what I want


# ratio_plot<-ggplot(final_I, aes(x=year, y=log(sum_field), group=County, fill=factor(County), colour=factor(County))) +
# geom_point()+
# geom_line()+
# xlab("Year") +
# ylab("Log(Sum Acreages)")+
# scale_x_discrete(name ="Year",
# limits=c(2008:2021))+
# scale_y_continuous(n.breaks=8, expand = expansion(mult = c(0, .1)))+
# labs(title = paste0("Illinois County Total Field Acres to NASS Acres"))+
# guides(fill=guide_legend(title="County"), colour=guide_legend(title="County"))+
# theme(panel.background = element_blank(),
# axis.line = element_line(colour = "black"),
# axis.title.x=element_text(margin = margin(t = 10, r = 0, b = , l = 0), size=14,face="bold"),
# axis.title.y=element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold"),
# axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ratio_plot
# 
# final_I$sum_nass<-ifelse(final_I$sum_nass == 0,NA,final_I$sum_nass)
# final_I<-na.omit(final_I)
# 
# Ioutput<-ratio_plot+
# geom_point(final_I,mapping=aes(y=log(sum_nass),group=County, shape=County),col='black') +
# geom_line(final_I,mapping=aes(y=log(sum_nass),group=County), col='black')+
# geom_point(final_I,mapping=aes(y=log(sum_cdl),group=County, shape=County),col='darkgrey') +
# geom_line(final_I,mapping=aes(y=log(sum_cdl),group=County),col='darkgrey')
# Ioutput


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

#### low vs high boxplots ----

#use threshold 1, all years
final_Ih<-final_Illinois[final_Illinois$County == 'Champaign' & final_Illinois$thresh==1,]
final_Mh<-final_Michigan[final_Michigan$County == 'Huron' & final_Michigan$thresh==1,]
final_Wh<-final_Wisconsin[final_Wisconsin$County == 'Rock' & final_Wisconsin$thresh==1,]

final_Ih$State<-"Illinois"
final_Mh$State<-"Michigan"
final_Wh$State<-"Wisconsin"
high<-rbind(final_Ih,final_Mh,final_Wh)

final_Il<-final_Illinois[final_Illinois$County == 'DuPage' & final_Illinois$thresh==1,]
final_Ml<-final_Michigan[final_Michigan$County == 'VanBuren' & final_Michigan$thresh==1,]
final_Wl<-final_Wisconsin[final_Wisconsin$County == 'Langlade' & final_Wisconsin$thresh==1,]

final_Il$State<-"Illinois"
final_Ml$State<-"Michigan"
final_Wl$State<-"Wisconsin"
final_Ml$County<-gsub("([a-z])([A-Z])","\\1 \\2",final_Ml$County)
low<-rbind(final_Il,final_Ml,final_Wl)

#high
nass_datH<-high %>% group_by(County) %>% mutate(avg =mean(sum_nass)) %>% mutate(avgcdl =mean(sum_cdl))
high_box<-ggplot(nass_datH, aes(x = as.factor(County), y = sum_field, color=as.factor(County)))+
  geom_boxplot()+
  xlab("County") +
  ylab("Sum of Crop Acreages")+
  theme(legend.position = "none",axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))+
  facet_wrap(.~County, scales = "free")

high_box
high_boxf<-high_box + 
  geom_point(nass_datH, mapping=aes(x=as.factor(County), y=avg,group=1),size=3,color="black", shape=17)+
  geom_point(nass_datH, mapping=aes(x=as.factor(County), y=avgcdl,group=1),size=3,color="darkgrey", shape=19)+
  facet_wrap(.~as.factor(County), scales = "free")
high_boxf


#low
nass_datL<-low %>% group_by(County) %>% mutate(avg =mean(sum_nass)) %>% mutate(avgcdl =mean(sum_cdl))
low_box<-ggplot(nass_datL, aes(x = as.factor(County), y = sum_field, color=as.factor(County)))+
  geom_boxplot()+
  xlab("County") +
  ylab("Sum of Crop Acreages")+
  theme(legend.position = "none",axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))+
  facet_wrap(.~County, scales = "free")

low_box
low_boxf<-low_box + 
  geom_point(nass_datL, mapping=aes(x=as.factor(County), y=avg,group=1),size=3,color="black", shape=17)+
  geom_point(nass_datL, mapping=aes(x=as.factor(County), y=avgcdl,group=1),size=3,color="darkgrey", shape=19)+
  facet_wrap(.~as.factor(County), scales = "free")
low_boxf



ggpubr::ggarrange(high_boxf,low_boxf, # list of plots
                  labels = 1, # labels
                  # common.legend = T, # COMMON LEGEND
                  # legend = "right", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol = 1)  # number of rows


list_of_final_data_by_countyI<-list()
for(n in 1:length(acreages_by_countyI)){ 
  #county_name<-'CHAMPAIGN'
  county<-acreages_by_countyI[[n]]
  county_name<-names(acreages_by_countyI[n])
  names(county)<-years
  
  list_of_year<-list()
  for(y in 1:length(county)){
    year_list<-county[[y]]
    list_of_field<-list()
    for(layer in 1:length(year_list)){
      layer_by_year<-year_list[[layer]]
      layer_by_year$year<-as.numeric(names(county[y]))
      layer_by_year$Category<-  toupper(layer_by_year$Category) 
      #layer_by_year<-layer_by_year[!layer_by_year$Category == "OTHER HAY/NON ALFALFA",] #add in winter wheat?? no
      layer_by_year<-year_list[[layer]]
      layer_by_year$year<-as.numeric(names(county[y]))
      layer_by_year$Category<-  toupper(layer_by_year$Category) 
      layer_by_year$Category[grepl('WINTER', layer_by_year$Category)] <- 'WHEAT'
      layer_by_year$Category[grepl('OTHER HAY/NON ALFALFA', layer_by_year$Category)] <- 'HAY'
      layer_by_year$Category[grepl('ALFALFA', layer_by_year$Category)] <- 'HAY'
      layer_by_year$x<-ifelse(layer_by_year$Class ==  36, with(layer_by_year,sum(x[Category =='HAY'])), layer_by_year$x)
      layer_by_year<-layer_by_year[!layer_by_year$Class == 37,] #drop the other hay class because we merged it with the alfalfa class
      layer_by_year$threshold<-15-layer
      
      list_of_field[[layer]]<-layer_by_year
      
    }
    
    list_of_year[[y]]<-do.call(rbind, list_of_field)
    
  }
  
  field_data<-do.call(rbind, list_of_year)
  #NASS data
  ill_nassx<-ill_nass[!ill_nass$Value == " (D)",]
  ill_nassx$Value<-as.numeric(as.numeric(gsub(",", "", ill_nassx$Value)))
  ill_nass_y<- ill_nassx %>%
    #filter(Year %in% c(layer_by_year$year)) %>%
    filter(County %in% county_name) %>%
    filter_at(vars(starts_with("Data.Item")), all_vars(grepl('HARVESTED|BEARING & NON-BEARING', .)))%>% #use 'non' to filter bearing and non-bearing
    filter_at(vars(starts_with("Data.Item")), all_vars(!grepl('OPERATIONS|SMALL|PROCESSING', .)))%>%
    group_by(Commodity, Year) %>% summarise(sum = sum(as.numeric(Value))) 
  
  
  names(field_data)[3]<-"Commodity"
  names(field_data)[2]<-"fieldacres"
  layer_by_year_crops<-left_join(field_data, ill_nass_y, by = c("Year","Commodity"))
  colnames(layer_by_year_crops)[8]<-"NASSacres"
  layer_by_year_crops[is.na(layer_by_year_crops)] <- 0 ##fix here
  
  #layer_by_year_crops<-na.omit(layer_by_year_crops) #IF there are no crops represented in NASS for that year, drop those rows
  layer_by_year_crops$County<-cntynames[[n]]
  list_of_final_data_by_countyI[[n]]<-layer_by_year_crops
  
}


cntynames<-c('CHAMPAIGN',"DU PAGE","MCHENRY")
list_of_plotsI<-list()
list_of_dataI<-list()
for(c in 1:length(list_of_final_data_by_countyI)){
  layer_by_year_crops<-list_of_final_data_by_countyI[[c]]
  extracted_cdl_dataI$CDLacres<-((extracted_cdl_dataI$count)*900)*0.000247105
  colnames(extracted_cdl_dataI)[1]<-'Class'
  layer_by_year_crops<-left_join(layer_by_year_crops, extracted_cdl_dataI, by=c("County","year","Class"))
  
  #Plot showing combined data
  sum_field<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(fieldacres)))
  sum_nass<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(NASSacres)))
  sum_cdl<-layer_by_year_crops %>% group_by(year, threshold) %>% summarise(sumf = sum(as.numeric(CDLacres)))
  all_data<-cbind(sum_field,sum_nass, sum_cdl)
  names(all_data)<-c("year","thresh","sum_field","year","thresh","sum_nass","year","thresh","sum_cdl")
  all_data<-all_data[,c(1:3,6,9)]
  
  #because some thresholds include crops that others don't, we need to modify the final NASS values to reflect that
  all_data_t <- all_data %>% group_by(year) %>% 
    mutate(sum_nass = ifelse(max(sum_nass) > min(sum_nass),  max(sum_nass), min(sum_nass))) 
  all_data_t <- all_data_t %>% group_by(year) %>% 
    mutate(sum_cdl = ifelse(max(sum_cdl) > min(sum_cdl),  max(sum_cdl), min(sum_cdl)))
  
  all_data_t$County<-names(field_list_ill_f)[c]
  list_of_dataI[[c]]<-all_data_t
  
}

final_Illinois<-do.call(rbind, list_of_dataI)
final_Illinois <- final_Illinois %>% group_by(thresh, County) %>% mutate(avgfield =mean(sum_field))

