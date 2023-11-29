### Probabilistic Crop Loading 

### 08c Comparison of on and off-field residues with an emphasis on bifenthrin and imidacloprid

on_field_subset<- on_field_residue_list[grep(c("IMIDACLOPRID|BIFENTHRIN"), names(on_field_residue_list))]
on_field_subset <- lapply(on_field_subset, transform, ID="On-field")

off_field_subset<- off_field_residue_list[grep(c("IMIDACLOPRID|BIFENTHRIN"), names(off_field_residue_list))]
off_field_subset <- lapply(off_field_subset, transform, ID="Off-field")

names(on_field_subset)<-paste0(names(on_field_subset),"_onfield")
names(off_field_subset)<-paste0(names(off_field_subset),"_offfield")


masterlist<-c(on_field_subset,off_field_subset)

seed<- masterlist[grep(c("Seed"), names(masterlist))]
soil<-  masterlist[grep(c("Soil"), names(masterlist))]
foliar<-  masterlist[grep(c("Foliar"), names(masterlist))]




#simple function to arrange data
gather_data<-function(x){
  df<-x
  df["ApplicationType"] <- NULL
  df<-gather(df, "Media", "Value", 5:ncol(df)-1)
  df
  
}


##Get foliar output
foliar_data<-lapply(foliar,gather_data)
foliar_data<-do.call(rbind,foliar_data)
foliar_data$Value<-ifelse(foliar_data$Value == 0,NA,foliar_data$Value)

##Get seed output
seed_data<-lapply(seed,gather_data)
seed_data<-do.call(rbind,seed_data)
seed_data$Value<-ifelse(seed_data$Value == 0,NA,seed_data$Value)

##Get seed output
soil_data<-lapply(soil,gather_data)
soil_data<-do.call(rbind,soil_data)
soil_data$Value<-ifelse(soil_data$Value == 0,NA,soil_data$Value)


#First designate type
foliar_data$ApplicationType<-"Foliar"
soil_data$ApplicationType<-"Soil"
seed_data$ApplicationType<-"Seed"

#Add in specific media for grouping
foliar_data$MediaSub<- sub("_.*", "", foliar_data$Media)
soil_data$MediaSub<- sub("_.*", "", soil_data$Media)
seed_data$MediaSub<- sub("_.*", "", seed_data$Media)

combine_all<-rbind(foliar_data,soil_data,seed_data)



## read in residues from DER
res<-read.csv(paste0(pest_dir,"/Residues/DER_ResidueData.csv"))
res<-na.omit(res) 
residues<-res
residues$Residues_DER_ugg<-((as.numeric(residues$Mean_ng_g_normalized_1.3mg_seed)*as.numeric(residues$Original_treatment_mg_seed))/residues$Normalized)/1000
residues$Chemical<-toupper(residues$Chemical)
residues$Plant<-toupper(residues$Plant)
residues$Plant<-ifelse(residues$Plant == "SOY","SOYBEANS",residues$Plant)
residues<-residues[,c(4,11,12,14)]
colnames(residues)<-c("Compound","Commodity","MediaSub","Residues_ugg")
residues$ApplicationType<-"Seed"
residues$Source<-"Field"


# read in residues from lit review
litres<-read.csv(paste0(pest_dir,"/Residues/Lit_ResidueData.csv"))
litres<-litres[,c(1:5)]
litres<-litres[,c(1,3,2,4,5)]
litres<-na.omit(litres)
litres$Source<-"Field"
colnames(litres)<-colnames(residues)
litres$Compound<-toupper(litres$Compound)

residues<-rbind(residues,litres)
residues<-residues[!residues$Commodity =="COTTON",]
residues$ID<-"On-field"

# get the estimated data
combine_all<-na.omit(combine_all)
combine_all$Source<-"Estimated"

names(combine_all)
names(residues)

combine_all<-combine_all[,c(2:4,6:9)]
colnames(combine_all)[4]<-"Residues_ugg"

residues<-residues[,c(1:2,7,4:5,3,6)]
residues<-residues[residues$Compound == "BIFENTHRIN" |residues$Compound == "IMIDACLOPRID",  ]

#combine all together
combine_all<- rbind(combine_all,residues)


forcomparisons<-combine_all[combine_all$MediaSub == "Pollen" | combine_all$MediaSub == "Nectar"| combine_all$MediaSub == "Soil", ]
forcomparisons<-na.omit(forcomparisons)
compare_outputs<-split(forcomparisons, list(forcomparisons$Compound), drop=T) 



library(RColorBrewer)
myColors <- brewer.pal(3,"Dark2")
names(myColors) <- unique(combine_all$ApplicationType)
colScale <- scale_colour_manual(name = "ApplicationType",values = myColors)
fillScale <- scale_fill_manual(name = "ApplicationType",values = myColors)

#x<-compare_outputs[[1]]

create_plot_of_methodcompare<-function(x){
  df<-x

  
  df$Value<-log(df$Residues_ugg)
  df$IDf= factor(df$ID, levels=c('On-field','Off-field'))
  
  
  out<-ggplot(df, aes(ApplicationType, (Value),color=ApplicationType, fill=ApplicationType)) +
    geom_boxplot()+
    colScale+
    fillScale+
    #scale_y_continuous(breaks=ticks, labels=format(logticks,3))+
   # facet_grid(rows = vars(MediaSub), cols = vars(IDf), scales="free_y")+
    
    ggh4x::facet_nested(MediaSub ~ IDf + Source, scales="free_y")+
    
    ylab("Log Residues [ug/g]")+
    #xlab(ifelse(df$Compound=="IMIDACLOPRID","Application Type", ""))+
    theme_bw()+
    theme(legend.position="none", axis.text.y = element_text(size=14,face="bold"), axis.text.x = element_text(size=14,face="bold"),  axis.title=element_text(size=14,face="bold"))+
    ggtitle(paste0(x$Compound)) 
  out
  
}


out_plots<-lapply(compare_outputs, create_plot_of_methodcompare)

compare_app_methods<-plot_grid(
  out_plots[[1]],
  out_plots[[2]],
  hjust=0, vjust=0, align= "h",  label_x = 0.01, ncol= 2,rel_widths = c(3,3))
compare_app_methods


#### Daily Time-step ---


#extract air/dust
air_df<-combine_all[combine_all$MediaSub == 'Air', ]
#extract air/dust
dust_df<-combine_all[combine_all$MediaSub == 'Dust', ]
#extract soil
soil_df<-combine_all[combine_all$MediaSub == 'Soil', ]
#extract pollen
pollen_df<-combine_all[combine_all$MediaSub == 'Pollen', ]
#extract nectar
nectar_df<-combine_all[combine_all$MediaSub == 'Nectar', ]

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(2,"Dark2")
names(myColors) <- unique(combine_all$Compound)
colScale <- scale_colour_manual(name = "Compound",values = myColors)


# airn<- ggplot(air_df, aes(day, Value, group=Compound, color=Compound)) +
#   geom_point(aes(shape=ApplicationType), size=3)+
#   #scale_shape_manual(values=c(16,4,8))+
#   colScale+
#   facet_wrap(~Commodity,scales = "free", nrow=1)+
#   ylab(expression(paste("Residues [ug/", m^{2},"]")))+
#   xlab("Days Post-Application")+
#  # geom_text(x=125, y=4000, label="Air", color="black", size=6)+
#   theme_bw() +
# #  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
#   theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
# 
# airn
# 
# 
# dustn<- ggplot(dust_df, aes(day, Value, group=Compound, color=Compound)) +
#   geom_point(aes(shape=ApplicationType), size=3)+
#   scale_shape_manual(values=c(16,4,8))+
#   facet_wrap(~Commodity,scales = "free", nrow=1)+
#   ylab(expression(paste("Residues [ug/", m^{2},"]")))+
#   xlab("Days Post-Application")+
# # geom_text(x=125, y=5.5, label="Dust", color="black", size=6)+
#   theme_bw() +
#  # theme(plot.margin = unit(c(1,1,1,1), "cm"))+
#   theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
# 
# dustn


soiln<- ggplot(soil_df, aes(day, (Value), color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  #geom_point(aes(shape=ApplicationType))+
  #scale_shape_manual(values=c(1,4,8))+
  scale_x_continuous(limits=c(0, 30), breaks=seq(0,30, by=5))+
 # facet_wrap(~Commodity,scales = "free", nrow=1)+
  facet_grid(rows = vars(ID), cols = vars(Commodity), scales="free_y")+
  ylab("Residues [ug/g]")+
  xlab("Days Post-Application")+
  #geom_text(x=125, y=0.05, label="Soil", color="black", size=6)+
  theme_bw()+
  theme()+
  #theme(plot.margin = unit(c(1,1,1,1), "cm"))
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
soiln


pollenn<- ggplot(pollen_df, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  #scale_x_continuous(limits=c(60, 130), breaks=seq(60,130, by=10),labels =c("0","7","17","27","37","47","57","67"))+
  # ifelse(pollen_df$Commodity == "CORN",
  # scale_x_continuous(limits=c(70, 130), breaks=seq(70,130, by=10),labels =c("7","17","27","37","47","57","67")),
  # scale_x_continuous(limits=c(60, 120), breaks=seq(60,120, by=10),labels =c("7","17","27","37","47","57","67"))
  # )+
  # geom_point(aes(shape=ApplicationType))+
  #scale_shape_manual(values=c(1,4,8))+
  #facet_wrap(Commodity ~ .,scales = "free", nrow=1)+
  facet_grid(rows = vars(ID), cols = vars(Commodity), scales="free_y")+
  ylab("")+
  xlab("Days Post-Application")+
  # geom_text(x=125, y=7.5, label="Pollen", color="black", size=6)+
  theme_bw()+
  theme()+
  #  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
pollenn

# c<-seq(70,130, by=10)
# s<-seq(60,120, by=10)
# 
# pollenn<-pollenn + facetted_pos_scales(
#   x = list(
#     Commodity == "CORN" ~ scale_x_continuous(limits=c(70, 130), breaks=c,labels =c("0","7","17","27","37","47","57")),
#     Commodity == "SOYBEANS" ~ scale_x_continuous(limits=c(60, 120),breaks=s,labels =c("0","7","17","27","37","47","57"))
#   )
# )
# 


nectarn<- ggplot(nectar_df, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  #scale_x_continuous(limits=c(60, 120), breaks=seq(60,120, by=10),labels =c("0","7","17","27","37","47","57"))+
  #geom_point(aes(shape=ApplicationType))+
  #scale_shape_manual(values=c(1,4,8))+
  # facet_wrap(~Commodity,scales = "free", nrow=1)+
  facet_grid(rows = vars(ID), cols = vars(Commodity), scales="free_y")+
  ylab("")+
  xlab("Days Post-Application")+
  #geom_text(x=125, y=0.9, label="Nectar", color="black", size=6)+
  theme_bw()+
  # theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),  axis.title=element_text(size=14,face="bold"))
nectarn

# nectarn<-nectarn + facetted_pos_scales(
#   x = list(
#     Commodity == "SOYBEANS" ~ scale_x_continuous(limits=c(60, 120),breaks=s,labels =c("0","7","17","27","37","47","57"))
#   )
# )
# 

compare_between_media<-plot_grid(
  soiln,
  pollenn,
  nectarn, 
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow = 1,rel_widths = c(3,3,3))
compare_between_media




all_plot_legend <- ggplot(combine_all, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype=ApplicationType), size=1)+
  colScale+
  facet_wrap(~Commodity,scales = "free", nrow=1)+
  ylab("Residues")+
  xlab("Day")+ 
  theme_bw()+
  theme( legend.key.size = unit(1, 'cm'), #change legend key size
         legend.key.height = unit(1, 'cm'), #change legend key height
         legend.key.width = unit(1, 'cm'), #change legend key width
         legend.title = element_text(size=16), #change legend title font size
         legend.text = element_text(size=14))



legend<-get_only_legend(all_plot_legend)

compare_between_mediaoff<-plot_grid(compare_between_media, legend,ncol = 2, rel_widths = c(7,1))
compare_between_mediaoff

