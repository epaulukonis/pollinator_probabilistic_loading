### Probabilistic Crop Loading 

### 08c Comparison of on and off-field residues 

on_field_subset<- on_field_residue_list[grep(c("IMIDACLOPRID|BIFENTHRIN|CARBARYL|GLYPHOSATE|CHLORPYRIFOS|THIAMETHOXAM"), names(on_field_residue_list))]
on_field_subset <- lapply(on_field_subset, transform, ID="On-field")

off_field_subset<- off_field_residue_list[grep(c("IMIDACLOPRID|BIFENTHRIN|CARBARYL|GLYPHOSATE|CHLORPYRIFOS|THIAMETHOXAM"), names(off_field_residue_list))]
off_field_subset <- lapply(off_field_subset, transform, ID="Off-field")


# on_field_subset<- on_field_residue_list[grep(c("IMIDACLOPRID|BIFENTHRIN"), names(on_field_residue_list))]
# on_field_subset <- lapply(on_field_subset, transform, ID="On-field")
# 
# off_field_subset<- off_field_residue_list[grep(c("IMIDACLOPRID|BIFENTHRIN"), names(off_field_residue_list))]
# off_field_subset <- lapply(off_field_subset, transform, ID="Off-field")



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


#function to extract legend 
get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 

options(scipen=0)

#### Comparison of application type residues ----
# read in residues from DER
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
residues$Source<-"Reported"
residues$Location<-"On-field"


# read in residues from lit review
litres<-read.csv(paste0(pest_dir,"/Residues/Lit_ResidueData_Ethan.csv"))

litres<-litres[,c(1:5)]
litres<-na.omit(litres)
litres$Commodity<-"Other"
litres$Source<-"Reported"

litres<-litres[,c(1,6,2,3,5,7,4)]
colnames(litres)<-colnames(residues)
litres$Compound<-toupper(litres$Compound)

residues<-rbind(residues,litres)
residues<-residues[!residues$Commodity =="COTTON",]

# get the estimated data
combine_all<-na.omit(combine_all)
combine_all$Source<-"Estimated"

names(combine_all)
names(residues)

combine_all<-combine_all[,c(2:4,6:9)]
colnames(combine_all)[4]<-"Residues_ugg"

residues<-residues[,c(1:2,7,4:5,3,6)]
#residues<-residues[residues$Compound == "BIFENTHRIN" |residues$Compound == "IMIDACLOPRID",  ]
names(residues)[3]<-"ID"

#combine all together
combine_all<- rbind(combine_all,residues)

names(combine_all)
names(residues)


forcomparisons<-combine_all[combine_all$MediaSub == "Pollen" | combine_all$MediaSub == "Nectar"| combine_all$MediaSub == "Soil", ]
forcomparisons<-na.omit(forcomparisons)
compare_outputs<-split(forcomparisons, list(forcomparisons$Compound), drop=T)



library(RColorBrewer)
myColors <- brewer.pal(3,"Dark2")
names(myColors) <- unique(combine_all$ApplicationType)
colScale <- scale_colour_manual(name = "ApplicationType",values = myColors)
fillScale <- scale_fill_manual(name = "ApplicationType",values = myColors)

x<-compare_outputs[[7]]

create_plot_of_methodcompare<-function(x){
  df<-x

  df$Value<-(as.numeric(df$Residues_ugg))
  df$IDf= factor(df$ID, levels=c('On-field','Off-field'))

  
  df<-df[!df$Value < 3e-5,]

  out<-ggplot(df, aes(ApplicationType, log(Value),color=ApplicationType, fill=ApplicationType)) +
    geom_boxplot()+
    colScale+
    fillScale+
    #scale_y_continuous(breaks=ticks, labels=format(logticks,3))+
   # facet_grid(rows = vars(MediaSub), cols = vars(IDf), scales="free_y")+

    ggh4x::facet_nested(MediaSub ~ IDf + Source, scales="free_y")+

    ylab("Log Residues [ug/g]")+
    xlab("Application Type")+
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


#### Daily Time-step organized by on/off-field ----
combine_all$ID<-factor(combine_all$ID,levels=c("On-field","Off-field"))

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
myColors <- brewer.pal(7,"Dark2")
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

  scale_x_continuous(limits=c(0, 150), breaks=seq(0,150, by=20))+
  facet_wrap(~Commodity + ID,scales = "free", nrow=2)+
  #facet_grid(rows = vars(Commodity), cols = vars(ID), scales="free")+
  ylab("Residues [ug/g]")+
  xlab("Days Post-Application")+
  theme_bw()+
  theme()+
  theme(legend.position="none",axis.text.y = element_text(size=12,face="bold"), 
        axis.text.x = element_text(size=12,face="bold"), 
        axis.title=element_text(size=14,face="bold"),
        plot.background = element_rect(color = "black"))
soiln


pollenn<- ggplot(pollen_df, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  facet_wrap(~Commodity + ID,scales = "free", nrow=2)+
  #facet_grid(rows = vars(Commodity), cols = vars(ID), scales="free")+
  ylab("")+
  xlab("Days After Pollen Emergence")+
  theme_bw()+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"), 
        axis.text.x = element_text(size=12,face="bold"),  
        axis.title=element_text(size=14,face="bold"),
        plot.background = element_rect(color = "black"))
pollenn

c<-seq(67,150, by=10)
s<-seq(56,140, by=10)

  pollenn<-pollenn + facetted_pos_scales(
  x = list(
  Commodity == "CORN" & ID == "On-field" ~ scale_x_continuous(limits=c(67, 150), breaks=c,labels =c("0","10","20","30","40","50","60","70","80")),
  Commodity == "SOYBEANS" & ID == "On-field"~ scale_x_continuous(limits=c(56, 140),breaks=s,labels =c("0","10","20","30","40","50","60","70","80"))
   )
  )


nectarn<- ggplot(nectar_df, aes(day, Value, color=Compound)) +
  geom_line(aes(linetype = ApplicationType), size=1)+
  colScale+
  facet_wrap(~Commodity + ID,scales = "free", nrow=3)+
  #facet_grid(rows = vars(Commodity), cols = vars(ID), scales="free_y")+
  ylab("")+
  xlab("Days After Nectar Emergence")+
  theme_bw()+
  theme(legend.position="none", axis.text.y = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=12,face="bold"),  
        axis.title=element_text(size=14,face="bold"),
        plot.background = element_rect(color = "black"))
nectarn

c<-seq(67,150, by=10)
s<-seq(56,140, by=10)

nectarn<-nectarn + facetted_pos_scales(
  x = list(
    
    Commodity == "CORN" & ID == "On-field" ~ scale_x_continuous(limits=c(67, 150), breaks=c,labels =c("0","10","20","30","40","50","60","70","80")),
    Commodity == "SOYBEANS" & ID == "On-field"~ scale_x_continuous(limits=c(56, 140),breaks=s,labels =c("0","10","20","30","40","50","60","70","80"))
  )
)


compare_between_media<-plot_grid(
  soiln,
  pollenn,
  nectarn, 
  labels = c('Soil','Pollen','Nectar'),
  
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow = 1,rel_widths = c(4,4,3))
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

compare_between_media<-plot_grid(compare_between_media, legend,ncol = 2, rel_widths = c(7,1))
compare_between_media



#### Daily time-step organized by chemical ----

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(3,"Dark2")
names(myColors) <- unique(combine_all$ApplicationType)
colScale <- scale_colour_manual(name = "Application Type",values = myColors)

mylines <- c("solid", "dotted")
names(mylines) <- unique(combine_all$ID)
lineScale <- scale_linetype_manual(values = mylines,name = "Location",)


combine_all$ID<-factor(combine_all$ID,levels=c("On-field","Off-field"))
#combine_all<-left_join(combine_all,beetox[,c(1:3)])
combine_all<-combine_all[with(combine_all, order(Compound,Commodity)), ] 
by_commodity<-split(combine_all, list(combine_all$Commodity), drop=T) 


#x<-by_commodity[[2]]
plot_all_applications_by_timeseries<-function(x){
  
  x<-na.omit(x)
 # x<-x[!x$Value < 3e-5,] #set level of detection of 0.03 ng/g
  x<-x[x$MediaSub == "Pollen" | x$MediaSub == "Nectar"| x$MediaSub == "Soil", ]
  #x$ApplicationType<-as.factor(x$ApplicationType,levels=c("Foliar","Seed","Soil"))
    
  # x$day<- ifelse(x$Commodity == "CORN"  & x$ID == "On-field" & x$MediaSub == "Pollen", x$day-67,x$day )
  # x$day<- ifelse(x$Commodity == "SOYBEANS" & x$ID == "On-field" & x$MediaSub == "Pollen" | x$Commodity == "SOYBEANS"  & x$ID == "On-field" & x$MediaSub == "Nectar",x$day-56,x$day )

  #here, we need to change the dates to reflect some x number of days post-planting of crop
  x$day<- ifelse(x$Commodity == "CORN" & x$ApplicationType== "Foliar" & x$ID == "Off-field" & x$MediaSub == "Pollen", x$day+67,x$day )
  x$day<- ifelse(x$Commodity == "CORN" & x$ApplicationType== "Foliar" & x$ID == "Off-field" & x$MediaSub == "Nectar", x$day+67,x$day )
  x$day<- ifelse(x$Commodity == "CORN" & x$ApplicationType== "Foliar" & x$ID == "Off-field" & x$MediaSub == "Soil", x$day+67,x$day )
  
  x$day<- ifelse(x$Commodity == "SOYBEANS" & x$ApplicationType== "Foliar" & x$ID == "Off-field" & x$MediaSub == "Pollen" ,x$day+56,x$day )
  x$day<- ifelse(x$Commodity == "SOYBEANS" & x$ApplicationType== "Foliar" & x$ID == "Off-field" & x$MediaSub == "Nectar", x$day+56,x$day )
  x$day<- ifelse(x$Commodity == "SOYBEANS" & x$ApplicationType== "Foliar" & x$ID == "Off-field" & x$MediaSub == "Soil", x$day+56,x$day )
  
  x$day<- ifelse(x$Commodity == "SOYBEANS" & x$ApplicationType== "Foliar" & x$ID == "On-field" & x$MediaSub == "Soil", x$day+56,x$day )
  x$day<- ifelse(x$Commodity == "CORN" & x$ApplicationType== "Foliar" & x$ID == "On-field" & x$MediaSub == "Soil", x$day+67,x$day )
  
  x$MediaSub<-factor(x$MediaSub,levels=c("Soil","Pollen","Nectar"))
  
  #surface area of a bumblebee
  SA<-2.216 #cm2
  
  # depo<-x[x$MediaSub == "Air" |x$MediaSub == "Dust" , ]
  # micro<-x[x$MediaSub == "Pollen" | x$MediaSub == "Nectar"| x$MediaSub == "Soil", ]
  # 
  # depo$EEC<-((depo$Value/10000)*SA)# convert to cm2, multiple by surface area of bumblebee to get eec in ug/bee, and divide by contact LD50
  # micro$EEC<-ifelse(micro$MediaSub == "Soil", (micro$Value/10000*SA), (micro$Value * IR))
  # x<-rbind(micro,depo)
  
  
  x<- x%>% mutate(EEC = case_when(MediaSub == "Soil" ~ Value/10000*SA,
                         MediaSub == "Air" || MediaSub == "Dust"~ Value/10000*SA,
                         MediaSub == "Nectar" ~ Value * 0.400, #ingestion rate foraging bee
                         TRUE ~ Value*0.030))

  split_by_commodity<-split(x,list(x$Compound), drop=T)
  all<-seq(0,150,by=25)
  #crop<-split_by_commodity[[5]]

 plot_base_plots<- function(crop){
    
  output<- ggplot(crop) +
    geom_line(aes(day, (EEC), linetype=ID, color=ApplicationType),size=1.2)+
    facet_nested_wrap(~MediaSub, scales="free",nrow=1)+
   # facet_grid(~MediaSub+Commodity)+
    scale_x_continuous(limits=c(0, 150),breaks=all)+
    #scale_x_continuous(limits=c(0, 80), breaks=all,labels =c("0","10","20","30","40","50","60","70","80"))+
    #facet_grid(rows = vars(ID), cols = vars(Commodity), scales="free_y")+
    ylab("")+
    xlab("")+
    # xlab(ifelse(crop$Commodity == "SOYBEANS","Days After Planting",""))+
    # geom_hline(yintercept = crop$Contact_LD50_ug_bee, col="red", linetype="longdash")+
    # geom_hline(yintercept = crop$Oral_LD50_ug_bee, col="darkred", linetype="longdash")+
    # geom_label(aes(x = 60, y = crop$Contact_LD50_ug_bee, label = paste0("Contact LD50")), size= 3, col='black')+
    # geom_label(aes(x = 20, y = crop$Oral_LD50_ug_bee, label = paste0("Oral LD50")), size= 3, col='black')+
    theme_bw()+
    theme(legend.position="none",axis.text.y = element_text(size=12,face="bold"),
          axis.text.x = element_text(size=12,face="bold", angle=45, vjust=0.9),  
          axis.title=element_text(size=14,face="bold"))+
          # plot.background = element_rect(color = "black"))+
    ggtitle(paste0(str_to_title(crop$Compound))) +
    colScale +
    lineScale
   
  output
  }
  
  # c<-seq(67,150, by=10)
  # s<-seq(56,140, by=10)
  # all<-seq(0,80,by=10)
  
 # output<-output + facetted_pos_scales(
 #    x = list(
 #      ID =="Off-field" ~ scale_x_continuous(limits=c(0, 80), breaks=all,labels =c("0","10","20","30","40","50","60","70","80")),
 #      MediaSub =="Soil" ~ scale_x_continuous(limits=c(0, 80), breaks=all,labels =c("0","10","20","30","40","50","60","70","80")),
 #      Commodity == "CORN" & ID == "On-field" &  MediaSub == "Pollen"  ~ scale_x_continuous(limits=c(67, 150), breaks=c,labels =c("0","10","20","30","40","50","60","70","80")),
 #      Commodity == "SOYBEANS" & ID == "On-field" &  MediaSub == "Pollen"~ scale_x_continuous(limits=c(56, 140),breaks=s,labels =c("0","10","20","30","40","50","60","70","80")),
 #      Commodity == "SOYBEANS" & ID == "On-field" &  MediaSub == "Nectar"~ scale_x_continuous(limits=c(56, 140),breaks=s,labels =c("0","10","20","30","40","50","60","70","80"))
 #      
 #    ) ) +
 
  


  rel_plots<-lapply(split_by_commodity,plot_base_plots)
  final_plots<-ggarrange(plotlist = rel_plots,nrow= 3, ncol=2)

  all_plot_legend <- ggplot(x) +
    geom_line(aes(day, Value, linetype=ID, color=ApplicationType),size=1.5)+
    facet_wrap(~Commodity,scales = "free", nrow=1)+
    ylab("Residues")+
    xlab("Day")+
    theme_bw()+
    theme( legend.key.size = unit(1, 'cm'), #change legend key size
           legend.key.height = unit(1, 'cm'), #change legend key height
           legend.key.width = unit(1, 'cm'), #change legend key width
           legend.title = element_text(size=16), #change legend title font size
           legend.text = element_text(size=14))+
    colScale +
    lineScale
  legend<-get_only_legend(all_plot_legend)
  
  #plot_grid(final_plots,legend)
  
  final<-plot_grid(final_plots,nrow=1, rel_widths = c(7))
  final
  
}


supplementary_daily_plots<-lapply(by_commodity, plot_all_applications_by_timeseries)

warnings()

# daily_time_step<-plot_grid(supplementary_daily_plots[[1]],
#                  supplementary_daily_plots[[2]],
#                  # supplementary_daily_plots[[3]],
#                  # supplementary_daily_plots[[4]],
#                  # supplementary_daily_plots[[5]],
#                  # supplementary_daily_plots[[6]],
#                  nrow=2, rel_widths = c(5,5))



daily_time_step_corn<-plot_grid(supplementary_daily_plots[[1]], legend, rel_widths = c(5,1))
daily_time_step_soy<-plot_grid(supplementary_daily_plots[[2]], legend, rel_widths = c(5,1))

y.grob <- textGrob("Rate-Adjusted EEC [ug/bee]", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Days Post-Planting", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))

grid.arrange(arrangeGrob(daily_time_step_corn, left = y.grob, bottom = x.grob))

grid.arrange(arrangeGrob(daily_time_step_soy, left = y.grob, bottom = x.grob))






#### Hazard quotients/Concentrations----
#### Hazard quotient info
beetox <-read.csv(paste0(pest_dir, "/BeeTox.csv"))
beetox$Compound<-toupper(beetox$Compound)


### First we need to get the EPA specific values for each compound for nectar/pollen/soil

#### Nectar/Pollen
#seed
Seed<-1 #mg/kg

#soil
koc <-read.csv(paste0(pest_dir, "/Models/Koc.csv"))
koc$Compound<-toupper(koc$Compound)
TSCF<-Li[,c(1,18)]
TSCF<-merge(TSCF,koc)
TSCF$TSCF<- 0.7*exp(-((TSCF$logKow-3.07)^2/2.44))  
TSCF$Soil<-( (10^(0.95*TSCF$logKow-2.05)+0.82) * TSCF$TSCF *(1.95/(0.2+1.95*TSCF$Koc*0.01)) )

#foliar
Foliar<-36 #mg/kg

epa_value<-TSCF[,c(1,5)]
epa_value$Foliar<-Foliar
epa_value$Seed<-Seed
epa_value<-gather(epa_value, "ApplicationType", "ug_g", 2:ncol(epa_value))

epa_value$Media<-"Nectar"

epa_value<-rbind(epa_value,epa_value)
epa_value[25:48,4]<-"Pollen"
epa_value$Commodity<-"CORN"

epa_value<-rbind(epa_value,epa_value)
epa_value[48:96,5]<-"SOYBEANS"


soil_conc<-epa_value[c(1:8,48:56),]
soil_conc$Media<-"Soil"

epa_value<-rbind(epa_value,soil_conc)

conc_soil<-apprates[,c(1,3,5,6,12)]
foliar_soil<-unique(conc_soil[conc_soil$ApplicationType == "FoliarI" |conc_soil$ApplicationType == "FoliarH", ])
foliar_soil$ApplicationType<-"Foliar"

foliar_soil$ug_g<-((foliar_soil$AvgRate*1.12085)/15)*exp(-foliar_soil$k_values*7)
foliar_soil$Media<-"Soil"
foliar_soil<-foliar_soil[,c(1,3:4,6:7)]

epa_value<-rbind(epa_value, foliar_soil)

seed_soil<-unique(conc_soil[conc_soil$ApplicationType == "Seed" , ])
seed_soil$ApplicationType<-"Seed"

seed_soil$ug_g<-((seed_soil$AvgRate*1.12085)/15)*exp(-seed_soil$k_values*7)
seed_soil$Media<-"Soil"
seed_soil<-seed_soil[,c(1,3:4,6:7)]
epa_value<-rbind(epa_value, seed_soil)


names(epa_value)[3]<-"EPA"
names(epa_value)[4]<-"MediaSub"
epa_value$ID<-"On-field"



combine_all<-na.omit(combine_all)
#surface area of a bumblebee
SA<-2.216 #cm2

#combine with beetox and set units
combine_all$units<-ifelse(combine_all$MediaSub == "Dust" | combine_all$MediaSub == "Air", "ug/m2","ug/g" )
combine_all<-left_join(combine_all,beetox[,c(1:3)])
#combine_all<-combine_all[with(combine_all, order(Compound,Commodity)), ] 
#organize endpoints
combine_all<- gather(combine_all, "ExposureLevel", "Endpoint", 10:11)

testy<-left_join(combine_all,epa_value)
combine_all<-testy


#customize colors
library(RColorBrewer)
myColors <- brewer.pal(5,"Dark2")
names(myColors) <- unique(combine_all$MediaSub)
colScale <- scale_colour_manual(name = "MediaSub",values = myColors)
fillScale <- scale_fill_manual(name = "MediaSub",values = myColors)

#split according to application type first
split_datasets_for_HQ<-split(combine_all, list(combine_all$ApplicationType), drop=T) 
#x<-split_datasets_for_HQ[[1]]

create_plot_of_rq<-function(x){
  df<-x
 # df<-na.omit(df)
  

  #calculate ingestion/contact based eecs
df<- df %>% mutate(EEC = case_when(MediaSub == "Soil" ~ Value/10000*SA,
                        MediaSub == "Air" || MediaSub == "Dust"~ Value/10000*SA,
                        MediaSub == "Nectar" ~ Value * 0.400,
                        TRUE ~ Value*0.030))


#remove rows where the condition for concentration is expressed twice
  df<-df[!(  df$MediaSub == "Air" & df$ExposureLevel == "Oral_LD50_ug_bee" |
             df$MediaSub == "Dust" & df$ExposureLevel == "Oral_LD50_ug_bee" |
             df$MediaSub == "Soil" & df$ExposureLevel == "Oral_LD50_ug_bee"|
             df$MediaSub == "Nectar" & df$ExposureLevel == "Contact_LD50_ug_bee"|
             df$MediaSub == "Pollen" & df$ExposureLevel == "Contact_LD50_ug_bee") ,]
  
  
  
  #df$endpoint_final<-df$EEC/(df$Endpoint/10)
  
  #split by crop
  df_list<-split(df, list(df$Commodity), drop=T)
 # crop<-df_list[[2]]

  plot_by_crop<-function(crop){
    
    # ticks <- (seq(-35,15, by=10))
    # logticks <- exp(ticks)
    
  #create plot
  out<-ggplot(crop) +
    geom_boxplot(aes(MediaSub, (EEC), color=MediaSub, fill=MediaSub),show.legend = FALSE)+
    geom_point(aes(x=MediaSub, y=(Endpoint)), pch=4,size=3,shape=4, fill="darkred")+
    geom_point(aes(x=MediaSub, y=(EPA)),pch=21,size=3,shape=1,fill="darkblue")+
   
    scale_y_continuous(trans=scales::log_trans(),
                       labels = scales::format_format())+

    facet_nested_wrap(~Compound +ID,nrow=1)+
    colScale+
    fillScale+
    #ylab(ifelse(df$ApplicationType == "Foliar","Hazard Quotient", ""))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(paste0(crop$ApplicationType,"-", crop$Commodity)) +
    theme(legend.position="right",axis.title=element_text(size=14,face="bold"),axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"), axis.title.y=element_blank(), axis.title.x=element_blank())
  out
  
  }
  
  output_by_crop<-lapply(df_list, plot_by_crop)
  
  final_plots<-ggpubr::ggarrange(plotlist = output_by_crop, ncol=1)
  
 
  # all_plot_legend <- 
  #   ggplot(df) +
  #   geom_boxplot(aes(MediaSub, log(EEC), color=MediaSub, fill=MediaSub))+
  #   geom_point(aes(x=MediaSub, y=log(Endpoint)), color = 'darkred')+
  #   #geom_label(aes(x=Compound, y=log(Endpoint), label = paste0("Acute LD50")), size= 2, col='black')+
  #   facet_nested_wrap(~Compound +ID,nrow=1)+
  #   # facet_grid(~ MediaSub  + ID)+
  #   #facet_wrap(~Compound, scales = "free", nrow=1)+
  #   colScale+
  #   fillScale+
  #   scale_y_continuous(limits=c(-30,5), breaks=seq(-30,5, by=10))+
  #   #ylab(ifelse(df$ApplicationType == "Foliar","Hazard Quotient", ""))+
  #   theme_bw()+
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #   ggtitle(paste0(crop$ApplicationType,"-", crop$Commodity)) +
  #   theme(legend.position="none",axis.title=element_text(size=14,face="bold"),axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"), axis.title.y=element_blank(), axis.title.x=element_blank())
  # 
  # legend<-get_only_legend(all_plot_legend)
  

  final_plots

}


list_of_plots<-lapply(split_datasets_for_HQ,  create_plot_of_rq)

compare_risk_top<-plot_grid(
  list_of_plots[[1]],
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(4))
compare_risk_top


compare_risk_bottom<-plot_grid(
  list_of_plots[[2]],
  list_of_plots[[3]],
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(3,2))
compare_risk_bottom

final<-plot_grid(compare_risk_top,compare_risk_bottom, hjust=0, vjust=0,  align= "h",ncol=2)

y.grob <- textGrob("Rate-Adjusted EEC [ug/bee]", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Media Type", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))

grid.arrange(arrangeGrob(final, left = y.grob, bottom = x.grob))

# legend_plot<-ggplot(eec_df, aes(MediaSub, Value, fill=MediaSub)) +
#   geom_hline(yintercept=log(0.4))+
#   geom_boxplot() +
#   #geom_point( size=3.2, aes(shape=MediaSub))+
#   # scale_y_continuous(limits=c(-30,5), breaks=seq(-30,5, by=10))+
#   
#   facet_wrap(Commodity ~ Compound, 
#              scales="free_y",nrow=2)+
#   #facet_wrap(~Compound, scales = "free", nrow=1)+
#   ylab("Risk Quotient")+
#   xlab("Compound")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         legend.key.size = unit(1, 'cm'), #change legend key size
#         legend.key.height = unit(1, 'cm'), #change legend key height
#         legend.key.width = unit(1, 'cm'), #change legend key width
#         legend.title = element_text(size=16), #change legend title font size
#         legend.text = element_text(size=14))+
#   #ggtitle(paste0(x$ApplicationType)) +
#   guides(fill=guide_legend(title="Media Type"))
# legend_plot
# 
# legend_plot<-ggplot(crop) +
#   geom_boxplot(aes(Compound, log(EEC), color=Compound, fill=Compound))+
#   geom_point(aes(x=Compound, y=log(Endpoint)), color = 'darkred')+
#   scale_color_manual
#   #geom_label(aes(x=Compound, y=log(Endpoint), label = paste0("Acute LD50")), size= 2, col='black')+
#   facet_nested_wrap(~MediaSub +ID,nrow=1)+
#   # facet_grid(~ MediaSub  + ID)+
#   #facet_wrap(~Compound, scales = "free", nrow=1)+
#   scale_y_continuous(limits=c(-30,5), breaks=seq(-30,5, by=10))+
#   #ylab(ifelse(df$ApplicationType == "Foliar","Hazard Quotient", ""))+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   ggtitle(paste0(crop$ApplicationType,"-", crop$Commodity)) +
#   theme(axis.title=element_text(size=14,face="bold"),axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"), axis.title.x=element_blank())
# legend_plot
# 
# 
# legend<-get_only_legend(legend_plot)
# 
# compare_RQ<-plot_grid(compare_RQ, legend,nrow=1, rel_widths = c(6.5,0.5))
# compare_RQ










#### Compare against field estimates ----
# read in residues from DER
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
residues$Source<-"Empirical"
residues$Location<-"On-field"


# read in residues from lit review
litres<-read.csv(paste0(pest_dir,"/Residues/Lit_ResidueData_Ethan.csv"))

litres<-litres[,c(1:5)]
litres<-na.omit(litres)
litres$Commodity<-"Other"
litres$Source<-"Empirical"

litres<-litres[,c(1,6,2,3,5,7,4)]
colnames(litres)<-colnames(residues)
litres$Compound<-toupper(litres$Compound)

residues<-rbind(residues,litres)
residues<-residues[!residues$Commodity =="COTTON",]

# get the estimated data
combine_all<-na.omit(combine_all)
combine_all$Source<-"Estimated"

names(combine_all)
names(residues)

combine_all<-combine_all[,c(2:4,6:9)]
colnames(combine_all)[4]<-"Residues_ugg"

residues<-residues[,c(1:2,7,4:5,3,6)]
#residues<-residues[residues$Compound == "BIFENTHRIN" |residues$Compound == "IMIDACLOPRID",  ]
names(residues)[3]<-"ID"

#combine all together
combine_all<- rbind(combine_all,residues)
combine_all$ID<-factor(combine_all$ID,levels=c("On-field","Off-field"))
residues_all<-combine_all


#testy<-residues_all[residues_all$Source == "Reported",]

list_by_applciationtype<-split(residues_all, f= residues_all$ApplicationType)
#x<-list_by_applciationtype[[2]]

create_plot_of_fieldcompare<-function(x){
  df<-x
  #df<-df[!df$Commodity == "Other",]
  df<-df[!c(df$MediaSub == "Dust" | df$MediaSub == "Air"), ]
  
  df<-df %>%
    group_by(MediaSub, Compound, ID )%>%
    filter(Residues_ugg > 3e-5) %>%
    filter(n_distinct(Source) > 1)
  
  list_by_media_sub<-split(df,list(df$MediaSub))
  mediasub<-list_by_media_sub[[1]]

  compare_by_mediasub<-function(mediasub){

ticks <- (c(-10,-8,-6,-4,-2,0,2))
logticks <- exp(ticks)

  out<-ggplot(mediasub, aes(Source, log(Residues_ugg),fill=Source)) +
    geom_boxplot() +
    geom_hline(yintercept=log(3e-5))+
    colScale+
    # scale_y_continuous(trans=scales::log_trans(),
    #                    labels = scales::format_format())+
    scale_y_continuous(breaks=ticks, labels=format(as.numeric(logticks),scientific=T, digits=3))+
    #scale_y_continuous(breaks=c(ticks,logticks), labels=c(ticks,exp(logticks)))+
    #facet_nested_wrap(~ApplicationType + Compound +ID,scales="free")+
    ggh4x:: facet_nested(.~ID+Compound, scales="free_y")+
    #facet_wrap(~Compound, scales = "free", nrow=1)+
    ylab("")+
   xlab("")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(paste0(mediasub$ApplicationType," ","Application", "-", mediasub$MediaSub)) +
    theme(legend.position="none", axis.text.y = element_text(size=14,face="bold"), axis.text.x = element_text(size=14,face="bold"),  axis.title=element_text(size=14,face="bold"))
  out
  }
  
  experimental_plots<-lapply(list_by_media_sub,compare_by_mediasub)
  final_plots<-ggpubr::ggarrange(plotlist = experimental_plots, nrow=1)
  final_plots
}


list_of_estimate_plots<-lapply(list_by_applciationtype,create_plot_of_fieldcompare)

compare_field_est<-
  
plot_grid(

  list_of_estimate_plots[[1]],
  list_of_estimate_plots[[2]],
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=2, ncol=1, rel_widths = c(6,6))






y.grob <- textGrob("Rate-Adjusted EEC", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("", 
                   gp=gpar(fontface="bold", col="black", fontsize=0))

grid.arrange(arrangeGrob(compare_field_est, left = y.grob, bottom = x.grob))

list_of_estimate_plots[[3]]


#### Colony life-cycle example - this will be tabled until chapter 3 ----

#corn
day<-c(0,30,45,90,120)
event<-c("Queen Foraging and Brood Established","Females Emerge and Forage","Queen Remains in Colony, Males and Females Forage","Peak Foraging Activity","Gynes and Males Begin to Depart")

#soy
day<-c(15,30,75,90,120)
event<-c("Females Emerge and Forage","Queen Remains in Colony, Males and Females Forage","Peak Foraging Activity","Gynes and Males Begin to Depart", "Queen Dies")
imp_event<-data.frame(day,event)

soil_dft<-soil_df[soil_df$Commodity == "CORN" & soil_df$ID == "On-field",]
soil_dft<-soil_dft %>% left_join(imp_event, by = "day")


soiln<- ggplot(soil_dft) +
  geom_line(aes(day, (Value), color=Compound, linetype = ApplicationType), size=1)+
  colScale+



  geom_vline(data = soil_dft %>% filter(!is.na(event)), 
             aes(xintercept = day), color = "gray70",  linetype = "dashed") +   
  

  ggrepel::geom_text_repel(data = imp_event, aes(x = day, y = 0.02, label = str_wrap(event, 10)), 
                           color = "black", direction = "y", size = 3.5, lineheight = 0.9, point.padding = 0.8)+
  
  
  scale_x_continuous(limits=c(0, 150), breaks=seq(0,150, by=20))+
  ylab("Residues [ug/g]")+
  xlab("Days Post-Application")+
  theme_bw()+
  theme(legend.position="none",axis.text.y = element_text(size=12,face="bold"), 
        axis.text.x = element_text(size=12,face="bold"), 
        axis.title=element_text(size=14,face="bold"),
        plot.background = element_rect(color = "black"))
soiln


# ### Get bee activity data 
# bombus_data<-read.csv(paste0(bombus_dir,"/bombus_ILL.csv"))
# hist(as.numeric(bombus_data$Month))


