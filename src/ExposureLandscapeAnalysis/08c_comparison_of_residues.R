### Probabilistic Crop Loading 

### 08c Comparison of on and off-field residues 

on_field_subset<- on_field_residue_list[grep(c("IMIDACLOPRID|BIFENTHRIN|CARBARYL|GLYPHOSATE|CHLORPYRIFOS|CLOTHIANIDIN|THIAMETHOXAM"), names(on_field_residue_list))]
on_field_subset <- lapply(on_field_subset, transform, ID="On-field")

off_field_subset<- off_field_residue_list[grep(c("IMIDACLOPRID|BIFENTHRIN|CARBARYL|GLYPHOSATE|CHLORPYRIFOS|CLOTHIANIDIN|THIAMETHOXAM"), names(off_field_residue_list))]
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

#for ease of visuals...remove thia as corn

seed_data<-seed_data[!(seed_data$Compound == "THIAMETHOXAM" & seed_data$Commodity == "CORN"),]

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

#imida<-foliar_data[foliar_data$Compound == "IMIDACLOPRID",]

combine_all<-rbind(foliar_data,soil_data,seed_data)


#get depo table
#surface area of a bumblebee
SA<-5 #cm2
depo<-combine_all %>% filter(MediaSub == "Air" | MediaSub == "Dust" ) %>% filter(!is.na(Value)) %>% mutate(contactexp =  (Value *SA)/10000 )
write.csv(depo,paste0(pest_dir,"/depositiontable.csv"))


#function to extract legend 
get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 

options(scipen=0)


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
  SA<-5 #cm2/bee
  
  # depo<-x[x$MediaSub == "Air" |x$MediaSub == "Dust" , ]
  # micro<-x[x$MediaSub == "Pollen" | x$MediaSub == "Nectar"| x$MediaSub == "Soil", ]
  # 
  # depo$EEC<-((depo$Value/10000)*SA)# convert to cm2, multiple by surface area of bumblebee to get eec in ug/bee, and divide by contact LD50
  # micro$EEC<-ifelse(micro$MediaSub == "Soil", (micro$Value/10000*SA), (micro$Value * IR))
  # x<-rbind(micro,depo)
  
  
  
  
  #value = ug/g OR ug/m2 if deposition
  
  x<- x%>% mutate(EEC = case_when(MediaSub == "Soil" ~ Value,
                         MediaSub == "Air" || MediaSub == "Dust"~ (Value/10000)*SA,
                         MediaSub == "Nectar" ~ Value * 0.400, #ingestion rate foraging bee
                         TRUE ~ Value*0.030))
  
  
  
  


  split_by_commodity<-split(x,list(x$Compound), drop=T)
  all<-seq(0,150,by=25)
  #crop<-split_by_commodity[[5]]

 plot_base_plots<- function(crop){
    
  output<- ggplot(crop) +
    geom_line(aes(day, (Value), linetype=ID, color=ApplicationType),size=1.2)+
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

y.grob <- textGrob("Residue Concentrations [ug/g]", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Days Post-Planting", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))

grid.arrange(arrangeGrob(daily_time_step_corn, left = y.grob, bottom = x.grob))

grid.arrange(arrangeGrob(daily_time_step_soy, left = y.grob, bottom = x.grob))






#### Hazard quotients/Concentrations----
#### Hazard quotient info
beetox <-read.csv(paste0(pest_dir, "/BeeTox.csv"))
beetox$Compound<-toupper(beetox$Compound)

#### App rates and timing:
apprates <-read.csv(paste0(pest_dir, "/AppRates.csv"))
apprates$Compound<-toupper(apprates$Compound)
apprates$k_values<-log((apprates$AvgRate/2)/apprates$AvgRate)/-(apprates$k_values)

#apprates<-crossing(apprates, type=c("Pollen","Nectar"))

### We want to compare the EECs we've simulated against the EPA specific EEC estimation methods for day 1
#there are a number of approaches the EPA uses to estimate EEC based on application type. We'll divide by type first

foliar<-apprates %>% filter(ApplicationType == "FoliarI" | ApplicationType == "FoliarH" )
seed<-apprates %>% filter(ApplicationType == "Seed"  )
soil<-apprates %>% filter(ApplicationType == "Soil"  )

## calculate contact and diet EEC foliar
foliar$Contact<-foliar$AvgRate*2.7
foliar$Diet<-foliar$AvgRate*110*0.292
foliar$ApplicationType<-"Foliar"
foliar<-foliar[,c(1,3,5,6,13:14)]
foliar<-gather(foliar, "Route", "EEC", 5:ncol(foliar))
foliar$Media<-ifelse(foliar$Route == "Contact", "Air","Pollen")
foliar<-unique(foliar)
foliar<-rbind(foliar,foliar[11:20,])
foliar[21:30,7]<-"Nectar"

#for concentrations in soil, I used the etterson reference and calculated a bee surface area value
# foliar<-rbind(foliar,foliar[21:30,])
# foliar[31:40,8]<-"Soil"
# foliar[31:40,7]<-((foliar[31:40,2]*1.12085)/15)*exp(-foliar[31:40,5]*7)/10000*2.216

names(foliar)

## calculate diet EEC seed
seed$Route<-"Diet"
seed$EEC<-1*0.292
seed$Media<-"Pollen"
seed<-seed[,c(1,3,5,6,13,14,15)]
seed<-rbind(seed,seed[1:6,])
seed[7:12,7]<-"Nectar"

## calculate diet EEC soil  
koc <-read.csv(paste0(pest_dir, "/Models/Koc.csv"))
koc$Compound<-toupper(koc$Compound)
TSCF<-Li[,c(1,18)]
TSCF<-merge(TSCF,koc)
TSCF$TSCF<- 0.7*exp(-((TSCF$logKow-3.07)^2/2.44))  
TSCF$EEC<-( (10^(0.95*TSCF$logKow-2.05)+0.82) * TSCF$TSCF *(1.95/(0.2+1.95*TSCF$Koc*0.01)) )
TSCF<-TSCF[,c(1,5)]
soil<-merge(soil,TSCF)
soil$Route<-"Diet"
soil$Media<-"Pollen"
soil<-soil[,c(1,3,5,6,14,13,15)]
soil<-rbind(soil,soil[1:2,])
soil[3:4,7]<-"Nectar"

names(foliar)
names(soil)
names(seed)

epa_value<-rbind(foliar,seed,soil)
epa_value<-epa_value[,c(1,3,4,6,7)]



##################Old code for soil calculations
# conc_soil<-apprates[,c(1,3,5,6,12)]
# foliar_soil<-unique(conc_soil[conc_soil$ApplicationType == "FoliarI" |conc_soil$ApplicationType == "FoliarH", ])
# foliar_soil$ApplicationType<-"Foliar"
# 
# foliar_soil$ug_g<-((foliar_soil$AvgRate*1.12085)/15)*exp(-foliar_soil$k_values*7)
# foliar_soil$Media<-"Soil"
# foliar_soil<-foliar_soil[,c(1,3:4,6:7)]
# 
# epa_value<-rbind(epa_value, foliar_soil)
# 
# seed_soil<-unique(conc_soil[conc_soil$ApplicationType == "Seed" , ])
# seed_soil$ApplicationType<-"Seed"
# 
# seed_soil$ug_g<-((seed_soil$AvgRate*1.12085)/15)*exp(-seed_soil$k_values*7)
# seed_soil$Media<-"Soil"
# seed_soil<-seed_soil[,c(1,3:4,6:7)]
# epa_value<-rbind(epa_value, seed_soil)

names(epa_value)[4]<-"EPA"
names(epa_value)[5]<-"MediaSub"
epa_value$ID<-"On-field"



combine_all<-na.omit(combine_all)
#surface area of a bumblebee
SA<-5 #cm2

#combine with beetox and set units
combine_all$units<-ifelse(combine_all$MediaSub == "Dust" | combine_all$MediaSub == "Air", "ug/m2","ug/g" )
combine_all<-left_join(combine_all,beetox[,c(1:3)])
#combine_all<-combine_all[with(combine_all, order(Compound,Commodity)), ] 
#organize endpoints
combine_all<- gather(combine_all, "ExposureLevel", "Endpoint", 10:11)

testy<-left_join(combine_all,epa_value)
combine_all<-testy

##read in HC5 data from cassandra; this represents fractions of SSDs 

hc5 <-read.csv(paste0(pest_dir, "/HC5.csv"))
hc5$Compound<-toupper(hc5$Compound)
hc5<-hc5[,c(1:4)]

testy<-left_join(combine_all,hc5)
combine_all<-testy


#look<-combine_all[combine_all$Compound == "CARBARYL",]

combine_all$Value = ifelse(combine_all$Value <= 1E-6,1E-6,combine_all$Value)


#customize colors
library(RColorBrewer)
myColors <- brewer.pal(8,"Dark2")
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
df<- df %>% mutate(EEC = Value * case_when(
                        MediaSub == "Air"  ~ SA/10000,
                        MediaSub == "Dust" ~ SA/10000,
                        MediaSub == "Soil" ~ 1/SA,
                        MediaSub == "Nectar" ~ 0.400,
                        MediaSub == "Pollen" ~  0.030))




#look<-df[df$Compound =="IMIDACLOPRID",]



#remove rows where the condition for concentration is expressed twice
  df<-df[!(  df$MediaSub == "Air" & df$ExposureLevel == "Oral_LD50_ug_bee" |
             df$MediaSub == "Dust" & df$ExposureLevel == "Oral_LD50_ug_bee" |
             df$MediaSub == "Soil" & df$ExposureLevel == "Oral_LD50_ug_bee"|
             df$MediaSub == "Nectar" & df$ExposureLevel == "Contact_LD50_ug_bee"|
             df$MediaSub == "Pollen" & df$ExposureLevel == "Contact_LD50_ug_bee") ,]
  
  
  
  #df$endpoint_final<-df$EEC/(df$Endpoint/10)
  
  #split by crop
  df_list<-split(df, list(df$Commodity), drop=T)
  #crop<-df_list[[2]]

  plot_by_crop<-function(crop){
    
    # ticks <- (seq(-35,15, by=10))
    # logticks <- exp(ticks)
    
   crop<-crop[,c(1:10,14,11:13)]
   crop<-gather(crop, "Comparison", "Endpoint", 12:ncol(crop))
   
  
   crop$Comparison<-ifelse(crop$Comparison == "Endpoint","LC50/LD50",crop$Comparison)
   crop$Comparison<-ifelse(crop$Comparison == "HC5","HC5",crop$Comparison)
   crop$Comparison<-ifelse(crop$Comparison == "EPA","EPA Tier I Dose",crop$Comparison)
   

   #crop$col <-myColors[match(crop$MediaSub, names(myColors))]
   

  #create plot
  out<-ggplot(crop, aes(x=MediaSub)) +
    geom_boxplot(aes(y=EEC), fill="lightgrey", show.legend = FALSE)+
    geom_point(aes(y=Endpoint, shape=Comparison, colour=Comparison, fill=Comparison), size=3)+

    # geom_point(aes(x=MediaSub, y=(Endpoint), colour=Endpoint), pch=4,size=3,shape=4, fill="darkred")+
    # geom_point(aes(x=MediaSub, y=(EPA), colour=EPA),pch=21,size=3,shape=1,fill="darkblue")+
    # geom_point(aes(x=MediaSub, y=(HC5), colour=HC5), size=3,shape=5, fill="lightgreen")+
   
    scale_y_continuous(trans=scales::log_trans(),
                       labels = scales::format_format(digits=3))+

    
    facet_nested_wrap(~Compound +ID,nrow=1)+
    scale_colour_manual(values=c('darkblue','darkgreen',"firebrick"))+
    scale_shape_manual(values = c(4,16,17))+
    # colScale+
    # fillScale+
    
    #ylab(ifelse(df$ApplicationType == "Foliar","Hazard Quotient", ""))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(paste0(crop$ApplicationType,"-", crop$Commodity)) +
    theme(legend.position="none",
          axis.title=element_text(size=14,face="bold"),
          axis.text.y = element_text(size=12,face="bold"),
          axis.text.x = element_text(size=12,face="bold", colour=
                                     
                                     ifelse(crop$ApplicationType=="Soil",
                                            c("#66A61E","#7570B3","#D95F02"),
                                            c("#E6AB02","#66A61E","#7570B3","#D95F02"))
                                     
                                     ),
          axis.title.y=element_blank(), axis.title.x=element_blank())
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
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(5))
compare_risk_top


compare_risk_bottom<-plot_grid(
  list_of_plots[[2]],
  list_of_plots[[3]],
  # labels = c('Air', 'Dust','Soil','Pollen','Nectar'),
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=1, rel_widths = c(3,1.8))
compare_risk_bottom

final<-plot_grid(compare_risk_top,compare_risk_bottom, hjust=0, vjust=0,  align= "h",nrow=2)





y.grob <- textGrob("Rate-Adjusted Environmmental Exposure Dose [ug/bee;ug/g]", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Media Type", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))

grid.arrange(arrangeGrob(compare_risk_top, left = y.grob, bottom = x.grob))
grid.arrange(arrangeGrob(compare_risk_bottom, left = y.grob, bottom = x.grob))

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

combine_all<-combine_all[,c(2:3,8,6,7,9,4)]
colnames(combine_all)[4]<-"Residues_ugg"

#residues<-residues[,c(1:2,7,4:5,3,6)]
#residues<-residues[residues$Compound == "BIFENTHRIN" |residues$Compound == "IMIDACLOPRID",  ]
names(residues)[7]<-"ID"

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
    #colScale+
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

compare_field_est<-plot_grid(
  list_of_estimate_plots[[1]],
  list_of_estimate_plots[[2]],
  hjust=0, vjust=0, align= "h",  label_x = 0.01, nrow=2, ncol=1, rel_widths = c(6,6))






y.grob <- textGrob("Residue Concentrations [ug/g]", 
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


