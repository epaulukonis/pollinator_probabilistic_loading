### Foraging Landscape

### 08 assessing metrics

###Let's look at a subset of individual exposure profiles to get some data for the results
library(ggh4x)

#### Evaluation of the original sampled fields  - look at scenario 1
print(list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/MC/only2014/sub"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
scenarios<- file.path(paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/MC/only2014/sub"), list.files(path=paste0(root_data_out, "/all_bombus/modified_sampled_fields/fields_within_habitat/MC/only2014/sub"), pattern='.shp$', all.files=TRUE, full.names=FALSE))
scenarios<-setNames(lapply(scenarios, st_read), tools::file_path_sans_ext(basename(scenarios)))
scenarios<-scenarios[(mixedsort(as.character(names(scenarios))))]

looky<-scenarios[[1]]
apps<-looky[looky$ApplctT != "None",]

eval<-looky %>% group_by(Compond, Commdty) %>% count(applctn)
head(eval)


###### WEIGHTED LANDSCAPE
print(list.files(path=paste0(root_data_out,'/all_forage/media_tables/sub'), pattern='.csv', all.files=TRUE, full.names=FALSE))
scenarios<- file.path(paste0(root_data_out,'/all_forage/media_tables/sub'), list.files(path=paste0(root_data_out,'/all_forage/media_tables/sub'), pattern='.csv', all.files=TRUE, full.names=FALSE))
scenarios<-setNames(lapply(scenarios, read.csv), tools::file_path_sans_ext(basename(scenarios)))



###### CONCENTRATIONS
#### Run scripts 1-3 first
#### look at scenario 1
sim<-4
final_on_field_history<-final_on_field_history_list[[sim]]
final_off_field_history_30m<-final_off_field_history_30m_list[[sim]]
final_off_field_history_60m<-final_off_field_history_60m_list[[sim]]
final_off_field_history_90m<-final_off_field_history_90m_list[[sim]]

#this removes any scenarios for which we don't have data, thus it throws off the function
final_on_field_history      <-keep(final_on_field_history, ~all(ncol(.x) >= 14))
final_off_field_history_30m <-keep(final_off_field_history_30m, ~all(ncol(.x) >= 14))
final_off_field_history_60m      <-keep(final_off_field_history_60m, ~all(ncol(.x) >= 14))
final_off_field_history_90m      <-keep(final_off_field_history_90m, ~all(ncol(.x) >= 14))


# #we're not going to include any soil applications; hence we remove them. in the future this will likely need to be modified
final_on_field_history<-purrr::discard(final_on_field_history, ~any(.x$ApplicationType == "Soil"))
final_off_field_history_30m <-purrr::discard(final_off_field_history_30m , ~any(.x$ApplicationType == "Soil"))
final_off_field_history_60m <-purrr::discard(final_off_field_history_90m , ~any(.x$ApplicationType == "Soil"))
final_off_field_history_90m <-purrr::discard(final_off_field_history_90m , ~any(.x$ApplicationType == "Soil"))

#we also don't haev DR data for this compound
final_on_field_history<-purrr::discard(final_on_field_history, ~any(.x$Compound == "GLYPHOSATE"))
final_off_field_history_30m <-purrr::discard(final_off_field_history_30m , ~any(.x$Compound == "GLYPHOSATE"))
final_off_field_history_60m <-purrr::discard(final_off_field_history_90m , ~any(.x$Compound == "GLYPHOSATE"))
final_off_field_history_90m <-purrr::discard(final_off_field_history_90m , ~any(.x$Compound == "GLYPHOSATE"))

#pull out clip


onfield<-do.call(rbind,final_on_field_history)
offfield3<-do.call(rbind,final_off_field_history_30m)
offfield6<-do.call(rbind,final_off_field_history_60m)
offfield9<-do.call(rbind,final_off_field_history_90m)

onfield$location<-"On-field"
offfield3$location<-"Off-field30"
offfield6$location<-"Off-field60"
offfield9$location<-"Off-field90"

#get average across 90m
offfield<-rbind(offfield3, offfield6,offfield9)
offfield<-gather(offfield, "Media","Concentrations",14:18) #gather all values
offfield<- offfield %>% group_by(Compound, Commodity, id, Media,Day) %>% mutate(avg=mean(Concentrations))
offfieldf<-offfield[offfield$location == "Off-field30",]
offfieldf$location<-"Off-field"
offfieldf$Concentrations<-offfieldf$avg
offfieldf<-offfieldf[,1:16]

onfield<-gather(onfield, "Media","Concentrations",14:18) #gather all values

names(onfield)
names(offfieldf)

conc<-rbind(onfield,offfieldf)
conc$type<-ifelse(conc$Media == "Air" | conc$Media == "Dust" | conc$Media == "Soil","Contact","Oral")


contact<-conc[conc$type == "Contact",]
contact<-contact[contact$Day >= 100 & contact$Day <=283 ,]
contact<-contact[!(contact$ApplicationType %in% "None"),]


oral<-conc[conc$type == "Oral",]
oral<-oral[oral$Day >= 100 & oral$Day <=283 ,]
oral<-oral[!(oral$ApplicationType %in% "None"),]
oral<-oral %>% group_by(Media,Commodity, Compound,location,Day) %>% mutate(avgconc =  mean(Concentrations))

final_output<-do.call(rbind,scenarios)
test<-merge(x = oral, y = final_output[ , c("Conc", "Day","Media","Compound")], by = c("Day","Media","Compound"), all.x=TRUE)


oral<-gather(oral,"Foraging","Value",18:19)
oralseed<-oral[oral$ApplicationType == "Seed",]

output<- ggplot(oralseed) +
  geom_line(aes(x = as.Date(date), (Value), linetype=location, color=Foraging),size=1.2)+
  #geom_line(aes(x = as.Date(date), (Conc), group=Media, color=Compound),size=1.2)+
  #geom_point(aes(x = as.Date(date), (Value)),size=1.2)+
  facet_nested_wrap(~ Media + Compound, scales="free",nrow=1)+
  #facet_wrap(~Media+Compound)+
  # scale_y_continuous(trans=scales::log_trans(),
  #                    labels = scales::format_format(digits=3))+
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b")+
 # lineScale+
  xlab("Month") +
  ylab("Concentration") +
  theme_bw()+
  theme(legend.title=element_blank(),
        axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "plain"))

output








#create plot
out<-ggplot(oralseed, aes(x=Foraging)) +
  geom_boxplot(aes(y=Value), fill="lightgrey", show.legend = FALSE)+
  #geom_point(aes(y=Conc), fill="darkgrey",size=3)+

  
  scale_y_continuous(trans=scales::log_trans(),
                     labels = scales::format_format(digits=3))+
  
  
  facet_nested_wrap(~Compound + location + Media,nrow=1)+
  scale_colour_manual(values=c('darkblue','darkgreen',"firebrick"))+
  scale_shape_manual(values = c(4,16,17))+

  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position="none",
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(size=12,face="bold"),
        axis.text.x = element_text(size=12,face="bold", colour=
                                     
                                     ifelse(oral$ApplicationType=="Seed",
                                            c("#E6AB02","#66A61E","#7570B3","#D95F02"),
                                            c("#E6AB02","#66A61E","#7570B3","#D95F02"))
                                   
        ),
        axis.title.y=element_blank(), axis.title.x=element_blank())
out












