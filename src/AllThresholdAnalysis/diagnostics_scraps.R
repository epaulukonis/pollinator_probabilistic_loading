

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