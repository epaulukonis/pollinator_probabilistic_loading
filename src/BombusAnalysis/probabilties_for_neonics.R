### This code is for examining trends in pesticide uses by class and year for the study region 

#### What is the proportion of seed treatment vs foliar in 2014/2015? ----
#this section looks at the proportion of 
county_pest_sub<-county_pest[county_pest$YEAR > 2011,]
county_pest_sub<-county_pest_sub %>% group_by(COMPOUND,COUNTY_FIPS_CODE)%>%mutate(prop = EPEST_HIGH_KG[YEAR == 2015][1]/EPEST_HIGH_KG[YEAR == 2014][1])
####



#### What crops are there? ----
# cdl<-cdl_data_ill_rec[[1]]
# unique(cdl)
#corn, soy, sorghum, winter wheat, other small grains, dbld crop winwht/soybeans, other crops, fallow/idle
####



#### What does the neonicotinoid useage look like over time? ----
#originally, let's look at seed treatments
compounds<-c("IMIDACLOPRID","THIAMETHOXAM","CLOTHIANIDIN")
county_pestx<-county_pest[county_pest$COMPOUND %in% compounds,]
county_pestx<-county_pestx %>% group_by(COMPOUND,YEAR) %>% summarise(sumx = sum(EPEST_HIGH_KG))

county_pestx %>%
  ggplot(aes(x=YEAR, y=sumx, group=COMPOUND, color=COMPOUND)) +
  geom_point()+
   geom_line()+
  annotate("text", x=2015, y=1000, label= "Imidacloprid: 10%") + 
  annotate("text", x = 2015, y=700, label = "Thiamethoxam: 23%")
####




#### What does the overall usage look like by class for a selected subset of insecticides + 3 herbicides? ----
#let's look at the useage history of the chosen compounds
county_pestt<- county_pest %>% group_by(COMPOUND,YEAR) %>% summarise(sumx = sum(EPEST_HIGH_KG))
class <-read.csv(paste0(pest_dir, "/PestClass.csv"))

county_pestt<-merge(county_pestt,class, by.x="COMPOUND", by.y="Compound")

#unique colors
c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)


county_pestt$Class<-factor(county_pestt$Class, levels=c("Carbamate","Neonicotinoid","Organophosphate",  "Pyrethroid","Herbicide"  ))

county_pestt %>%
  ggplot(aes(x=YEAR, y=(sumx), group=COMPOUND, color=COMPOUND)) +
  geom_point()+
  geom_line()+
  facet_wrap(~Class, nrow=3, ncol=2, scales="free_y")+
  xlab("Year") + ylab("Total kg")+
  scale_color_manual(values=c(c25))+
  scale_x_continuous(breaks=seq(1999,2021,1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####






#### What is the useage history for ALL compounds? ----
#now let's look at the useage history of all pesticides in these counties
county_pest_ALL<- list %>% group_by(COMPOUND,YEAR) %>% summarise(sumx = sum(EPEST_HIGH_KG))
county_pest_ALL<- list %>% group_by(COMPOUND) %>% summarise(avgx = mean(EPEST_HIGH_KG))


county_pest_ALL<-county_pest_ALL[with(county_pest_ALL,order(-avgx)),]
county_pest_ALL<-county_pest_ALL[county_pest_ALL$avgx >1,]
county_pest_ALL<-county_pest_ALL[1:93,]


Compound<-c(
  "ACEPHATE",
  "CLOTHIANIDIN",
  "CHLORPYRIFOS",
  "CARBOFURAN",
  "DIMETHOATE",
  "MALATHION",
  "CHLORETHOXYFOS",
  "PHORATE",
  "IMIDACLOPRID",
  "THIAMETHOXAM",
  "BIFENTHRIN",
  "PERMETHRIN",
  "CARBARYL"
  
)


a <- ifelse(county_pest_ALL$COMPOUND %in% Compound, "red", "black")

county_pest_ALL %>%
  ggplot(aes(x=reorder(COMPOUND,-avgx), y=avgx)) +
  geom_bar(stat="identity", color='skyblue',fill='steelblue')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = a))

####






 

