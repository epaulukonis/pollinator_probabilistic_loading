### Foraging Landscape Analysis

### 0X Transferring some code for figure generation

# Edited by E. Paulukonis February 2024


#county data
county_pest<-list()
county_pest[1:17]<-file.path(paste0(pest_dir,"/PNSP/County"), list.files(paste0(pest_dir,"/PNSP/County"), pattern='.txt', all.files=TRUE, full.names=FALSE))
county_pest_list<-lapply(county_pest, function(x) read.table(x, header=TRUE, sep ='\t'))

#here, we add in the county codes as well as the specific compounds we'd like to focus on  
codes<-c(7, 111, 201)
Compound<-c(
  "CLOTHIANIDIN",
  "CHLORPYRIFOS",
  "CARBOFURAN",
  "IMIDACLOPRID",
  "THIAMETHOXAM",
  "BIFENTHRIN",
  "CARBARYL",
  "GLYPHOSATE"
  
)

#get state, county, and compounds
get_data<-function(x){
  x<-x[x$STATE_FIPS_CODE == 17,]
  x<-x[x$COUNTY_FIPS_CODE %in% codes,]
  x<-x[x$COMPOUND %in% Compound,] 
}
county_pest<-lapply(county_pest_list, get_data)
#get into dataframe
county_pest<-do.call(rbind,county_pest)
#group by compound and year, summarize with epest high 
county_pest<-county_pest %>% group_by(COMPOUND, YEAR) %>% summarise(sumx=sum(EPEST_HIGH_KG))

#get pesticide class 
class <-read.csv(paste0(pest_dir, "/PestClass.csv"))

county_pestt<-merge(county_pest,class, by.x="COMPOUND", by.y="Compound")

unique(county_pestt$Class)

county_pestt$Class<-factor(county_pestt$Class, levels=c( "Insecticide-Carbamate","Insecticide-Neonicotinoid","Insecticide-Organophosphate",  
                                                         "Insecticide-Pyrethroid","Herbicide-Phosphonate"))

county_pestt %>%
  ggplot(aes(x=YEAR, y=(sumx), group=COMPOUND, color=COMPOUND)) +
  geom_point()+
  geom_line()+
  facet_wrap(~Class, ncol=1, scales="free_y")+
  xlab("Year") + ylab("Total kg")+
  #scale_color_manual(values=c(c25))+
  scale_x_continuous(breaks=seq(1999,2021,1))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####



