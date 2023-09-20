### Probabilistic Crop Loading 

### 07 evaluating climate change impacts to crop plantings

# Edited by E. Paulukonis March 2023

affinis_all<-read.csv(paste0(bombus_dir,"/bombus_ILL.csv"))
affinis_all$ID<-row.names(affinis_all)
coordinates(affinis_all)=~decimalLongitude+decimalLatitude #get coordinates
proj4string(affinis_all)=CRS("+init=epsg:4326") 
affinis_all<-st_as_sf(affinis_all)
BP<-affinis_all
#BP<-st_transform(affinis_all, crs(fv[[1]])) #bombus points

BP$Month <- factor(BP$Month , levels=c("March","April","May","June","July","August","September","October","November"))
#what's the distribution of occurence dates?
ggplot(BP, aes(Month)) + geom_bar()


#here's Illinois
all_states<-readOGR(state_dir, layer = "tl_2021_us_county") #read in states
study<-all_states[all_states$STATEFP == "17",]
rm(all_states)
study<-st_as_sf(study)
study<-st_transform(study,crs(fv[[1]]))

#let's get points that only fall in Illinois
output<-st_intersection(BP,study)
#BP$Month <- factor(BP$Month , levels=c("March","April","May","June","July","August","September","October","November"))
#what's the distribution of occurrence dates?
ggplot(output, aes(Month)) + geom_bar()





BP_f<-BP[BP$Year >= 2000,]
BP_f<-BP_f[BP_f$institutionCode != "iNaturalist",]
BP_f<-na.omit(BP_f)



file<-("C:/Users/EPAULUKO/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Paulukonis_Documents/manuscript_bombus_toxicity/BEs/0123852-230224095556074.csv")


test<-read.table(file, sep = "" , header = T ,
           na.strings ="", fill = TRUE, stringsAsFactors= F)

rm(test)

unique(test$family)
 


#here, we'll parse out the corn and soy applications themselves and 

months<-levels(BP$Month)
test<-pd[[1]]
test$Month<-gsub(".*-","",test$DateX)


monthsx<-as.data.frame(cbind(months, unique(test$Month)))


list_of_dates_c<-list()
list_of_dates_s<-list()
for(dates in 1:length(pd)){
test<-pd[[dates]]
test$Month<-gsub(".*-","",test$DateX)
monthsx<-as.data.frame(cbind(months[1:5], unique(test$Month)))

testC<-test
testS<-test

testC$Monthf<-monthsx$V1[match(testC$Month,monthsx$V2)]
testS$Monthf<-monthsx$V1[match(testS$Month,monthsx$V2)]

list_of_dates_c[[dates]]<-testC
list_of_dates_s[[dates]]<-testS

}

corn<-do.call(rbind,list_of_dates_c)
corn<-corn[,c(1:2,4:7)]

soy<-do.call(rbind,list_of_dates_s)
soy<-soy[,c(1,3:7)]






# Entering data
year <- c(2014, 2015, 2016, 2017, 2018, 2019,2020)
course <- c(35, 30, 40, 25, 30, 35, 65)
penroll <- c(0.3, 0.25, 0.3, 0.5, 0.4, 0.2, 0.6)

# Creating Data Frame
perf <- data.frame(year, course, penroll)

# Plotting Multiple Charts and changing 
# secondary axis to percentage
library(ggplot2)
ggp <- ggplot(perf)  + 
  geom_bar(aes(x=year, y=course),stat="identity", fill="cyan",colour="#006000")+
  geom_line(aes(x=year, y=100*penroll),stat="identity",color="red",size=2)+
  labs(title= "Courses vs Students Enrolled in GeeksforGeeks",
       x="Year",y="Number of Courses Sold")+
  scale_y_continuous(sec.axis=sec_axis(
    ~.*0.01,name="Percentage of Students Enrolled", labels=scales::percent))
ggp






  ggplot(corn,aes(x = Monthf, y = Years, color = Corn, group = Years)) +
  geom_line()

