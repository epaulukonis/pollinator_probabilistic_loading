### Foraging Landscape

### 07 Estimating probability of survival

"A modification of 2) is: Cassandra has a mix of unadjusted 48 and 96 hour data within each curve she has fit. 
Matt Etterson says that this is the way they have been doing it with OPP/EFED (not applying any time adjustments). 
So we could pretend those curves are 72 hour curves that Cassandra has generated and then adjust them to 24 hours for your use. 
This could be done by reproducing the curves in R to create a data set of concentrations and associated mortality, then dividing all the concentrations by 3 (assuming that n=1), 
and then refitting the curves in R with the drc package. This would be a rough approximation of what the curves would be if each of the LD50s were adjusted for time and then refit."



param <-read.csv(paste0(pest_dir, "/HC_params.csv"))
tox<-read.csv(paste0(pest_dir, "/BeeTox.csv"))
tox<-tox[,1:3]
tox$Compound<-str_to_title(tox$Compound)


#install.packages(c("ordinal","EnvStats", "ExtDist" ))

#### Oral ----
# generate daily mortalities based on exposure
library(drc)
library(ordinal)
library(EnvStats)
library(ExtDist)
grab_quantiles <- seq(0.001, 0.999, 0.001) #what's the associted mortality %?
# times 3 for Haber's rule (n=1) assuming derived relationships are for 72 hours and we want 24


# bifenthrin gumbel -0.6375, 1.1569, log10
bifenthrin_tox_quantiles <- ((10^(qgumbel(grab_quantiles, -0.6375, 1.1569)))*3)/1000
# carbaryl gumbel 0.9888, 0.9538, log10
carbaryl_tox_quantiles <- ((10^(qgumbel(grab_quantiles, 0.9888, 0.9538)))*3)/1000
# chlorpyrifos triangular -1.919 1.5918, log10
chlorpyrifos_tox_quantiles <- ((10^qtri(grab_quantiles, min = -1.919, max = 1.5918, mode = (-1.919 + 1.5918)/2))*3)/1000
# clothianidin triangular -2.7065, 0.6504, log10
clothianidin_tox_quantiles <- ((10^qtri(grab_quantiles, min = -2.7065, max = 0.6504, mode = (-2.7065 + 0.6504)/2))*3)/1000
# imidacloprid weibull 8.255, 0.4450
imidacloprid_tox_quantiles <- ((10^qWeibull(grab_quantiles, 8.255, 0.445))*3)/1000
# thiamethoxam logistic -0.0566, 0.3559, log10
thiamethoxam_tox_quantiles <- (10^(qlogis(grab_quantiles, -0.0566, 0.3559))*3)/1000

tox_quantiles <- cbind(grab_quantiles, bifenthrin_tox_quantiles, carbaryl_tox_quantiles, chlorpyrifos_tox_quantiles,
                       clothianidin_tox_quantiles, imidacloprid_tox_quantiles, thiamethoxam_tox_quantiles)

tox_quantiles<-as.data.frame(tox_quantiles)
#tox_quantiles$grab_quantiles<-1-tox_quantiles$grab_quantiles

dr<-gather(tox_quantiles, "Compound","Dose", 2:7)
colnames(dr)[1]<-"Mortality"
dr$Compound<- str_to_title(sub("\\_.*", "", dr$Compound))

dr<-merge(x = dr, y = tox[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee")], by = "Compound", all.x=TRUE)

#get example of modeled min/max
library(purrr)
comp<-map(output, 1)
comp<-do.call(rbind,comp)
comp<-comp %>% group_by(Compound) %>%
  mutate(max = max(Dose)) %>%
  mutate(min = min(Dose))

comp$Compound<-str_to_title(comp$Compound)

comp<-comp[comp$Day==1,]
comp<-comp[comp$type == "Oral",]

dr<-merge(x = dr, y = comp[ , c("Compound",  "max", "min")], by = "Compound", all.x=TRUE)


library(ggpmisc) 
#look at DR curves
ggplot(dr, aes(x = (Dose), y = Mortality)) + 
 geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
  geom_line(aes(color = Compound), size=1.2) +
  geom_point(aes(x=Oral_LD50_ug_bee, y=0.5), shape=19)+
  facet_wrap(~Compound,scales = "free_x")+
  xlab("Dose (ug/bee)") +
  ylab("Mortality") +
  scale_x_continuous(trans=scales::log_trans(),
                     labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(legend.position = "none")


# print(list.files(path=paste0(root_data_out, "/all_forage/media_tables"), pattern='.csv', all.files=TRUE, full.names=FALSE))
# scenarios<- file.path(paste0(root_data_out, "/all_forage/media_tables"), list.files(path=paste0(root_data_out, "/all_forage/media_tables"), pattern='.csv', all.files=TRUE, full.names=FALSE))
# scenarios<-setNames(lapply(scenarios, read.csv), tools::file_path_sans_ext(basename(scenarios)))
# scenarios<-scenarios[(mixedsort(as.character(names(scenarios))))]



#add new scenario # to each compound

function(x){

  curve<-dr[dr$Compound %in% daily_exposures$Compound,]
  
  oral<-daily_exposures[daily_exposures$type == "Oral",]
  colnames(oral)[6]<-"Dosen"


  test = curve %>% left_join(oral , by='Compound') %>%
    mutate(dif = abs(Dosen - Dose)) %>%
    group_by(Day) %>% filter(dif == min(dif))
  
  
  testy<-test[order(test$Day),,drop=FALSE]
  testy$Survival<-1-testy$Mortality
  
  
  ggplot(testy, aes(x = (Day), y = Survival)) + 
    geom_line(aes(color = Compound), size=1.2) +
    geom_vline(xintercept = 100,col="darkgreen")+
    geom_vline(xintercept = 121,col="darkblue")+
    geom_vline(xintercept = 151,col="darkred")+
    
    geom_segment(aes(x=90,xend=110,y=0.60,yend=0.60), alpha = 0.5)+
    geom_segment(aes(x=111,xend=131,y=0.60,yend=0.60), alpha = 0.5)+
    geom_segment(aes(x=141,xend=161,y=0.60,yend=0.60), alpha = 0.5)+
  
  
    geom_text(aes(x=103, label="Emergence", y=0.25), colour="darkgreen", angle=90, text=element_text(size=11)) +
    geom_text(aes(x=124, label="Nest Initiation", y=0.25), colour="darkblue", angle=90, text=element_text(size=11))+
    geom_text(aes(x=154, label="Queen Ends Foraging", y=0.31), colour="darkred", angle=90, text=element_text(size=11))+
    
    xlab("Time (Day)") +
    ylab("Probability of Survival") +
    # scale_x_continuous(trans=scales::log_trans(),
    #                    labels = scales::format_format(digits=3))+
    theme_bw() +
    theme(legend.position = "none")
  
  }
### Contact ----

