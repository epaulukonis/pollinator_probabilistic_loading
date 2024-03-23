### Foraging Landscape

### 07 Estimating probability of survival

"A modification of 2) is: Cassandra has a mix of unadjusted 48 and 96 hour data within each curve she has fit. 
Matt Etterson says that this is the way they have been doing it with OPP/EFED (not applying any time adjustments). 
So we could pretend those curves are 72 hour curves that Cassandra has generated and then adjust them to 24 hours for your use. 
This could be done by reproducing the curves in R to create a data set of concentrations and associated mortality, then dividing all the concentrations by 3 (assuming that n=1), 
and then refitting the curves in R with the drc package. This would be a rough approximation of what the curves would be if each of the LD50s were adjusted for time and then refit."



#param <-read.csv(paste0(pest_dir, "/HC_params.csv"))
tox<-read.csv(paste0(pest_dir, "/BeeTox.csv"))
tox<-tox[,1:3]
tox$Compound<-str_to_title(tox$Compound)

library(drc)
library(ordinal)
library(EnvStats)
library(ExtDist)
library(ggpmisc) 
grab_quantiles <- seq(0.001, 0.999, 0.001) #what's the associated mortality %?
# times 3 for Haber's rule (n=1) assuming derived relationships are for 72 hours and we want 24


# print(list.files(path=paste0(root_data_out, "/all_forage/media_tables"), pattern='.csv', all.files=TRUE, full.names=FALSE))
# scenarios<- file.path(paste0(root_data_out, "/all_forage/media_tables"), list.files(path=paste0(root_data_out, "/all_forage/media_tables"), pattern='.csv', all.files=TRUE, full.names=FALSE))
# scenarios<-setNames(lapply(scenarios, read.csv), tools::file_path_sans_ext(basename(scenarios)))
# scenarios<-scenarios[(mixedsort(as.character(names(scenarios))))]

#add new scenario # to each compound


#install.packages(c("ordinal","EnvStats", "ExtDist" ))

#### Oral ----
# generate daily survival
##### oral
# bifenthrin gumbel -0.6375, 1.1569, log10
bifenthrin_tox_quantileso <- ((10^(qgumbel(grab_quantiles, -0.6375, 1.1569)))*3)/1000
# carbaryl gumbel 0.9888, 0.9538, log10
carbaryl_tox_quantileso <- ((10^(qgumbel(grab_quantiles, 0.9888, 0.9538)))*3)/1000
# chlorpyrifos triangular -1.919 1.5918, log10
chlorpyrifos_tox_quantileso <- ((10^qtri(grab_quantiles, min = -1.919, max = 1.5918, mode = (-1.919 + 1.5918)/2))*3)/1000
# clothianidin triangular -2.7065, 0.6504, log10
clothianidin_tox_quantileso <- ((10^qtri(grab_quantiles, min = -2.7065, max = 0.6504, mode = (-2.7065 + 0.6504)/2))*3)/1000
# imidacloprid weibull 8.255, 0.4450
imidacloprid_tox_quantileso <- ((10^qWeibull(grab_quantiles, 8.255, 0.445))*3)/1000
# thiamethoxam logistic -0.0566, 0.3559, log10
thiamethoxam_tox_quantileso <- (10^(qlogis(grab_quantiles, -0.0566, 0.3559))*3)/1000

tox_quantileso <- cbind(grab_quantiles, bifenthrin_tox_quantileso, carbaryl_tox_quantileso, chlorpyrifos_tox_quantileso,
                       clothianidin_tox_quantileso, imidacloprid_tox_quantileso, thiamethoxam_tox_quantileso)

tox_quantileso<-as.data.frame(tox_quantileso)

dr<-gather(tox_quantileso, "Compound","Dose", 2:7)
colnames(dr)[1]<-"Mortality"
dr$Compound<- str_to_title(sub("\\_.*", "", dr$Compound))

dr<-merge(x = dr, y = tox[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee")], by = "Compound", all.x=TRUE)
#look at DR curves
ggplot(dr, aes(x = (Dose), y = Mortality)) + 
  # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
  geom_line(aes(color = Compound), size=1.2) +
  geom_point(aes(x=Oral_LD50_ug_bee, y=0.5), shape=19)+
  facet_wrap(~Compound,scales = "free_x")+
  xlab("Dose (ug/bee)") +
  ylab("Mortality") +
  scale_x_continuous(trans=scales::log_trans(),
                     labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(legend.position = "none")



### implement beepop+ way 

seq_data <- seq(0.00005, 0.05, 0.0001) #what's the associated mortality %?

#imidacloprid
imidaslope<-as.data.frame(cbind(grab_quantiles,imidacloprid_tox_quantileso))
colnames(imidaslope)<-c("y","x")
riserun <-  diff(imidaslope$y)/diff(imidaslope$x)
slope<-max(riserun)
# [1] 487.7172
which.max(riserun)
# [1] 168

imidaslope[531,] #look right?

eecs<-imidacloprid_tox_quantileso


eec<-0.0006
ld50<-0.006
slope<-2
z<-(slope)*(log(eec)-log(ld50)) #
phat<-(1/(2*pi)^0.5)*exp(-(z^2)/2)
print(phat)



test_df<-as.data.frame(cbind(eecs,phat,ldx))



p1 <- ggplot(data = test_df,
             aes(x = (eecs), y = 1-phat)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = binomial(link = "probit")),
              colour = "#FF0000", se = TRUE)
p1




function(x){
  ind_compound<-x
 
  ind_compound<-exp_dose_output[[2]]
  ind_compound<- Map(cbind, ind_compound, index = seq_along(ind_compound))
  ind_compound<-as.data.frame(do.call(rbind, ind_compound))
  ind_compound$Compound<-str_to_title(ind_compound$Compound)
  
  
  curve<-dr[dr$Compound %in% ind_compound$Compound,]
  
  
  ### oral
  oral<-ind_compound[ind_compound$type == "Oral",]
  colnames(oral)[6]<-"Dosen"


  test = curve %>% left_join(oral , by='Compound') %>%
    mutate(dif = abs(Dosen - Dose)) %>%
    group_by(Day) %>% filter(dif == min(dif))

  
  daily_prob<-test[order(test$index,test$Day),,drop=FALSE]
  daily_prob$Survival<-1-daily_prob$Mortality
  daily_prob$Survival <- ifelse(daily_prob$Survival >= 0.999, 1,daily_prob$Survival)
  
  
daily_prob_plot<-  ggplot(daily_prob, aes(x = (Day), y = Survival)) + 
    geom_line(aes(color = index), size=1.2) +
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
daily_prob_plot

cumulative_survival<- apply(daily_prob[1:365,14], 2, prod) #but need to actually cut off at end of foraging!
  
  }
### Contact ----
# generate daily survival
# bifenthrin weibull
bifenthrin_tox_quantilesd <- ((10^qWeibull(grab_quantiles, 0.0520, 0.9930))*3)/1000
# carbaryl triangular
carbaryl_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -3.2441, max = 2.9293, mode = (-3.2441 + 2.9293)/2))*3)/1000
# chlorpyrifos triangular
chlorpyrifos_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -3.4647, max = 1.0538, mode = (-3.4647 + 1.0538)/2))*3)/1000
# clothianidin triangular
clothianidin_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -2.6705, max = -1.0526, mode = (-2.6705 + -1.0526)/2))*3)/1000
# imidacloprid normal
imidacloprid_tox_quantilesd <- ((10^qnorm(grab_quantiles, -2.0408, 1.1062))*3)/1000
# thiamethoxam logistic -0.0566, 0.3559, log10
thiamethoxam_tox_quantilesd <- ((10^qtri(grab_quantiles, min = -3.4085, max = -0.3356, mode = (-3.4085 + -0.3356)/2))*3)/1000

tox_quantilesd <- cbind(grab_quantiles, bifenthrin_tox_quantilesd, carbaryl_tox_quantilesd, chlorpyrifos_tox_quantilesd,
                        clothianidin_tox_quantilesd, imidacloprid_tox_quantilesd, thiamethoxam_tox_quantilesd)

tox_quantilesd<-as.data.frame(tox_quantilesd)

dr<-gather(tox_quantilesd, "Compound","Dose", 2:7)
colnames(dr)[1]<-"Mortality"
dr$Compound<- str_to_title(sub("\\_.*", "", dr$Compound))

dr<-merge(x = dr, y = tox[ , c("Compound",  "Contact_LD50_ug_bee", "Oral_LD50_ug_bee")], by = "Compound", all.x=TRUE)

ggplot(dr, aes(x = (Dose), y = Mortality)) + 
  # geom_line(aes(xmax=max,xmin=min, y=0.5), alpha = 0.7, col = "darkred")+
  geom_line(aes(color = Compound), size=1.2) +
  geom_point(aes(x=Contact_LD50_ug_bee, y=0.5), shape=19)+
  facet_wrap(~Compound,scales = "free_x")+
  xlab("Dose (ug/bee)") +
  ylab("Mortality") +
  # scale_x_continuous(trans=scales::log_trans(),
  #                    labels = scales::format_format(digits=3))+
  theme_bw() +
  theme(legend.position = "none")


function(x){
  ind_compound<-x
  
  ind_compound<-exp_dose_output[[2]]
  ind_compound<- Map(cbind, ind_compound, index = seq_along(ind_compound))
  ind_compound<-as.data.frame(do.call(rbind, ind_compound))
  ind_compound$Compound<-str_to_title(ind_compound$Compound)
  
  
  curve<-dr[dr$Compound %in% ind_compound$Compound,]
  
  
  ###contact
  contact<-ind_compound[ind_compound$type == "Contact",]
  colnames(contact)[6]<-"Dosen"
  
  
  test = curve %>% left_join(contact , by='Compound') %>%
    mutate(dif = abs(Dosen - Dose)) %>%
    group_by(Day) %>% filter(dif == min(dif))
  
  
  daily_prob<-test[order(test$index,test$Day),,drop=FALSE]
  daily_prob$Survival<-1-daily_prob$Mortality
  daily_prob$Survival <- ifelse(daily_prob$Survival >= 0.999, 1,daily_prob$Survival)
  
  
  daily_prob_plot<-  ggplot(daily_prob, aes(x = (Day), y = Survival)) + 
    geom_line(aes(color = index), size=1.2) +
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
  daily_prob_plot
  
  cumulative_survival<- apply(daily_prob[1:365,14], 2, prod) #but need to actually cut off at end of foraging! i.e., after certain day mortality to foundress queen not important
  
}
