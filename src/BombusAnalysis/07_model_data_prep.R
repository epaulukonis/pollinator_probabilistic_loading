### Probabilistic Crop Loading 

### 07 process data for model estimations

# Edited by E. Paulukonis September 2023


PWC_dir<-"C:/Users/EPAULUKO/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/apps/pwc_2.001/pesticide runs"

print(list.files(path=PWC_dir, pattern='.zts$', all.files=TRUE, full.names=FALSE))
PWC_runs<- file.path(PWC_dir, list.files(path=PWC_dir, pattern='.zts$', all.files=TRUE, full.names=FALSE))
PWC_runs<-setNames(lapply(PWC_runs, read.table, header= FALSE, sep= "",
                          skip = 3, stringsAsFactors = FALSE, row.names=NULL), tools::file_path_sans_ext(basename(PWC_runs)))

#create function to rename and process as dataframes
set_df_names<-function(x){
 colnames(x)<-c("YYYY","MM","DD","RUNF0","ESLS0","RFLX1","EFLX1","DCON1","INFL0")
 x<-x[!x$YYYY<1999, ]
 x
}

PWC_runsf<-lapply(PWC_runs,set_df_names)
rm(PWC_runs)

#save to r.data file if desired
#save(PWC_runsf, file = paste(PWC_dir,"/przmout.RData",sep = ""))

# test<-PWC_runsf[[1]]
# df<-test

PWC_data_list<-list()
for(scenario in 1:length(PWC_runsf)){
  
  df<-PWC_runsf[[scenario]]
  
  #subset and use 2021 for now
  df<-df[df$YYYY ==2021,]
  df$n<-1:365
 
  #first get days at day of application 
  # df<-if (grepl('soy',names(PWC_runsf[scenario]))){ 
  # df[df$n>= 161 & df$n<=311,] # later for soy
  # } else { 
  #    df[df$n>= 147 & df$n<=297,]} # earlier for corn
  
  
  PWC_data_list[[scenario]]<-df
  
  #combo_off<-sum(df$RFLX1+df$EFLX1) #total runoff, grams per cm2
  
  
}
names(PWC_data_list)<-names(PWC_runsf)
