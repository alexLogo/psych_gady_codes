#Psychophysics analysis & Unreal input creating script
run_unreal_psy<-function(study_number=readline("Study number:"),sub_n = readline("Subject number:"),attempt= readline(prompt = "Fitting attempt number:")){

#type run_unreal_psy() in the console
  #enter subject number:001
  #enter attempt number:1
  #check results in results folder
  #
  #if you need to edit after a repeat, place the new result in the results folder
  # and type repeat_jnd() in the console
  #enter subject number:001
  #enter attempt number:1
  #check results
  
    
library(stringi)
#sub_n <- readline("Subject number:");
#attempt<- readline(prompt = "Fitting attempt number:"); 
subject_folder<-gsub(" ","",paste("C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\unreal_",study_number,"\\sub_",sub_n))

   
#C:\Users\User\Desktop\unreal_psychophysics\unreal_05\sub_001

setwd("C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\rdas")
load("repeat_jnd.rda")
load("psy_paths.rda")
paths<-psy_paths(subject_folder,attempt,sub_n)
subject_folder<-paths[1]
psychophysics_folder<-paths[2]
output_matlab<-paths[3]
unreal_random_temps<-paths[4]
results_folder<-paths[5]
unreal_input_folder<- paths[6]
rda_folder<-paths[7]

setwd(paths[1]) #subject folder path
try(dir.create(results_folder),silent=TRUE)
setwd(paths[5]) #create a folder in results_sub.. to keep the input we made for experiment 2
try(dir.create(unreal_input_folder),silent=TRUE)
setwd(paths[7])
load("name_conditions_and_domains.rda")
load("unreal_psy.rda")
load("load_thresholds.rda")
load("repeat_list.rda")
#load("unreal_glm.rda")
unreal_psy(paths,attempt,sub_n,study_number)
load_thresholds(paths)


#doAgain contains a list of the conditions we need to redo
setwd(paths[3])
doAgain_f<-paste0("doAgain_values_sub_",sub_n,".csv")
if (file.exists(doAgain_f)){
  doAgain<-read.csv(doAgain_f,header=TRUE, stringsAsFactors = TRUE);
  repeat_list(paths,sub_n,attempt,doAgain)
  } else {cat('\n','No repeats needed','\n')}
}

#save(run_unreal_psy, file = 'C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\rdas\\run_unreal_psy.rda')
