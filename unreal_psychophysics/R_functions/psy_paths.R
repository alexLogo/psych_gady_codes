psy_paths<-function(subject_folder,attempt,sub_n){
  
library(stringi)
subject_folder<-subject_folder
sub_n <- stri_sub(subject_folder,-3)  

#paths
#subject_folder<-stri_sub(subject_folder,-29)
psychophysics_folder<-stri_sub(subject_folder,1,51)
#desktop<-stri_sub(subject_folder,1,22)
output_matlab_folder<-gsub(" ","",paste(psychophysics_folder,"\\output_matlab"))
unreal_random_temps_folder<-gsub(" ","",paste(psychophysics_folder,"\\unreal_random_temps"));
results_folder<-gsub(" ","",paste(subject_folder,"\\results_sub_",sub_n,'_attempt_',as.character(attempt)))
unreal_input_folder<-gsub(" ","",paste(results_folder,"\\unreal_input_sub_",sub_n,'_attempt_',as.character(attempt)))
rda_folder<-"C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\rdas"
txt_folder<-"C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\txt_files_for_matlab"
log_values_folder<-"C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\log_values_for_matlab"
paths<-c(subject_folder,psychophysics_folder,output_matlab_folder,unreal_random_temps_folder,results_folder,unreal_input_folder,rda_folder,txt_folder,log_values_folder)
return(paths)
}

#save(psy_paths, file='C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\rdas\\psy_paths.rda')