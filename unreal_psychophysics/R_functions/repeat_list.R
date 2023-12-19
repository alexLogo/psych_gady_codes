repeat_list<-function(paths,sub_n,attempt,doAgain){
  
  #This funciton takes the doAgain file...
  #and deletes the conditions that arent there from the retake file
  
  # #doAgain contains a list of the conditions we need to redo
  # setwd(paths[3])
  # doAgain_f<-paste0("doAgain_values_sub_",sub_n,".csv")
  # doAgain<-read.csv(doAgain_f,header=TRUE, stringsAsFactors = TRUE);
  
  #retake_template.. contains all possible staircase conditions to choose from
  setwd(paths[4])
  retake_f<-"retake_template_unreal_05.csv"
  retake_main<-read.csv(retake_f,header=FALSE, stringsAsFactors = TRUE);
  colnames(retake_main)[1]<-"conditions"
  calibration_row<-retake_main[1,]
  final_retake<-data.frame()

  #condition name vec
  #ConditionName_vec<-c("Grow","Shrink","Delay","Heavy","Light","Fast","Saturated","Unsaturated","Ripple")
  
    #for (name in doAgain$doAgain){
       
     # }
  
  
  
  #cannot compare factors like strings, have to have same levels
  doAgain$doAgain <- factor(doAgain$doAgain, levels=levels(retake_main$conditions))
  retake_list<-as.list(doAgain$doAgain)  
  
  #yas queen
  if (length(retake_list)>0){
    for (n in retake_list){
      holder<-retake_main[retake_main$conditions==n,]
      try(final_retake<-rbind(final_retake,holder),silent=TRUE)
    }
    final_retake<-subset(final_retake,select=-c(conditions))
    calibration_row<-subset(calibration_row,select=-c(conditions))
    final_retake<-rbind(calibration_row,final_retake)
    final_retake<-head(final_retake, - 1)
    #setwd("C:\\Users\\User\\Desktop")
    setwd("C:\\Users\\user\\OneDrive\\Desktop\\Unreal_Experiment\\UnrealData\\Plans\\repeat_JND")
    retake_filename<-gsub(" ","",paste("trials_to_repeat_JND_attempt_",attempt,"_sub_",as.character(sub_n),".csv"))
    fwrite(final_retake,retake_filename,col.names=FALSE)
  }
}

#save(repeat_list, file = 'C:\\Users\\User\\OneDrive\\Desktop\\unreal_psychophysics\\rdas\\repeat_list.rda')
