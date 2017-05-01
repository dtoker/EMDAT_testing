# Dereck Toker
# January 9, 2017
# UBC - Department of Computer Science

# Last UPDATE: January 9, 2017

# This script is part 2 of 2, in order to test the correctness of EMDAT files
# Here, we take internam EMDAT data, and verify it's being correctly output

#Reading in:
  #EMDAT Internal Files: EMDATdata_VARIABLES_PXX.txt


#Testing:
  # EMDAT Output Files: tobiiv3_sample_features.tsv

# EMDAT PARAMS UTILIZED
VALID_SAMPLES_PROP_SACCADE = 1


setwd("C:/Users/admin/Dropbox/PhD/EMDAT-testing/Part2_EMDATInternal_EMDATOutput")




readfiles_part2 <- function(participant, scenes){
  
  emdat_export.df <- read.csv(paste("tobiiv3_sample_features_P",  participant, ".tsv", sep=""), sep="\t")
  
  scenes_all <- c(scenes, paste(participant,"_allsc", sep=""))
  
  
  #loop over the scenes
  for (scene in scenes_all) {

      emdat_export.df.scene <- subset(emdat_export.df, Sc_id == scene)
   
      checked_result1 <- check_correctness_fix(emdat_export.df.scene, participant, scene)
      #checked_result2 <- check_correctness_sac(...)
      #checked_result3 <- check_correctness_eve(...)
      #checked_result4 <- check_correctness_gazesample(...)
        
      #browser()
  }
}




# This function checks the correctness of fixations
# LIST OF COLUMS TO TEST:
#  numfixations
#  sumfixationduration
#  stddevfixationduration
#  meanfixationduration

#  fixationrate ONGOING

#  TODO:
#  numsegments
#  length
#  abspathanglesrate
#  eyemovementvelocity
#  fixationsaccadetimeratio
#  meanabspathangles
#  meanpathdistance
#  meanrelpathangles
#  relpathanglesrate
#  stddevabspathangles
#  stddevpathdistance
#  stddevrelpathangles
#  sumabspathangles
#  sumpathdistance
#  sumrelpathangles

check_correctness_fix <- function(emdat_output.df, participant, a_scene){
  
  #read in the corresponding internal EMDAT data file
  internal_data.df <- read.csv(paste("EMDATdata_fix_P", participant, ".tsv", sep=""), sep="\t")
 
  #subset the file according to the scene (if scene == XX_allsc, then keep it all)
  if (grepl("all",a_scene)){
    internal_data.df <- subset(internal_data.df, grepl(paste("P",participant,sep=""),scene))
  } 
  else {
    internal_data.df <- subset(internal_data.df, !grepl(paste("P",participant,sep=""),scene))
    internal_data.df <- subset(internal_data.df, grepl(a_scene,scene))
  }
  
  
  # numfixations
  output_value <- subset(emdat_output.df, select=numfixations)[1,]
  try(if(nrow(internal_data.df) != output_value)
    stop(paste("Error: numfixation does not match for participant:", participant, " and scene: ", a_scene)))
  
  # sumfixationduration
  internal_value <- sum(subset(internal_data.df, select=fixationduration))
  output_value <- subset(emdat_output.df, select=sumfixationduration)[1,]
  try(if( internal_value  != output_value)
    stop(paste("Error: sumfixationduration does not match for participant:", participant, " and scene: ", a_scene)))
  
  #browser()
  
  # stddevfixationduration
  internal_value <- round(sd(subset(internal_data.df, select=fixationduration)$fixationduration), digits=7)
  output_value <- subset(emdat_output.df, select=stddevfixationduration)[1,]
  try(if( internal_value  != output_value)
    stop(paste("Error: stddevfixationduration does not match for participant:", participant, " and scene: ", a_scene)))
  
  #  meanfixationduration
  internal_value <- round(mean(subset(internal_data.df, select=fixationduration)$fixationduration), digits=7)
  output_value <- subset(emdat_output.df, select=meanfixationduration)[1,]
  try(if( internal_value  != output_value)
    stop(paste("Error: meanfixationduration does not match for participant:", participant, " and scene: ", a_scene)))
  
  
  #  fixationrate
  segment_start_time <- subset(head(internal_data.df,1), select=timestamp)[1,]
  segment_end_time <- subset(tail(internal_data.df,1), select=timestamp)[1,]
  internal_value <- signif((nrow(internal_data.df) / (segment_end_time - segment_start_time)), digits=9)
  output_value <- subset(emdat_output.df, select=fixationrate)[1,]
  
  browser()
  try(if( internal_value  != output_value)
    stop(paste("Error: fixationrate does not match for participant:", participant, " and scene: ", a_scene)))
  
}



# This function checks the correctness of saccades
# LIST OF COLUMS TO TEST:
#  longestsaccadedistance
#  longestsaccadeduration
#  maxsaccadespeed
#  numsaccades
#  stddevsaccadedistance
#  stddevsaccadeduration
#  stddevsaccadespeed
#  sumsaccadedistance
#  sumsaccadeduration
#  meansaccadedistance
#  meansaccadeduration
#  meansaccadespeed
#  minsaccadespeed 



# This function checks the correctness of events
# LIST OF COLUMS TO TEST:
#  doubleclicrate
#  keypressedrate
#  leftclicrate
#  numdoubleclic
#  numevents
#  numkeypressed
#  numleftclic
#  numrightclic
#  rightclicrate
#  timetofirstdoubleclic
#  timetofirstkeypressed
#  timetofirstleftclic
#  timetofirstrightclic




# This function checks the correctness of pupil and head distance
# LIST OF COLUMS TO TEST:
#  numsamples
#  enddistance
#  endpupilsize
#  maxdistance
#  maxpupilsize
#  maxpupilvelocity
#  meandistance
#  meanpupilsize
#  meanpupilvelocity
#  mindistance
#  minpupilsize
#  minpupilvelocity
#  startdistance
#  startpupilsize
#  stddevdistance
#  stddevpupilsize
#  stddevpupilvelocity



#Scene, Segment

P16 <- readfiles_part2("16", c("part1","part2"),1,1)
P17 <- readfiles_part2("17", c("part1","part2"),1,1)
P18 <- readfiles_part2("18", c("main_task"), 2)


