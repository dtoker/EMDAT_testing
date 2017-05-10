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

# C:/Users/admin/Dropbox/PhD/EMDAT-testing/Part2_EMDATInternal_EMDATOutput
setwd("C:/git00/EMDAT_testing_actual/Part2_EMDATInternal_EMDATOutput")
source("C:/git00/EMDAT_testing_actual/EMDAT_test_utils.R")

readfiles_part2 <- function(participant, seg_file){
  
  emdat_export.df <- read.csv(paste("tobiiv3_sample_features_P",  participant, ".tsv", sep=""), sep="\t")
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  
  #extract scene names
  scene.names <- unique(seg_file.df[,"scene"])
  
  # loop over the scenes
  for (a_scene in scene.names) {
    
    # extract segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
    
    # checked_result1 <- check_correctness_fix(emdat_export.df.scene, participant, a_scene,
    #                                          segment.names)
    #checked_result2 <- check_correctness_sac(emdat_export.df.scene, participant, a_scene)
    checked_result3 <- check_correctness_eve(emdat_export.df.scene, participant, a_scene,
                                             segment.names)
    # checked_result4 <- check_correctness_gazesample(emdat_export.df.scene, participant, a_scene,
    #                                                 segment.names)
    
    #browser()
    }
}


# This function checks the correctness of fixations
# LIST OF COLUMS TO TEST:
#  numfixations
#  sumfixationduration
#  stddevfixationduration
#  meanfixationduration
#  numsegments
#  sumpathdistance
#  meanpathdistance
#  stddevpathdistance
#  sumabspathangles
#  meanabspathangles
#  stddevabspathangles
#  fixationrate
#  fixationsaccadetimeratio

#  TODO:
#  abspathanglesrate
#  eyemovementvelocity
#  meanrelpathangles
#  relpathanglesrate
#  stddevrelpathangles
#  sumrelpathangles

check_correctness_fix <- function(emdat_output.df, participant, a_scene, segment.names){
  
  # reads in the needed internal EMDAT data files
  internal_data.df <- read.csv(paste("EMDATdata_fix_P", participant, ".tsv", sep=""), sep="\t")
  gazesample_data.df <- read.csv(paste("EMDATdata_gazesample_P", participant, ".tsv", sep=""), sep="\t")
  saccade_data.df <- read.csv(paste("EMDATdata_sac_P", participant, ".tsv", sep=""), sep="\t")
  
  # keeps all segments belonging to the scene in a vector 
  # only one data set (P18) contains a scene consisting of multiple segments     
  internal_data.df <- subset(internal_data.df, grepl(a_scene, scene))
  gazesample_data.df <- subset(gazesample_data.df, grepl(a_scene, scene))
  saccade_data.df <- subset(saccade_data.df, grepl(a_scene, scene))
  
  internal_data_vector <- c()
  gazesample_data_vector <- c()
  saccade_data_vector <- c()
  
  for(i in 1:length(segment.names)) {
    
    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
    gazesample_data_vector[[i]] <- subset(gazesample_data.df, grepl(segment.names[i], scene))
    saccade_data_vector[[i]] <- subset(saccade_data.df, grepl(segment.names[i], scene))
  }
 
### numfixations ###
  output_value <- subset(emdat_output.df, select=numfixations)[1,]
  verify_equivalence(nrow(internal_data.df),output_value, participant, a_scene, "numfixation")
  
### sumfixationduration ###
  internal_value <- sum(subset(internal_data.df, select=fixationduration))
  output_value <- subset(emdat_output.df, select=sumfixationduration)[1,]
  verify_equivalence(internal_value, output_value, participant, a_scene, "sumfixationduration")
  
### stddevfixationduration ###
  internal_value <- round(sd(subset(internal_data.df, select=fixationduration)$fixationduration), digits=9)
  output_value <- subset(emdat_output.df, select=stddevfixationduration)[1,]
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevfixationduration")
  
### meanfixationduration ###
  internal_value <- round(mean(subset(internal_data.df, select=fixationduration)$fixationduration), digits=9)
  output_value <- subset(emdat_output.df, select=meanfixationduration)[1,]
  verify_equivalence(internal_value, output_value, participant, a_scene, "meanfixationduration")

###  fixationrate ###
  output_value <- subset(emdat_output.df, select=fixationrate)[1,]
  scene_length <- compute_scene_length(segment.names, gazesample_data_vector)
  internal_value <- signif((nrow(internal_data.df) / scene_length), digits=12)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "fixationrate")
  
###  numsegments ###
  output_value <- subset(emdat_output.df, select=numsegments)[1,]
  internal_value <- length(segment.names)
  verify_equivalence(internal_value, output_value, participant, a_scene, "numsegments")

### sumpathdistance ###
### meanpathdistance ###  
  output_value_sum <- subset(emdat_output.df, select=sumpathdistance)[1,]
  output_value_mean <- subset(emdat_output.df, select = meanpathdistance)[1,]
  
  internal_value_sum <- 0
  numerator <- 0
  denominator <- 0
  
  for(i in 1:length(segment.names)){
    
    path_length_vector <- find_path_length_vector(
      internal_data_vector[[i]]$mappedfixationpointx,
      internal_data_vector[[i]]$mappedfixationpointy
    )
    
    internal_value_sum <- internal_value_sum + sum(path_length_vector)
    numerator <- numerator+length(path_length_vector)*mean(path_length_vector)
    denominator <- denominator+length(path_length_vector)
  }
  
  internal_value_sum <- signif(internal_value_sum, digits = 12)
  internal_value_mean_temp <- numerator/denominator
  internal_value_mean <- signif(internal_value_mean_temp, digits = 12)
  
  verify_equivalence(internal_value_sum, output_value_sum, participant, a_scene, "sumpathdistance")
  verify_equivalence(internal_value_mean, output_value_mean, participant, a_scene, "meanpathdistance")

### stddevpathdistance ###
  # for obj in obj_list:
  #   t = eval('obj.'+totalfeat)
  # if t > 0:
  #   sd = eval('obj.'+sdfeat)
  # if math.isnan(sd): sd = 0
  # meanobj = eval('obj.'+meanfeat)
  # 
  # num += (t-1) * sd**2 + t * (meanobj-meanscene)**2
  # den += t
  # 
  # if den > 1:
  #   return math.sqrt(float(num)/(den-1))
  # return 0
  
  output_value <- subset(emdat_output.df, select = stddevpathdistance)[1,]
  numerator <- 0
  denominator <- 0

  for(i in 1:length(segment.names)){

    path_length_vector <- find_path_length_vector(
      internal_data_vector[[i]]$mappedfixationpointx,
      internal_data_vector[[i]]$mappedfixationpointy
    )

    numerator <- numerator+(length(path_length_vector)-1)*(sd(path_length_vector)^2)+
                                   length(path_length_vector)*(mean(path_length_vector)-internal_value_mean_temp)^2
    
    denominator <- denominator+length(path_length_vector)

  }
  internal_value <- signif(sqrt(numerator/(denominator-1)), digits = 12)
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevpathdistance")
  
  
### sumabspathangles ###
### meanabspathangles ###  
  
  output_sum_absangle <- subset(emdat_output.df, select = sumabspathangles)[1,]
  internal_sum_absangle <-0
  
  output_mean_absangle <- subset(emdat_output.df, select = meanabspathangles)[1,]
  internal_mean_absangle <-0
  numerator <- 0
  denominator <- 0
  
  for(i in 1:length(segment.names)){
    
    abs_angle_vector <- find_abs_angle_vector(internal_data_vector[[i]]$mappedfixationpointx,
                                              internal_data_vector[[i]]$mappedfixationpointy)
    
    internal_sum_absangle <- internal_sum_absangle + sum(abs_angle_vector)
    
    numerator <- numerator+ 
      length(abs_angle_vector)*mean(abs_angle_vector)
    denominator <- denominator+length(abs_angle_vector)
  }
  
  internal_sum_absangle <- signif(internal_sum_absangle, digits = 12)
  internal__mean_absangle_temp <- numerator/denominator
  internal_mean_absangle <- signif(internal__mean_absangle_temp, digits = 12)
  
  verify_equivalence(internal_sum_absangle, output_sum_absangle, participant, a_scene, "sumabspathangles")
  verify_equivalence(internal_mean_absangle, output_mean_absangle, participant, a_scene, "meanabspathangles")

### stddevabspathangles ###
  # for obj in obj_list:
  #   t = eval('obj.'+totalfeat)
  # if t > 0:
  #   sd = eval('obj.'+sdfeat)
  # if math.isnan(sd): sd = 0
  # meanobj = eval('obj.'+meanfeat)
  # 
  # num += (t-1) * sd**2 + t * (meanobj-meanscene)**2
  # den += t
  # 
  # if den > 1:
  #   return math.sqrt(float(num)/(den-1))
  # return 0
  
  output_value <- subset(emdat_output.df, select = stddevabspathangles)[1,]
  numerator <- 0
  denominator <- 0
  
  for(i in 1:length(segment.names)){
    
    abs_angle_vector <- find_abs_angle_vector(internal_data_vector[[i]]$mappedfixationpointx,
                                              internal_data_vector[[i]]$mappedfixationpointy)
    
    numerator <- numerator+(length(abs_angle_vector)-1)*(sd(abs_angle_vector)^2)+
      length(abs_angle_vector)*(mean(abs_angle_vector)-internal__mean_absangle_temp)^2
    
    denominator <- denominator+length(abs_angle_vector)
    
  }
  internal_value <- signif(sqrt(numerator/(denominator-1)), digits = 12)
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevabspathangles")
  
### fixationsaccadetimeratio ###
  output_value <- subset(emdat_output.df, select=fixationsaccadetimeratio)[1,]
  numerator <- 0
  segs_size <- length(segment.names)
  
  for(i in 1:segs_size){
    
    fix_sum <- sum(subset(internal_data_vector[[i]], select=fixationduration))
    sac_sum <- sum(subset(saccade_data_vector[[i]], select=saccadeduration))
    numerator <- numerator + fix_sum/sac_sum
  }
  internal_value <- signif(numerator/segs_size, digits=12) 
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "fixationsaccadetimeratio")
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
#  sumsaccadeduration
#  meansaccadedistance
#  meansaccadeduration
#  meansaccadespeed
#  minsaccadespeed

# TO BE REVISITED:
#  sumsaccadedistance (error for P16 part2: last digit off by 0.00001)

check_correctness_sac <- function(emdat_output.df, participant, a_scene){
  
  # read in the corresponding internal EMDAT data file
  internal_data.df <- read.csv(paste("EMDATdata_sac_P", participant, ".tsv", sep=""), sep="\t")
  
  # keeps all segments belonging to the scene
  # only one data set (P18) contains a scene consisting of multiple segments     
  internal_data.df <- subset(internal_data.df, grepl(a_scene, scene))
 
### longestsaccadedistance ###
  output_value <- subset(emdat_output.df, select=longestsaccadedistance)[1,]
  internal_value <- signif(max(subset(internal_data.df, select=saccadedistance)$saccadedistance), digits = 12)
  
  try(if(internal_value  != output_value)
    stop(paste("Error: longestsaccadedistance does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### longestsaccadeduration ###
  output_value <- subset(emdat_output.df, select=longestsaccadeduration)[1,]
  internal_value <- max(subset(internal_data.df, select=saccadeduration)$saccadeduration)
  
  try(if(internal_value  != output_value)
    stop(paste("Error: longestsaccadeduration does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### maxsaccadespeed ###  
  output_value <- subset(emdat_output.df, select=maxsaccadespeed)[1,]
  internal_value <- max(subset(internal_data.df, select=saccadespeed)$saccadespeed)
  
  try(if(internal_value  != output_value)
    stop(paste("Error: maxsaccadespeed does not match for participant:", participant, " and scene: ", a_scene))
  )

### numsaccades ###
  output_value <- subset(emdat_output.df, select=numsaccades)[1,]
  internal_value <- nrow(internal_data.df)
  
  try(if(internal_value  != output_value)
    stop(paste("Error: numsaccades does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### stddevsaccadedistance ###
  output_value <- subset(emdat_output.df, select=stddevsaccadedistance)[1,]
  internal_value <- signif(sd(subset(internal_data.df, select=saccadedistance)$saccadedistance), 
                           digits = 10)
  
  try(if(internal_value  != output_value)
    stop(paste("Error: stddevsaccadedistance does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### stddevsaccadeduration ###
  output_value <- subset(emdat_output.df, select=stddevsaccadeduration)[1,]
  internal_value <- signif(sd(subset(internal_data.df, select=saccadeduration)$saccadeduration), 
                           digits = 10)
  
  try(if(internal_value  != output_value)
    stop(paste("Error: stddevsaccadeduration does not match for participant:", participant, " and scene: ", a_scene))
  )

### stddevsaccadespeed ###
  output_value <- subset(emdat_output.df, select=stddevsaccadespeed)[1,]
  internal_value <- signif(sd(subset(internal_data.df, select=saccadespeed)$saccadespeed), 
                           digits = 10)
  
  try(if(internal_value  != output_value)
    stop(paste("Error: stddevsaccadespeed does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### sumsaccadedistance (NEEDS FIX)###
  
  # error for P16 scene part2: internal_value = 43154.20657, output_value = 43154.20658
  
  output_value <- subset(emdat_output.df, select=sumsaccadedistance)[1,]
  internal_value <- signif(sum(subset(internal_data.df, select=saccadedistance)$saccadedistance),
                           digits = 10)
  
  try(if(internal_value != output_value)
    stop(paste("Error: sumsaccadedistance does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### sumsaccadeduration ###
  output_value <- subset(emdat_output.df, select=sumsaccadeduration)[1,]
  internal_value <- sum(subset(internal_data.df, select=saccadeduration)$saccadeduration)
  
  try(if(internal_value != output_value)
    stop(paste("Error: sumsaccadeduration does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### meansaccadedistance ###
  output_value <- subset(emdat_output.df, select=meansaccadedistance)[1,]
  internal_value <- signif(mean(subset(internal_data.df, select=saccadedistance)$saccadedistance),
                           digits = 10)
  
  try(if(internal_value != output_value)
    stop(paste("Error: meansaccadedistance does not match for participant:", participant, " and scene: ", a_scene))
  )

### meansaccadeduration ###
  output_value <- subset(emdat_output.df, select=meansaccadeduration)[1,]
  internal_value <- signif(mean(subset(internal_data.df, select=saccadeduration)$saccadeduration),
                           digits = 10)
  
  try(if(internal_value != output_value)
    stop(paste("Error: meansaccadeduration does not match for participant:", participant, " and scene: ", a_scene))
  )

### meansaccadespeed ###
  output_value <- subset(emdat_output.df, select=meansaccadespeed)[1,]
  internal_value <- signif(mean(subset(internal_data.df, select=saccadespeed)$saccadespeed),
                           digits = 10)
  
  try(if(internal_value != output_value)
    stop(paste("Error: meansaccadespeed does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### minsaccadespeed ###  
  output_value <- subset(emdat_output.df, select=minsaccadespeed)[1,]
  internal_value <- min(subset(internal_data.df, select=saccadespeed)$saccadespeed)
  
  try(if(internal_value != output_value)
    stop(paste("Error: minsaccadespeed does not match for participant:", participant, " and scene: ", a_scene))
  )
}  

# This function checks the correctness of events
# LIST OF COLUMS TO TEST:
#  doubleclicrate
#  leftclicrate
#  keypressedrate
#  numkeypressed
#  numdoubleclic
#  numleftclic
#  numrightclic
#  rightclicrate
#  numevents
#  timetofirstdoubleclic
#  timetofirstleftclic
#  timetofirstkeypressed
#  timetofirstrightclic

#  TO DO:


check_correctness_eve <- function(emdat_output.df, participant, a_scene, segment.names){
  
### set up the tests ###
  
  # read in the needed internal EMDAT data files
  internal_data.df <- read.csv(paste("EMDATdata_eve_P", participant, ".tsv", sep=""), sep="\t")
  gazesample_data.df <- read.csv(paste("EMDATdata_gazesample_P", participant, ".tsv", sep=""), sep="\t")
  
  # keeps all segments belonging to the scene
  # only one data set (P18) contains a scene consisting of multiple segments     
  internal_data.df <- subset(internal_data.df, grepl(a_scene, scene))
  gazesample_data.df <- subset(gazesample_data.df, grepl(a_scene, scene))
  
  internal_data_vector <- c()
  gazesample_data_vector <- c()
  
  for(i in 1:length(segment.names)) {
    
    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
    gazesample_data_vector[[i]] <- subset(gazesample_data.df, grepl(segment.names[i], scene))
  }
  
  length <- compute_scene_length(segment.names, gazesample_data_vector)

### numdoubleclic ###
  output_value <- subset(emdat_output.df, select=numdoubleclic)[1,]
  double_clicks <- 0
  left_clicks <- 0

  for(i in 1:length(segment.names)){

    clicks <- find_double_and_left_clicks(internal_data_vector[[i]])
    double_clicks <- double_clicks + clicks[1]
    left_clicks <- left_clicks + clicks[2]
    if(i == 1){
      first_double_click <- clicks[3]
      first_left_click <- clicks[4]
    }
  }
  internal_value <- double_clicks

  verify_equivalence(internal_value, output_value, participant, a_scene, "numdoubleclic")

### timetofirstdoubleclic ###
  output_value <- subset(emdat_output.df, select=timetofirstdoubleclic)[1,]
  internal_value <- first_double_click
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "timetofirstdoubleclic")
  
### doubleclicrate ###
  output_value <- subset(emdat_output.df, select=doubleclicrate)[1,]
  internal_value <- signif(double_clicks/length, digits=12)

  verify_equivalence(internal_value, output_value, participant, a_scene, "doubleclicrate")

### numleftclic ###
  output_value <- subset(emdat_output.df, select=numleftclic)[1,]
  internal_value <- left_clicks
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "numleftclic")

### timetofirstleftclic ###
  output_value <- subset(emdat_output.df, select=timetofirstleftclic)[1,]
  internal_value <- first_left_click
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "timetofirstleftclic")
  
### leftclicrate ###
  output_value <- subset(emdat_output.df, select=leftclicrate)[1,]
  internal_value <- signif(left_clicks/length, digits=12)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "leftclicrate")  

### numkeypressed ###  
  output_value <- subset(emdat_output.df, select=numkeypressed)[1,]
  keypress.df <- subset(internal_data.df, event=="KeyPress")
  
  keypress <- nrow(keypress.df)
  
  internal_value <- keypress
  verify_equivalence(internal_value, output_value, participant, a_scene, "numkeypressed")

### timetofirstkeypressed ###
  output_value <- subset(emdat_output.df, select=timetofirstkeypressed)[1,]
  first_keypress.df <- subset(internal_data_vector[[1]], event=="KeyPress")
  
  if(nrow(first_keypress.df) == 0){
    first_keypressed <- -1
  }else{
    first_keypressed <- subset(first_keypress.df, select=timestamp)[1,]
  }
  internal_value <- first_keypressed
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "timetofirstkeypressed")
  
### keypressedrate ###
  output_value <- subset(emdat_output.df, select=keypressedrate)[1,]
  internal_value <- signif(keypress/length, digits=12) 
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "keypressedrate")
  
### numrightclic ###  
  output_value <- subset(emdat_output.df, select=numrightclic)[1,]
  rightclicks.df <- subset(internal_data.df, event=="RightMouseClick")
  
  rightclicks <- nrow(rightclicks.df)
  
  internal_value <- rightclicks
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "numrightclic")
  
### timetofirstrightclic ###
  output_value <- subset(emdat_output.df, select=timetofirstrightclic)[1,]
  first_rightclick.df <- subset(internal_data_vector[[1]], event=="RightMouseClick")
  
  if(nrow(first_rightclick.df) == 0){
    first_rightclick <- -1
  }else{
    first_rightclick <- subset(first_rightclick.df, select=timestamp)[1,]
  }
  internal_value <- first_rightclick
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "timetofirstrightclic")
  
### rightclicrate ###
  output_value <- subset(emdat_output.df, select=rightclicrate)[1,]
  internal_value <- signif(rightclicks/length, digits=12) 
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "rightclicrate")
  
### numevents ###
  output_value <- subset(emdat_output.df, select=numevents)[1,]
  internal_value <- double_clicks + left_clicks + keypress + rightclicks
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "numevents")
}

# This function checks the correctness of pupil and head distance
# LIST OF COLUMS TO TEST:
#  numsamples
#  enddistance (Assumed: the enddistance is the last valid headdistance)
#  endpupilsize (Assumed: the endpupilsize is the last valid rawpupilsize)
#  maxdistance (Assumed:only valied values considered)
#  maxpupilsize (Assumed:only valied values considered)
#  maxpupilvelocity (Assumed: -1 values are disregarded)
#  meanpupilsize (Assumed:only valied values considered)
#  meanpupilvelocity (Assumed: -1 values are disregarded. Note: R 'round' rather than 'signif' is used here)
#  mindistance (Assumed:only valied values considered)
#  minpupilsize (Assumed:only valied values considered)
#  minpupilvelocity (Assumed: -1 values are disregarded)
#  startdistance (Assumed: the startdistance is the first valid headdistance)
#  startpupilsize (Assumed: the startpupilsize is the first valid rawpupilsize)
#  stddevpupilsize (Note: R 'round' rather than 'signif' is used here)
#  stddevpupilvelocity (Assumed: -1 values are disregarded. Note: R 'round' rather than 'signif' is used here)
#  length 

# TO REVISIT:
#  meandistance (Error for P 18. Assumed: only valid values considered)
#  stddevdistance (Error for P 18. Assumed: only valid values considered)



check_correctness_gazesample <- function(emdat_output.df, participant, a_scene, segment.names){
  
  # read in the needed internal EMDAT data file
  internal_data.df <- read.csv(paste("EMDATdata_gazesample_P", participant, ".tsv", sep=""), sep="\t")
  
  # keeps all segments belonging to the scene in the vetor format  
  # only one data set (P18) contains a scene consisting of multiple segments     
  internal_data.df <- subset(internal_data.df, grepl(a_scene, scene))
  internal_data_vector <- c()
  for(i in 1:length(segment.names)) {
    
    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
  }
  
  
### numsamples ###
  output_value <- subset(emdat_output.df, select=numsamples)[1,]
  internal_value <- nrow(internal_data.df)
  
  try(if(internal_value != output_value)
    stop(paste("Error: numsamples does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### enddistance ###
  
  # Assumption: enddistance is the last valid headdistance
  
  output_value <- subset(emdat_output.df, select=enddistance)[1,]
  internal_value <- tail(
    subset(internal_data.df, select=headdistance, is_valid_headdistance==TRUE)$headdistance, 1)
  
  try(if(internal_value != output_value)
    stop(paste("Error: enddistance does not match for participant:", participant, " and scene: ", a_scene))
  )

### endpupilsize ###
  
  # Assumption: endpupilsize is the last valid rawpupilsize
  
  output_value <- subset(emdat_output.df, select=endpupilsize)[1,]
  internal_value <- tail(
    subset(internal_data.df, select=rawpupilsize, is_valid_pupil==TRUE)$rawpupilsize, 1)
  
  try(if(internal_value != output_value)
    stop(paste("Error: endpupilsize does not match for participant:", participant, " and scene: ", a_scene))
  )

### maxdistance ###
  output_value <- subset(emdat_output.df, select=maxdistance)[1,]
  
  # Since the max happens to be non-negative in this case, the is_valid_headdistance==TRUE condition
  # does not make difference. But the internal value does not match the output_value in the 
  # corresponding  mindistancetest without that condition. So to be consistent, the condition 
  # is added here as well.
  internal_value <- max(subset(internal_data.df, select=headdistance, is_valid_headdistance==TRUE)$headdistance)
  
  try(if(internal_value != output_value)
    stop(paste("Error: maxdistance does not match for participant:", participant, " and scene: ", a_scene))
  )

###  maxpupilsize ###
  output_value <- subset(emdat_output.df, select=maxpupilsize)[1,]
  
  # Remark similar to the above on the condition of R subset operation holds here as well.   
  internal_value <- max(subset(internal_data.df, select=rawpupilsize, is_valid_pupil==TRUE)$rawpupilsize)
  
  try(if(internal_value != output_value)
    stop(paste("Error: maxpupilsize does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### maxpupilvelocity ###
  output_value <- subset(emdat_output.df, select=maxpupilvelocity)[1,]
  
  # The condition in the subet operaiton does not make practical difference here. But, the convention
  # is followed in computing the mean and min, so that it is added here for consistency.      
  internal_value <- max(subset(internal_data.df, select=pupilvelocity, pupilvelocity != -1)$pupilvelocity)
  
  try(if(internal_value != output_value)
    stop(paste("Error: maxpupilvelocity does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### meandistance(NEEDS FIX) ### ##
  
  # Error for P18: internal_value = 624.6470933, output_value = 625.018883
  # Assumption: only valid values are considered
  
  output_value <- subset(emdat_output.df, select=meandistance)[1,]
  
  numerator <- 0
  denominator <- 0
  
  for(i in 1:length(segment.names)){
    
    valid_data = subset(internal_data_vector[[i]], select=headdistance, is_valid_headdistance==TRUE)
    numerator <- numerator+nrow(valid_data)*mean(subset(valid_data)$headdistance)
    denominator <- denominator+nrow(valid_data)
  }
  
  internal_mean_dist_temp <- numerator/denominator
  internal_mean_dist <- signif(internal_mean_dist_temp, digits = 10)
  
  verify_equivalence(internal_mean_dist, output_value, participant, a_scene, "meandistance")
  
  
  
  # output_value <- subset(emdat_output.df, select=meandistance)[1,]
  # internal_value <- signif(
  #   mean(subset(internal_data.df, select=headdistance, is_valid_headdistance==TRUE)$headdistance),
  #   digits = 10)
  # 
  # try(if(internal_value != output_value)
  #   stop(paste("Error: meandistance does not match for participant:", participant, " and scene: ", a_scene))
  # )
  
### meanpupilsize ###
  
  # Assumption: only valid values are considered
  
  output_value <- subset(emdat_output.df, select=meanpupilsize)[1,]
  internal_value <- signif(
    mean(subset(internal_data.df, select=rawpupilsize, is_valid_pupil==TRUE)$rawpupilsize),
    digits = 10)
  
  try(if(internal_value != output_value)
    stop(paste("Error: meanpupilsize does not match for participant:", participant, " and scene: ", a_scene))
  )

### meanpupilvelocity ###
  
  # Assumption: the value -1 is disregarded
  # Note: round rather than siginf is used 
  
  output_value <- subset(emdat_output.df, select=meanpupilvelocity)[1,]
  internal_value <- round(
    mean(subset(internal_data.df, select=pupilvelocity, pupilvelocity != -1)$pupilvelocity),
    digits = 9)
  
  try(if(internal_value != output_value)
    stop(paste("Error: meanpupilvelocity does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### mindistance ###
  output_value <- subset(emdat_output.df, select=mindistance)[1,]
  internal_value <- min(subset(internal_data.df, select=headdistance, is_valid_headdistance==TRUE)$headdistance)
  
  try(if(internal_value != output_value)
    stop(paste("Error: mindistance does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### minpupilsize ###
  output_value <- subset(emdat_output.df, select=minpupilsize)[1,]
  internal_value <- min(subset(internal_data.df, select=rawpupilsize, is_valid_pupil==TRUE)$rawpupilsize)
  
  try(if(internal_value != output_value)
    stop(paste("Error: minpupilsize does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### minpupilvelocity ###
  
  # Assumption: the value -1 is disregarded 
  
  output_value <- subset(emdat_output.df, select=minpupilvelocity)[1,]
  internal_value <- min(subset(internal_data.df, select=pupilvelocity, pupilvelocity != -1)$pupilvelocity)
  
  try(if(internal_value != output_value)
    stop(paste("Error: minpupilvelocity does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### startdistance ###
  
  # Assumption: startdistance is the first valid headdistance
  
  output_value <- subset(emdat_output.df, select=startdistance)[1,]
  internal_value <- head(
    subset(internal_data.df, select=headdistance, is_valid_headdistance==TRUE)$headdistance, 1)
  
  try(if(internal_value != output_value)
    stop(paste("Error: startdistance does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### startpupilsize ###

  # Assumption: startpupilsize is the first valid rawpupilsize

  output_value <- subset(emdat_output.df, select=startpupilsize)[1,]
  internal_value <- head(
    subset(internal_data.df, select=rawpupilsize, is_valid_pupil==TRUE)$rawpupilsize, 1)

  try(if(internal_value != output_value)
    stop(paste("Error: startpupilsize does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### stddevdistance (NEEDS FIX)###
  
  # Error for P18: internal_value = 28.4432276, output_value = 28.12112579
  # Assumption: only valid values are considered
  
  output_value <- subset(emdat_output.df, select=stddevdistance)[1,]
  internal_value <- signif(
    sd(subset(internal_data.df, select=headdistance, is_valid_headdistance==TRUE)$headdistance),
    digits = 10)
  
  try(if(internal_value != output_value)
    stop(paste("Error: stddevdistance does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### stddevpupilsize ###
  
  # Assumption: only valid values are considered
  # Note: round rather than signif is used
  
  output_value <- subset(emdat_output.df, select=stddevpupilsize)[1,]
  internal_value <- round(
    sd(subset(internal_data.df, select=rawpupilsize, is_valid_pupil==TRUE)$rawpupilsize),
    digits = 9)
  
  try(if(internal_value != output_value)
    stop(paste("Error: stddevpupilsize does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### stddevpupilvelocity ###
  
  # Assumption: pupilvelocity = -1 is disregarded
  # Note: round rather than signif is used
  
  output_value <- subset(emdat_output.df, select=stddevpupilvelocity)[1,]
  internal_value <- round(
    sd(subset(internal_data.df, select=pupilvelocity, pupilvelocity != -1)$pupilvelocity),
    digits = 9)
  
  try(if(internal_value != output_value)
    stop(paste("Error: stddevpupilvelocity does not match for participant:", participant, " and scene: ", a_scene))
  )
  
### length ###
  
  output_value <- subset(emdat_output.df, select=length)[1,]
  internal_value <- compute_scene_length(segment.names, internal_data_vector)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "length")
  
}

P16 <- readfiles_part2("16", "TobiiV3_sample_16.seg")
P17 <- readfiles_part2("17", "TobiiV3_sample_17.seg")
P18 <- readfiles_part2("18", "TobiiV3_sample_18.seg")

