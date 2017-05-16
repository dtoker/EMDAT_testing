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

### REMARKS ###

# 1) The logic for processing segment level data in calculating the corresponding scene level 
#    feature value in the original EMDAT code is not necessarily reflected in computation of 
#    the expected value in the tests, unless it is deemed essential as, for instance, in 
#    computing a weigthed mean or in processing fixation, where the data straddling segment 
#    boundaries can affect the computed value;
# 2) Given the intended goal of these tests, namely checking the accuracy of the computed output 
#    values from EMDAT by using the different langauge, R, rather than maximizing branch coverage of
#    the python EMDAT code, the test script, as is, may fail if edge case values are passed. A degree
#    of contorl over the input is assumed.


### HOW TO RUN ###

# Set the working directory with the file path to the test package (i.e. setwd(<file path>)), 
# and then 'source' EMDAT_part2.R.  
# Note: the file path to the test package is assumed not to contain 'Part2_EMDATInternal_EMDATOutput' 
# in the name.
wd <- getwd()
if(!grepl("/Part2_EMDATInternal_EMDATOutput", wd)){
  source("EMDAT_testUtils.R", chdir = T)
  setwd(paste(wd, "/Part2_EMDATInternal_EMDATOutput", sep = ""))
}

### TEST SCRIPT ###

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
    
    checked_result1 <- check_correctness_fix(emdat_export.df.scene, participant, a_scene,
                                             segment.names)
    checked_result2 <- check_correctness_sac(emdat_export.df.scene, participant, a_scene,
                                             segment.names)
    checked_result3 <- check_correctness_eve(emdat_export.df.scene, participant, a_scene,
                                             segment.names)
    checked_result4 <- check_correctness_gazesample(emdat_export.df.scene, participant, a_scene,
                                                    segment.names)
  }
  report_success(participant)
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
#  sumrelpathangles
#  meanrelpathangles
#  stddevrelpathangles
#  abspathanglesrate
#  eyemovementvelocity
#  relpathanglesrate

check_correctness_fix <- function(emdat_output.df, participant, a_scene, segment.names){
  
### set up the tests ###
  
  # reads in the needed internal EMDAT data files
  internal_data.df <- read.csv(paste("EMDATdata_fix_P", participant, ".tsv", sep=""), sep="\t")
  gazesample_data.df <- read.csv(paste("EMDATdata_gazesample_P", participant, ".tsv", sep=""), sep="\t")
  saccade_data.df <- read.csv(paste("EMDATdata_sac_P", participant, ".tsv", sep=""), sep="\t")
  
  # keeps all segments belonging to the scene in data frame format 
  # only one data set (P18) contains a scene consisting of multiple segments     
  internal_data.df <- subset(internal_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
  gazesample_data.df <- subset(gazesample_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
  saccade_data.df <- subset(saccade_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
  
  segs_length <- length(segment.names)
  
  # stores the data by segment into vectors 
  internal_data_vector <- c()
  gazesample_data_vector <- c()
  saccade_data_vector <- c()
  for(i in 1:segs_length) {
    
    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
    gazesample_data_vector[[i]] <- subset(gazesample_data.df, grepl(segment.names[i], scene))
    saccade_data_vector[[i]] <- subset(saccade_data.df, grepl(segment.names[i], scene))
  }
 
### numfixations ###
  output_value <- subset(emdat_output.df, select=numfixations)[1,]
  verify_equivalence(nrow(internal_data.df),output_value, participant, a_scene, "numfixations")
  
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

### fixationrate ###
  output_value <- subset(emdat_output.df, select=fixationrate)[1,]
  scene_length <- compute_scene_length(segment.names, gazesample_data_vector)
  internal_value <- signif((nrow(internal_data.df) / scene_length), digits=12)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "fixationrate")
  
### numsegments ###
  output_value <- subset(emdat_output.df, select=numsegments)[1,]
  internal_value <- length(segment.names)
  verify_equivalence(internal_value, output_value, participant, a_scene, "numsegments")

### sumpathdistance ###
### meanpathdistance ###
### eyemovementvelocity ###  
  output_sum <- subset(emdat_output.df, select=sumpathdistance)[1,]
  output_mean <- subset(emdat_output.df, select = meanpathdistance)[1,]
  output_velocity <- subset(emdat_output.df, select=eyemovementvelocity)[1,]
  
  results <- find_sum_mean_rate(internal_data_vector, 
                                find_path_length_vector, segs_length, scene_length)
  
  verify_equivalence(results$sum, output_sum, participant, a_scene, "sumpathdistance")
  verify_equivalence(results$mean, output_mean, participant, a_scene, "meanpathdistance")
  verify_equivalence(results$rate, output_velocity, participant, a_scene, "eyemovementvelocity")

### stddevpathdistance ###
  output_value <- subset(emdat_output.df, select = stddevpathdistance)[1,]
  internal_value <- find_fixation_sd(results$data_storage, results$temp_mean, segs_length)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevpathdistance")

### sumabspathangles ###
### meanabspathangles ###
### abspathanglesrate ###  
  output_sum <- subset(emdat_output.df, select = sumabspathangles)[1,]
  output_mean <- subset(emdat_output.df, select = meanabspathangles)[1,]
  output_rate <- subset(emdat_output.df, select = abspathanglesrate)[1,]
  
  results <- find_sum_mean_rate(internal_data_vector, 
                                find_abs_angle_vector, segs_length, scene_length)
  
  verify_equivalence(results$sum, output_sum, participant, a_scene, "sumabspathangles")
  verify_equivalence(results$mean, output_mean, participant, a_scene, "meanabspathangles")
  verify_equivalence(results$rate, output_rate, participant, a_scene, "abspathanglesrate")
  
### stddevabspathangles ###
  output_value <- subset(emdat_output.df, select = stddevabspathangles)[1,]
  internal_value <- find_fixation_sd(results$data_storage, results$temp_mean, segs_length)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevabspathangles")
  
### fixationsaccadetimeratio ###
  output_value <- subset(emdat_output.df, select=fixationsaccadetimeratio)[1,]
  numerator <- 0
  
  for(i in 1:segs_length){
    
    fix_sum <- sum(subset(internal_data_vector[[i]], select=fixationduration))
    sac_sum <- sum(subset(saccade_data_vector[[i]], select=saccadeduration))
    numerator <- numerator + fix_sum/sac_sum
  }
  internal_value <- signif(numerator/segs_length, digits=12) 
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "fixationsaccadetimeratio")
  
### sumrelpathangles ###
### meanrelpathangles ###
### relpathanglesrate ###  
  output_sum <- subset(emdat_output.df, select = sumrelpathangles)[1,]
  output_mean <- subset(emdat_output.df, select = meanrelpathangles)[1,]
  output_rate <- subset(emdat_output.df, select = relpathanglesrate)[1,]
  
  results <- find_sum_mean_rate(internal_data_vector, 
                                find_rel_angle_vector, segs_length, scene_length)
  
  verify_equivalence(results$sum, output_sum, participant, a_scene, "sumrelpathangles")
  verify_equivalence(results$mean, output_mean, participant, a_scene, "meanrelpathangles")
  verify_equivalence(results$rate, output_rate, participant, a_scene, "relpathanglesrate")
  
### stddevrelpathangles ###
  output_value <- subset(emdat_output.df, select = stddevrelpathangles)[1,]
  internal_value <- find_fixation_sd(results$data_storage, results$temp_mean, segs_length)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevrelpathangles") 
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
#  sumsaccadedistance

check_correctness_sac <- function(emdat_output.df, participant, a_scene, segment.names){

### set up the tests ###
  
  # read in the corresponding internal EMDAT data file
  internal_data.df <- read.csv(paste("EMDATdata_sac_P", participant, ".tsv", sep=""), sep="\t")
  
  # keeps all segments belonging to the scene in data frame format
  # only one data set (P18) contains a scene consisting of multiple segments     
  internal_data.df <- subset(internal_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
  
  segs_length <- length(segment.names)
  
  # stores the data by segment into a vector
  internal_data_vector <- c()
  for(i in 1:segs_length) {
    
    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
  }
 
### longestsaccadedistance ###
  output_value <- subset(emdat_output.df, select=longestsaccadedistance)[1,]
  internal_value <- signif(max(subset(internal_data.df, select=saccadedistance)$saccadedistance), digits = 12)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "longestsaccadedistance")
  
### longestsaccadeduration ###
  output_value <- subset(emdat_output.df, select=longestsaccadeduration)[1,]
  internal_value <- max(subset(internal_data.df, select=saccadeduration)$saccadeduration)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "longestsaccadeduration")
  
### maxsaccadespeed ###  
  output_value <- subset(emdat_output.df, select=maxsaccadespeed)[1,]
  internal_value <- max(subset(internal_data.df, select=saccadespeed)$saccadespeed)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "maxsaccadespeed")

### numsaccades ###
  output_value <- subset(emdat_output.df, select=numsaccades)[1,]
  internal_value <- nrow(internal_data.df)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "numsaccades")

### meansaccadedistance ###
  output_value <- subset(emdat_output.df, select=meansaccadedistance)[1,]
  
  results <- find_saccade_mean(internal_data_vector, "saccadedistance", segs_length)
  
  verify_equivalence(results$mean, output_value, participant, a_scene, "meansaccadedistance")
    
### stddevsaccadedistance ###
  output_value <- subset(emdat_output.df, select=stddevsaccadedistance)[1,]
  
  internal_value <- find_saccade_sd(
    internal_data_vector, "saccadedistance", segs_length, results$temp_mean)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevsaccadedistance")

### meansaccadeduration ###
  output_value <- subset(emdat_output.df, select=meansaccadeduration)[1,]
  
  results <- find_saccade_mean(internal_data_vector, "saccadeduration", segs_length)
  
  verify_equivalence(results$mean, output_value, participant, a_scene, "meansaccadeduration")

### stddevsaccadeduration ###
  output_value <- subset(emdat_output.df, select=stddevsaccadeduration)[1,]
  
  internal_value <- find_saccade_sd(
    internal_data_vector, "saccadeduration", segs_length, results$temp_mean)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevsaccadeduration")

### meansaccadespeed ###
  output_value <- subset(emdat_output.df, select=meansaccadespeed)[1,]
  
  results <- find_saccade_mean(internal_data_vector, "saccadespeed", segs_length)
  
  verify_equivalence(results$mean, output_value, participant, a_scene, "meansaccadespeed")
  
### stddevsaccadespeed ###
  output_value <- subset(emdat_output.df, select=stddevsaccadespeed)[1,]
  
  internal_value <- find_saccade_sd(
    internal_data_vector, "saccadespeed", segs_length, results$temp_mean)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevsaccadespeed")
  
### sumsaccadedistance ###
  output_value <- subset(emdat_output.df, select=sumsaccadedistance)[1,]
  internal_value <- signif(sum(subset(internal_data.df, select=saccadedistance)$saccadedistance),
                           digits = 12)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "sumsaccadedistance")
  
### sumsaccadeduration ###
  output_value <- subset(emdat_output.df, select=sumsaccadeduration)[1,]
  internal_value <- sum(subset(internal_data.df, select=saccadeduration)$saccadeduration)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "sumsaccadeduration")
  
### minsaccadespeed ###  
  output_value <- subset(emdat_output.df, select=minsaccadespeed)[1,]
  internal_value <- min(subset(internal_data.df, select=saccadespeed)$saccadespeed)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "minsaccadespeed")
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

check_correctness_eve <- function(emdat_output.df, participant, a_scene, segment.names){
  
### set up the tests ###
  
  # read in the needed internal EMDAT data files
  internal_data.df <- read.csv(paste("EMDATdata_eve_P", participant, ".tsv", sep=""), sep="\t")
  gazesample_data.df <- read.csv(paste("EMDATdata_gazesample_P", participant, ".tsv", sep=""), sep="\t")
  
  # keeps all segments belonging to the scene in data frame format
  # only one data set (P18) contains a scene consisting of multiple segments     
  internal_data.df <- subset(internal_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
  gazesample_data.df <- subset(gazesample_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
  
  segs_length <- length(segment.names)
    
  # stores the data by segment into vectors
  internal_data_vector <- c()
  gazesample_data_vector <- c()
  for(i in 1:segs_length) {
    
    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
    gazesample_data_vector[[i]] <- subset(gazesample_data.df, grepl(segment.names[i], scene))
  }
  
  scene_length <- compute_scene_length(segment.names, gazesample_data_vector)

### numdoubleclic ###
  output_value <- subset(emdat_output.df, select=numdoubleclic)[1,]
  double_clicks <- 0
  left_clicks <- 0

  for(i in 1:segs_length){

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
  internal_value <- signif(double_clicks/scene_length, digits=12)

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
  internal_value <- signif(left_clicks/scene_length, digits=12)
  
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
  internal_value <- signif(keypress/scene_length, digits=12) 
  
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
  internal_value <- signif(rightclicks/scene_length, digits=12) 
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "rightclicrate")
  
### numevents ###
  output_value <- subset(emdat_output.df, select=numevents)[1,]
  internal_value <- double_clicks + left_clicks + keypress + rightclicks
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "numevents")
}


# This function checks the correctness of pupil and head distance
# LIST OF COLUMS TO TEST:
#  numsamples
#  enddistance 
#  endpupilsize 
#  maxdistance 
#  maxpupilsize 
#  meanpupilsize 
#  mindistance 
#  minpupilsize 
#  minpupilvelocity (See note on rounding in the code below) 
#  startdistance 
#  startpupilsize 
#  stddevpupilsize 
#  length 
#  meandistance 
#  stddevdistance 
#  maxpupilvelocity (See note on rounding in the code below)
#  meanpupilvelocity (See note on rounding in the code below)
#  stddevpupilvelocity (See note on rounding in the code below)

check_correctness_gazesample <- function(emdat_output.df, participant, a_scene, segment.names){

### set up the tests ###    
  
  # read in the needed internal EMDAT data file
  internal_data.df <- read.csv(paste("EMDATdata_gazesample_P", participant, ".tsv", sep=""), sep="\t")
  
  # keeps all segments belonging to the scene in data frame format  
  # only one data set (P18) contains a scene consisting of multiple segments     
  internal_data.df <- subset(internal_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
  
  segs_length <- length(segment.names)
  
  # stores the data by segment into a vector
  internal_data_vector <- c()
  for(i in 1:segs_length) {
    
    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
  }
  
### numsamples ###
  output_value <- subset(emdat_output.df, select=numsamples)[1,]
  internal_value <- nrow(internal_data.df)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "numsamples")
  
### enddistance ###
  output_value <- subset(emdat_output.df, select=enddistance)[1,]
  internal_value <- tail(
    subset(internal_data.df, select=headdistance, is_valid_headdistance==TRUE)$headdistance, 1)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "enddistance")

### endpupilsize ###
  output_value <- subset(emdat_output.df, select=endpupilsize)[1,]
  internal_value <- tail(
    subset(internal_data.df, select=rawpupilsize, is_valid_pupil==TRUE)$rawpupilsize, 1)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "endpupilsize")

### maxdistance ###
  output_value <- subset(emdat_output.df, select=maxdistance)[1,]
  
  # Since the max happens to be non-negative in this case, the is_valid_headdistance==TRUE condition
  # does not make difference. But the internal value does not match the output_value in the 
  # corresponding  mindistancetest without that condition. So to be consistent, the condition 
  # is added here as well.
  internal_value <- max(subset(internal_data.df, select=headdistance, is_valid_headdistance==TRUE)$headdistance)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "maxdistance")

### maxpupilsize ###
  output_value <- subset(emdat_output.df, select=maxpupilsize)[1,]
  
  # Remark similar to the above on the condition of R subset operation holds here as well.   
  internal_value <- max(subset(internal_data.df, select=rawpupilsize, is_valid_pupil==TRUE)$rawpupilsize)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "maxpupilsize")
  
### maxpupilvelocity ###
  
  # NOTE: rounding to eight significant figures here is necessary because the internal data 
  # representaiton of valid pupil velosity in EMDAT contains measures (in float structure) that 
  # have greater precision than we are able to print out into the intermediate files
  # (i.e., data_gazesample); we cannot calculate the value in our test to the same precision as 
  # EMDAT does.
  
  output_value <- signif(subset(emdat_output.df, select=maxpupilvelocity)[1,], digits = 8)
  
  # The condition in the subet operaiton does not make practical difference here. But, the convention
  # is followed in computing the mean and min, so that it is added here for consistency.      
  internal_value <- max(subset(internal_data.df, select=pupilvelocity, pupilvelocity != -1)$pupilvelocity)
  internal_value <- signif(internal_value, digits = 8)
  verify_equivalence(internal_value, output_value, participant, a_scene, "maxpupilvelocity")
  
### meandistance ###
  output_value <- subset(emdat_output.df, select=meandistance)[1,]
  
  results <- find_gaze_mean(internal_data_vector, "headdistance", "is_valid_headdistance", "eql", 
                            TRUE, segs_length, 12)
  
  verify_equivalence(results$mean, output_value, participant, a_scene, "meandistance")

### stddevdistance ###
  output_value <- subset(emdat_output.df, select=stddevdistance)[1,]
  
  internal_value <- find_gaze_sd(internal_data_vector, "headdistance", "is_valid_headdistance", "eql",
                                 TRUE, segs_length, results$temp_mean, 12)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevdistance")

### meanpupilsize ###
  output_value <- subset(emdat_output.df, select=meanpupilsize)[1,]
  
  results <- find_gaze_mean(internal_data_vector, "rawpupilsize", "is_valid_pupil", "eql", 
                            TRUE, segs_length, 12)
  
  verify_equivalence(results$mean, output_value, participant, a_scene, "meanpupilsize")

### stddevpupilsize ###
  output_value <- subset(emdat_output.df, select=stddevpupilsize)[1,]
  
  internal_value <- find_gaze_sd(internal_data_vector, "rawpupilsize", "is_valid_pupil", "eql",
                                 TRUE, segs_length, results$temp_mean, 12)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevpupilsize")
  
### meanpupilvelocity ###
  
  # NOTE: rounding to six significant figures here is necessary because the internal data 
  # representaiton of valid pupil velosity in EMDAT contains measures (in float structure) that 
  # have greater precision than we are able to print out into the intermediate files
  # (i.e., data_gazesample); we cannot calculate the value in our test to the same precision as 
  # EMDAT does.
  
  output_value <- signif(subset(emdat_output.df, select=meanpupilvelocity)[1,], digits = 6)
  
  results <- find_gaze_mean(internal_data_vector, "pupilvelocity", "pupilvelocity","not_eql", -1, 
                            segs_length, 6)
  
  verify_equivalence(results$mean, output_value, participant, a_scene, "meanpupilvelocity")

### stddevpupilvelocity ###
  
  # NOTE: rounding to six significant figures here is necessary because the internal data 
  # representaiton of valid pupil velosity in EMDAT contains measures (in float structure) that 
  # have greater precision than we are able to print out into the intermediate files
  # (i.e., data_gazesample); we cannot calculate the value in our test to the same precision as 
  # EMDAT does.
  
  output_value <- signif(subset(emdat_output.df, select=stddevpupilvelocity)[1,], digits = 6)
  
  internal_value <- find_gaze_sd(internal_data_vector, "pupilvelocity", "pupilvelocity", "not_eql",
                                 -1, segs_length, results$temp_mean, 6)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "stddevpupilvelocity")
     
### mindistance ###
  output_value <- subset(emdat_output.df, select=mindistance)[1,]
  internal_value <- min(subset(internal_data.df, select=headdistance, is_valid_headdistance==TRUE)$headdistance)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "mindistance")
  
### minpupilsize ###
  output_value <- subset(emdat_output.df, select=minpupilsize)[1,]
  internal_value <- min(subset(internal_data.df, select=rawpupilsize, is_valid_pupil==TRUE)$rawpupilsize)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "minpupilsize")
  
### minpupilvelocity ###
  
  # NOTE: rounding to eight significant figures here is necessary because the internal data 
  # representaiton of valid pupil velosity in EMDAT contains measures (in float structure) that 
  # have greater precision than we are able to print out into the intermediate files
  # (i.e., data_gazesample); we cannot calculate the value in our test to the same precision as 
  # EMDAT does.
  
  output_value <- signif(subset(emdat_output.df, select=minpupilvelocity)[1,], digits = 8)
  internal_value <- min(subset(internal_data.df, select=pupilvelocity, pupilvelocity != -1)$pupilvelocity)
  internal_value <- signif(internal_value, digits = 8)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "minpupilvelocity")
  
### startdistance ###
  output_value <- subset(emdat_output.df, select=startdistance)[1,]
  internal_value <- head(
    subset(internal_data.df, select=headdistance, is_valid_headdistance==TRUE)$headdistance, 1)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "startdistance")
  
### startpupilsize ###
  output_value <- subset(emdat_output.df, select=startpupilsize)[1,]
  internal_value <- head(
    subset(internal_data.df, select=rawpupilsize, is_valid_pupil==TRUE)$rawpupilsize, 1)

  verify_equivalence(internal_value, output_value, participant, a_scene, "startpupilsize")

### length ###
  output_value <- subset(emdat_output.df, select=length)[1,]
  internal_value <- compute_scene_length(segment.names, internal_data_vector)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "length")
}


P16 <- readfiles_part2("16", "TobiiV3_sample_16.seg")
P17 <- readfiles_part2("17", "TobiiV3_sample_17.seg")
P18 <- readfiles_part2("18", "TobiiV3_sample_18.seg")


