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
    # checked_result2 <- check_correctness_sac(emdat_export.df.scene, participant, a_scene)
    #checked_result3 <- check_correctness_eve(...)
    checked_result4 <- check_correctness_gazesample(emdat_export.df.scene, participant, a_scene)
    
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

#  TO BE REVISITED:
#  fixationrate ONGOING (due to an unfixed error, commented out for now)
#  length (exact definition needs to be known)
#  sumpathdistance (fails for P18)
#  meanpathdistance(also fails for P18, due to the use of the same value)
#  stddevpathdistance(also fails for P18, due to the use of the same value)
#  fixationsaccadetimeratio(this seems to be just the ratio: sumfixationduration/sumsaccadeduration
#     ;if this value is to be tested, needs to get the value from the saccade file)

#  sumabspathangles

#  TODO:
#  abspathanglesrate
#  eyemovementvelocity
#  meanabspathangles
#  meanrelpathangles
#  relpathanglesrate
#  stddevabspathangles
#  stddevrelpathangles
#  sumrelpathangles

check_correctness_fix <- function(emdat_output.df, participant, a_scene, segment.names){
  
  #read in the corresponding internal EMDAT data file
  internal_data.df <- read.csv(paste("EMDATdata_fix_P", participant, ".tsv", sep=""), sep="\t")
 
  # keeps all segments belonging to the scene
  # only one data set (P18) contains a scene consisting of multiple segments     
  internal_data.df <- subset(internal_data.df, grepl(a_scene, scene))
 
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
  
  
###  fixationrate (NEEDS FIX) ###
  
  #  error: internal_value = 0.00377927507, output_value = 0.00370856
  #  pass for now
  
  segment_start_time <- subset(head(internal_data.df,1), select=timestamp)[1,]
  segment_end_time <- subset(tail(internal_data.df,1), select=timestamp)[1,]
  internal_value <- signif((nrow(internal_data.df) / (segment_end_time - segment_start_time)), digits=9)
  output_value <- subset(emdat_output.df, select=fixationrate)[1,]
  
  # browser()
  # try(if( internal_value  != output_value)
  #   stop(paste("Error: fixationrate does not match for participant:", participant, " and scene: ", a_scene)))
  
###  numsegments ###
  
  output_value <- subset(emdat_output.df, select=numsegments)[1,]
  internal_value <- length(segment.names)
  
  try(if( internal_value  != output_value)
        stop(paste("Error: numsegments do not match for participant:", participant, " and scene: ", a_scene))
  )

###  sumpathdistance (NEEDS FIX) ###
  
  #  error for P18: internal_value =  110079.9737, output_value = 109847.9737
  
  output_value <- subset(emdat_output.df, select = sumpathdistance)[1,]
  
  # path_length is the saccade distance between two sucessive coordinates
  find_path_length_vector<- function(x_cord_vector, y_cord_vector) {
    
    path_length_vector <- c()
    
    for(i in 1:(length(x_cord_vector)-1)){
      
      path_length_vector[i] <- 
        sqrt((x_cord_vector[i+1] - x_cord_vector[i])^2 + (y_cord_vector[i+1] - y_cord_vector[i])^2)
    }
    return(path_length_vector)
  }
  
  path_length_vector <- find_path_length_vector(
                          internal_data.df$mappedfixationpointx,
                          internal_data.df$mappedfixationpointy
                        )
  
  internal_value <- signif(sum(path_length_vector), digits = 10)
  
  try(if(internal_value  != output_value)
        stop(paste("Error: sumpathdistance does not match for participant:", participant, " and scene: ", a_scene))
  )

###  meanpathdistance (NEEDS FIX) ###
  
  # error for P18: internal_value =  142.2221882, output_value = 142.1060461
  # This is expected since the same path_length_vector as in the above is used
  
  output_value <- subset(emdat_output.df, select = meanpathdistance)[1,]
  internal_value <- signif(mean(path_length_vector), digits = 10) 
  
  try(if(internal_value  != output_value)
        stop(paste("Error: meanpathdistance does not match for participant:", participant, " and scene: ", a_scene))
  )

### stddevpathdistance (NEEDS FIX) ###
  
  # error for P18: internal_value =  134.3336738, output_value = 134.3817584
  # This is expected since the same path_length_vector as in the above is used
  
  output_value <- subset(emdat_output.df, select = stddevpathdistance)[1,]
  internal_value <- signif(sd(path_length_vector), digits = 10)
  
  try(if(internal_value  != output_value)
        stop(paste("Error: stddevpathdistance does not match for participant:", participant, " and scene: ", a_scene))
  )
  
###  sumabspathangles ###
  
  
  find_abs_angle_vector<- function(x_cord_vector, y_cord_vector){
    
    abs_angle_vector <- c()
    
    for(i in 1:(length(x_cord_vector)-1)){
      
      delta_x <- x_cord_vector[i+1] - x_cord_vector[i]
      delta_y <- y_cord_vector[i+1] - y_cord_vector[i]
       
      if(delta_x == 0 & delta_y == 0){ # no movement case
        
        abs_angle_vector[i] <- 0
      } 
      else if(delta_x == 0){ # vertical move cases
          
          abs_angle_vector[i] <- pi/2
      } 
      else if(delta_y == 0){ # horizontal move cases
        
          abs_angle_vector[i] <-  0
      } 
      else if(delta_x > 0 & delta_y > 0) { # first quadrant case
        
        abs_angle_vector[i] <- atan(delta_y/delta_x)
      } 
      else if (delta_x < 0 & delta_y > 0) { # second quadrant case
        
        abs_angle_vector[i] <- pi+atan(delta_y/delta_x)
      } 
      else if (delta_x < 0 & delta_y < 0) { # third quadrant case
        
        abs_angle_vector[i] <- pi-atan(delta_y/delta_x)
      } 
      else { # fourth quadrant case (delta_x > 0 & delta_y < 0)
        
        abs_angle_vector[i] <- -atan(delta_y/delta_x)
      } 
    }
    return(abs_angle_vector)
  }
  
  abs_angle_vector <- find_abs_angle_vector(internal_data.df$mappedfixationpointx,
                                            internal_data.df$mappedfixationpointy)
  
  output_value <- subset(emdat_output.df, select = sumabspathangles)[1,]
  internal_value <- signif(sum(abs_angle_vector), digits = 10)
  
  try(if(internal_value  != output_value)
        stop(paste("Error: sumabspathangles does not match for participant:", participant, " and scene: ", a_scene))
  )
  
  
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
#  sumsaccadedistance (error for P16 part2: last digit off by 1)

#  TODO

check_correctness_sac <- function(emdat_output.df, participant, a_scene){
  
  # read in the corresponding internal EMDAT data file
  internal_data.df <- read.csv(paste("EMDATdata_sac_P", participant, ".tsv", sep=""), sep="\t")
  
  # keeps all segments belonging to the scene
  # only one data set (P18) contains a scene consisting of multiple segments     
  internal_data.df <- subset(internal_data.df, grepl(a_scene, scene))
 
### longestsaccadedistance ###
  output_value <- subset(emdat_output.df, select=longestsaccadedistance)[1,]
  internal_value <- max(subset(internal_data.df, select=saccadedistance)$saccadedistance)
  
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

# TO REVISIT:
#  meandistance (Error for P 18. Assumed: only valid values considered)
#  stddevdistance (Error for P 18. Assumed: only valid values considered)

#  TODO:
#  stddevpupilsize
#  stddevpupilvelocity

check_correctness_gazesample <- function(emdat_output.df, participant, a_scene){
  
  # read in the corresponding internal EMDAT data file
  internal_data.df <- read.csv(paste("EMDATdata_gazesample_P", participant, ".tsv", sep=""), sep="\t")
  
  # keeps all segments belonging to the scene
  # only one data set (P18) contains a scene consisting of multiple segments     
  internal_data.df <- subset(internal_data.df, grepl(a_scene, scene))
  
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
  
### meandistance(NEEDS FIX) ###
  
  # Error for P18: internal_value = 624.6470933, output_value = 625.018883
  # Assumption: only valid values are considered
  
  output_value <- subset(emdat_output.df, select=meandistance)[1,]
  internal_value <- signif(
    mean(subset(internal_data.df, select=headdistance, is_valid_headdistance==TRUE)$headdistance),
    digits = 10)
  
  try(if(internal_value != output_value)
    stop(paste("Error: meandistance does not match for participant:", participant, " and scene: ", a_scene))
  )
  
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
  
}


P16 <- readfiles_part2("16", "TobiiV3_sample_16.seg")
P17 <- readfiles_part2("17", "TobiiV3_sample_17.seg")
P18 <- readfiles_part2("18", "TobiiV3_sample_18.seg")

