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
  
  #loop over the scenes
  for (a_scene in scene.names) {
    
    #extract segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
    
    checked_result1 <- check_correctness_fix(emdat_export.df.scene, participant, a_scene, segment.names)
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

#  numsegments  


#  fixationrate ONGOING (due to an unfixed error, commented out for now)

#  TODO:
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

check_correctness_fix <- function(emdat_output.df, participant, a_scene, segment.names){
  
  #read in the corresponding internal EMDAT data file
  internal_data.df <- read.csv(paste("EMDATdata_fix_P", participant, ".tsv", sep=""), sep="\t")
 
  #subset the file according to the scene (if scene == XX_allsc, then keep it all)
  if (!grepl("all",a_scene)){
    internal_data.df <- subset(internal_data.df, grepl(a_scene, scene))
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
  #  error: internal_value = 0.00377927507, output_value = 0.00370856
  #  pass for now
  segment_start_time <- subset(head(internal_data.df,1), select=timestamp)[1,]
  segment_end_time <- subset(tail(internal_data.df,1), select=timestamp)[1,]
  internal_value <- signif((nrow(internal_data.df) / (segment_end_time - segment_start_time)), digits=9)
  output_value <- subset(emdat_output.df, select=fixationrate)[1,]
  
  # browser()
  # try(if( internal_value  != output_value)
  #   stop(paste("Error: fixationrate does not match for participant:", participant, " and scene: ", a_scene)))
  
  #  numsegments
  output_value <- subset(emdat_output.df, select=numsegments)[1,]
  internal_value <- length(segment.names)
  try(if( internal_value  != output_value)
    stop(paste("Error: numsegments do not match for participant:", participant, " and scene: ", a_scene)))

  #  length
  #  Error: internal_value = 26336 (rounded to a whole number), output_value = 53390 
  output_value <- subset(emdat_output.df, select=length)[1,]
  
  # for the corresponding internal value, sum the saccade distance between two sucessive coordinates
  # (the two vectors has the same length)   
  find_length <- function(x_cord_vector, y_cord_vector) {
    length <- 0
    for(i in 1:(length(x_cord_vector)-1)){
      length <- length + 
        sqrt((x_cord_vector[i+1] - x_cord_vector[i])^2 + (y_cord_vector[i+1] - y_cord_vector[i])^2)
    }
    return(length)
  }
  
  internal_value <- find_length(internal_data.df$mappedfixationpointx,
                                 internal_data.df$mappedfixationpointy)
  
  # try(if( internal_value  != output_value)
  #   stop(paste("Error: length does not match for participant:", participant, " and scene: ", a_scene)))
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

# 
# P16 <- readfiles_part2("16", c("part1","part2"),1,1)
# P17 <- readfiles_part2("17", c("part1","part2"),1,1)
# P18 <- readfiles_part2("18", c("main_task"), 2)

#refactor to make these work like this:    Can steal code from part1 testing (needs to dogenerate a list of scene names, and calculate #segements per scene)
P16 <- readfiles_part2("16", "TobiiV3_sample_16.seg")
P17 <- readfiles_part2("17", "TobiiV3_sample_17.seg")
P18 <- readfiles_part2("18", "TobiiV3_sample_18.seg")

