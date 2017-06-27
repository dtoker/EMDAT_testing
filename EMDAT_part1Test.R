# Dereck Toker
# December 7, 2015
# UBC - Department of Computer Science

# Last UPDATE: January 22, 2016

# This script is part 1 of 2, in order to test the correctness of EMDAT files
# Here, we read in RAW Tobii V3 data, and verify it's being correctly represented internally within EMDAT for a given seg file

#Reading in:
#   Tobii Export Files: PXX_Data_Export.tsv
#         Segment File: TobiiV3_sample_XX.seg

#Testing:
# EMDAT Internal Files: EMDATdata_VARIABLES_PXX.txt

# EMDAT PARAMS UTILIZED
VALID_SAMPLES_PROP_SACCADE = 1


setwd("C:/Users/admin/Dropbox/PhD/EMDAT-Work/Part1_TobiiV3Output_EMDATInternal")

# This function reads in Tobii Export Data and a Seg file for one given participant, and then checks correctness

readfiles_part1 <- function(tobii_export, seg_file, participant){
  
  tobii_export.df <- read.csv(tobii_export, sep="\t")
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  
  #extract scene names
  scene.names <- unique(seg_file.df[,"scene"])
  
  #loop over the scenes
  for (scene_name in scene.names) {
    
    #extract segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==scene_name)[,"segment"])
    
    
    #loop over the segments
    for (segment_name in segment.names) {
      
      #acquire the start time and end time for this particular segment in the scene
      start_time <- subset(seg_file.df, scene==scene_name & segment==segment_name)[,"start"]
      end_time <- subset(seg_file.df, scene==scene_name & segment==segment_name)[,"end"]
      
      #pass in the start & end time, and the tobii and emdat files to check for correctness
      checked_result1 <- check_correctness_fix(start_time, end_time, tobii_export.df, segment_name, participant)
      checked_result2 <- check_correctness_sac(start_time, end_time, tobii_export.df, segment_name, participant)
      checked_result3 <- check_correctness_eve(start_time, end_time, tobii_export.df, segment_name, participant)
      checked_result4 <- check_correctness_gazesample(start_time, end_time, tobii_export.df, segment_name, participant)
      
      #browser()
      
    }
    
  }
  
}



# This function checks the correctness of fixations
# LIST OF COLUMS TO TEST:
#  fixationindex
#  timestamp
#  fixationduration
#  mappedfixationpointx
#  mappedfixationpointy

check_correctness_fix <- function(start_time, end_time, tobii_export.df, segment_name, participant){
  
  #read in the internal EMDAT data file
  EMDAT_data.df <- read.csv(paste("EMDATdata_fix_", participant, ".tsv", sep=""), sep="\t")
  
  #subset the correct data based on the segment 
  EMDAT_data.df <- subset(EMDAT_data.df, scene == paste(participant,segment_name, sep=""))
  
  #subset the Tobii data based on start and end time stamps
  tobii_export.sub <- subset(tobii_export.df, RecordingTimestamp >= start_time & RecordingTimestamp <= end_time)
  
  #TEST: fixationindex
  #subset to just grab the fixations
  tobii_export.sub <- subset(tobii_export.sub, GazeEventType == "Fixation")
  #first column to construct, fixation_index
  fixations.df <- data.frame(fixationindex = unique(tobii_export.sub[,"FixationIndex"]))
  
  #check and see if the first and last fixation are partial (i.e., the begin before the start_time or end after the end_time)
  # if so, remove. 
  first_fix_id <- min(fixations.df$fixationindex)
  tobii_export.first_fix <- subset(tobii_export.sub, FixationIndex == first_fix_id) 
  if (head(tobii_export.first_fix)[1,"GazeEventDuration"] > nrow(tobii_export.first_fix)*8.33) { #120hz
    fixations.df <- subset(fixations.df, fixationindex != first_fix_id)
  }
  last_fix_id <- max(fixations.df$fixationindex)
  tobii_export.last_fix <- subset(tobii_export.sub, FixationIndex == last_fix_id)
  if (head(tobii_export.last_fix)[1,"GazeEventDuration"] > nrow(tobii_export.last_fix)*8.33) {#120hz
    fixations.df <- subset(fixations.df, fixationindex != last_fix_id)
  }
  #also need to remove fixations that occur off the screen for either the x or y coordinate
  fixes <- unique(fixations.df$fixationindex)
  
  for (fix in fixes){
    x_fix <- subset(tobii_export.sub, FixationIndex == fix)[1,"FixationPointX..MCSpx."]
    y_fix <- subset(tobii_export.sub, FixationIndex == fix)[1,"FixationPointY..MCSpx."]
    if (is.na(y_fix) | is.na(x_fix)) fixations.df <- subset(fixations.df, fixationindex != fix)
  }
  
  try(if(nrow(EMDAT_data.df) != nrow(fixations.df)) 
    stop(paste("Error: quantity of fixation indexes do not match. EMDAT:", nrow(EMDAT_data.df), "TObii:",nrow(fixations.df))))
  
  #TEST: timestamp, fixationduration, mappedfixationpointx, mappedfixationpointy
  testFunc <- function(fix_index){ 
    
    EMDAT_data.row  <-  subset(EMDAT_data.df, fixationindex == fix_index)
    tobii_export.row <- subset(tobii_export.sub, FixationIndex == fix_index & !is.na(ValidityLeft))[1,]  
    
    try(if(EMDAT_data.row$timestamp != tobii_export.row$RecordingTimestamp)    
      stop(paste("Error: Participant", participant, "segment", segment_name, "fixation timestamp does not match for fixation_index", fix_index,"EMDAT:", EMDAT_data.row$timestamp, "TObii:",tobii_export.row$RecordingTimestamp)))  
    try(if(EMDAT_data.row$fixationduration != tobii_export.row$GazeEventDuration)    
      stop(paste("Error: Participant", participant, "segment", segment_name, "fixationduration does not match for fixation_index", fix_index,"EMDAT:", EMDAT_data.row$fixationduration, "TObii:",tobii_export.row$GazeEventDuration)))
    try(if(EMDAT_data.row$mappedfixationpointx != tobii_export.row$FixationPointX..MCSpx.)    
      stop(paste("Error: Participant", participant, "segment", segment_name, "mappedfixationpointx does not match for fixation_index", fix_index,"EMDAT:", EMDAT_data.row$mappedfixationpointx, "TObii:",tobii_export.row$FixationPointX..MCSpx.)))
    try(if(EMDAT_data.row$mappedfixationpointy != tobii_export.row$FixationPointY..MCSpx.)    
      stop(paste("Error: Participant", participant, "segment", segment_name, "appedfixationpointy does not match for fixation_index", fix_index,"EMDAT:", EMDAT_data.row$mappedfixationpointy, "TObii:",tobii_export.row$FixationPointY..MCSpx.)))     
    
  }
  
  apply(fixations.df, 1, testFunc)
}


# This function checks the correctness of saccades
# LIST OF COLUMS TO TEST:
#  saccadeindex
#  timestamp  
#  saccadeduration	
#  saccadedistance	
#  saccadespeed	
#  saccadeacceleration	
#  saccadestartpointx	
#  saccadestartpointy	
#  saccadeendpointx	
#  saccadeendpointy	
#  saccadequality
check_correctness_sac <- function(start_time, end_time, tobii_export.df, segment_name, participant){
  
  #read in the internal EMDAT data file
  EMDAT_data.df <- read.csv(paste("EMDATdata_sac_", participant, ".tsv", sep=""), sep="\t")
  
  #subset the correct data based on the segment 
  EMDAT_data.df <- subset(EMDAT_data.df, scene == paste(participant,segment_name, sep=""))
  
  #subset the Tobii data based on start and end time stamps
  tobii_export.sub <- subset(tobii_export.df, RecordingTimestamp >= start_time & RecordingTimestamp <= end_time)
  
  #TEST: fixationindex
  #subset to just grab the fixations
  tobii_export.sub <- subset(tobii_export.sub, GazeEventType == "Saccade")
  #first column to construct, fixation_index
  saccades.df <- data.frame(saccadeindex = unique(tobii_export.sub[,"SaccadeIndex"]))
  
  saccs <- unique(saccades.df$saccadeindex)  
  for (sacc in saccs){
    rows <- which(tobii_export.df$SaccadeIndex == sacc) #get the row ids for the saccades
    prev_row <- min(rows)-1 #get the row prior to the first
    next_row <- max(rows)+1 #get the row after the last
    prev_type <- tobii_export.df[prev_row,]$GazeEventType
    next_type <- tobii_export.df[next_row,]$GazeEventType
    if (prev_type == "Unclassified" | next_type == "Unclassified"){
      saccades.df <- subset(saccades.df, saccadeindex != sacc)
    } 
    
    #and remove in betweens
    check_unclassifieds <- subset(tobii_export.sub, SaccadeIndex == sacc)
    if (VALID_SAMPLES_PROP_SACCADE) check_unclassifieds <- subset(check_unclassifieds, (ValidityLeft != 0 & ValidityRight != 0) | (is.na(ValidityLeft) & is.na(ValidityRight)))  
    if (nrow(check_unclassifieds) != 0 & !all(is.na(check_unclassifieds$EyeTrackerTimestamp))) saccades.df <- subset(saccades.df, saccadeindex != sacc)    
    
  }
  
  try(if(nrow(EMDAT_data.df) != nrow(saccades.df)) 
    stop(paste("Error: quantity of saccades indexes do not match. EMDAT:", nrow(EMDAT_data.df), "TObii:",nrow(saccades.df))))
  
  
  #TEST: timestamp  saccadeduration  saccadedistance	saccadespeed	saccadeacceleration
  #   saccadestartpointx	saccadestartpointy	saccadeendpointx	saccadeendpointy	saccadequality
  testFunc <- function(sac_index){ 
    
    EMDAT_data.row  <-  subset(EMDAT_data.df, saccadeindex == sac_index) #the row to check
    
    tobii_export.row <- subset(tobii_export.sub, SaccadeIndex == sac_index) # a set of rows to compute the values
    
    row_index <- which(tobii_export.df$SaccadeIndex == sac_index)
    prev_row <- min(row_index)-1 #get the row prior to the first
    next_row <- max(row_index)+1 #get the row after to the last
    while (is.na(tobii_export.df[next_row,]$EyeTrackerTimestamp)) next_row <- next_row+1
    
    #timestamp
    try(if(EMDAT_data.row$timestamp != tobii_export.df[prev_row,]$RecordingTimestamp)
      stop(paste("Error: Participant", participant, "saccade", segment_name, "saccade timestamp does not match for sac_index", sac_index,"EMDAT:", EMDAT_data.row$timestamp, "TObii:",tobii_export.df[prev_row,]$RecordingTimestamp)))
    
    
    #saccadeduration
    duration <- tobii_export.df[next_row,]$RecordingTimestamp - tobii_export.df[prev_row,]$RecordingTimestamp
    try(if(EMDAT_data.row$saccadeduration != duration)    
      stop(paste("Error: Participant", participant, "saccade", segment_name, "saccadeduration does not match for sac_index", sac_index,"EMDAT:", EMDAT_data.row$saccadeduration, "TObii:",duration)))
    
    #  saccadedistance
    xy_fixations.df <- subset(tobii_export.df[prev_row:next_row,], subset = !is.na(EyeTrackerTimestamp), select=c(FixationPointX..MCSpx., FixationPointY..MCSpx., GazePointX..ADCSpx., GazePointY..ADCSpx.))
    
    total_distance <- as.double(0)
    size <- nrow(xy_fixations.df)
    
    for (i in 1:size) {
      x <- xy_fixations.df[i,]$GazePointX..ADCSpx
      if (is.na(x)){
        a <- xy_fixations.df[i,]$FixationPointX..MCSpx.
        b <- xy_fixations.df[i,]$FixationPointY..MCSpx. 
        xy_fixations.df[i,"GazePointX..ADCSpx."] <- a
        xy_fixations.df[i,"GazePointY..ADCSpx."] <- b
      }
      
    }
    
    first_x <- xy_fixations.df[1,]$GazePointX..ADCSpx.
    first_y <- xy_fixations.df[1,]$GazePointY..ADCSpx.
    
    
    for (i in 2:size) {
      d_x <- (xy_fixations.df[i,]$GazePointX..ADCSpx. - first_x)^2
      d_y <- (xy_fixations.df[i,]$GazePointY..ADCSpx. - first_y)^2
      first_x <- xy_fixations.df[i,]$GazePointX..ADCSpx.
      first_y <- xy_fixations.df[i,]$GazePointY..ADCSpx.
      total_distance <- total_distance + sqrt(d_x + d_y)
      
    }
    
    total_distance <- signif(total_distance,6)
    emdat.distance <- signif(EMDAT_data.row$saccadedistance,6)    
    try(if(emdat.distance != total_distance)    
      stop(paste("Error: Participant", participant, "saccade", segment_name, "saccade distance does not match for sac_index", sac_index,"EMDAT:", emdat.distance, "TObii:",total_distance)))
    
    #  saccadespeed
    speed <- EMDAT_data.row$saccadedistance/EMDAT_data.row$saccadeduration
    speed <- signif(speed,6)
    emdatspeed <- signif(EMDAT_data.row$saccadespeed,6)
    try(if(emdatspeed != speed)    
      stop(paste("Error: Participant", participant, "saccade", segment_name, "saccadespeed does not match for sac_index", sac_index,"EMDAT:", emdatspeed , "TObii:",speed)))
    
    #  saccadeacceleration
    try(if(EMDAT_data.row$saccadeacceleration != -1)    
      stop(paste("Error: Participant", participant, "saccade", segment_name, "saccadeacceleration does not match for sac_index", sac_index,"EMDAT:", EMDAT_data.row$saccadeacceleration , "TObii:",speed)))
    
    #  saccadestartpointx	
    xpoint <- xy_fixations.df[1,"GazePointX..ADCSpx."]
    try(if(EMDAT_data.row$saccadestartpointx != xpoint)    
      stop(paste("Error: Participant", participant, "saccade", segment_name, "saccadxstartpoint does not match for sac_index", sac_index,"EMDAT:", EMDAT_data.row$saccadestartpointx , "TObii:",xpoint)))
    
    #  saccadestartpointy	
    ypoint <- xy_fixations.df[1,"GazePointY..ADCSpx."]
    try(if(EMDAT_data.row$saccadestartpointy != ypoint)    
      stop(paste("Error: Participant", participant, "saccade", segment_name, "saccadystartpoint does not match for sac_index", sac_index,"EMDAT:", EMDAT_data.row$saccadestartpointy , "TObii:",ypoint))) 
    
    #  saccadeendpointx	
    xpoint <- xy_fixations.df[size,"GazePointX..ADCSpx."]
    try(if(EMDAT_data.row$saccadeendpointx != xpoint)    
      stop(paste("Error: Participant", participant, "saccade", segment_name, "saccadxendpoint does not match for sac_index", sac_index,"EMDAT:", EMDAT_data.row$saccadestartpointx , "TObii:",xpoint)))
    
    #  saccadeendpointy	
    ypoint <- xy_fixations.df[size,"GazePointY..ADCSpx."]
    try(if(EMDAT_data.row$saccadeendpointy != ypoint)    
      stop(paste("Error: Participant", participant, "saccade", segment_name, "saccadyendpoint does not match for sac_index", sac_index,"EMDAT:", EMDAT_data.row$saccadestartpointy , "TObii:",ypoint)))
    
    
    #  saccadequality
    quality <- 1
    size <- nrow(tobii_export.row)
    
    
    for (i in 1:size) {
      a <- tobii_export.row[i,]$ValidityLeft <= 1
      b <- tobii_export.row[i,]$ValidityRight <= 1
      #if (sac_index == 821) browser()
      if (!(is.na(tobii_export.row[i,]$EyeTrackerTimestamp))){
        if (!(a | b)) quality <- -1
      }
    }
    try(if(EMDAT_data.row$saccadequality != quality)    
      stop(paste("Error: Participant", participant, "saccade", segment_name, "saccadequality does not match for sac_index", sac_index,"EMDAT:", EMDAT_data.row$saccadequality , "TObii:",quality)))
    
  }
  
  apply(saccades.df, 1, testFunc)
  
  
}


# This function checks the correctness of events
# LIST OF COLUMS TO TEST:
# timestamp
# event
# event_key
# x_coord
# y_coord
# key_code
# key_name
# description

check_correctness_eve <- function(start_time, end_time, tobii_export.df, segment_name, participant){
  
  #read in the internal EMDAT data file
  EMDAT_data.df <- read.csv(paste("EMDATdata_eve_", participant, ".tsv", sep=""), sep="\t")
  
  #subset the correct data based on the segment 
  EMDAT_data.df <- subset(EMDAT_data.df, scene == paste(participant,segment_name, sep=""))
  
  #subset the Tobii data based on start and end time stamps
  tobii_export.sub <- subset(tobii_export.df, RecordingTimestamp >= start_time & RecordingTimestamp <= end_time)
  
  tobii_export.sub <- subset(tobii_export.sub,  !is.na(MouseEventIndex) | !is.na(KeyPressEventIndex))
  
  try(if(nrow(EMDAT_data.df) != nrow(tobii_export.sub))
    stop(paste("Error: Participant", participant, "number of events does not match", sac_index,"EMDAT:", nrow(EMDAT_data.df) , "TObii:",nrow(tobii_export.sub))))
  
  
  
  testFunc <- function(emdat.row, tobii.row, i){
    
    # timestamp
    try(if(emdat.row$timestamp != tobii.row$RecordingTimestamp)
      stop(paste("Error: Participant", participant, "row", i, "event timestamp does not match for rowindex", i,"EMDAT:", emdat.row$timestamp, "TObii:",tobii.row$RecordingTimestamp)))
    
    # event
    event <- emdat.row$event
    type <- ""
    if (event == "LeftMouseClick"){
      type <- "mouse"
      event <- "Left"
      try(if(tobii.row$MouseEvent != "Left")
        stop(paste("Error: Participant", participant, "row", i, "event does not match for rowindex", i,"EMDAT:", event, "TObii:",tobii.row$MouseEvent)))
      
      # x_coord
      x_coord <- emdat.row$x_coord
      try(if(tobii.row$MouseEventX..ADCSpx. != x_coord)
        stop(paste("Error: Participant", participant, "row", i, "x-coord does not match for rowindex", i,"EMDAT:", x_coord, "TObii:",tobii.row$MouseEventX..ADCSpx.)))  
      
      # y_coord
      y_coord <- emdat.row$y_coord
      try(if(tobii.row$MouseEventY..ADCSpx. != y_coord)
        stop(paste("Error: Participant", participant, "row", i, "y-coord does not match for rowindex", i,"EMDAT:", y_coord, "TObii:",tobii.row$MouseEventY..ADCSpx.)))
      
      # key_name
      try(if(as.character(emdat.row$key_name) != "None")
        stop(paste("Error: Participant", participant, "row", i, "key name does not match for rowindex", i,"EMDAT:", emdat.row$key_name, "TObii:","n/a"))) 
      
    }
    
    if (event == "KeyPress"){
      type <- "keypress"
      try(if(is.na(tobii.row$KeyPressEventIndex))
        stop(paste("Error: Participant", participant, "row", i, "event does not match for rowindex", i,"EMDAT:", event, "TObii:",tobii.row$KeyPressEventIndex))) 
      
      
      # x_coord
      x_coord <- emdat.row$x_coord
      try(if("None" != x_coord)
        stop(paste("Error: Participant", participant, "row", i, "x-coord does not match for rowindex", i,"EMDAT:", x_coord, "TObii:",tobii.row$MouseEventX..ADCSpx.)))  
      
      # y_coord
      y_coord <- emdat.row$y_coord
      try(if("None" != y_coord)
        stop(paste("Error: Participant", participant, "row", i, "y-coord does not match for rowindex", i,"EMDAT:", y_coord, "TObii:",tobii.row$MouseEventY..ADCSpx.)))  
      
      # key_name
      key_name <- as.character(emdat.row$key_name)
      try(if(as.character(tobii.row$KeyPressEvent) != key_name)
        stop(paste("Error: Participant", participant, "row", i, "key name does not match for rowindex", i,"EMDAT:", key_name, "TObii:",tobii.row$KeyPressEvent)))  
      
    }
    
    # event_key
    try(if(emdat.row$event_key != "None")
      stop(paste("Error: Participant", participant, "row", i, "event_key does not match for rowindex", i,"EMDAT:", emdat.row$event_key, "TObii:","n/a"))) 
    
    # key_code
    try(if(emdat.row$key_code != "None")
      stop(paste("Error: Participant", participant, "row", i, "key_code does not match for rowindex", i,"EMDAT:", emdat.row$key_code, "TObii:","n/a"))) 
    
    
    
    
    
    # description    
    try(if(emdat.row$description != "None")
      stop(paste("Error: Participant", participant, "row", i, "description does not match for rowindex", i,"EMDAT:", emdat.row$description, "TObii:","n/a"))) 
    
    
    
  }
  
  size <- nrow(tobii_export.sub)
  for (i in 1:size){ 
    emdat.row <- EMDAT_data.df[i,]
    tobii.row <- tobii_export.sub[i,]
    testFunc(emdat.row, tobii.row, i)
  }
  
}

# This function checks the correctness of pupil and head distance
# LIST OF COLUMS TO TEST:
#timestamp  
#rawpupilsize  
#pupilvelocity	
#headdistance	
#is_valid_pupil
#is_valid_headdistance  
#stimuliname	

check_correctness_gazesample <- function(start_time, end_time, tobii_export.df, segment_name, participant){
  
  
  #read in the internal EMDAT data file
  EMDAT_data.df <- read.csv(paste("EMDATdata_gazesample_", participant, ".tsv", sep=""), sep="\t")
  
  #subset the correct data based on the segment 
  EMDAT_data.df <- subset(EMDAT_data.df, scene == paste(participant,segment_name, sep=""))
  
  #subset the Tobii data based on start and end time stamps
  tobii_export.sub <- subset(tobii_export.df, RecordingTimestamp >= start_time & RecordingTimestamp <= end_time)
  
  tobii_export.sub <- subset(tobii_export.sub, !is.na( EyeTrackerTimestamp))
  
  try(if(nrow(EMDAT_data.df) != nrow(tobii_export.sub))
    stop(paste("Error: Participant", participant, "number of pupil/head rows do not match", "EMDAT:", nrow(EMDAT_data.df) , "TObii:",nrow(tobii_export.sub))))
  
  
  testFunc <- function(emdat.row, tobii.row, i){
    
    # timestamp
    try(if(emdat.row$timestamp != tobii.row$RecordingTimestamp)
      stop(paste("1Error: Participant", participant, "row", i, "gazesample timestamp does not match for rowindex", i,"EMDAT:", emdat.row$timestamp, "TObii:",tobii.row$RecordingTimestamp)))
    
    
    #is_valid_pupil
    valid_l <- tobii.row$ValidityLeft
    valid_r <- tobii.row$ValidityRight
    valid_sum <- valid_l+valid_r
    valid_pupil <- "FALSE" 
    if (valid_sum < 8) valid_pupil <- "TRUE"
    
    try(if(emdat.row$is_valid_pupil != valid_pupil)
      stop(paste("2Error: Participant", participant, "emdat timestamp", emdat.row$timestamp, "Pupil validity does not match for rowindex", i,"EMDAT:", emdat.row$is_valid_pupil, "TObii:",valid_pupil)))
    
    #is_valid_headdistance
    left <- tobii.row$DistanceLeft
    right <- tobii.row$DistanceRight
    valid_head <- "TRUE"
    if (is.na(left) & is.na(right)) valid_head <- "FALSE"
    else if (is.na(left)) {
      if (right == 0) valid_head <- "FALSE"}
    else if (is.na(right)) {
      if (left == 0) valid_head <- "FALSE"}
    
    #browser()
    
    try(if(emdat.row$is_valid_headdistance != valid_head)
      stop(paste("3Error: Participant", participant, "emdat timestamp", emdat.row$timestamp, "Head validity does not match for rowindex", i,"EMDAT:", emdat.row$is_valid_headdistance, "TObii:",valid_head)))
    
    
    #rawpupilsize
    if (valid_pupil){
      emdat_pupil <- signif(emdat.row$rawpupilsize,4)
      left <- tobii.row$PupilLeft
      right <- tobii.row$PupilRight
      if (is.na(left)) tobii_pupil <- signif(right,4)
      else if (is.na(right)) tobii_pupil <- signif(left,4)
      else tobii_pupil <- signif((left+right)/2 ,4)
    }
    else {
      emdat_pupil <- emdat.row$rawpupilsize
      tobii_pupil <- -1
    }
    
    try(if(tobii_pupil != emdat_pupil)
      stop(paste("4Error: Participant", participant, "emdat timestamp", emdat.row$timestamp, "raw pupil does not match for rowindex", i,"EMDAT:", emdat_pupil, "TObii:",tobii_pupil)))  
    
    
    #pupilvelocity
    if (valid_pupil){
      
      #find a previous valid row
      prev_row <- which(tobii_export.df$RecordingTimestamp == emdat.row$timestamp)
      a_row <- tobii_export.df[prev_row-1,]
      if (is.na(a_row$EyeTrackerTimestamp)) a_row <- tobii_export.df[prev_row-2,]
      valid_l <- a_row$ValidityLeft
      valid_r <- a_row$ValidityRight
      valid_sum <- valid_l+valid_r
      is_valid <- "FALSE"
      if (is.na(valid_l) | is.na(valid_r)) is_valid <- "FALSE"
      else if (valid_sum < 8) is_valid <- "TRUE"
      
      if (is_valid){      
        #do it
        emdat_velocity <- signif(emdat.row$pupilvelocity,6)
        prev_left <- a_row$PupilLeft
        prev_right <- a_row$PupilRight
        if (is.na(prev_left) | is.na(tobii.row$PupilLeft)){
          prev_pupil <- prev_right
          tobii_pupil <- tobii.row$PupilRight
        } 
        else if (is.na(prev_right) | is.na(tobii.row$PupilRight)){  
          prev_pupil <- prev_left
          tobii_pupil <- tobii.row$PupilLeft
        }
        else prev_pupil <- (prev_left+prev_right)/2
        time <- emdat.row$timestamp -  a_row$RecordingTimestamp   
        tobii_velocity <- round((abs(tobii_pupil - prev_pupil)/time),19)
        tobii_velocity <- signif(tobii_velocity,6)
        #if (emdat.row$timestamp == "73579") browser()
        if (is.na(prev_right) & is.na(tobii.row$PupilLeft)){  
          tobii_velocity <- -1
        }
        if (is.na(prev_left) & is.na(tobii.row$PupilRight)){  
          tobii_velocity <- -1
        } 
        if (tobii_velocity < 0.0000000000000001) tobii_velocity <- 0
        if (emdat_velocity < 0.0000000000000001) emdat_velocity <- 0
      }
      else {
        emdat_velocity <- emdat.row$pupilvelocity
        tobii_velocity <- -1  
      }
    }
    else {
      emdat_velocity <- emdat.row$pupilvelocity
      tobii_velocity <- -1      
    }
    
    try(if(tobii_velocity != emdat_velocity)
      stop(paste("5Error: Participant", participant, "emdat timestamp", emdat.row$timestamp, "pupil velocity does not match for rowindex", i,"EMDAT:", emdat_velocity, "TObii:",tobii_velocity)))  
    
    
    
    
    if (valid_head){
      #rawpupilsize  
      emdat_head <- signif(emdat.row$headdistance,5)
      left <- tobii.row$DistanceLeft
      right <- tobii.row$DistanceRight
      if (is.na(left)) tobii_head <- signif(right,5)
      else if (is.na(right)) tobii_head <- signif(left,5) 
      else tobii_head <- signif((left+right)/2 ,5)
    }
    else {
      emdat_head <- emdat.row$headdistance
      if (is.na(left) & is.na(right)) tobii_head <- -1
      else tobii_head <- 0
    }
    #print(emdat.row$timestamp)
    try(if(tobii_head != emdat_head)
      stop(paste("Error: Participant", participant, "emdat timestamp", emdat.row$timestamp, "head distance does not match for rowindex", i,"EMDAT:", emdat_head, "TObii:",tobii_head)))  
    
    
    
    #stimuliname
    try(if(as.character(emdat.row$stimuliname) != as.character(tobii.row$MediaName))
      stop(paste("Error: Participant", participant, "emdat timestamp", emdat.row$timestamp, "Media/Stimul name does not match for rowindex", i,"EMDAT:", as.character(emdat.row$stimuliname), "TObii:",as.character(tobii.row$MediaName))))
    
    
    
  }
  
  size <- nrow(tobii_export.sub)
  for (i in 1:size){ 
    emdat.row <- EMDAT_data.df[i,]
    tobii.row <- tobii_export.sub[i,]
    testFunc(emdat.row, tobii.row, i)
  }
  
}



P16 <- readfiles_part1("P16_Data_Export.tsv", "TobiiV3_sample_16.seg","P16")
P17 <- readfiles_part1("P17_Data_Export.tsv", "TobiiV3_sample_17.seg","P17")
P18 <- readfiles_part1("P18_Data_Export.tsv", "TobiiV3_sample_18.seg","P18")