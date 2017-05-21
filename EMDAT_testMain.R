source("EMDAT_part2Test.R")

### HOW TO RUN ###

# Set the working directory with the file path to the test package (i.e. setwd(<file path>)), 
# and then 'source' EMDAT_testMain.R


### Part2 Tests ###

# Set up the tests
# Choose particpants to run the tests on
participants <- list("101a", "101b")

# Run
# Note: second argument takes the last participant of the study, not necessarily the
#       last element in the list of participants given to the first argument    
run_part2Test(participants, "101b")
