#######################################
# BEST
# 
######################################

# najpierw ustawiæ WD
# setwd("D:/R/R_PROG_PA3")

best <- function(state, outcome)
  {
  # setting wd
  setwd("D:/R/R_PROG_PA3")
  
  # reading data and renaming columns
  df_outcome_raw <- read.csv("outcome-of-care-measures.csv",
                              stringsAsFactors=FALSE,
                              na.strings="Not Available")
  names(df_outcome_raw)[11] <- "heart attack"
  names(df_outcome_raw)[17] <- "heart failure"
  names(df_outcome_raw)[23] <- "pneumonia"
  
  # checking if value of "outcome" is valid
  outcome_all <- c("heart attack", "heart failure", "pneumonia")
  if (outcome %in% outcome_all==FALSE)
    {
    stop('invalid outcome')
    }
  
  # checking if value of "state" is valid (state is the 7th column in df)
  state_all <- unique(df_outcome_raw[,7])
  if (state %in% state_all==FALSE)
    {
    stop('invalid state')
    }
  
  #subsetting only the selected rows (for given state) and columns (for outcome)
  df_outcome <-
    df_outcome_raw[df_outcome_raw$State==state,c("Hospital.Name", outcome)]
  
  # ordering by outcome (col2) and name (col1) - overwrting
  df_outcome <- df_outcome[order(df_outcome[2], df_outcome[1]),]
  
  #returning the name of the best hospital
  df_outcome[1,1]
  
  }
