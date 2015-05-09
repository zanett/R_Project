#######################################
# RANKHOSPITAL
# 
######################################

# najpierw ustawiæ WD
# setwd("D:/R/R_PROG_PA3")

rankhospital <- function(state, outcome, num="best")
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
  # changing column name
  names(df_outcome)[2] <- "Rate"
  
  # removing NAs - overwriting
  df_outcome <- df_outcome[!is.na(df_outcome$Rate),]
  
  # ordering by outcome (col2) and name (col1) - overwrting
  df_outcome <- df_outcome[order(df_outcome[2], df_outcome[1]),]
  
  # adding column with rank - 0 as default value
  df_outcome$Rank <- 0
  # ranking - hospitals are already ordered so just a simple "for loop"
  for (i in 1:length(df_outcome[,2]))
    {
    df_outcome[i,3] <- i
    }
  
  # determining numeric values for "best" and "worst"
  if (num=="best")
    {num <- min(df_outcome[,3])}
  if (num=="worst")
    {num <- max(df_outcome[,3])}
  
  # converting character(0) to NA
  result <- df_outcome[df_outcome$Rank==num,1]
  if (length(result)==0)
    {result <- NA}

  # printing the result
  result
    
  }
