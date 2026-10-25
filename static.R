##########################
# Static Code ############
##########################
library(tidyverse)
library(ggplot2)
#Call Train&Test Dataset to merge
testdata <- read_delim("~/Data/test.csv", delim=";")
traindata <- read_delim("~/Data/train.csv", delim=";")
#Merge datasets
alldata <- bind_rows(testdata,traindata)

#output combined datasets as R dataset for use.
#saveRDS(alldata, file = "~/Data/combined.bankdata.rds")
#Test loading of datafile
#testload <- load("~/Data/combined.bankdata.rds")

alldata <- mutate(alldata, job = as.factor(job), 
                  marital = as.factor(marital), 
                  education = as.factor(education), 
                  default = as.factor(default), 
                  housing = as.factor(housing), 
                  loan = as.factor(loan), 
                  contact = as.factor(contact), 
                  poutcome = as.factor(poutcome), 
                  y = as.factor(y)
                  )

# Frequency Table (Counts)
# One Way
one_way_table <- function(var){
  tbl <- table(var)
  
  # Convert to a readable data frame
  df <- as.data.frame(tbl)
  
  # Add a percentage column for extra utility
  df$Percentage <- paste0(round((df$Freq / sum(df$Freq)), 3))
  
  return(df)
}

#test oneway static function
one_way_table(alldata$job)

# Two Way
two_way_table <- function(var1, var2){
  tbl <- table(var1, var2)
  
  # Convert to a readable data frame
  df <- as.data.frame(tbl)
  
  # Add a percentage column for extra utility
  df$Percentage <- paste0(round((df$Freq / sum(df$Freq)), 3))
  
  return(df)
}

#test oneway static function
two_way_table(alldata$job, alldata$marital)