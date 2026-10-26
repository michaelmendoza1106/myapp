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

#test twoway static function
two_way_table(alldata$job, alldata$marital)

#Numerical Summaries 
# Function to calculate grouped summaries
summaries <- function(data, group_var, num_var) {
 df <- data %>%
    group_by({{ group_var }}) %>%
    summarise(
      Count  = n(),
      Mean   = mean({{ num_var }}, na.rm = TRUE),
      Median = median({{ num_var }}, na.rm = TRUE),
      SD     = sd({{ num_var }}, na.rm = TRUE),
      Min    = min({{ num_var }}, na.rm = TRUE),
      Max    = max({{ num_var }}, na.rm = TRUE),
      .groups = "drop"
    )
 
 return(df)
}

#test Summary Function
test_summary <- summaries(alldata, marital, age)
test_summary