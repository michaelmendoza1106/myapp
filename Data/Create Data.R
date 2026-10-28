##########################
# Static Code ############
##########################
library(tidyverse)
library(ggplot2)
library(tidyplots)
library(rlang)
#Call Train&Test Dataset to merge
testdata <- read_delim("~/Data/test.csv", delim=";")
traindata <- read_delim("~/Data/train.csv", delim=";")
#Merge datasets
alldata <- bind_rows(testdata,traindata)

#output combined datasets as R dataset for use.
saveRDS(alldata, file = "combined.bankdata.rds")


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