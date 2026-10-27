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

# bar static function
# Define the function
barplot <- function(data, x, group_var) {
  ggplot(data, aes(x = {{ x }}, fill = {{ group_var }})) +
    geom_bar(position = "dodge") + 
    theme_minimal() +
    labs(
      x = deparse(substitute(x)),
      y = "Count",
      fill = deparse(substitute(group_var))
    )
}

#test fucntion
barplot(alldata, job, loan)

# side by side static function
# Define the function
side_by_side <- function(data, x, y, group_var) {
  ggplot(data, aes(x = {{ x }}, y = {{ y }}, fill = {{ group_var }})) +
    geom_col(position = "dodge") + 
    theme_minimal() +
    labs(
      x = deparse(substitute(x)),
      y = deparse(substitute(y)),
      fill = deparse(substitute(group_var))
    )
}

#test function
side_by_side(alldata, job, balance, loan)

#histogram function
group_histogram <- function(data, x, y = NULL, group_var) {
  
  # Base plot mapping the X variable and the fill/grouping variable
  p <- ggplot(data, aes(x = {{ x }}, fill = factor({{ group_var }})))
  
  # Check if a custom Y variable/weight is provided
  if (is.null(substitute(y))) {
    # Standard frequency count histogram
    p <- p + geom_histogram(position = "identity", alpha = 0.6, color = "white")
  } else {
    # If a Y variable is explicitly given, map it to the y aesthetic or as a weight
    p <- p + geom_histogram(aes(y = {{ y }}), stat = "identity", position = "dodge")
  }
  
  # Clean up formatting and labels
  p <- p + 
    theme_minimal() +
    labs(fill = "Group")
  
  return(p)
}

# Test Histogram
group_histogram(alldata, balance, group_var = loan)
group_histogram(alldata, balance, duration, group_var = loan)

#create scatter plot output
boxplot <- function(data, x, y, fill = NULL) {
  
  # Initialize the base plot with standard aesthetics
  p <- ggplot(data, aes(x = {{ x }}, y = {{ y }}))
  
  # Check if a multivariate fill variable was provided
  if (!missing(fill)) {
    p <- p + geom_boxplot(aes(fill = {{ fill }}), outlier.alpha = 0.6)
  } else {
    # Default single-grouping boxplot with a clean default color
    p <- p + geom_boxplot(fill = "blue", outlier.alpha = 0.6)
  }
  
  # Apply a clean theme and layout adjustments
  p <- p + 
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(),
      legend.position = "top"
    )
  
  return(p)
}

#test the function
boxplot(alldata, age, job, education)

#scatter plot static function
scatter_plot <- function(data, x, y, group_var) {
  p <- ggplot(data, aes_string(x = deparse(substitute(x)), y = deparse(substitute(y)), color = deparse(substitute(group_var)), shape = deparse(substitute(group_var)))) +
        geom_point(size = 3, alpha = 0.8) +
        labs(
          x = deparse(substitute(x)),
          y = deparse(substitute(y)),
          color = deparse(substitute(group_var)),
          shape = deparse(substitute(group_var)),
          title = paste("Scatter Plot of", deparse(substitute(y)), "vs", deparse(substitute(x)), "by", deparse(substitute(group_var)))
        ) +
        theme_minimal()
  
  return(p)
}

#test function
scatter_plot(alldata, age, balance, default)

# violin plotting function
violin_plot <- function(data, x, y, group_var = NULL, facet_var = NULL) {
  # Capture the expressions as symbols
  x_sym <- ensym(x)
  y_sym <- ensym(y)
  
  # Determine if group_var was provided using substitute
  has_group <- !missing(group_var)
  has_facet <- !missing(facet_var)
  
  # Base plot mapping x and y dynamically using the injection operator (!!)
  p <- ggplot(data, aes(x = !!x_sym, y = !!y_sym))
  
  # group graph
  if (has_group) {
    group_sym <- ensym(group_var)
    p <- p + geom_violin(aes(fill = !!group_sym), scale = "width", adjust = 1) +
      labs(fill = as.character(group_sym))
  } else { 
  # default no group 
    p <- p + geom_violin(fill = "skyblue", scale = "width", adjust = 1)
  }
  
  # faceting
  # Add dynamic faceting if a facet variable is provided
  if (has_facet) {
    facet_sym <- ensym(facet_var)
    p <- p + facet_wrap(vars(!!facet_sym))
  }
  
  p <- p + theme_minimal() +
    labs(
      x = as.character(x_sym),
      y = as.character(y_sym),
      title = paste("Violin Plot of", as.character(y_sym), "by", as.character(x_sym))
    )
  
  return(p)
}

#test the function
violin_plot(alldata, marital, balance, job, default)