##########################
# Static Code ############
##########################
library(tidyverse)
library(ggplot2)
library(tidyplots)
library(rlang)

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
#one_way_table(alldata$job)

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
#two_way_table(alldata$job, alldata$marital)

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
#test_summary <- summaries(alldata, all, age)
#test_summary

# bar static function
# Define the function
barplot <- function(data, x, group_var, x_label, fill_label) {
  ggplot(data, aes(x = {{ x }}, fill = {{ group_var }})) +
    geom_bar(position = "dodge") + 
    theme_minimal() +
    labs(
      x = x_label,
      y = "Count",
      fill = x_label
    )
}

#test fucntion
#barplot(alldata, job, loan, "Personal Loan Status", "loan")

# side by side static function
# Define the function
histogram <- function(data, x, y, group_var, x_label, y_label, group_label) {
  ggplot(data, aes(x = {{ x }}, y = {{ y }}, fill = {{ group_var }})) +
    geom_col(position = "dodge") + 
    theme_minimal() +
    labs(
      x = x_label,
      y = y_label,
      fill = group_label
    )
}

#test function
#side_by_side(alldata, job, balance, loan)

#create scatter plot output
boxplot <- function(data, x, y, fill ) {
  
  # Initialize the base plot with standard aesthetics
  p <- ggplot(data, aes(x = {{ x }}, y = {{ y }})) + 
    geom_boxplot(aes(fill = {{ fill }}), outlier.alpha = 0.6) + 
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(),
      legend.position = "top"
    )
  
  return(p)
}

#test the function
# boxplot(alldata, age, job, education)

#scatter plot static function
 scatter_plot <- function(data, x, y, group_var, x_label, y_label, group_label) {
  p <- ggplot(data, aes(x = {{ x }}, y = {{ y }}, color = {{ group_var }}, shape = {{ group_var }} )) +
        geom_point(size = 3, alpha = 0.6) +
        labs(
          x = x_label,
          y = y_label,
          color = group_label,
          shape = group_label
        ) +
        theme_minimal()
  
  return(p)
}

#test function
# scatter_plot(alldata, age, balance, default, "Age", "Balance", "Default Status")

# violin plotting function
violin_plot <- function(data, x, y, facet_var, x_label, y_label) {
  
  # Base plot
  p <- ggplot(data, aes(x = {{ x }}, y = {{ y }})) + 
    geom_violin( scale = "width", adjust = 1) +
    facet_wrap(vars({{ facet_var }})) + 
    theme_minimal() +
    labs(
      x = x_label,
      y = y_label
    )
  
  return(p)
}

#test the function
# violin_plot(alldata, marital, balance, job)