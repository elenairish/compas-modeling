# load libraries
library(tidyverse)

# read in data
compas_data <- read_csv('https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-raw.csv')

# filter to include only Risk of Violence
compas_data_rov <- compas_data %>% 
  filter(DisplayText == 'Risk of Violence')
