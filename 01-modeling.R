# load libraries
library(tidyverse)
library(lubridate)
library(effects)

# read in data
compas_data <- read_csv('https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-raw.csv')

# filter to include only Risk of Violence
compas_data_rov <- compas_data %>% 
  filter(DisplayText == 'Risk of Violence',
         DecileScore > -1)

# mutate to add years and correct dates before 1969
compas_data_rov <- compas_data_rov %>% 
  mutate(Screening_Date = as.Date(Screening_Date, format="%m/%d/%y"),
         DateOfBirth = as.Date(DateOfBirth, format="%m/%d/%y"))

compas_data_rov$DateOfBirth[format(compas_data_rov$DateOfBirth, format = "%Y") > 2010] <- 
  compas_data_rov$DateOfBirth[format(compas_data_rov$DateOfBirth, format = "%Y") > 2010] - years(100)

# add age column
compas_data_rov <- compas_data_rov %>% 
  mutate(age = as.period(interval(compas_data_rov$DateOfBirth, compas_data_rov$Screening_Date))$year)
           
# make linear model comparing age and risk of violence score      
age_rov_model <- lm(DecileScore ~ age,
                    data = compas_data_rov)

# get effects for different age ranges
effects_df <- effect("age", age_rov_model, xlevels=8) %>%
  data.frame()

# plot effects
effects_df %>% 
  ggplot(aes(x = age,
             y = fit,
             ymin = lower,
             ymax = upper,
             label = round(fit, digits=2))) + 
  geom_errorbar() + 
  geom_label()

compas_data_rov %>% 
  ggplot(aes(x = age,
             y = DecileScore)) + 
  geom_col()

# create new column with age range as categorical variable
compas_data_rov$age_groups = cut(compas_data_rov$age, breaks=c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

# plot risk of violence scores by age
compas_data_rov %>% 
  ggplot(aes(x = DecileScore)) + 
  geom_bar() + 
  facet_wrap(~age_groups) # still need to fix labels, add title, etc

     
         

