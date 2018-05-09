library(ggplot2)
library(knn)
library(dplyr)
library(caret)
kickstarter.df <- read.csv("ks-projects-201801.csv")

# Changed launched and dealine to date format

# format relevant variables  
kickstarter.df <- kickstarter.df %>% mutate("time_period" = deadline - launched)

select(name, main_category, country, launched, )