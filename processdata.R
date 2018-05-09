library(ggplot2)
library(knn)
library(data.table)
library(dplyr)
library("lubridate")
library(caret)

kickstarter.df <- data.frame(fread("ks-projects-201801.csv"))

# Change launched and dealine to date format
kickstarter.df$deadline <- ymd(kickstarter.df$deadline)
kickstarter.df$launched <- ymd_hms(kickstarter.df$launched)
kickstarter.df$launched <- as.Date(kickstarter.df$launched)

# select and format relevant variables  
kickstarter.df <- kickstarter.df %>% mutate("total_days" = deadline - launched) %>%
                  select(name, main_category, country, launched, total_days, usd_pledged_real, usd_goal_real, backers, state) %>%
                  rename(category = main_category, pledged = usd_pledged_real, goal = usd_goal_real)
# create target column
kickstarter.df$target <- ifelse(kickstarter.df$state == "successful", 1, 0)

