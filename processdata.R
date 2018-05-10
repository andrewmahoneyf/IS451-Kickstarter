library(ggplot2)
library(data.table)
library(dplyr)
library("lubridate")
library(caret)
library(rpart)
library(rpart.plot)

kickstarter.df <- fread("ks-projects-201801.csv")

# change launched and dealine to date format
kickstarter.df$deadline <- ymd(kickstarter.df$deadline)
kickstarter.df$launched <- ymd_hms(kickstarter.df$launched)
kickstarter.df$launched <- as.Date(kickstarter.df$launched)

# create target column
kickstarter.df$successful <- ifelse(kickstarter.df$state == "successful", 1, 0)

# select and format relevant variables  
kickstarter.df <- kickstarter.df %>% mutate("total_days" = deadline - launched) %>%
  select(name, main_category, country, launched, deadline, total_days, usd_pledged_real, usd_goal_real, backers, state, successful) %>%
  rename(category = main_category, pledged = usd_pledged_real, goal = usd_goal_real)

# create ggplots
country_plot <- ggplot(data = kickstarter.df) +
                  geom_point(aes(x = pledged, y = country, color = country))

category_plot <- ggplot(data = kickstarter.df) +
                  geom_point(aes(x = pledged, y = category, color = category))

# create data frame with only predictor variables
kickstarter.tree.df <- kickstarter.df %>% select(category, country, total_days, goal, successful)

# partition data
set.seed(1)  
train.index <- sample(1:nrow(kickstarter.tree.df), nrow(kickstarter.tree.df)*0.6)  
train.df <- kickstarter.tree.df[train.index, ]
valid.df <- kickstarter.tree.df[-train.index, ]

# default classification tree, optimized by pruning
default.ct <- rpart(successful ~ ., data = train.df, method = "class")
## plot tree
tree <- prp(default.ct, type = 1, extra = 1, varlen = -10)

# Classification accuracy measure of the default tree
default.ct.point.pred <- predict(default.ct, valid.df, type = "class")

# generate confusion matrix for validation data
confusionMatrix(default.ct.point.pred, factor(valid.df$successful))
