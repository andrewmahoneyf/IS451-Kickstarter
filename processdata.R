library(ggplot2)
library(data.table)
library(dplyr)
library(lubridate)
library(caret)
library(rpart)
library(rpart.plot)
library(forecast)

kickstarter.df <- fread("./Data/ks-projects-201801.csv")

# change launched and dealine to date format
kickstarter.df$deadline <- ymd(kickstarter.df$deadline)
kickstarter.df$launched <- ymd_hms(kickstarter.df$launched)
kickstarter.df$launched <- as.Date(kickstarter.df$launched)

# create target column
kickstarter.df$successful <- ifelse(kickstarter.df$state == "successful", 1, 0)

# select and format relevant variables  
kickstarter.full.df <- kickstarter.df %>% mutate("total_days" = deadline - launched) %>%
  select(name, main_category, country, launched, deadline, total_days, usd_pledged_real, usd_goal_real, backers, state, successful) %>%
  rename(category = main_category, pledged = usd_pledged_real, goal = usd_goal_real) %>%
  filter(total_days <= 500)
kickstarter.full.df$total_days <- as.numeric(kickstarter.full.df$total_days, units="days")

# create data frame with only predictor variables
kickstarter.df <- kickstarter.full.df %>% select(category, country, total_days, goal, successful)

# partition data
set.seed(1)  
train.index <- sample(1:nrow(kickstarter.df), nrow(kickstarter.df)*0.6)  
train.df <- kickstarter.df[train.index, ]
valid.df <- kickstarter.df[-train.index, ]


################################# ggplots #####################################
# plot of only successful kickstarters
kickstarter.successful.df <- kickstarter.full.df %>% filter(successful == 1)

country_plot <- ggplot(data = kickstarter.successful.df) +
                  geom_point(aes(x = pledged, y = country, color = country)) +
                  labs(title = "Country Funding")

country_plot2 <- ggplot(data = kickstarter.successful.df, aes(country), position = position_stack(reverse = TRUE)) +
                    geom_bar() +
                    coord_flip() +
                    labs(title = "Top Countries")

category_plot <- ggplot(data = kickstarter.successful.df) +
                    geom_point(aes(x = pledged, y = category, color = category)) +
                    labs(title = "Categories Funding")

category_plot2 <- ggplot(data = kickstarter.successful.df, aes(category), position = position_stack(reverse = TRUE)) +
                    geom_bar() +
                    coord_flip() +
                    labs(title = "Top Categories")

days_plot <- ggplot(data =  subset(kickstarter.successful.df, total_days <= 50 & total_days > 10), aes(total_days)) +
                geom_bar() +
                labs(title = "Average Days")


########################## classification trees ################################
# default classification tree, optimized by pruning
default.ct <- rpart(successful ~ ., data = train.df, method = "class")

# plot tree
tree <- prp(default.ct, type = 1, extra = 1, varlen = -10)

# classification accuracy measure of the default tree
default.ct.point.pred <- predict(default.ct, valid.df, type = "class")

# generate confusion matrix for validation data
confusionMatrix <- confusionMatrix(default.ct.point.pred, factor(valid.df$successful))
fourfoldplot(confusionMatrix$table)


####################### multiple linear regression ##############################
# linear regression of success on all 4 predictors in the training set
kickstarter.lm <- lm(successful~., data = train.df)
options(scipen = 999)
summary(kickstarter.lm)

# make predictions on a new set. 
kickstarter.lm.pred <- predict(kickstarter.lm, valid.df)
options(scipen = 999, digits = 3)
accuracy(kickstarter.lm.pred, valid.df$successful)
options(scipen=999, digits = 0)

# get the residuals
all.residuals <- valid.df$successful - kickstarter.lm.pred
hist(all.residuals, breaks = 25, xlab = "Residuals")


############################ Logistic Regression #################################

