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
  filter(total_days <= 500, goal <= 2000000)
kickstarter.full.df$total_days <- as.numeric(kickstarter.full.df$total_days, units="days")

# create data frame with only predictor variables
kickstarter.df <- kickstarter.full.df %>% select(category, country, total_days, goal, successful)

# partition data
set.seed(1)  
train.index <- sample(1:nrow(kickstarter.df), nrow(kickstarter.df)*0.6)  
train.df <- kickstarter.df[train.index, ]
valid.df <- kickstarter.df[-train.index, ]

########################## classification trees ################################
# default classification tree, optimized by pruning
default.ct <- rpart(successful ~ ., data = train.df, method = "class")

# plot tree
tree <- prp(default.ct,branch=1,extra=101,type=1,nn=TRUE)

# classification accuracy measure of the default tree
default.ct.point.pred <- predict(default.ct, valid.df, type = "class")

# generate confusion matrix for validation data
confusionMatrix <- confusionMatrix(default.ct.point.pred, factor(valid.df$successful))
fourfoldplot(confusionMatrix$table)

################################# ggplots #####################################
# remove outliers for graphs and regressions 
kickstarter.df <- filter(kickstarter.df, goal >= 5000, goal <= 100000)

# dataframe for successful kickstarters as well as percent of successful
kickstarter.successful.df <- kickstarter.full.df %>% filter(successful == 1)

kickstarter.category.percent.df <- kickstarter.df %>% group_by(successful, category) %>%
  summarise (n = n()) %>% mutate(percentage = (n / sum(n)) * 100) %>%
  filter(successful == 1)

kickstarter.country.percent.df <- kickstarter.df %>% group_by(successful, country) %>%
  summarise (n = n()) %>% mutate(percentage = (n / sum(n)) * 100) %>%
  filter(successful == 1)

# plots
country_plot <- ggplot(data = kickstarter.successful.df) +
                  geom_point(aes(x = pledged, y = country, color = country)) +
                  labs(title = "Successful Funding")

country_plot2 <- ggplot(kickstarter.country.percent.df, aes(x = country, y = percentage)) + 
                    geom_bar(stat = "identity") +
                    coord_flip() +
                    labs(title = "Top Countries by % Successful")

days_plot <- ggplot(data =  subset(kickstarter.successful.df, total_days <= 50 & total_days > 10), aes(total_days)) +
                geom_bar() +
                labs(title = "Successful Kickstarter Average Days")

category_plot <- ggplot(data = kickstarter.successful.df) +
                    geom_point(aes(x = pledged, y = category, color = category)) +
                    labs(title = "Successful Funding")

category_plot2 <- ggplot(kickstarter.category.percent.df, aes(x = category, y = percentage)) + 
                    geom_bar(stat = "identity") +
                    coord_flip() +
                    labs(title = "Top Categories by % Successful")


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
# Remove unsuccessful variables based on plots
kickstarter.df <- kickstarter.df %>% 
                  filter(category %in% c("Technology", "Music", "Games", "Food", "Film & Video", "Fashion", "Design")) %>%
                  select(-country)

# create reference categories
kickstarter.df$category <- factor(kickstarter.df$category)
kickstarter.df$category <- relevel(kickstarter.df$category, ref = "Food")

# create training and validation sets
set.seed(5)
train.index <- sample(1:nrow(kickstarter.df), nrow(kickstarter.df)*0.6)  
train.df <- kickstarter.df[train.index, ]
valid.df <- kickstarter.df[-train.index, ]

# run logistic model, and show coefficients 
logit.reg <- glm(successful ~ ., data = train.df, family = "binomial")
summary(logit.reg)

# set the cutoff to be 0.5 and evaluate the model
## use predict() with type = "response" to compute predicted probabilities
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# Choose cutoff value and evaluate classification performance
pred <- ifelse(logit.reg.pred > 0.5, 1, 0)

confusionMatrix <- confusionMatrix(factor(pred), factor(valid.df$successful))
fourfoldplot(confusionMatrix$table)
