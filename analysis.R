library(tidyverse)
library(questionr)
library(caTools)
library(randomForest)

data <- read.csv(
  "showwcase_sessions.csv"
)



#----Examining the data-----

head(data)
summary(data)


# ----Cleaning----
#NA/Missing values in data set
freq.na(data)

#Since there are minimal NA values in the data set, we will just remove them because it will not effect our analysis
data <- na.omit(data)

#Creating Categorical values for use with ML - always a good practice

data$projects_added[data$projects_added == 'TRUE'] <- 1
data$projects_added[data$projects_added == 'FALSE'] <- 0
data$likes_given[data$likes_given == 'TRUE'] <- 1
data$likes_given[data$likes_given == 'FALSE'] <- 0
data$comment_given[data$comment_given == 'TRUE'] <- 1
data$comment_given[data$comment_given == 'FALSE'] <- 0
data$likes_given[data$likes_given == 'TRUE'] <- 1
data$likes_given[data$likes_given == 'FALSE'] <- 0
data$inactive_status[data$inactive_status == 'TRUE'] <- 1
data$inactive_status[data$inactive_status == 'FALSE'] <- 0
data$bug_occured[data$bug_occured == 'TRUE'] <- 1
data$bug_occured[data$bug_occured == 'FALSE'] <- 0

#creating a dataset for unique customers (in theory, new users) - may or may not get to this

unique_id <- unique(data$customer_id)


#for now, we won't need session_id or customer_id, date, or inactive_duration
data_original <- data
data <- subset(data_original,select = -c(session_id,customer_id, login_date, inactive_duration))

#For the sake of this assignment, given the data we will assume that "user engagement" relates mostly to whether they are "browsing"
#doing things like giving likes and comments and spending more time on Showwcase.


#We want to get some insights from GLM and Random Forest as to what might increase user engagements


#we'll start with glmsince it will sort of be our frame of reference
  #rreframing the data without unecessary values


#splitting
split <- sample.split(data, SplitRatio = .75)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)


#building glm classifier
classifier_duration_glm <- glm(session_duration~., family = gaussian, data = training_set)
classifier_duration
plot(classifier_duration)


#building randomforest
library(tree)
tree_duration <- tree(session_duration~., data = training_set)
tree_duration
plot(tree_duration)
text(tree_duration, pretty = 10)

#we should always text survival bias to make sure we are examining the data from all sides, which in this case means we should see what GLM thinks

#creating survival data
survival_data <- data_original
survival_data <- subset(survival_data, select = -c(session_duration, login_date,session_id,customer_id, inactive_status))

split_2 <- sample.split(survival_data, SplitRatio = .75)
training_set <- subset(survival_data, split == TRUE)
test_set <- subset(survival_data, split == FALSE)


#building glm classifier
classifier_duration_glm_survivor <- glm(inactive_duration~., family = gaussian, data = training_set)
classifier_duration_glm_survivor
plot(classifier_duration_glm_survivor)




