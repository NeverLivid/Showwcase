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

#changing durations to minutes to better understand the data

data$session_duration <- data$session_duration/60
data$inactive_duration <- data$inactive_duration/60

#For the sake of this assignment, given the data we will assume that "user engagement" relates mostly to whether they are "browsing"
#doing things like giving likes and comments and spending more time on Showwcase.

#therefore, we will define an engaged user who has liked, commented, or posted a project in their session.
# we will also create an "engagement score" from 1-3 based upon how many actions they placed in their session (likes, comments, projects)

data <- data %>% mutate(engaged = ifelse(sum(projects_added+likes_given+comment_given) >= 1, 1, 0),
                        engage_score = projects_added + comment_given + likes_given)

#now if we examine engaged and egagement_score we can see that all of our users in this data are "engaged", atleast by our minimum standard.
#That's totally fine but it means we need to expand our metric some more.
hist(data$engage_score)

#Looking at our data, our most useful metrics are probably the numbers of likes and comments as well as the session duration. Lets add points 
#to user engagements score based on how many comments and likes they give, and how long their session was.

hist(data$engaged)
hist(data$engage_score)

med_comments <- median(data$session_comments_given)
med_likes <- median(data$session_likes_given)
avg_session <- mean(data$session_duration)


data <- data %>% mutate(engage_score = projects_added + comment_given + likes_given + trunc(session_likes_given/med_likes)
                        + trunc(session_comments_given/med_comments) 
                        + trunc(session_duration/avg_session))

hist(data$engage_score) #looks a lot better!

#lets also save this so we can load it into Tableau
library(readr)
write.csv(data, file = 'data.csv')


#We want to get some insights from GLM and Decision Trees as to what the models think might increase user engagement status


#we'll start with glm 
#reframing the data without unnecessary values
training_set <- subset(data, select = c(session_projects_added,session_likes_given,session_comments_given, inactive_duration, session_duration, bugs_in_session,engage_score))



#building glm classifier
classifier_glm <- glm(engage_score~., family = gaussian, data = training_set)
classifier_glm
plot(classifier_glm)


#building a decision tree
library(tree)
tree_duration <- tree(engage_score~., data = training_set)
tree_duration
plot(tree_duration)
text(tree_duration, pretty = 20)

#if you view the metrics for our ML models, you can see they are basicly worthless because of the lack of predictive value in the dataset.


#------ Graphics ------
#install.packages("ggthemes")
library(ggthemes)
#plotting session_duration
duration_plot <- data %>% ggplot(aes(session_duration))+geom_histogram()+geom_vline(xintercept = mean(data$session_duration))+theme_igray()
duration_plot <- duration_plot +ggtitle('Session Duration Plot')
duration_plot

#plotting inactivity
inactivity_plot <- data %>% ggplot(aes(inactive_duration))+geom_histogram() + geom_vline(xintercept = mean(data$inactive_duration))+theme_igray()
inactivity_plot <- inactivity_plot + ggtitle("Inactivity Duration Plot")
inactivity_plot

#plot of engage_score
engage_plot <- data %>% ggplot(aes(engage_score)) + geom_histogram()+ geom_vline(xintercept = mean(data$engage_score))+theme_igray()
engage_plot <- engage_plot+ggtitle("Engagement Histogram")
engage_plot
ggsave("engagement.png")
