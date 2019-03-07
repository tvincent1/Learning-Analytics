# Learning Analytics Project - capstone for Data Science PH125x.9
# Professional Certification
# Tasha Vincent
# March 2019


# Create data frame for each session

library(data.table)
library(tidyverse)
library(dplyr)

setwd ("~/R/LearningAnalytics_Genoa/EPMDataset/EPM Dataset 2/Data/Processes/Session 1") 
s1 <- list.files() %>% 
  map_df(~fread(., stringsAsFactors = FALSE, colClasses = list(integer64= 7)))

setwd ("~/R/LearningAnalytics_Genoa/EPMDataset/EPM Dataset 2/Data/Processes/Session 2") 
s2 <- list.files() %>% 
  map_df(~fread(., stringsAsFactors = FALSE, colClasses = list(integer64= 7)))
setwd ("~/R/LearningAnalytics_Genoa/EPMDataset/EPM Dataset 2/Data/Processes/Session 3") 
s3 <- list.files() %>% 
  map_df(~fread(., stringsAsFactors = FALSE, colClasses = list(integer64= 7)))
setwd ("~/R/LearningAnalytics_Genoa/EPMDataset/EPM Dataset 2/Data/Processes/Session 4") 
s4 <- list.files() %>% 
  map_df(~fread(., stringsAsFactors = FALSE, colClasses = list(integer64= 7)))
setwd ("~/R/LearningAnalytics_Genoa/EPMDataset/EPM Dataset 2/Data/Processes/Session 5") 
s5 <- list.files() %>% 
  map_df(~fread(., stringsAsFactors = FALSE, colClasses = list(integer64= 7)))
setwd ("~/R/LearningAnalytics_Genoa/EPMDataset/EPM Dataset 2/Data/Processes/Session 6") 
s6 <- list.files() %>% 
  map_df(~fread(., stringsAsFactors = FALSE, colClasses = list(integer64= 7)))


#explore dimensions of each subset

summary(s5)


#merge all sessions into one data frame
sessions2 <- bind_rows(list(s1, s2, s3, s4, s5, s6))


#explore session data
summary(sessions2)

#add headers
colnames(sessions2) <- as.character(features$V2)

head(sessions2)

#convert time cols into Datetime format
#library(fasttime)
#library(lubridate)
#first, replace nonstandard separators in date
#sessions$date <- str_replace(sessions$start_time,"\\.11\\.", "\\-11\\-")
#sessions$date1 <- str_replace(sessions$date,"\\.10\\.", "\\-10\\-")

#sessions$startdate <- strptime(sessions$date1, format ="%m-%d-%Y %H:%M:%S")
str(sessions2)

#sessions[, (enddate) := as.POSIXct(sessions[[enddate]])]
#class(sessions[[enddate]])
  

#**************** examine grades per student
  summary(intermediate_grades)
  class(intermediate_grades)
  colnames(intermediate_grades) <- gsub(" ","",colnames(intermediate_grades))
  
  quiz <- gather(intermediate_grades, Session, Score, 'Session2':'Session6', factor_key = TRUE)
  
  #Combine the two final exam sessions, keeping just the last score for students who retoook it
  retake <- semi_join(final_grades2, final_grades, by= "Student ID")
  final1 <- anti_join(final_grades, final_grades2, by= "Student ID")
  final1a <- anti_join(final_grades2, final_grades, by= "Student ID")
  final <- bind_rows(final1, final1a, retake)

head(final)
summary(final)

items <- gather(final, Question, Score, 'ES 1.1 \r\n(2 points)':'TOTAL\r\n(100 points)', factor_key = TRUE)
  
overall <- items %>%
  filter(Question =='TOTAL\r\n(100 points)') %>%
  select(-Question)
   
#Explore user variations in # activities

library(ggplot2)
user_act2 <- sessions2 %>% 
  select(student_Id, session, activity, mouse_movement, keystroke) %>%
  group_by(student_Id) %>%
  summarize(activities = n_distinct(activity), mmove = sum(mouse_movement), keys=sum(keystroke))

summary(user_act2)
user_act2 %>% 
  arrange(activities) %>%
  ggplot(aes(activities)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Count of Activites per Student") +
  ylab("# Students")
  geom_bar(stat = "identity")
  
 p1 <- user_act %>% 
    ggplot(aes(activities)) +
    geom_density(kernel = "gaussian", fill = "blue") +
    ggtitle("Count of Activites per Student")
    ggsave("Activities_Student.png", height = 5, width = 5)
  
p2 <-  user_act %>% 
      ggplot(aes(mmove)) +
      geom_density(kernel = "gaussian", fill = "blue") +
      scale_x_log10() +
      ggtitle("Mouse Movements per Student")  +
      xlab("Mouse Movement")
    ggsave("Mouse_Student.png", height = 5, width = 5)
    
  p3 <-  user_act %>% 
      ggplot(aes(keys)) +
      geom_density(kernel = "gaussian", fill = "blue") +
      scale_x_log10() +
      ggtitle("Keystrokes per Student") +
      xlab("Keystrokes")
    ggsave("Keystrokes_Student.png", height = 5, width = 5)
    
    library(gridExtra)
    grid.arrange(p1, p2, p3, ncol=3)


#Explore variability by student by session
    user_act_ses <- sessions %>% 
      select(student_Id, session, activity, mouse_movement, keystroke) 
    
    user_act_ses <- user_act_ses %>%
      filter(activity != "Other")
    
    user_act_ses <- user_act_ses %>%
      filter(activity != "Blank")%>%
      group_by(student_Id, session) %>%
      summarize(activities = n_distinct(activity), mmove = sum(mouse_movement), keys=sum(keystroke))
    
summary(user_act_ses)
 user_act_ses %>% 
      ggplot(aes(student_Id,activities, group=session)) +
      geom_boxplot() +
      ggtitle("Activites per Student Session") 
 #   ggsave("Activities_Student_Session.png", height = 5, width = 5)

  user_act_ses %>% 
      arrange(session) %>%
      ggplot(aes(student_Id)) +
      geom_histogram(binwidth = 5) +
      ggtitle("Students per Session Actvity count band") +
      ylab("# Students")
  #  ggsave("Students_Session_Activities.png")
    
  # user_act_ses %>% 
      ggplot(aes(student_Id,mmove, group=student_Id)) +
      geom_boxplot() +
      ggtitle("Mouse Moves per Student Session") 
#    ggsave("Mouse_Student_Session.png", height = 5, width = 5)
    
    #user_act_ses %>% 
      ggplot(aes(student_Id,keys, group=student_Id)) +
      geom_boxplot() +
      ggtitle("Keystrokes per Student Session") 
   # ggsave("Keys_Student_Session.png", height = 5, width = 5)
    
    library(gridExtra)
    grid.arrange(p4, p5, p6, ncol=3) 
    ggsave("Student Session Metrics.png", height = 5, width = 10)


  user_actt_ses %>%
    ggplot(aes(student_Id,n_distinct(session), fill=as.character(session), position="stack")) +
    geom_col() +
    labs(y="") +
    guides(color=guide_legend(title="Session")) +
    scale_fill_manual(values= c("lightblue","skyblue", "royalblue", "blue", "navy", "black")) +
    theme(axis.text.y=element_blank(), legend.title = element_blank()) +
    ggtitle("Sessions Completed per Student")
    ggsave("Sessions per Student.png")
#We can see that different students completed different sessions.
    
#*********
    #Explore variability by student by activity type
    user_actt_ses <- user_act_ses %>% 
      select(activity, student_Id) %>%
      group_by(student_Id, activity) %>%
      summarize(act_type = n())
    
    summary(user_actt_ses)
    user_actt_ses %>% 
      ggplot(aes(student_Id,act_type, fill=activity, position="stack")) +
      geom_col() +
      labs(y="") +
      guides(color=guide_legend(title="Activity Type")) +
    #  scale_fill_manual(values= c("skyblue", "royalblue", "blue", "navy")) +
      theme(axis.text.y=element_blank(), legend.title = element_blank()) +
      ggtitle("Activities Completed by Type per Student")
      ggsave("Activities per Student.png")

 #collapse types
    library(stringr)

user_actt <- user_act_ses %>%
  select(activity, student_Id) %>%
  mutate(act_type = activity) 
  user_actt$act_type[grepl('Deeds', user_actt$act_type)] <- 'Deeds'
  user_actt$act_type[grepl('Study', user_actt$act_type)] <- 'Study'
  user_actt$act_type[grepl('Text', user_actt$act_type)] <- 'Text'
  user_actt$act_type[grepl('FSM', user_actt$act_type)] <- 'FSM'
  user_actt$act_type[grepl('Fsm', user_actt$act_type)] <- 'FSM'

  user_actt  %>%
    group_by(student_Id, act_type) %>%
#    summarize(n()) %>%
  ggplot(aes(student_Id,act_type, fill = act_type, position="stack")) +
    geom_col() +
    labs(y="") +
    theme(axis.text.y=element_blank(), legend.title = element_blank()) +
    ggtitle("Activities Completed by Type per Student")
  ggsave("ActivitiesPerTypeByStudent.png")  

#look at activities by type per session
  user_actt_ses <- sessions %>%
    select(student_Id, activity, session ) %>%
    filter(activity != "Other") %>%
    filter(activity != "Blank") %>%
    mutate(act_type = activity) 
  user_actt_ses$act_type[grepl('Deeds', user_actt_ses$act_type)] <- 'Deeds'
  user_actt_ses$act_type[grepl('Study', user_actt_ses$act_type)] <- 'Study'
  user_actt_ses$act_type[grepl('Text', user_actt_ses$act_type)] <- 'Text'
  user_actt_ses$act_type[grepl('FSM', user_actt_ses$act_type)] <- 'FSM'
  user_actt_ses$act_type[grepl('Fsm', user_actt_ses$act_type)] <- 'FSM'

  user_actt_ses  %>%
    group_by(student_Id, session, act_type) %>%
    ggplot(aes(student_Id, act_type, fill = act_type, position="stack")) +
    geom_col() +
    facet_grid(session~.) +
    labs(y="") +
    theme(axis.text.y=element_blank(), legend.title = element_blank()) +
    ggtitle("Session Activities Completed by Type per Student")
  ggsave("Session ActivitiesPerTypeByStudent.png")  
  
 
library(stringr)
#rename(items$`Student ID`, items$`Student_ID`)

#    filter(Question == 'TOTAL\r\n(100 points)') %>%
#overall <- overall %>%
 # mutate(StudentID = as.numeric(overall$'Student ID')) 

library(RColorBrewer)

overall %>%
  ggplot(aes(Score)) +
  geom_histogram(binwidth = 5)  +  
  ggtitle("Final Exam Score Distribution")
  
overall %>%
    ggplot(aes(StudentID, Score, fill=Score)) + 
    geom_col() +
    ggtitle("Final Exam Scores by Student") 
#  scale_fill_brewer(palette="blues") 

ggsave("FinalByStudent.png")  
 
display.brewer.pal(n = 7, name = "RdYlGn")
head(items)  
summary(items)
class(items)

quiz %>%
    group_by(StudentID, Session) %>%
    ggplot(aes(StudentID, Score, fill = Score, position_stack())) +
    geom_col() +
    facet_grid(Session~.) +
    ggtitle("Session Assessment Scores by Student")
  ggsave("Quiz Scores By Student.png")  

  
#***************** Predict how students will score on Final based on prior activities

# create test and training subsets
# Validation (or test_set) set will be 30% of final exam data (overall) data

library(caret)
set.seed(1)
test_index <- createDataPartition(y = overall$StudentID, times = 1, p = 0.3, list = FALSE)

users <- overall %>% 
  select(- Question, -"Student ID")

user_act2 <- user_act2 %>% 
  mutate(StudentID = student_Id) %>%
  select(StudentID, activities, mmove, keys)

metrics <- left_join(users, user_act2, by = "StudentID")

test <- filter(metrics, StudentID %in% c(test_index))

train <- anti_join(metrics, test, by = "StudentID")

summary(train)
summary(test)

# *********** Evaluating Results ************
#create a function to evaluate whether the predicted scores are close to the actual scores for a given student.

RMSE <- function(true_scores, predicted_scores){
  sqrt(mean((true_scores - predicted_scores)^2))
}

# ***************** Models **********************

# Basic model predicts that any student will get the average score

mu <- mean(train$Score)
mu

# check results using RMSE function

avg_only <- RMSE(test$Score, mu)
avg_only


# **** Store results in a data frame *******

rmse_results <- data_frame(Model = "Simple average", RMSE = avg_only)


# ****** # Activities effect on predicted rating ******************

# see whether the number of activities completed, mouse movements or keystrokes is a factor in final score
fit <- lm(Score ~ activities + mmove + keys, data = train)

summary(fit)
library(broom)
coefs <- tidy(fit, conf.int = TRUE)
coefs

#graph the predicted results

test %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, Score, label = StudentID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

#enter the lm values in a model

predicted_score <- 23.7 + 0.226*test$activities +  0.00000543*test$mmove + 0.000988*test$keys

# check results using RMSE function

model <- RMSE(test$Score, predicted_score)
model


# **** add results to data frame *******

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Linear Regression",  
                                     RMSE = model ))


# see whether the session scores are predictive in final score
session_grades <- intermediate_grades %>% 
  mutate(StudentID = StudentId) %>%
  select(-StudentId)

metrics2 <- left_join(users, session_grades, by = "StudentID")

test2 <- filter(metrics2, StudentID %in% c(test_index))

train2 <- anti_join(metrics2, test, by = "StudentID")

summary(train2)
summary(test2)

fit2 <- lm(Score ~ Session2 + Session3 + Session4 + Session5 + Session6, data = train2)

summary(fit2)
coefs2 <- tidy(fit2, conf.int = TRUE)
coefs2

#graph the predicted results

test2 %>%
  mutate(R_hat = predict(fit2, newdata = .)) %>%
  ggplot(aes(R_hat, Score, label = StudentID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

#enter the lm values in a model

predicted_score2 <- 24.9 + 2.1*test2$Session2 + 1.96*test2$Session3 + 0.0884*test2$Session4 + 0.98*test2$Session5 + 9.36*test2$Session6

# check results using RMSE function

model2 <- RMSE(test2$Score, predicted_score2)
model2


# **** add results to data frame *******

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Session Scores",  
                                     RMSE = model2 ))


rmse_results %>% knitr::kable()

