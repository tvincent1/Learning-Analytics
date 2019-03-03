# Create data frame for each session

library(data.table)
library(tidyverse)
library(dplyr)

#repeat for directories 1-6
setwd ("~/R/LearningAnalytics_Genoa/EPMDataset/EPM Dataset 2/Data/Processes/Session 1")

s1 <- 
  list.files() %>% 
  map_df(~fread(., stringsAsFactors = FALSE, colClasses = list(integer64= 7)))

#explore dimensions of each subset

summary(s5)


#merge all sessions into one data frame
sessions <- bind_rows(list(s1, s2, s3, s4, s5, s6))


#explore session data
summary(sessions)

#add headers
colnames(sessions) <- as.character(features$V2)

head(sessions)

#convert time cols into Datetime format
library(fasttime)
library(lubridate)
#first, replace nonstandard separators in date
sessions$date <- str_replace(sessions$start_time,"\\.11\\.", "\\-11\\-")
sessions$date1 <- str_replace(sessions$date,"\\.10\\.", "\\-10\\-")

sessions$startdate <- strptime(sessions$date1, format ="%m-%d-%Y %H:%M:%S")
str(sessions)

sessions[, (enddate) := as.POSIXct(sessions[[enddate]])]
class(sessions[[enddate]])

#Explore user variations in # activities

library(ggplot2)
user_act <- sessions %>% select(student_Id, session, activity, mouse_movement, keystroke) %>%
  group_by(student_Id) %>%
  summarize(activities = n_distinct(activity), mmove = sum(mouse_movement), keys=sum(keystroke))

summary(user_act)
user_act %>% 
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
    user_act_ses <- sessions %>% select(student_Id, session, activity, mouse_movement, keystroke) %>%
      group_by(student_Id, session) %>%
      summarize(activities = n_distinct(activity), mmove = sum(mouse_movement), keys=sum(keystroke))
    
summary(user_act_ses)
  p4 <- user_act_ses %>% 
      ggplot(aes(student_Id,activities, group=student_Id)) +
      geom_boxplot() +
      ggtitle("Activites per Student Session") 
    ggsave("Activities_Student_Session.png", height = 5, width = 5)

    user_act_ses %>% 
      arrange(activities) %>%
      ggplot(aes(activities)) +
      geom_histogram(binwidth = 5) +
      ggtitle("Students per Session Actvity count band") +
      ylab("# Students")
    ggsave("Students_Session_Activities.png")
    
    p5 <-  user_act_ses %>% 
      ggplot(aes(student_Id,mmove, group=student_Id)) +
      geom_boxplot() +
      ggtitle("Mouse Moves per Student Session") 
    ggsave("Mouse_Student_Session.png", height = 5, width = 5)
    
    p6 <-  user_act_ses %>% 
      ggplot(aes(student_Id,keys, group=student_Id)) +
      geom_boxplot() +
      ggtitle("Keystrokes per Student Session") 
    ggsave("Keys_Student_Session.png", height = 5, width = 5)
    
    library(gridExtra)
    grid.arrange(p4, p5, p6, ncol=3) 
    ggsave("Student Session Metrics.png", height = 5, width = 10)


  user_act_ses %>%
    ggplot(aes(student_Id,n_distinct(session), fill=as.character(session), position="stack")) +
    geom_col() +
    labs(y="") +
    guides(color=guide_legend(title="Session")) +
#    scale_fill_manual(values= c("skyblue", "royalblue", "blue", "navy")) +
    theme(axis.text.y=element_blank(), legend.title = element_blank()) +
    ggtitle("Sessions Completed per Student")
    ggsave("Sessions per Student.png")
#We can see that different students completed different sessions.
    
#*********
    #Explore variability by student by activity type
    user_act_ses <- sessions %>% 
      select(activity, student_Id) %>%
      group_by(student_Id, activity) %>%
      summarize(act_type = n())
    
    summary(user_act_ses)
    user_act_ses %>% 
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

user_actt <- sessions %>%
  select(activity, student_Id) %>%
  mutate(act_type = activity) 
  user_actt$act_type[grepl('Deeds', user_actt$act_type)] <- 'Deeds'
  user_actt$act_type[grepl('Study', user_actt$act_type)] <- 'Study'
  user_actt$act_type[grepl('Text', user_actt$act_type)] <- 'Text'
  user_actt$act_type[grepl('FSM', user_actt$act_type)] <- 'FSM'
  user_actt$act_type[grepl('Fsm', user_actt$act_type)] <- 'FSM'
  user_actt$act_type[grepl('Other', user_actt$act_type)] <- 'zz Other'
    
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
    mutate(act_type = activity) 
  user_actt_ses$act_type[grepl('Deeds', user_actt_ses$act_type)] <- 'Deeds'
  user_actt_ses$act_type[grepl('Study', user_actt_ses$act_type)] <- 'Study'
  user_actt_ses$act_type[grepl('Text', user_actt_ses$act_type)] <- 'Text'
  user_actt_ses$act_type[grepl('FSM', user_actt_ses$act_type)] <- 'FSM'
  user_actt_ses$act_type[grepl('Fsm', user_actt_ses$act_type)] <- 'FSM'
  user_actt_ses$act_type[grepl('Other', user_actt_ses$act_type)] <- 'zz Other'
  
  user_actt_ses  %>%
    group_by(student_Id, session, act_type) %>%
    ggplot(aes(student_Id, act_type, fill = act_type, position="stack")) +
    geom_col() +
    facet_grid(session~.) +
    labs(y="") +
    theme(axis.text.y=element_blank(), legend.title = element_blank()) +
    ggtitle("Session Activities Completed by Type per Student")
  ggsave("Session ActivitiesPerTypeByStudent.png")  
  
  # examine grades per student
  summary(intermediate_grades)
  class(intermediate_grades)
  colnames(intermediate_grades) <- gsub(" ","",colnames(intermediate_grades))
  
  quiz <- gather(intermediate_grades, Session, Score, 'Session2':'Session6', factor_key = TRUE)

  quiz %>%
    group_by(StudentId, Session) %>%
    ggplot(aes(StudentId, Score, fill = Score, position_stack())) +
    geom_col() +
    facet_grid(Session~.) +
    ggtitle("Session Assessment Scores by Student")
  ggsave("Quiz Scores By Student.png")  

  #Combine the two final exam sessions, keeping just the last score for students who retoook it
  retake <- semi_join(final_grades2, final_grades, by= "Student ID")
  final1 <- anti_join(final_grades, final_grades2, by= "Student ID")
  final1a <- anti_join(final_grades2, final_grades, by= "Student ID")
  final <- bind_rows(final1, final1a, retake)

head(final)
summary(final)
items <- gather(final, Question, Score, 'ES 1.1 \r\n(2 points)':'TOTAL\r\n(100 points)', factor_key = TRUE)
    
library(stringr)
#rename(items$`Student ID`, items$`Student_ID`)

#    filter(Question == 'TOTAL\r\n(100 points)') %>%
overall <- overall %>%
  mutate(StudentID = as.numeric(overall$'Student ID')) 

library(RColorBrewer)

overall %>%
  ggplot( aes(Score)) +
  geom_histogram(binwidth = 5)   
    
overall %>%
  group_by(StudentID) %>%
  ggplot(aes(StudentID, Score), color = "blue") + 
    geom_col() +
    ggtitle("Final Exam Scores by Student") +
  scale_fill_manual(values = "royalblue")
#   +
#  scale_fill_brewer(palette=) 

ggsave("FinalByStudent.png")  
 
display.brewer.pal(n = 7, name = "RdYlGn")
head(items)  
summary(items)
class(items)

#* Predict how students will score on Final based on prior activities

# createtest and training subsets
# Validation (or test_set) set will be 30% of final exam data (overall) data

library(caret)
set.seed(1)
test_index <- createDataPartition(y = overall$StudentID, times = 1, p = 0.3, list = FALSE)

quiz <- quiz %>% 
 select(-StudentId)

overall <- overall %>% 
  select(- Question, -"Student ID")

Scores_df <- union(quiz, overall)

user_act <- user_act %>% 
  mutate(StudentID = student_Id) %>%
  select(-student_Id)

class(test_index)


test <- filter(user_act, StudentID %in% c(test_index))
test <- left_join(test, exam_test, by = "StudentID")

train <- filter(user_act, StudentID %in% c(exam_train$StudentID)) 
train <- left_join(train, exam_train, by = "StudentID")

testscore <- filter(Scores_df, StudentID %in% c(test_index))
trainscore <- anti_join(Scores_df, testscore, by = "StudentID")

exam_train <- trainscore %>%
  filter(Session =="TOTAL\r\n(100 points)") %>%
  select(-Session)
exam_test <- testscore %>%
  filter(Session =="TOTAL\r\n(100 points)")  %>%
  select(-Session)

summary(train)
summary(exam_train)

# *********** Evaluating Results ************
#create a function to evaluate whether the predicted scores are close to the actual scores for a given student.

RMSE <- function(true_scores, predicted_scores){
  sqrt(mean((true_scores - predicted_scores)^2))
}

# ***************** Models **********************

# Basic model predicts that any student will get the average score

mu <- mean(exam_train$Score)
mu

# check results using RMSE function

avg_only <- RMSE(exam_test$Score, mu)
avg_only


# **** Store results in a data frame *******

rmse_results <- data_frame(Model = "Simple average", RMSE = avg_only)


# ****** # Activities effect on predicted rating ******************

# see whether the number of activities completed, mouse movementsor keystrokes is a factor in final score
cor_act = cor(train$activities, exam_train$Score)
cor_mouse = cor(log10(train$mmove), exam_train$Score)
cor_key = cor(train$keys, exam_train$Score)

cor_results <- data.frame(Number_of = "Activities", Correlation = cor_act)
cor_results <- bind_rows(cor_results,
                         data_frame(Number_of="Mouse Movements", Correlation = cor_mouse ))
cor_results <- bind_rows(cor_results,
                         data_frame(Number_of="Keystrokes", Correlation = cor_key ))

cor_results %>% knitr::kable()

plot(exam_train$Score, train$activities)
plot(exam_train$Score, log10(train$mmove))
plot(exam_train$Score, train$keys)
 
# create a term for activity count effect, ac, as the difference in average count and the student's count 
# multiplied by its correlation coefficient.

act_avg = mean(train$activities)
ac = cor_act*(train$activities-act_avg)

#add to model and assess results
avg_act <- mu + ac

model2<- RMSE(exam_test$Score, avg_act)

rmse_results <- bind_rows(rmse_results, 
                          data_frame(Model = "Simple average  + # Activities", 
                                     RMSE = model2))

# create a term for mouse movement effect, mm, as the difference in average count and the student's count 
# divided by 10^4 and multiplied by its correlation coefficient.

avg_m = mean(train$mmove/10^4)
mm = cor_mouse*(train$mmove/10^4-avg_m)

#add to model and assess results
avg_act_m <- mu + ac + mm

model3<- RMSE(exam_test$Score, avg_act_m)

rmse_results <- bind_rows(rmse_results, 
                          data_frame(Model = "Simple average  + # Activities + Mouse Movement", 
                                     RMSE = model3))

# create a term for keystroke count effect, kc, as the difference in average count and the student's count 
# multiplied by its correlation coefficient.

ks_avg = mean(train$keys/10^3)
kc = cor_key*(train$keys/10^3-ks_avg)

#add to model and assess results
avg_act_m_k <- mu + ac + mm + kc

model4<- RMSE(exam_test$Score, avg_act_m_k)

rmse_results <- bind_rows(rmse_results, 
                          data_frame(Model = "Simple average  + # Activities + Mouse Movement + Keystrokes", 
                                     RMSE = model4))
rmse_results %>% knitr::kable()

