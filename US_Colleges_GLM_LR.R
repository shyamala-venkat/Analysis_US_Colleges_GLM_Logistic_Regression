#---------------------------------------------------------------------------------#
# Shyamala Venkatakrishnan                                             01/26/2023 #
#                                                                                 #
#             ALY6015: Module 3 Assignment - GLM and Logistic Regression          #                        #
#---------------------------------------------------------------------------------#

install.packages(c("dplyr","ggplot2","sqldf","tidyverse","RColorBrewer",
                   "plotly","gmodels","formattable","tidyr"))

loadlibrary <- c("dplyr","ggplot2","sqldf","tidyverse","RColorBrewer",
                 "plotly","gmodels","formattable","tidyr")

lapply(loadlibrary,require,character.only=TRUE)


install.packages("ISLR")
library(ISLR)

college_df <- College


head(college_df)
summary(college_df)
View(college_df)
dim(college_df)

install.packages('DataExplorer') 

library(DataExplorer)

create_report(college_df)
rownames(college_df)

college_df = college_df %>%
  mutate(
    college_name = rownames(college_df)
  )

college_df = college_df %>%
  mutate(
    Type = case_when(
      Private == "Yes" ~ "Private",
      Private == "No"  ~ "Public"
    )
  )

View(college_df)

#EDA:

install.packages("psych")
library(psych) 

formattable(describe(college_df), 
            caption = "Descriptive statistics summary of the baseball dataset")

describe(college_df)

#1. What is the percentage of private and public universities in this dataset?

private_table <- table(college_df$Type)

private_table_perc <- round((private_table / 777) * 100, digits = 2)

private_table_df <- data.frame(private_table_perc)

colnames(private_table_df)[1] <- "Type_of_university"
colnames(private_table_df)[2] <- "Percentage"

ggplot(private_table_df, aes(x = "", y = Percentage, fill = Type_of_university)) +
  geom_col() + geom_text(aes(label = paste(Percentage,"%", sep = ""))
                         ,position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Type of university")) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette = "Set2")+
  ggtitle("Percentage of private and public universities")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))

#Almost 73% of colleges are private universities and rest 27% of colleges are 
#public universities.

#2. How many applications are received in public and private universities?

ggplot(college_df, aes(x=Apps))+
  geom_histogram(color="darkblue", fill="lightblue")+
  facet_grid(. ~ Type)+
  theme_bw()+xlab("No. of Applications")+ylab("No. of colleges")+
  ggtitle("Distribution of applications received in public vs private universities")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))

#3. Top 10 colleges with max  number of applications received.

avg_applns_df <- sqldf("select college_name,Apps as Applications from 
                     college_df group by college_name
                    order by Applications desc limit 10")


ggplot(avg_applns_df, aes(x = reorder(college_name, Applications), y = Applications)) +
  geom_col(width = 0.5,aes(fill  = Applications))+
  ggtitle("Top 10 colleges with max number of applications")+
  xlab("College name")+
  ylab("No. of applications")+theme_bw()+coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))



#5. Top 10 colleges with the least acceptance rate:


least_accept_df <- sqldf("select college_name,Type,round((Accept/Apps) * 100,2) 
                    as Acceptance_rate from college_df 
                    group by college_name
                    order by acceptance_rate limit 15")

ggplot(least_accept_df, aes(x = reorder(college_name, -acceptance_rate),
                           y = acceptance_rate)) +
  geom_col(width = 0.5,aes(fill  = acceptance_rate))+
  ggtitle("Top 15 colleges with the least acceptance rate")+
  xlab("College name")+
  ylab("Acceptance rate")+theme_bw()+coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))

install.packages("devtools")
library(devtools)

devtools::install_github("yogevherz/plotme",force = TRUE)

library(plotme)

least_accept_df %>% 
  count(Type, college_name,Acceptance_rate) %>% 
  count_to_sunburst()

#6. Which type of colleges generally are costlier to study? 

c1_df <- sqldf("select Type,avg(Outstate)
                    as outstate from college_df 
                    group by Type")

c1_df

least_accept_df <- sqldf("select college_name,Type,round((Accept/Apps) * 100,2) 
                    as Acceptance_rate from college_df 
                    group by college_name
                    order by acceptance_rate limit 15")

c1 <- ggplot(college_df, aes(fill=Type, y=Outstate, x=Type)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_brewer(palette = "Pastel1")

c2 <- ggplot(college_df, aes(fill=Type, y=Room.Board, x=Type)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_brewer(palette = "Pastel1")

c3 <-  ggplot(college_df, aes(fill=Type, y=Books, x=Type)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_brewer(palette = "Pastel1")

c4 <-  ggplot(college_df, aes(fill=Type, y=Personal, x=Type)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_brewer(palette = "Pastel1")


c5 <-  ggplot(college_df, aes(fill=Type, y=Expend, x=Type)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_brewer(palette = "Pastel1")


install.packages("lattice")
library(lattice)
library(gridExtra)
library(grid)

grid.arrange(c1,c2,c3,c4,c5,ncol=2,nrow=3)

#7. Which type of colleges have good graduation rates? Name the top 15

top_grad_rate_df <- sqldf("select college_name,Type,`Grad.Rate` 
                    as Graduation_Rate from college_df 
                    order by Graduation_Rate desc limit 15")

top_grad_rate_df %>% 
  count(Type, college_name,Graduation_Rate) %>% 
  count_to_sunburst()

#8. Which type of colleges have more qualified faculties with a PhD or a terminal degree? 

q1_phd <- sqldf("select Type, avg(PhD) as avg_PhD from college_df 
                   group by Type")

q2_terminal <- sqldf("select Type, avg(Terminal) as avg_Terminal from college_df 
                   group by Type")

q1 <- ggplot(q1_phd, aes(fill=Type, y=avg_PhD, x=Type)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_brewer(palette = "Pastel2")+
  ylab("Avg. no. of faculties with a PhD degree")+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))

q2 <- ggplot(q2_terminal, aes(fill=Type, y=avg_Terminal, x=Type)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_brewer(palette = "Pastel2")+
  ylab("Avg. no. of faculties with a Terminal degree")+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))

grid.arrange(q1,q2,ncol=2,nrow=1, 
             top=textGrob("Avg. No of faculties with a PhD/Terminal degree",gp=gpar(fontsize=14,font=3)))

#9. what are the top 15 colleges with a lower Student to Faculty Ratio?

sf_ratio <- sqldf("select Type as 'Type of University', college_name,`S.F.Ratio`
                       as Student_Faculty_Ratio from college_df
                      order by Student_Faculty_Ratio limit 15")

prince <- sqldf("select Type, college_name,`S.F.Ratio`
                       as Student_Faculty_Ratio from college_df
                      where college_name like '%Princeton%'")
formattable(sf_ratio, 
            caption = "Top 15 colleges with a lower Student to Faculty Ratio",
            align = c("l","c","r"))

ggplot(neigh_recent, aes(x = reorder(Neighborhood, count_houses) 
                         , y = count_houses)) +
  geom_segment(aes(x = reorder(Neighborhood, count_houses), 
                   xend = reorder(Neighborhood, count_houses),
                   y = 0, yend = count_houses)) +
  geom_point(size = 4, pch = 21, bg = 4, col = 1)  +
  xlab("Neighborhood")+theme_bw()+
  ylab("No of houses")+
  ggtitle("Which neighborhood has the most number of houses built after 1980?")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))

#Splitting the data into training and testTerminalt:

set.seed(3456)

q2trainIndex <- createDataPartition(college_df$Private, p = 0.7, list = FALSE,
                                  times = 1)
caret_train <- college_df[ trainIndex,]
caret_test <- college_df[-trainIndex,]

#Glm model

install.packages("leaps")
library(leaps)

models_best <- regsubsets(Private~., data = caret_train, 
                          nvmax = 5)

summary(models_best)

model2 <- glm(Private ~ Outstate + F.Undergrad, data = caret_train, 
              family = binomial(link = "logit"))
summary(model2)

#Trainset predictions:

probabilities.train <- predict(model2, newdata = caret_train, type = "response")
predicted.classes.min <- as.factor(ifelse(probabilities.train >= 0.5, "Yes", "No"))

#Model accuracy:

confusionMatrix(predicted.classes.min, caret_train$Private, positive = 'Yes')

#Model accuracy is high = 0.93
#sensitivity = 0.96 = Tue positive rate - the percentage of colleges 
#the model correctly predicted to be private university.

#Specificity = 0.87 = True negative rate -the percentage of colleges 
#the model correctly predicted to be public university. 

#Error rate = 0.0606 = 6.06%

#Accuracy,precision,recall,specificity

accuracy <- (tp+tn)/tp+tn+fp+fn

accuracy <- (381 + 131)/(131+15+18+381)
accuracy

#Precision is defined as positive predicted values
precision <- tp/tp+fp
precision <- 381/(381+18)

precision

#Recall -  it is the proportion of actual positive cases which are correctly identified.

recall <- tp/(tp+fn)
recall <- 381/(381+15)

#Test set predictions;
probabilities.test <- predict(model2, newdata = caret_test, type = "response")
predicted.classes.min <- as.factor(ifelse(probabilities.test >= 0.5, "Yes", "No"))

#Model accuracy:

confusionMatrix(predicted.classes.min, caret_test$Private, positive = 'Yes')

#Accuracy is almost same with both  train and testing data.Hence there is no 
#problem of overfitting

#Roc and Auc
library(pROC)

ROC1 <- roc(caret_test$Private,probabilities.test )

plot(ROC1, col = "blue", ylab = "TP Rate", 
     xlab = "FP Rate")

auc <- auc(ROC1)

#AUC of this model is close to 1.
#this indicates that the model does a very good job of predicting
#whether or not a college is a private university or public univ.

