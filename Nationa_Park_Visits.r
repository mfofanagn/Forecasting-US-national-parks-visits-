
if (!require("RCurl")) install.packages("RCurl")
library(RCurl)
URL <- getURL("https://raw.githubusercontent.com/mfofanagn/Forecasting-US-national-parks-visits-/master/park_visits.csv")
visits <- read.csv(text = URL)

str(visits)

# Manage missing and outlear values

colSums(is.na(visits)) 

visits = visits[rowSums(is.na(visits)) == 0, ]
visits = visits[visits$logVisits != 0, ]

str(visits)

# From data structure above, let's transform Month  variable as factor

visits$Month = as.factor(visits$Month)

str(visits)

summary(visits)

library(tidyverse)

head(visits)

visits %>% 
  ggplot(aes(logVisits)) + 
  geom_histogram( bins=30) +
  ggtitle("Distribution of LogVisits variable") +
  labs(subtitle  ="", 
       x="logVisits" , 
       y="population", 
       caption ="source data : visits set") # +
  #theme(panel.border = element_rect(colour="black", fill=NA)) 


#Checking normality

qqnorm(visits$logVisits);qqline(visits$logVisits)

plot(ecdf(visits$logVisits))

# 10 Most visited park

visits %>% group_by(ParkName) %>%
  summarize(n = sum(logVisits)) %>%
  top_n(10, ) %>%
  ggplot(aes(x=reorder(ParkName,n,desc) , y=n)) +
  geom_bar(stat='identity')  +
  labs(x="", y="") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))#+
  #geom_text(aes(label= n), hjust=-0.1, size=3) 
  #labs(title="Top 20 movies title based \n on number of ratings" , caption = "source data: edx set")

#Most visited parkType

visits %>% group_by(ParkType) %>%
  summarize(n = sum(logVisits)) %>%
  ggplot(aes(x=reorder(ParkType,n,desc) , y=n)) +
  geom_bar(stat='identity')  +
  labs(x="", y="") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))#+
  #geom_text(aes(label= n), hjust=-0.1, size=3) 
  #labs(title="Top 20 movies title based \n on number of ratings" , caption = "source data: edx set")

# Most visited region

visits %>% group_by(Region) %>%
  summarize(n = sum(logVisits)) %>%
  ggplot(aes(x=reorder(Region,n,desc) , y=n)) +
  geom_bar(stat='identity')  +
  labs(x="", y="") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

## 20 Most visited state

visits %>% group_by(State) %>%
  summarize(n = sum(logVisits)) %>%
  top_n(20,) %>%
  ggplot(aes(x=reorder(State,n,desc) , y=n)) +
  geom_bar(stat='identity')  +
  labs(x="", y="") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=2.5))

#Most frequents months

visits %>% group_by(Month) %>%
  summarize(n = sum(logVisits)) %>%
  ggplot(aes(x=reorder(Month,n,desc) , y=n)) +
  geom_bar(stat='identity')  +
  labs(x="", y="")# +
  #theme(axis.text.x=element_text(angle=90,hjust=1,vjust=2.5))

##Year with the most  visits

visits %>% group_by(Year) %>%
  summarize(n = sum(logVisits)) %>%
  ggplot(aes(x=reorder(Year,n,desc) , y=n)) +
  geom_bar(stat='identity')  +
  labs(x="", y="")# +
  #theme(axis.text.x=element_text(angle=90,hjust=1,vjust=2.5))

#Let's go in detail in the interesting categorical variable ParkType to analyze visits in each type of park

ggplot(data = visits, aes(x=ParkType, y=logVisits)) + geom_boxplot() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=2.5))

#Numerical relationship between logVisits and Cost

cor(visits$logVisits, visits$cost)

#There is a positive correlation between logVisits and cost, which could mean higher cost has fewer influence on frequentations
# likely because more expensive parks are often more popular due to other features of the parks

# Time serie analysis

ts=ts(visits$logVisits,start=c(2010,1),freq=12)

# Seasonal decomposition

fit <- stl(ts, s.window="period")
plot(fit)

# Data splipting

#We are interested in predicting the log visits, let’s subset our dataset into a training and a testing set 
#by splitting based on the year: 
# training would contain 2010-2014 years of data, 
# and testing would be 2015-2016 data.


training = subset(visits, Year >= 2010 & Year <= 2014)
dim(training)

testing = subset(visits, Year >= 2015 & Year <= 2016)
dim(testing)

# Let's start by a simple linear model
# we will use those independants variable
# laglogVisits, laglogVisitsYear, Year, Month, Region, ParkType, and cost

visitsLM = lm(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data = training)
summary(visitsLM)

# Predict test data
# we will metrics R square to evlauate our models

 LM.pred = predict(visitsLM, newdata=testing)
 LM.sse = sum((LM.pred - testing$logVisits)^2)
 LM.ssm = sum((LM.pred - mean(training$logVisits)) ^ 2) 

 R2.LM =  1 - LM.sse / LM.ssm
 print(R2.LM)

# Residuals plot

hist((LM.pred - testing$logVisits),breaks = 50)

# In addition to the logistic regression model, we can also train a regression tree. 
# Use the same set of variables as the previous problem 
# (laglogVisits, laglogVisitsYear, Year, Month, Region, ParkType, and cost), train a regression tree with cp = 0.05

install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

visitsTree = rpart(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data = training, cp=0.05)
prp(visitsTree)


# Evaluate Regression tree on test data set

tree.pred = predict(visitsTree, newdata=testing)
tree.sse = sum((tree.pred - testing$logVisits)^2)
tree.ssm = sum((tree.pred - mean(training$logVisits))^2) 

R2.tree = 1 - tree.sse / tree.ssm
print(R2.tree)

# Tree model performs less well than linear regession
# let's use tree with cross validation

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

 set.seed(201)
 tr.control = trainControl(method = "cv", number = 10)
 numFolds = trainControl( method = "cv", number = 10 )
 cpGrid = expand.grid( .cp = seq(0.0001,0.005,0.0001)) 
 train(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data = training, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

#Final Regression Tree
#let's re-run the regression tree on the training data, 
#now using the cp value equal to the one selected in the previous problem cp = 1e-04.


visitsTree = rpart(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data = training, cp = 1e-04)

# Let's evaluate tree model with cross validation

tree.pred2 = predict(visitsTree, newdata=testing)
tree.sse = sum((tree.pred - testing$logVisits)^2)
tree.sse = sum((tree.pred2 - testing$logVisits)^2)
tree.ssm = sum((testing$logVisits - mean(training$logVisits))^2)
R2.tree = 1 - tree.sse / tree.ssm
print(R2.tree)

# big improvement noticed

#Random Forest
#We can potentially further improve the models by using a random forest.
# we train a random forest model with the same set of covariates, 
# and using just default parameters 
#This may take a few minutes.


install.packages("randomForest")
library(randomForest)

#Model building and evaluated

set.seed(201)
RandonForest = randomForest(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data = training) 
Forest.pred3 = predict(RandonForest, newdata=testing)
Forest.sse = sum((Forest.pred3 - testing$logVisits)^2)
#Forest.ssm = sum((testing$logVisits - mean(visits$logVisits))^2)
Forest.ssm = sum((Forest.pred3 - mean(training$logVisits))^2)
R2.Forest = 1 - Forest.sse / Forest.ssm
print(R2.Forest)

# new improvements
