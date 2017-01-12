#map working directory
path <- "C:/Users/yudhisthirs/Desktop/DataCamp_Material/10.Projects/6. Who will leave"

#set working directory
setwd(path)


#Load data
hr <- read.csv("HR_comma_sep.csv",stringsAsFactors=FALSE)

#backup the org dataset
hr <- hr


#intialise diff libraries
library(ggplot2)
library(caret)
                
                #####################################
                ##### Step1-Univariate Analysis######
                #####################################

head(hr)

# Let's start by looking at the data type of each column
str(hr)

#Let's separate continuous & categorical var's for univariate analysis
hr_cont <- subset(hr,select = c(satisfaction_level,last_evaluation))

#Let's convert Factor into catogory
#convert character to factors for sales & salary variable

hr$sales[hr$sales == "accounting"] <- "1"
hr$sales[hr$sales == "hr"] <- "2"
hr$sales[hr$sales == "IT"] <- "3"
hr$sales[hr$sales == "management"] <- "4"
hr$sales[hr$sales == "marketing"] <- "5"
hr$sales[hr$sales == "product_mng"] <- "6"
hr$sales[hr$sales == "RandD"] <- "7"
hr$sales[hr$sales == "sales"] <- "8"
hr$sales[hr$sales == "support"] <- "9"
hr$sales[hr$sales == "technical"] <- "10"

hr$salary[hr$salary == "low"] <- "1"
hr$salary[hr$salary == "medium"] <- "2"
hr$salary[hr$salary == "high"] <- "3"

#converting character to int
hr$sales <- as.integer(hr$sales)
hr$salary <- as.integer(hr$salary)


hr_cat <- subset(hr,select = c(number_project,average_montly_hours,time_spend_company,Work_accident,left,promotion_last_5years,sales,salary))

# Installing "psych" package to get summary statistics
#install.packages ("psych")
library(psych)

describe(hr_cont)

#2. Categorical Variables:In case of categorical variables, 
#we generally use frequency table to understand distribution of each category. 
#It can be measured using two metrics, Count and Count% against each category.
#Before checking the count of categories, 
#lets check the number of unique values in each categorical variable.

apply(hr_cat,2,function(x){length(unique(x))})


#A.Analysis of left variable
ggplot(hr, aes(x = left)) + geom_bar()
table(hr_cat$left)
as.matrix((prop.table(table(hr_cat$left))))
#23% employee's has left the organisation out of 100%


#B.Analysis of number_project variable
ggplot(hr, aes(x = number_project)) + geom_bar()
table(hr_cat$number_project)
as.matrix((prop.table(table(hr_cat$number_project))))
ggplot(hr, aes(x = left)) + geom_bar()
#29% of population has worked on 4 projects, followed by 27% pop on 3 projects, and so on

#C.Analysis of time_spend_company variable
ggplot(hr, aes(x = time_spend_company)) + geom_bar()
table(hr_cat$time_spend_company)
as.matrix((prop.table(table(hr_cat$time_spend_company))))
#Maximum Emploee's use to stay with org till 3 yrs after that the attrition rate is increasing yr on yr

#D.Analysis of Work_accident variable
ggplot(hr, aes(x = Work_accident)) + geom_bar()
table(hr_cat$Work_accident)
as.matrix((prop.table(table(hr_cat$Work_accident))))
#14% of the employees  had a work accident


#D.Analysis of promotion_last_5years variable
ggplot(hr, aes(x = promotion_last_5years)) + geom_bar()
table(hr_cat$promotion_last_5years)
as.matrix((prop.table(table(hr_cat$promotion_last_5years))))
#Company has pramotated only 2% of Employee's in last 5 yr,Strange!


#E.Analysis of sales(department) variable
ggplot(hr, aes(x = sales)) + geom_bar()
table(hr_cat$sales)
as.matrix((prop.table(table(hr_cat$sales))))
#Employee's from "Sales-dept" are on top of the list who leaves the org followed by tech-dept


#F.Analysis of salary variable
ggplot(hr, aes(x = salary)) + geom_bar()
table(hr_cat$salary)
as.matrix((prop.table(table(hr_cat$salary))))
#Employee's who are getting Less Salary are top of the list of leaving org,As Expected!




                #####################################
                ## Step2- Multivariate Analysis######
                #####################################


#Multivariate Analysis finds out the relationship between two or more variables. Here, we look for association and disassociation between variables at a pre-defined significance level.
#The type of visualization technique to use depends on the type variable. Thus, there can be 3 combinations of the 2 types of variables:
#categorical - categorical
#continuous - continuous
#categorical - continuous

#1. Both Categorical:
# We need to install "gmodels"
#install.packages("gmodels")
library(gmodels)

#number_project vs left
bartable = table(hr$left,hr$number_project) 
barplot(bartable, beside=TRUE, ylim=c(0, 15000),col=c("green", "red"),legend.text=TRUE, xlab="number_project", ylab="number_employee's",main="number_project vs left")
CrossTable(hr$number_project,hr$left,digits=2,prop.r=FALSE, prop.c=TRUE,prop.t=FALSE,format=c("SAS"))
# As you can see in freq table out of 3571 employee's 1567 emp. (44%) who worked on 2 projects 
#has the highest % of leaving org.


#average_montly_hours vs left
#First lets do Label Encoding for average_montly_hours variable
#convert average_montly_hours to numeric
hr$average_montly_hours <- as.numeric(hr$average_montly_hours)

hr$average_montly_hours[findInterval(hr$average_montly_hours, c(90,120)) == 1L] <- 1
hr$average_montly_hours[findInterval(hr$average_montly_hours, c(120,150)) == 1L] <- 2
hr$average_montly_hours[findInterval(hr$average_montly_hours, c(150,180)) == 1L] <- 3
hr$average_montly_hours[findInterval(hr$average_montly_hours, c(180,210)) == 1L] <- 4
hr$average_montly_hours[findInterval(hr$average_montly_hours, c(210,240)) == 1L] <- 5
hr$average_montly_hours[findInterval(hr$average_montly_hours, c(240,270)) == 1L] <- 6
hr$average_montly_hours[findInterval(hr$average_montly_hours, c(270,300)) == 1L] <- 7
hr$average_montly_hours[findInterval(hr$average_montly_hours, c(300,330)) == 1L] <- 8

bartable = table(hr$left,hr$average_montly_hours) 
barplot(bartable, beside=TRUE, ylim=c(0, 15000),col=c("green", "red"),legend.text=TRUE, xlab="average_montly_hours", ylab="number_employee's",main="average_montly_hours vs left")
CrossTable(hr$average_montly_hours,hr$left,digits=2,prop.r=FALSE, prop.c=TRUE,prop.t=FALSE,format=c("SAS"))
# As we can see there is small trend in this variable, as we can understand empl
#doesn't want to spend more tym in ofc...So as the avg_time spent is increasing
#there is high chance that the person will think for switch

#Using same code we can find the insights for remaining var's.....


#3.Categorical-Continuous Combination
#satisfaction_level vs Left
boxplot(hr$satisfaction_level ~ hr$left,xlab='left organisation',ylab='satisfaction_level')
#Ok, so this isn't so surprising, the satisfaction levels of those who left varies greatly while the median of that group is about 0.4. Of those who stuck around, 
#the median is about 0.7, a notable difference.

#last_evaluation vs Left
boxplot(hr$last_evaluation ~ hr$left,xlab='left organisation',ylab='last_evaluation')
#This strange, ppl who rated highly in last appraisal are higher chance of leaving
#might be they are more confident about getting job outside





                #####################################
                ### Step3-Missing Value Treatment####
                #####################################
#1. Checking missing values

#Checking no. of missing in complete dataset
table(is.na(hr))
#data don't hv any missing obs


                
                #####################################
                ###   Step4-Outlier Treatment    ####
                #####################################

boxplot(hr$satisfaction_level, horizontal = TRUE)
#no outliers

boxplot(hr$last_evaluation, horizontal = TRUE)
#no outliers

boxplot(hr$number_project, horizontal = TRUE)
#no outliers

boxplot(hr$average_montly_hours, horizontal = TRUE)
#no outliers

boxplot(hr$time_spend_company, horizontal = TRUE)
#we can see many observations(1280) are far away from Upeer Quartile range.
#Now let's remove those 
#3ways to deal
#1. Imputation
#2. Capping
#3. Prediction

# here I am using first method
summary(hr$time_spend_company)
#imputing outliers with median
hr$time_spend_company <- ifelse(hr$time_spend_company > 5,median(hr$time_spend_company), hr$time_spend_company)
max(hr$time_spend_company)


                
                #####################################
                ### Step5-Feature Engineering ###
                #####################################
#Not required

                
                #####################################
                ### Step6-  Predictive Modeling   ###
                #####################################





#Before building any model just split the data into train and test
set.seed(123)
rows <- sample(nrow(hr))
hr <- hr[rows, ]
split <- round(nrow(hr) * .70)


# Create train
train <- hr[1:split, ]

#check whether levels in left variable are equally distributed or not
prop.table(table(train$left))

# Create test
test <- hr[(split + 1):nrow(hr), ]
prop.table(table(test$left))




#####################################
#Fitting a logistic regression model#
#####################################



# Fit glm model
model_logistic <- glm(left ~ ., family="binomial",train)


# Predict on test
p_logistic <- predict(model_logistic,test,type="response")


# Calculate class probabilities: 
p_class_logistic <- ifelse(p_logistic > 0.50,"1","0")


# Create confusion matrix
confusionMatrix(p_class_logistic, test$left)
#at 0.5 cutoff we got ~82% accuracy,voila!

#Try another threshold
p_class_logistic <- ifelse(p_logistic > 0.60,"1","0")
confusionMatrix(p_class_logistic, test$left)
#Accuracy is 77%


p_class_logistic <- ifelse(p_logistic > 0.80,"1","0")
confusionMatrix(p_class_logistic, test$left)
#Accuracy is 75%


p_class_logistic <- ifelse(p_logistic > 0.40,"1","0")
confusionMatrix(p_class_logistic, test$left)
#Accuracy : 0.8318 Sensitivity : 0.8878 Specificity : 0.6559 


p_class_logistic <- ifelse(p_logistic > 0.30,"1","0")
confusionMatrix(p_class_logistic, test$left)
#Accuracy : 0.8318 Sensitivity : 0.8324    Specificity : 0.8280 
#The conclusion is at 0.30 cut-off we are getting good results


# Make ROC curve
#install.packages('caTools')
library(caTools)
colAUC(p_logistic, test$left, plotROC = TRUE)

#Further we can do AUC based prunning, here I haven't done that




#####################################
#Fitting a Decision Tree model      #
#####################################



library(rpart)
fit_default <- rpart(left ~ ., method = "class",data = train)
plot(fit_default,uniform=TRUE)
text(fit_default,use.n=TRUE)


library(rpart.plot)
prp(fit_default,extra = 1)


#Making predictions using the decision tree
p_dt <-  predict(fit_default, newdata = test,type ="class")
#or
#pred_undersample = predict(ptree_undersample, newdata = test_set)

#Constructing a confusion matrix
confusionMatrix(p_dt, test$left)
#Accuracy : 0.9538   Sensitivity : 0.9801 Specificity : 0.8712  
#As expected see the above three evaluation parameters, DT outperforms over GLM




#####################################
#Fitting a Random Forest model      #
#####################################



library(randomForest)

model <- randomForest(left ~ . , data = train)
model


p_rf <- predict(model, newdata = test)
p_class_rf <- ifelse(p_rf > 0.30,"1","0")


confusionMatrix(p_class_rf, test$left)
#Accuracy : 0.9824 Sensitivity : 0.9859 Specificity : 0.9715
#RF outperforms over DT & GLM