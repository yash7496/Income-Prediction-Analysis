library(faraway)
#Five-Number Summary
summary(Income)

#Workclass combining
Income$workclass <- as.character(Income$workclass)

Income$workclass[Income$workclass == "Without-pay" | 
                   Income$workclass == "Never-worked"] <- "Unwaged"

Income$workclass[Income$workclass == "State-gov" |
                   Income$workclass == "Local-gov"] <- "Government"

Income$workclass[Income$workclass == "Self-emp-inc" |
                   Income$workclass == "Self-emp-not-inc"] <- "Self-employed"
table(Income$workclass)

#Marital Status Combining
Income$marital.status <- as.character(Income$marital.status)
Income$marital.status[Income$marital.status == "Married-civ-spouse" |
                        Income$marital.status == "Married-spouse-absent"] <- "Married"


Income$marital.status[Income$marital.status == "Divorced" |
                        Income$marital.status == "Separated" |
                        Income$marital.status == "Widowed"] <- "Not-Married"

table(Income$marital.status)

#Native Country Combining
Income$native.country <- as.character(Income$native.country)
North_America <- c("Canada", "Cuba", "Dominican-Republic", "El-Salvador", "Guatemala",
                   "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
                   "Outlying-US(Guam-USVI-etc)", "Puerto-Rico", "Trinadad&Tobago",
                   "United-States")
Asia <- c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
          "Philippines", "Taiwan", "Thailand", "Vietnam")
South_America <- c("Columbia", "Ecuador", "Peru")

Europe <- c("England", "France", "Germany", "Greece", "Holand-Netherlands",
            "Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland",
            "Yugoslavia")
Other <- c("South", "?")

Income$native.country[Income$native.country %in% North_America] <- "North America"
Income$native.country[Income$native.country %in% Asia] <- "Asia"
Income$native.country[Income$native.country %in% South_America] <- "South America"
Income$native.country[Income$native.country %in% Europe] <- "Europe"
Income$native.country[Income$native.country %in% Other] <- "Other"

table(Income$native.country)

Income$native.country <- as.factor(Income$native.country)
Income$marital.status <- as.factor(Income$marital.status)
Income$workclass <- as.factor(Income$workclass)
str(Income)

Income[Income == "?"] <- NA
table(Income$workclass)
summary(Income)

Income[Income == "?"] <- NA
table(Income$marital.status)


# remove all the "NA" cases
Income=na.exclude(Income)
summary(Income)

#Checking for outliers

plot(Income$age,ylab="Age")

#removing Outliers from Age column

outliers_index=which(Income$age>80)
Income=Income[-outliers_index,]


#Histogram

library(ggplot2)
ggplot(Income, aes(age)) + geom_histogram(aes(fill = income), color = "blue",
                                         binwidth = 3)
#Here the coloring is indicative of percentage. From this plot we can see that the percentage of people who make above 50K peaks out at roughly 35% between ages 30 and 50. 

#Bar
#To determine Income of Native Countries.

library(ggplot2)
ggplot(data = Income) +
  aes(x = native.country, fill = income) +
  geom_bar(position = "dodge") +
  labs(title = "Income of Native Countries",
       x = "Native Country",
       y = "Number of people") +
  theme_dark()

#Bar
#To determine Income of Workclass.
ggplot(data = Income) +
  aes(x = workclass, fill = income) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Income of Workclass",
       x = "Workclass",
       y = "Number of People") +
  theme_minimal()

#Bar
#To determine Income of Marital Status.

ggplot(data = Income) +
  aes(x = marital.status, fill = income) +
  geom_bar(position = "dodge") +
  labs(title = "Income of Marital Status ",
       x = "Marital Status") +
  theme_grey()

#Boxplot
#To determine hours perweek people work in the Native country.
ggplot(data = Income) +
  aes(x = native.country, y = hours.per.week, fill = marital.status) +
  geom_boxplot() +
  labs(title = "Native Country vs Hours per week",
       x = "Native Country",
       y = "Hours per week") +
  theme_linedraw()

#Boxplot
ggplot(data = Income) +
  aes(x = gender, y = hours.per.week, fill = native.country) +
  geom_boxplot() +
  labs(title = "Gender vs Hours per week for all native countries",
       x = "Gender",
       y = "Hours per week") +
  theme_minimal()


#stripchart
r=stripchart(Income$hours.per.week,
             method = "jitter", 
             col = "orange", 
             pch=1,
             main="Stripcharts",
             xlab="Number of hours per week")
r

#Density Plot
m <- mean(Income$hours.per.week, na.rm = TRUE) 
std <- sd(Income$hours.per.week, na.rm = TRUE) 
plot(density(Income$hours.per.week),   
     main = "Density plot") 
curve(dnorm(x, mean=m, sd=std),   
      col="darkblue", 
      xlab="Hours per week",
      lwd=0.25,    add=TRUE)

#Skewness

library(moments)
skewness(Income$age)
skewness(Income$fnlwgt)                                         
skewness(Income$educational.num)
skewness(Income$capital.gain)
skewness(Income$capital.loss)
skewness(Income$hours.per.week)

#Out of the 6 variables skewness of 5 variables values implies that the distribution of the data are slightly skewed to the right or posit ively skewed.They are skewed to the right because the computed values are positive, because the values are greater than zero.
#The skewness of 1 variable implies that the distribution of the data are slightly skewed to the left or negatively skewed.They are skewed to the left because the computed values are negative, because the values are lesser than zero.
summary(Income)

#Linear Model
set.seed(100)  
trainingRowIndex <- sample(1:nrow(Income), 0.8*nrow(Income))  
train <- Income[trainingRowIndex, ]  
test  <- Income[-trainingRowIndex, ] 

linear_model <- glm(income ~ ., family = binomial(), train)
summary(linear_model)
steplinear <- step(linear_model)

par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

plot(steplinear)

#Prediction
Prediction_Analysis <- predict(linear_model, test, type = "response")
table(test$income, Prediction_Analysis >= 0.5)
AUC_model=auc(test$income,Prediction_Analysis)
AUC_model
#Accuracy = TP+TN/TP+FP+TN+FN
(6326 + 1406) / (6326+517+934+1406)

#Sensitivity=TP/TP+FN
(1406)/(1406+934)

#Specificity=TN/TN+FP
(6326)/(6326+517)

#Logistic Regression
summary(Income)
Income$workclass <- factor(Income$workclass)
mylogit <- glm(income ~ workclass+education+native.country, data = Income, family = "binomial")
summary(mylogit)

# Correlation coefficient
cor(Income$age, Income$educational.num)
#It is a strong and positive correlation.

log_model <-glm(income ~age+fnlwgt+educational.num+capital.gain+capital.loss+hours.per.week,
family="binomial",data=Income)
summary(log_model)

library(pROC)
Prediction_Analysis1 <- predict(log_model, test, type = "response")
Prediction_Analysis1

AUC_model1=auc(test$income,Prediction_Analysis1)
AUC_model1

log_model2 <-glm(income ~age+fnlwgt+educational.num+capital.gain+capital.loss,
                 family="binomial",data=Income)
summary(log_model2)

Prediction_Analysis2 <- predict(log_model2, test, type = "response")

AUC_model2=auc(test$income,Prediction_Analysis2)
AUC_model2

log_model3 <-glm(income ~age+fnlwgt+educational.num+capital.gain,
                 family="binomial",data=Income)
summary(log_model3)

Prediction_Analysis3 <- predict(log_model3, test, type = "response")

AUC_model3=auc(test$income,Prediction_Analysis3)
AUC_model3

log_model4 <-glm(income ~age+fnlwgt+educational.num,
                 family="binomial",data=Income)
summary(log_model4)

Prediction_Analysis4 <- predict(log_model4, test, type = "response")

AUC_model4=auc(test$income,Prediction_Analysis4)
AUC_model4



