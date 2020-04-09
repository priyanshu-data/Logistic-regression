creditcard <- read.csv(file.choose()) # Choose the claimants Data set
sum(is.na(creditcard))
creditcard <- na.omit(creditcard) # Omitting NA values from the Data
View(creditcard)
creditcard <- creditcard[,-1] # Removing the first column which is is an Index
View(creditcard)
colnames(creditcard)
summary(creditcard)

# Preparing a linear regression 
mod_lm <- lm(card~.,data=creditcard)
pred1 <- predict(mod_lm,creditcard)
pred1
plot(creditcard$card,pred1)

# We can no way use the linear regression technique to classify the data
plot(pred1)

# We can also include NA values but where ever it finds NA value
# probability values obtained using the glm will also be NA 
# So they can be either filled using imputation technique or
# exlclude those values 

# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(card~.,data=creditcard,family = "binomial")

# To calculate the odds ratio manually we are going to take exp of coef(model)
exp(coef(model))
# Confusion matrix table 
prob <- predict(model,creditcard,type="response")
summary(model) # AIC: 13360

# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,creditcard$card)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 85.97

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
creditcard[,"prob"] <- prob
creditcard[,"pred_values"] <- pred_values
creditcard[,"yes_no"] <- yes_no

View(creditcard[,c(1,13:15)])

table(creditcard$card,creditcard$pred_values)

# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob,creditcard$card)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

# More area under the ROC Curve better is the logistic regression model obtained
## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
