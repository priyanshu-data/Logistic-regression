library(car)
install.packages("ROCR")
library(ROCR)
# Question 1 ----
"Whether the client has subscribed a term deposit or not "
# Reading the bank data
Bank <- read.csv("D:/Assignment/Logistic Regression/bank-full.csv",sep = ";")
head(Bank)
colnames(Bank)
str(Bank)

# Clean unnecessary columns 
table(Bank$job);table(Bank$marital);table(Bank$education);table(Bank$default);table(Bank$housing);table(Bank$loan);table(Bank$contact);table(Bank$day);table(Bank$campaign);table(Bank$previous);table(Bank$poutcome);table(Bank$y);class(Bank$y)

df_bank <- Bank[,-c(9)]
df_bank$y

# Model 1 __________________________________________________________
model1 <- glm(y~.,data = df_bank,family = "binomial")
summary(model1) # AIC= 22184
pred1 <- predict(model1,df_bank)
plot(pred1,df_bank$y)
plot(pred1)
prob1 <-predict(model1,df_bank,type = "response")
confu1 <- table(prob1>0.5,df_bank$y)
accuracy1 <- sum(diag(confu1))/sum(confu1);accuracy1 # Efficiency of my model is 0.9011303 

# Train and Test Split model
set.seed(101)
Test_Spl <- sample(x = 1:nrow(df_bank),size = round(nrow(df_bank)*(30/100)),replace = F)
Train_1 <- df_bank[-c(Test_Spl),]
Test_1 <- df_bank[c(Test_Spl),]
model2 <- glm(y~.,data = Train_1,family = "binomial")
summary(model2) # AIC = 15393
pred2 <- predict(model2,Test_1)
prob2 <-predict(model2,Test_1,type = "response")
plot(prob2,Test_1$y,
     col=ifelse((prob2<0.5 & Test_1$y =="no")|(prob2>0.5 & Test_1$y =="yes"),"green","red")
     ,pch = ifelse((prob2<0.5 & Test_1$y =="no")|(prob2>0.5 & Test_1$y =="yes"),1,4) # Uncomment it (Ctrl+Shift+C) if you want different character also
)
# In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
plot(pred2,col=ifelse((prob2<0.5 & Test_1$y =="no")|(prob2>0.5 & Test_1$y =="yes"),"green","red"))

confu2 <- table(prob2>0.5,Test_1$y) ;confu2
accuracy2 <- sum(diag(confu2))/sum(confu2);accuracy2 # Efficiency of my model is 0.9005382 


influencePlot(model1)
influencePlot(model2)
influ <-as.integer(intersect(rownames(influencePlot(model1)),rownames(influencePlot(model2))));influ
# ROC Curve
rocrpred<-prediction(prob2,Test_1$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
install.packages("ubun")

str(rocrperf)
rocrperf@x.values
plot(rocrperf,colorize=T) #text.adj=c(-0.2,1.7)

# Model2 _______________________________________________________
df_bank2 <- df_bank[-c(influ),]
model_B_2 <- glm(y~.,data = df_bank2,family = "binomial")
summary(model_B_2) # AIC= 22177
Y_B_2 <- predict(model_B_2,df_bank2)
plot(Y_B_2,df_bank2$y)
plot(Y_B_2)
prob_B_2 <-predict(model_B_2,df_bank2,type = "response")
confu_B_2 <- table(prob_B_2>0.5,df_bank2$y)
effi_B_2 <- sum(diag(confu_B_2))/sum(confu_B_2);effi_B_2 # Efficiency of my model is 0.901148 

# Train and Test Split and Model Evaluation
set.seed(101)
Test_Spl_2 <- as.integer(sample(x = rownames(df_bank2),size = round(nrow(df_bank2)*(30/100)),replace = F))
Train_B_2 <- df_bank2[-c(Test_Spl_2),]
Test_B_2 <- df_bank2[c(Test_Spl_2),]
model_BT_2 <- glm(y~.,data = Train_B_2,family = "binomial")
summary(model_BT_2) # AIC = 15456
Y_BT_2 <- predict(model_BT_2,Test_B_2)
prob_BT_2 <-predict(model_B_2,Test_B_2,type = "response")

plot(prob_BT_2,Test_B_2$y,
     col=ifelse((prob_BT_2<0.5 & Test_B_2$y =="no")|(prob_BT_2>0.5 & Test_B_2$y =="yes"),"green","red")
     #,pch = ifelse((prob_BT_1<0.5 & Test_B_1$y =="no")|(prob_BT_1>0.5 & Test_B_1$y =="yes"),1,4)
)
# In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
plot(Y_BT_2,col=ifelse((prob_BT_2<0.5 & Test_B_2$y =="no")|(prob_BT_2>0.5 & Test_B_2$y =="yes"),"green","red"))

confu_BT_2 <- table(prob_BT_2>0.5,Test_B_2$y) ;confu_BT_2
effi_BT_2 <- sum(diag(confu_BT_2))/sum(confu_BT_2);effi_BT_2 # Efficiency of my model is 0.9005382 


#Cutoff value

str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
