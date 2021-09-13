library(plyr)
library(gmodels)
library(ggplot2)
library(dplyr)
library(corrplot)
library(ROSE)
library(pROC)
library(ROCR)
library(e1071)
library(Metrics)

cs_data <- read.csv('C:/Users/prath/Desktop/DAPA/dataset/train - Copy.csv')


par(mfrow=c(2,2))
boxplot(cs_data$Region_Code, main = "Region code Plot")
boxplot(cs_data$Policy_Sales_Channel, main = "Policy channel Plot")
boxplot(cs_data$Annual_Premium, main = "Annual Premium Plot")
boxplot(cs_data$Vintage, main = "Vintage Plot")

cs_data$Gender <- as.factor(cs_data$Gender)
cs_data$Driving_License <- as.factor(cs_data$Driving_License )
cs_data$Previously_Insured <- as.factor(cs_data$Previously_Insured)
cs_data$Vehicle_Age <- as.factor(cs_data$Vehicle_Age)
cs_data$Vehicle_Damage <- as.factor(cs_data$Vehicle_Damage)
cs_data$Response <- as.factor(cs_data$Response)
#cs_data$Gender <- as.factor(cs_data$Gender)
#cs_data$Gender <- as.factor(cs_data$Gender)
#cs_data$Gender <- as.factor(cs_data$Gender)

par(mfrow=c(2,2))
boxplot(cs_data$Region_Code, main = "Bedroom Plot")
boxplot(cs_data$Vehicle_Damage, main = "Bathroom Plot")
boxplot(cs_data$Annual_Premium, main = "Living SqFt Plot")
boxplot(cs_data$Vintage, main = "Above SqFt Plot")







dx = sort(sample(nrow(cs_data), nrow(cs_data)*.7))
train_cs <- cs_data[dx,];
test_cs  <- cs_data[-dx,];
#test_cs <- select(test_cs, -Response)

dim(train_cs)
dim(test_cs)


#LR

#library(ISLR)
#CS_glm <- glm(Response ~., family=binomial(link='logit'),data=train_cs)
#summary(CS_glm)

#prediction <- predict(CS_glm, newdata=test_cs )

#prediction[prediction < 0.5] <- 0
#prediction[prediction > 0.5] <- 1

#prediction <- as.factor(prediction)

#library(caret)

#accuracy <- confusionMatrix(table(test_cs$Response , prediction))
#accuracy



#randomforest
library(randomForest)
rf_model_CS <- randomForest(Response ~ .,
                            data = train_cs, 
                            ntree = 101,
                            replace = TRUE,
                            nodesize = 9,
                            importance = TRUE); print(rf_model_CS)


prediction_rf_cs <- predict(rf_model_CS, test_cs)

library(caret)
accuracy_rf_cs <- confusionMatrix(prediction_rf_cs, test_cs$Response)
accuracy_rf_cs


varImpPlot(rf_model_CS)



##Ploting ROC curve and AUC for test and train set
