#***********************************************************************************
#                             Cross validation
#***********************************************************************************

setwd("D:/Git projects/ML in R")



library(rpart)
library(rpart.plot)        
library(fBasics)
library(caTools)  # split data
library(caret)    #provide metrics for confusion matrix



# data


df = read.csv('credit.csv') 

df$clientid = NULL

attach(df)


# drop na values

m_age = mean(age[age>0 ], na.rm = T)  # average without considering negative values

df$age = ifelse(df$age < 0 |is.na(df$age) , m_age, df$age)




# scaling data

df[, 1:3] = scale(df[, 1:3])

# Encode class

df$default = factor(df$default, levels = c(0,1))



#*******************************************************************************
#                               Algoritms
#*******************************************************************************



#---- Algorithm Naive Bayes

control_train = trainControl(method = 'cv', number = 10)    # ten fold
model1 = train(default ~., data=df, trainControl = control_train, method='nb') # nb = Naive Bayes

model1


#---- Algorithm random forests


rm(list = c('control_train', 'model1'))


control_train = trainControl(method = 'cv', number = 10)    # ten fold
model2 = train(default ~., data=df, trControl = control_train, method='rf') 

model2


saveRDS(model2, 'rf.RDS') # Save model



#---- Algorithm Logit



control_train = trainControl(method = 'repeatedcv', number = 10, repeats = 2)    # ten fold
model3 = train(default ~., data=df, trControl = control_train, method='glm', family='binomial') 



#-------- Algorithm KNN




control_train = trainControl(method = 'cv', number = 10)    # ten fold
model4 = train(default ~., data=df, trControl = control_train, method='knn') 





#-------- Algorithm Neural Networks (Multi layer Perceptron)



control_train = trainControl(method = 'cv', number = 10)    # ten fold
model4 = train(default ~., data=df, trControl = control_train, method='mlp') 

model4

saveRDS(model4, 'NN.RDS')


#-------- Algorithm SVM Radial


control_train = trainControl(method = 'cv', number = 10)    # ten fold
model5 = train(default ~., data=df, trControl = control_train, method='svmRadial') 



#------------ predict using model


age = c(1.3, 1.43, 0.32, 	-1.68605374)
income = c(1.2, 4, -0.1, 1.509185756)
loan = c(1.2, -1, 0.43, 	1.42040957)

t1 = rbind(income, age, loan)

prev = predict(model3, t1)

prev

#--------- Get accuracy of 10 models 
# SVM

svm_a = c()

for(i in 1:30){
  control_train = trainControl(method = 'cv', number = 10)    # ten fold
  model5 = train(default ~., data=df, trControl = control_train, method='svmRadial') 

  res = model5$results$Accuracy[3]
  cat('SVM Accuracy is:', gsub('[.]', ',', res))
  cat('\n')
  svm_a = c(svm_a, res)
}


# Naive Bayes

nb_a = c()

for(i in 1:10){
  control_train = trainControl(method = 'cv', number = 10)    # ten fold
  model5 = train(default ~., data=df, trControl = control_train, method='nb') 
  
  res = model5$results$Accuracy[2]
  cat('Naive Bayes Accuracy is:', gsub('[.]', ',', res))   #gsub replace . by ,
  cat('\n')
  nb_a = c(svm_a, res)
}







# Neural Net

neural_a = c()

for(i in 1:10){
  control_train = trainControl(method = 'cv', number = 10)    # ten fold
  model5 = train(default ~., data=df, trControl = control_train, method='mlp') 
  
  res = model5$results$Accuracy[2]
  cat('Neural Net Accuracy is:', gsub('[.]', ',', res))    
  cat('\n')
  neural_a = c(neural_a, res)
}




# Random Forests


rf_a = c()

for(i in 1:10){
  control_train = trainControl(method = 'cv', number = 10)    # ten fold
  model5 = train(default ~., data=df, trControl = control_train, method='rf') 
  
  res = model5$results$Accuracy[2]
  cat('Random Forests Accuracy is:', gsub('[.]', ',', res))    
  cat('\n')
  rf_a = c(rf_a, res)
}









