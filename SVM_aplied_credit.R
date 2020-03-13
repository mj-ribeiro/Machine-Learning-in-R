#---------------------------------------------------------------------------------
#                                 SVM
#---------------------------------------------------------------------------------

setwd("D:/Git projects/ML in R")




library(rpart)
library(rpart.plot)        
library(fBasics)
library(caTools)  # split data
library(caret)    #provide metrics for confusion matrix
library(class)  # knn

#----------  Data


df = read.csv('credit.csv') 

df$clientid = NULL

attach(df)


#---------- Drop na values


m_age = mean(age[age>0 ], na.rm = T)  # average without considering negative values

df$age = ifelse(df$age < 0 |is.na(df$age) , m_age, df$age)




#---------- scaling data


df[ ,1:3] = scale(df[, 1:3])

#---------- Encode class


df$default = factor(df$default, levels = c(0,1))



#--------- Train and test


set.seed(1)
div = sample.split(df$income, SplitRatio = 0.75)
df_train = subset(df, div == TRUE)
df_test = subset(df, div == FALSE)



#------------ Algorithm


library(e1071)

clas = svm(formula= default ~., data=df_train,
           type= 'C-classification', kernel='radial', cost=5)
summary(clas)

prev = predict(clas, newdata = df_test[-4])



#------------ Confusion Matrix

conf_matrix = table(df_test[ ,4], prev)
conf_matrix


confusionMatrix(conf_matrix)














