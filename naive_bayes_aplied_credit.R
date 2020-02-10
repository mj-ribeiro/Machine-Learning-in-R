#--------------------------------------------------------------------------------
#                               NAIVE BAYES
#--------------------------------------------------------------------------------


setwd("D:/Git projects/ML in R")

# libraries

library(fBasics)
library(e1071)
library(caTools)
library(caret)  #provide metrics for confusion matrix



# read data

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



# train and test

set.seed(1)
div = sample.split(df$income, SplitRatio = 0.75)
df_train = subset(df, div == TRUE)
df_test = subset(df, div == FALSE)




# algorith

# train 

clas = naiveBayes(x= df_train[-4], y= df_train$default)
print(clas)


# test

prev = predict(clas, newdata = df_test[-4])


comp = prev == df_test$default
summary(comp)         # hits = 468, erros=32



# confusion matrix


conf_matrix = table(df_test$default, prev)

conf_matrix    

confusionMatrix(conf_matrix)







