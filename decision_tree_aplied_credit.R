#---------------------------------------------------------------------------------
#                             DECISION TREES
#---------------------------------------------------------------------------------

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


pie(table(df$default), radius = 1)
text(locator(n=1),
     paste(round(prop.table(table(df$default))[1],
                 digits=2)*100,"%"))
text(locator(n=1),                                    # esse locator permite escolher o lugar da figura 
     paste(round(prop.table(table(df$default))[2],   # que vc quer escrever
                 digits=2)*100,"%"))


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




#---- algorithm

clas = rpart(formula = default~., data = df_train)

rpart.plot(clas)

#prev = predict(clas, newdata = df_test[-4]) # get the probability

prev = predict(clas, newdata = df_test[-4], type='class') # get the class


#---- confusion matrix


conf_matrix = table(df_test[ , 4], prev)

conf_matrix

confusionMatrix(conf_matrix)





















