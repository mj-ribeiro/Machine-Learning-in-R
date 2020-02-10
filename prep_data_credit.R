setwd("D:/Git projects/ML in R")

library(fBasics)
library(e1071)
library(caTools)



df = read.csv('credit.csv') 

df$clientid = NULL

attach(df)

#------------------------------ SUMMARY DATA

summary(df)


### Other way to summarize data

library(fBasics)
basicStats(df)


# there are negative values in age

f1 = age<0 | is.na(age)


table(f1)   #there are three values less than three and three NA's



# see values

df[age <0 & !is.na(age),  ]


##--- Drop this values

# first aproach

#df = df[df$age>0, ]


# second aproach

mean(age, na.rm=T)
m_age = mean(age[age>0 ], na.rm = T)  # average without considering negative values


# changing negative values and NA's

if (df$age<0){          # this command doesn't work
  df$age = df$age*(-1)  # if and else doesn't commands vectorized
}else{
  df$age = df$age
}

# Other way is use the ifelse

df$age = ifelse(df$age<0 | is.na(df$age), m_age, df$age)

attach(df)

summary(df$age)
basicStats(df$age)

#---------------------------------- SCALING DATA


df[ , 1:3] = scale(df[ , 1:3])  #padronization (score Z)


#--------------------------------- GRAPHS


library(ggplot2)


g1 = ggplot(data=df, aes(x=loan, y=income))

g1+ geom_point(colour='blue')



#--------------------- TRAIN & TEST

#install.packages('caTools')


set.seed(1)

div = sample.split(df$default, SplitRatio = 0.75)

df_train = subset(df, div == T)
df_test = subset(df, div == F)
















