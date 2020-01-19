setwd("D:/Git projects/ML in R")

df = read.csv('credit.csv') 

attach(df)

summary(df)


### Other way to summarize data

library(fBasics)
basicStats(df)