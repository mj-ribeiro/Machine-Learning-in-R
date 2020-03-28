#***********************************************************************************
#                             Neural Networks
#***********************************************************************************


library(rpart)
library(rpart.plot)        
library(fBasics)
library(caTools)  # split data
library(caret)    #provide metrics for confusion matrix



#-------------- Read data

base = read.csv('census.csv') 

base$X = NULL



attach(base)


#------------------ CATEGORIZING DATA


base$sex = as.numeric(factor(base$sex, levels = unique(base$sex), labels = c(1, 0)))

base$workclass = as.numeric(factor(base$workclass, levels = c(' Federal-gov', ' Local-gov', ' Private', ' Self-emp-inc', ' Self-emp-not-inc', ' State-gov', ' Without-pay'), labels = c(1, 2, 3, 4, 5, 6, 7)))
base$education = as.numeric(factor(base$education, levels = c(' 10th', ' 11th', ' 12th', ' 1st-4th', ' 5th-6th', ' 7th-8th', ' 9th', ' Assoc-acdm', ' Assoc-voc', ' Bachelors', ' Doctorate', ' HS-grad', ' Masters', ' Preschool', ' Prof-school', ' Some-college'), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)))
base$marital.status = as.numeric(factor(base$marital.status, levels = c(' Divorced', ' Married-AF-spouse', ' Married-civ-spouse', ' Married-spouse-absent', ' Never-married', ' Separated', ' Widowed'), labels = c(1, 2, 3, 4, 5, 6, 7)))
base$occupation = as.numeric(factor(base$occupation, levels = c(' Adm-clerical', ' Armed-Forces', ' Craft-repair', ' Exec-managerial', ' Farming-fishing', ' Handlers-cleaners', ' Machine-op-inspct', ' Other-service', ' Priv-house-serv', ' Prof-specialty', ' Protective-serv', ' Sales', ' Tech-support', ' Transport-moving'), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)))
base$relationship = as.numeric(factor(base$relationship, levels = c(' Husband', ' Not-in-family', ' Other-relative', ' Own-child', ' Unmarried', ' Wife'), labels = c(1, 2, 3, 4, 5, 6)))
base$race = as.numeric(factor(base$race, levels = c(' Amer-Indian-Eskimo', ' Asian-Pac-Islander', ' Black', ' Other', ' White'), labels = c(1, 2, 3, 4, 5)))
base$native.country = as.numeric(factor(base$native.country, levels = c(' Cambodia', ' Canada', ' China', ' Columbia', ' Cuba', ' Dominican-Republic', ' Ecuador', ' El-Salvador', ' England', ' France', ' Germany', ' Greece', ' Guatemala', ' Haiti', ' Holand-Netherlands', ' Honduras', ' Hong', ' Hungary', ' India', ' Iran', ' Ireland', ' Italy', ' Jamaica', ' Japan', ' Laos', ' Mexico', ' Nicaragua', ' Outlying-US(Guam-USVI-etc)', ' Peru', ' Philippines', ' Poland', ' Portugal', ' Puerto-Rico', ' Scotland', ' South', ' Taiwan', ' Thailand', ' Trinadad&Tobago', ' United-States', ' Vietnam', ' Yugoslavia'), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41)))
#base$income = as.numeric(factor(base$income, levels = c(' <=50K', ' >50K'), labels = c(0, 1)))


#---------------- SCALING


base[ , 1] = as.numeric(scale(base[ , 1]))

base[ , 3] = as.numeric(scale(base[ , 3]))

base[ , 5] = as.numeric(scale(base[ , 5]))

base[ , 11:13] = as.numeric(scale(base[ , 11:13]))


#----------------------- TRAIN AND TEST


set.seed(1)

div = sample.split(base$income, SplitRatio = 0.85)

base_train = subset(base, div == T)
base_test = subset(base, div == F)



# ------------ Algorithm

library(h2o)
h2o.init()

clas = h2o.deeplearning(y= 'income',
                        training_frame = as.h2o(base_train),
                        hidden = c(9),
                        epochs = 1000,
                        export_weights_and_biases=TRUE)


prev = h2o.predict(clas, newdata=as.h2o(base_test[, -15]))

prev = prev$predict
prev = as.vector(prev)


weights1 <- h2o.weights(clas,matrix_id=1)



#------------- Confusion Matrix


conf_matrix = table(base_test[,15], prev)


conf_matrix


confusionMatrix(conf_matrix)


                
              














