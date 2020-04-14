#---------------------------------------------------------------------------------
#                                 Logit
#---------------------------------------------------------------------------------

setwd("D:/Git projects/ML in R")



df = read.csv('naive_base.csv')
df = df[df$risco != 'moderado',  ]


df$historia = factor(df$historia, levels = c('ruim', 'desconhecida', 'boa'), labels = c(0, 1, 2))
df$divida = factor(df$divida, levels = c('alta', 'baixa'), labels = c(0, 1))
df$garantias = factor(df$garantias, levels = c('nenhuma', 'adequada'), labels = c(0, 1))
df$renda = factor(df$renda, levels = c('0_15', '15_35', 'acima_35'), labels = c(0, 1, 2))
df$risco = factor(df$risco, levels = c('alto', 'baixo'), labels = c(0, 1))





#----- Model

clas = glm(risco ~ . , family = binomial, data=df)

clas

clas$fitted.values






# let's see the new customers are deadbeat

# história: boa, dívida: alta, garantias: nenhuma, renda: >35
# hitóstia: ruim, dívida: alta, garantias: adequada, renda: <15

historia = c('boa', 'ruim')
divida = c('alta', 'alta')
garantias = c('nenhuma', 'adequada')
renda = c('acima_35', '0_15')
df2 = data.frame(historia, divida, garantias, renda)


prev = predict(clas, df2, type='response')  # type make probabilities appear
prev


risco = ifelse(prev < 0.5, 'baixo', 'alto')









