#install.packages("class", dependencies = T)
library(class)

head(iris)
summary(iris)

#aqui não tem treino e teste; teremos treino e dados a classificar(tempo real)
#o algoritmo vai pegar cada linha que tem que classificar e buscar a instância
#mais próxima nos dados de treino. 
#ver qual é a classe daquele que estiver mais próximo e atribuí-la
#a linha a ser classificada

#para dividir os dados (150 pois são 150 instâncias de iris)
amostra = sample(2,150, replace=T, prob=c(0.7,0.3)) 
treino = iris[amostra==1, ]
a_classificar = iris[amostra==2, ]
dim(treino)
dim(a_classificar)

#previsão: knn(vizinhos mais próximos)
#parâmetros:
#treino[, 1:4] - onde vamos tentar localizar o vizinho mais próximo
#a_classificar[, 1:4] - dados que queremos classificar (de cada instância daqui
#buscamos o vizinho mais próximo de treino[, 1:4] )
#treino[, 5] - diz onde estão as classes para atribuir aos dados a_classificar
#k=3 - podemos testar com outros valores para ver se o resultado melhora ou não
previsao = knn(treino[, 1:4], a_classificar[, 1:4], treino[, 5], k=3)
confusao = table(a_classificar[,5], previsao)
confusao
indice_acertos = (confusao[1]+confusao[5]+confusao[9])/sum(confusao)
indice_acertos
indice_erros = (confusao[2]+confusao[3]+confusao[4]+confusao[6]+confusao[7]+confusao[8])/sum(confusao)
indice_erros


