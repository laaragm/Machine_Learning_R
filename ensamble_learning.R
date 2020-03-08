install.packages("randomForest", dependencies = T)
library(randomForest)

#usamos múltiplos algoritmos criando vários modelos e ele busca
#a melhor performance combinando esses modelos
#usa árvores de decisão e cria um modelo combinando os modelos com melhor performance
credito = read.csv(file.choose(), sep=',', header=T)
amostra = sample(2,1000,replace=T, prob=c(0.7, 0.3))
treino = credito[amostra==1, ]
teste = credito[amostra==2, ]
#ntree é o número de florestas que serão induzidas
#pode usar também outros argumentos para reduzir o tamanho da amostra,
#reduzir a complexidade da árvore, etc
floresta = randomForest(class ~., data=treino, ntree=100, importance=T)
#mostra a importância de cada atributo no modelo de ensamble learning
#o primeiro gráfico mostra o quão pior o modelo ficaria sem cada um dos atributos
#utilizados; o segundo mostra o índice de Gini
varImpPlot(floresta) 
previsao = predict(floresta, teste)
confusao = table(previsao, teste$class)
confusao
indice_acertos = ((confusao[1]+confusao[4])/sum(confusao))
indice_acertos
indice_erros = ((confusao[2]+confusao[3])/sum(confusao))
indice_erros



