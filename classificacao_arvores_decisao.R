#ctrl+l limpa o console
install.packages("rpart", dependencies = T)
library(rpart)

#Abrindo o arquivo de dados e separando as amostras
credito = read.csv(file.choose(), sep=",", header=T)
amostra = sample(2,1000,replace=T, prob=c(0.7,0.3))
treino = credito[amostra==1, ]
teste = credito[amostra==2, ]

#Criando o modelo
#method="class" pois estamos falando de classificação
arvore_decisao = rpart(class ~., data=treino, method="class")
arvore_decisao
#Plotando uma árvore muito feia - tem outros pacotes do R que tem árvores melhores
plot(arvore_decisao) 
text(arvore_decisao, use.n=T, all=T, cex=.8) #coloca as infos das folhas

#Previsão com os dados teste
#mostra a probabilidade de cada instância estar em uma classe ou outra (bad/good)
testando = predict(arvore_decisao, newdata=teste)
#vamos converter pois não tem como fazer matriz de confusão com a 
#probabilidade, estamos convertendo para uma forma que poderemos
#medir o resultado
conversao_p_medir_resul = cbind(teste, testando) #cbind() adiciona coluna
fix(conversao_p_medir_resul) 
#criar uma outra coluna de acordo com as probabilidades de good/bad - classe
conversao_p_medir_resul['Result'] = ifelse(conversao_p_medir_resul$bad>=0.5,
                                           "bad", "good")
conversao_p_medir_resul
matriz_confusao = table(conversao_p_medir_resul$class,
                        conversao_p_medir_resul$Result)
matriz_confusao
indice_acertos = (matriz_confusao[1]+matriz_confusao[4])/sum(matriz_confusao)
indice_acertos
indice_erros = (matriz_confusao[2]+matriz_confusao[3])/sum(matriz_confusao)
indice_erros

