install.packages("e1071", dependencies = T)
library(e1071)

#Queremos prever se cada um dos novos clientes será um bom ou mau pagador
credito = read.csv(file.choose(), se=",", header=T) #selecionar o arquivo credit.csv
head(credito) #para ver os primeiros registros do arquivo
dim(credito) #1000 instâncias(linhas) e 21 atributos(colunas)

#modelo genérico: aproximadamente 70% treino e 30% teste
#samples() vai fazer 1000 sorteios de 1 e 2, onde a probabilidade
#de gerar 1 é de 70%, e a probabilidade de gerar 2 é de 30%
amostra = sample(2,1000,replace=T,prob=c(0.7,0.3))
amostra
#pegando as amostras onde a linha seja 1 para formar o conjunto de treino
treino = credito[amostra==1,]
dim(treino)
#pegando as amostras onde a linha seja 2 para formar o conjunto de teste
teste = credito[amostra==2,]
dim(teste)

#Criando o modelo
#primeiro parâmetro do naiveBayes: atributos que utilizaremos para treinar 
#o modelo; segundo parâmetro: dados
#separando os atributos com o '~' para diferenciar a variável explicativa
#da variável de resposta(class)
# '.' seleciona todos os outros atributos
modelo = naiveBayes(class ~., treino)
modelo #modelo vai ser um objeto do R (classe dele é naiveBayes)

#Fazendo a previsão do modelo 
#predict(modelo, dados_que_tem_atributos_pra_fazer_a_previsao)
predicao = predict(modelo, teste)
predicao #vetor dizendo se é um bom ou mau pagador
#montando uma matriz de confusão para avaliar a predição
confusao = table(teste$class, predicao)
confusao #bad/bad e good/good temos os acertos; os outros são erros

#Medindo o desempenho
#calculando o índice de acertos
indice_acertos = ((confusao[1]+confusao[4])/sum(confusao))
indice_acertos
#calculando o índice de erros
indice_erros = ((confusao[2]+confusao[3])/sum(confusao))
indice_erros
#É um bom modelo? Podemos levá-lo para a produção? 
#Aí depende do negócio, nenhum modelo é determinístico, cada um tem um viés. 
#Você escolhe qual vai ser o mais interessante (quem decide o ponto de corte da 
#probabilidade é o negócio)
#Assim, definimos se vamos otimizar o modelo ou se ele já pode ir para produção


#---------------------------------------------------------------------------------
#Simulando o modelo em produção
novo_credito = read.csv(file.choose(),sep=',', header=T) #novocredito.csv
novo_credito
dim(novo_credito)
predict(modelo, novo_credito)

