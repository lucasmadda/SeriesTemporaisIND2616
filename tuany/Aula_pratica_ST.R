####################################
#Aula Invertida (Séries temporais)
#Aluna: Tuany Barcellos
#Prof: Cyrino
####################################

###################
#Aula prática ETS
###################

###################################
#Instalando os pacotes necessários
###################################

install.packages("forecast")
install.packages("readxl")
install.packages("ggplot2")

library(forecast)
library(readxl)
library(ggplot2)

########################
#Apresentando os dados
#######################

############################
#Remover notação científica 
############################
options(scipen=999)

##################
#Vendas de padaria
##################

##################################################################
#Leitura dos dados mensais de vendas de pão (mar/2018 a fev/2022) 
##################################################################

dados_pad = read_excel("dadospadaria.xlsx")
dados_pad

dados_pad = ts(data = dados_pad$Venda, start = c(2018,3),frequency = 12)

forecast::autoplot(dados_pad, xlab = "Tempo", ylab = "Vendas pão (un)")

########################
#Decomposição da série
########################

#Observando o gráfico dos dados, podemos inferir sobre três 
#componentes:

#1) Uma componente linear de crescimento dos dados
#2) Uma componente sazonal nos dados
#3) Uma componente aleatória nos dados

#Para observar as três componentes, podemos decompor a série usando 
#o seguinte comando:

dados_pad_dec = decompose(dados_pad)
dados_pad_dec

#A nova variável armazena a decomposição da série temporal, com isso, 
#podemos observar o gráfico das componentes separadas:

plot(dados_pad_dec)

#########################
#Aplicação do modelo ETS
#########################

ets_pad <- dados_pad %>% ets()
ets_pad

########################
#Previsão para 12 meses 
#######################

previsao_pad <- ets_pad %>% forecast(h=12)
previsao_pad

autoplot(dados_pad, xlab = "Tempo", ylab = "Vendas pão (un)")+
  autolayer(previsao_pad$mean, series="Previsao")


#####################
#Vendas de Farmacia
#####################

######################################################################
##Leitura dos dados de venda de máscaras descart (abr/2017 a mar/2022) 
######################################################################

dados_farm = read_excel("dadosfarmacia.xlsx")
dados_farm

dados_farm = ts(data = dados_farm$Venda, start = c(2017,4),frequency = 12)
dados_farm

forecast::autoplot(dados_farm, xlab = "Tempo", ylab = "Vendas máscaras (un)")

dados_farm_dec = decompose(dados_farm)
dados_farm_dec

plot(dados_farm_dec)

#########################
#Aplicação do modelo ETS
#########################

ets_farm <- dados_farm %>% ets()
ets_farm

########################
#Previsão para 12 meses 
#######################

previsao_farm <- ets_farm %>% forecast(h=12)
previsao_farm

autoplot(dados_farm, xlab = "Tempo", ylab = "Vendas máscaras (un)")+
  autolayer(previsao_farm$mean, series="Previsao")


################
#Energia eólica
################

###################################################################################
#Leitura dos dados mensais de geração de energia eólica em MW (jan/2014 a set/2020)
###################################################################################

dados_eol = read_excel("dadoseolica.xlsx")
dados_eol


##################
#Grafico da série
##################

p=ggplot(data = dados_eol, aes(x = Data, y = Ger)) + 
  geom_line(color="brown", size = 1)+
  xlab("Tempo") +
  ylab("Geração eólica (MW)")+
  theme_minimal()
p

##############
#Decomposição
#############

dados_eol = ts(data = dados_eol$Ger, start = c(2014,1),frequency = 12)

dados_eol_dec = decompose(dados_eol)
dados_eol_dec

plot(dados_eol_dec)

########################################
# função ggmonthplot do pacote forecast
########################################

ggmonthplot(dados_eol) + theme_minimal() 


ggseasonplot(dados_eol, year.labels = TRUE) + geom_point() + theme_minimal()

#########################
#Aplicação do modelo ETS
########################

#############
#Aplicação 1
##############

ets_eol <- dados_eol %>% ets()
ets_eol

########################
#Previsão para 12 meses 
########################

previsao_eol <- ets_eol %>% forecast(h=12)
previsao_eol

autoplot(dados_eol, xlab = "Tempo", ylab = "Geração eólica (MW)")+
  autolayer(previsao_eol$mean, series="Previsao")

###################################################################################
#Aplicação 2 - separando parte da série para ajustar o modelo e parte para avaliar 
#a capacidade preditiva do modelo
###################################################################################

treinamento = window(dados_eol, start = c(2014,1), end = c(2020,3)) 

teste = window(dados_eol, start= c(2020,4), end = c(2020,9))

ets_eol <- treinamento %>% ets()
ets_eol

#######################
#Previsão para 6 meses 
#######################

previsao_eol <- ets_eol %>% forecast(h=6)
previsao_eol

autoplot(dados_eol, xlab = "Tempo", ylab = "Geração eólica (MW)")+
  autolayer(previsao_eol$mean, series="Previsao") 

teste
previsao_eol

ErroETS = accuracy(previsao_eol, teste)
ErroETS


###############################################################################

####################
#Aula prática ARIMA
####################

####################
#Instalando pacotes
####################

install.packages("readxl")
install.packages("fable")
install.packages("forecast")
install.packages("ISwR")
install.packages("randtests")
install.packages("ggplot2")
install.packages("lmtest")

library("readxl")
library("fable")
library("forecast")
library("ISwR")
library("randtests")
library("ggplot2")
library("lmtest")

############################
#Remover notação científica 
############################

options(scipen=999)


dados1 = read_excel("base_consumo.xlsx")
dados1

###########################
#Grafico da serie original
###########################

p=ggplot(data = dados1, aes(x = Data, y = serie)) + 
  geom_line(color="brown", size = 1)+
  xlab("Tempo") +
  ylab("Consumo industrial de energia (kw))")+
  theme_minimal()
p


#Criando uma variável que recebe os valores da variável dados
#com a estrutura de uma série temporal

########################################################################
#Os dados são mensais, iniciando em janeiro de 2004 a fevereiro de 2020
########################################################################

consumoBJ = ts(dados1$serie, frequency = 12, start = c(2004,1))
consumoBJ

####################################################################
#Visualizando a série e sua autocorrelação e autocorrelação parcial
###################################################################

ggtsdisplay(consumoBJ)

###################################################
#Transformação Box-Cox pra estabilizar a variância
###################################################

lambda = BoxCox.lambda(consumoBJ) #log de verossimilhança
lambda

consumobc = BoxCox(consumoBJ, lambda = lambda)

ggtsdisplay(consumobc) #variância constante 

######################################################################
#Podemos notar que a séria não apresenta comportamento estacionário
#####################################################################


############################################################################
# H0: Existe pelo menos uma raiz dentro do círculo unitário (Não Estacionaria)
# H1: Não existe raízes dentro do círculo unitário (Estacionaria)
############################################################################

######################################
#Usando o teste de  Dickey and Fuller
#####################################

install.packages("urca")
library(urca)

#################################################################################
#Obs: A função ur.df()calcula o teste Dickey-Fuller aumentado. Se o tipo for 
#definido como "none"uma interceptação, nem uma tendência será incluída na regressão
#de teste. Se estiver definido para "drift"uma interceptação, é adicionado e, se 
#estiver definido para "trend"uma interceptação, uma tendência é adicionada
################################################################################

summary(ur.df(consumoBJ, type='none', lags=0))

#Nota-se que o p-valor é o mesmo de 0.09, não rejeitando a hipótese nula, ou seja,
# a série é não estacionária.

################################################################
#Verificando quantas diferenciações serão necessárias para que 
#a série seja estacionária
################################################################

ndiffs(consumobc)
#Será necessário apenas uma diferenciação

#######################
#Diferenciando a série
######################

dyt <- diff(consumobc,1)

ggtsdisplay(dyt)

#Testando a estacionariedade. após ter diferenciado a série

summary(ur.df(dyt, type='none', lags=0))

#Pode-se notar que o p-valor obitido é muito pequeno, então ao
#nível de significâncioa de 5%, rejeita-se a hipótese nula, logo
# a série diferenciada é estacionária.


#########################
#Particionando a série
######################

#Separando a base em treino e teste, uma com a série de dados em que os modelos 
#serão ajustados, (janeiro de 2004 a fevereiro de 2019) e a outra com os dados 
#para comparação (março de 2019 a fev de 2020). 


treinoBJ = ts(consumoBJ, frequency = 12, start = c(2004,1), end = c(2019,2))
treinoBJ

testeBJ = ts(consumoBJ, frequency = 12, start = c(2019,3), end = c(2020,2))
testeBJ

###################
#ajuste automatico
##################

ajuste = auto.arima(treinoBJ) 

ajuste

coeftest(ajuste)

############################
#plotando a parte de treino
############################

plot(treinoBJ,
     ylab=expression(Y[t]),
     main='',
     bty='l',
     col='blue')

###############################
#plotando os valores ajustados
###############################

lines(ajuste$fitted,col="red")

############################################################################
#Comparar a série original (em azul) com os valores ajustados (em vermelho)
############################################################################

accuracy(treinoBJ, ajuste$fitted)

#####################################################
#realizar previsão dos 12 meses separados para teste
####################################################

prev = forecast(ajuste, h = 12)
prev

plot(prev,
     ylab=expression(Y[t]),
     main='',
     bty='l',
     col='darkred')

#Linha azul estimativa pontual, azul escuro intervalo de confinça de 80%
#azul mais claro intervalo de confiança de 95%

###########################################################
#Plotar só as previsões contra a base de validação (teste)
##########################################################

plot(as.numeric(testeBJ), type = "l", ylim = c(12000000, 15000000))
lines(as.numeric(prev$mean), col = "red")

accuracy(testeBJ, prev$mean)

##################################
#Diagnóstico do resíduo do modelo 
##################################

tsdiag(ajuste)
#aparente não são autocorrelacionados 

qqnorm(ajuste$residuals)
qqline(ajuste$residuals, col = "red")

checkresiduals(ajuste)

Box.test(residuals(ajuste), type="Ljung-Box")
#p-valor = 0.9404 (são i.i.d.)

