####################################
#Aula Invertida (Séries temporais)
#Aluna: Tuany Barcellos
#Prof: Cyrino
####################################


###########################################
#Aula Pratica
###############################

#######################
#Exemplo do livro FPP3
#######################

###################
#Instalando pacote
###################

install.packages("fpp3")
library(fpp3)
library(fable)

###################################################
#Modelagem ARIMA em fable - Cap (9) - seção (9.7)
##################################################

#A função ARIMA() no pacote fablepacote utiliza uma variação do algoritmo 
#Hyndman-Khandakar, que combina testes de raiz unitária, minimização do AICc 
#para obter um modelo ARIMA. Os argumentos para ARIMA(). 
#O que é descrito aqui é o comportamento padrão.

###############################################
#Série exportações da República Centro-Africana
###############################################


global_economy %>%
  filter(Code == "CAF") %>%
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")



#O gráfico mostra alguma não estacionaridade, com um declínio geral. 
#A melhora em 1994 deve-se a um novo governo que derrubou a junta militar e teve 
#algum sucesso inicial, antes que a agitação causasse mais declínio econômico.

#N há evidência de mudança de variância, então não faremos uma transformação Box-Cox.

#Para abordar a não estacionaridade, vamos tomar uma primeira diferença dos dados.


global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(difference(Exports), plot_type='partial')


#A PACF mostrado sugere um modelo AR(2); então um modelo candidato inicial é um 
#ARIMA(2,1,0). A ACF sugere um modelo MA(3); então um candidato alternativo é 
#um ARIMA(0,1,3).

#Ajustamos um modelo ARIMA(2,1,0) e um modelo ARIMA(0,1,3) juntamente com duas 
#seleções de modelo automatizadas, uma usando o procedimento stepwise padrão e 
#outra trabalhando mais para pesquisar um modelo melhor ajustado.

caf_fit <- global_economy %>%
  filter(Code == "CAF") %>%
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE))


caf_fit %>% pivot_longer(!Country, names_to = "Model name",
                         values_to = "Orders")

glance(caf_fit) %>% arrange(AICc) %>% select(.model:BIC)


#Os quatro modelos têm valores de AICc quase idênticos. Dos modelos ajustados, 
#a pesquisa completa descobriu que um ARIMA(3,1,0) fornece o menor valor de AICc, 
#seguido de perto pelo ARIMA(2,1,0) e ARIMA(0,1,3) - o último dois são os modelos 
#que adivinhamos dos gráficos ACF e PACF. A seleção automatizada por etapas 
#identificou um modelo ARIMA(2,1,2), que possui o maior valor de AICc dos quatro 
#modelos.



#O gráfico ACF dos resíduos do modelo ARIMA(3,1,0) mostra que todas as 
#autocorrelações estão dentro dos limites, indicando que os resíduos estão se 
#comportando como ruído branco.


caf_fit %>%
  select(search) %>%
  gg_tsresiduals()


#O teste de ljung_box retorna um p valor acima do alpha estabelicido,
#sugerindo também que os resíduos são ruído branco.

augment(caf_fit) %>%
  filter(.model=='search') %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

#As previsões dos modelos

caf_fit %>%
  forecast(h=5) %>%
  filter(.model=="search") %>%
  autoplot(global_economy)

#Observe que as previsões médias são muito semelhantes ao que obteríamos com um 
#passeio aleatório (equivalente a um ARIMA(0,1,0)). O trabalho extra para incluir
#termos AR e MA fez pouca diferença para as previsões pontuais neste exemplo, 
#embora os intervalos de previsão sejam muito mais estreitos do que para um 
#modelo de passeio aleatório.
