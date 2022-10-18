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

##################################################
#Comparando ARIMA()e ETS()em dados não sazonais
###################################################

#Podemos usar a validação cruzada de séries temporais para comparar os modelos ARIMA 
#e ETS. Vamos considerar a população australiana do conjunto de global_economydados.

aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Population = Population/1e6)

aus_economy %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    ETS(Population),
    ARIMA(Population)
  ) %>%
  forecast(h = 1) %>%
  accuracy(aus_economy) %>%
  select(.model, RMSE:MAPE)


#Neste caso, o modelo ETS tem maior precisão nas medidas de desempenho validadas 
#cruzadas. Abaixo, geramos e traçamos previsões para os próximos 5 anos geradas 
#a partir de um modelo ETS.

aus_economy %>%
  model(ETS(Population)) %>%
  forecast(h = "5 years") %>%
  autoplot(aus_economy %>% filter(Year >= 2000)) +
  labs(title = "Australian population",
       y = "People (millions)")


#############################################
#Comparando ARIMA()e ETS()em dados sazonais
############################################

#Neste caso, queremos comparar os modelos sazonais ARIMA e ETS aplicados aos dados
#trimestrais de produção de cimento (de aus_production). Como a série é 
#relativamente longa, podemos usar um conjunto de treinamento e de teste em vez 
#de validação cruzada de séries temporais. A vantagem é que isso é muito mais 
#rápido. Criamos um conjunto de treinamento desde o início de 1988 até o final de
#2007 e selecionamos um modelo ARIMA e um modelo ETS usando as funções ARIMA()e 
#ETS().

cement <- aus_production %>%
  select(Cement) %>%
  filter_index("1988 Q1" ~ .)
train <- cement %>% filter_index(. ~ "2007 Q4")


#A saída abaixo mostra o modelo selecionado e estimado por ARIMA(). 
#O modelo ARIMA faz bem em capturar toda a dinâmica nos dados, pois os resíduos
#parecem ser ruído branco.

fit_arima <- train %>% model(ARIMA(Cement))
report(fit_arima)


fit_arima %>% gg_tsresiduals(lag_max = 16)

#Gráficos de diagnóstico de resíduos para o modelo ARIMA ajustados aos dados 
#trimestrais de treinamento de produção de cimento.

augment(fit_arima) %>%
  features(.innov, ljung_box, lag = 16, dof = 6)


#A saída abaixo também mostra o modelo ETS selecionado e estimado por ETS(). 
#Este modelo também se sai bem em capturar toda a dinâmica nos dados, já que os
#resíduos também parecem ser ruído branco.

fit_ets <- train %>% model(ETS(Cement))
report(fit_ets)

fit_ets %>%
  gg_tsresiduals(lag_max = 16)

#Gráficos de diagnóstico de resíduos para o modelo ETS ajustados aos dados 
#trimestrais de treinamento de produção de cimento.

augment(fit_ets) %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

#A saída abaixo avalia o desempenho de previsão dos dois modelos concorrentes sobre
#o conjunto de teste. Neste caso, o modelo ARIMA parece ser o modelo um pouco mais 
#preciso baseado no conjunto de testes RMSE, MAPE e MASE.

bind_rows(
  fit_arima %>% accuracy(),
  fit_ets %>% accuracy(),
  fit_arima %>% forecast(h = 10) %>% accuracy(cement),
  fit_ets %>% forecast(h = 10) %>% accuracy(cement)
) %>%
  select(-ME, -MPE, -ACF1)

#Abaixo geramos e traçamos previsões do modelo ARIMA para os próximos 3 anos.

cement %>%
  model(ARIMA(Cement)) %>%
  forecast(h="3 years") %>%
  autoplot(cement) +
  labs(title = "Cement production in Australia",
       y = "Tonnes ('000)")

