# lendo a biblioteca
library(fpp3)


# Manipulando os dados para trazer o custo dos medicamento de diabetes
a10 <- PBS %>%
        filter(ATC2 == "A10") %>%
        select(Month, Concession, Type, Cost) %>%
        summarise(TotalC = sum(Cost)) %>%
        mutate(Cost = TotalC/1e6) # trocando a unidade de doláres para milhões de doláres


# Plotando a série temporal
autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")


# Plotando a série de maneira anual, para visualizar graficamente existe uma sazonalidade
a10 %>%
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")


# Plotando autocorrelação da série
a10 %>%
  ACF(Cost, lag_max = 48) %>%
  autoplot() +
  labs(title="Australian antidiabetic drug sales")


# Plotando todos os componentes
a10 %>%
  model(
    classical_decomposition(Cost, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Australian antidiabetic drug sales")
