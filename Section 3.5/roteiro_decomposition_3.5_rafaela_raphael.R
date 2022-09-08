
# Roteiro da Seção 3.5

# Abrir os pacotes necessários

library(fpp3)
library(seasonal)

# Ler a base de dados
# A base de dados começa em 1939.
# Há vásrios tipos de títulos.
# O filter seleciona os dados a partir de 1990 cujo título é "Retail Trade".
# O select seleciona todas as colunas menos a coluna Series_ID.

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

# Visualização da série

autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

########### X-11 Method ###########

# Decomposição da série
# Os dados são mensais -> pode usar o X-11 Method!
# X_13ARIMA_SEATS -> por default é multiplicativo!

x11_dcmp <- us_retail_employment %>%
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) %>%
  components()

# Visualização da decomposição da série

autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using X-11.")

# Análise da trend-cycle component and the seasonally adjusted data

x11_dcmp %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

# Análise da sazonal de sub-séries

x11_dcmp %>%
  gg_subseries(seasonal)

########### SEATS ###########

# Decomposição da série
# Os dados são mensais -> pode usar o SEATS!
# X_13ARIMA_SEATS -> por default é multiplicativo!
seats_dcmp <- us_retail_employment %>%
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) %>%
  components()

autoplot(seats_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using SEATS")

########### SEATS com  Air Passenger ########### 

air <- as_tsibble(AirPassengers)

# Decomposição da sperie
# Os dados são mensais -> pode usar o SEATS!
# X_13ARIMA_SEATS -> por default é multiplicativo!
seats_dcmp <- air %>%
  model(seats = X_13ARIMA_SEATS(value ~ seats())) %>%
  components()

autoplot(seats_dcmp) +
  labs(title =
         "Decomposition of  Air Passengers using SEATS")


#Exemplos do pacote Seasonal

#Realizando um ajuste sazonal

ms <- seas(AirPassengers)  # Usando SEATS
mx <- seas(AirPassengers, x11 = "") # Usando X11

# Vê as escolhas realizadas pela função seas
static(ms)

# Dataframe apos o ajuste sazonal 

final(ms)

# plot do original e apos o ajuste sazonal

plot(ms)

# sumário com diversas informações importantes

summary(ms)
