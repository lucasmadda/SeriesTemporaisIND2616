# lendo a biblioteca
library(fpp3)



# Importando a série e plotando
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")


# Plotando autocorrelação da série
us_retail_employment %>%
  ACF(Employed, lag_max = 48) %>%
  autoplot() +
  labs(title="Australian antidiabetic drug sales - FAC")


# Média móvel 1 de 12 meses
us_retail_employment <- us_retail_employment %>%
  mutate(
    `12-MA`= slider::slide_dbl(Employed, mean,
                               .before = 6, .after = 5, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE)
    )
    

autoplot(us_retail_employment, `2x12-MA`) +
labs(y = "Persons (thousands)",
     title = "Total employment in US retail 2x12-MA")


us_retail_employment['detrended'] <- us_retail_employment['Employed'] - us_retail_employment["2x12-MA"]


autoplot(us_retail_employment, detrended) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail detrended")

us_retail_employment %>%
  gg_season(detrended, labels = "both") +
  labs(y = "Persons (Thousands)",
       title = "Seasonal plot: Total employment in US retail detrended")



us_retail_employment$Y_m_T <- us_retail_employment$Employed - us_retail_employment$`2x12-MA`

us_retail_employment %>%
  gg_subseries(Y_m_T) +
  labs(
    y = "Persons - Thousands",
    title = "Total employment in US retail Componentes"
  )

us_retail_employment$m <- as.integer(month(us_retail_employment$Month))

m_mean <- us_retail_employment[c('m','detrended')] %>%
  group_by(m) %>%
  summarise(mmean = mean(detrended, na.rm = TRUE))


us_retail_employment <- left_join(us_retail_employment,m_mean, by = 'm')



us_retail_employment$seasonal <- us_retail_employment$mmean

autoplot(us_retail_employment, seasonal) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail - Seasonal Component")


us_retail_employment$random <- us_retail_employment$Employed - us_retail_employment$`2x12-MA` - us_retail_employment$seasonal

autoplot(us_retail_employment, random) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail - Random Component")

ggplot(us_retail_employment,aes(random,na.rm=TRUE)) + 
  geom_histogram(bins=20) + 
  geom_vline(aes(xintercept=mean(random, na.rm = TRUE),color='red'))+
  labs(title = "Random Component Values Distribution")


us_retail_employment %>%
  model(
    classical_decomposition(Employed, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")
