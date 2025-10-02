# 0 - Ambientação ----

pacman::p_load(
  poliscidata,
  tidyverse
)

# 1 - Análise ----

dados_paises <- world |> 
  drop_na(dem_score14, hdi, gdp08)

## Analisando o índice de democracia e IDH
## Curva hipotética que mostra a relação entre os dados.
## CEF = Esperança de Y dado X. E[Y|X]

ggplot(dados_paises, aes(x = hdi, y = dem_score14)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(se = FALSE, 
              spam = 0.3) + # spam permite maior detalhamento na curva
  geom_smooth(se = FALSE, method = "lm", colour = "red") + #modelo linear
  theme_classic()

## 1.1 - Regressão linear ----

modelo_democracia <- lm(dem_score14 ~hdi,
                       data = dados_paises)

summary(modelo_democracia)
## A função stargazer do pacote stargazer é melhor para visualizar que o summary
stargazer::stargazer(modelo_democracia, type = "text")


## Quando o IDH é zero, espera-se que o score de democracia seja 0,97 (valor do intercepto). 
## A mudança de uma unidade no IDH, espera-se o aumento de 7,43 no score de democracia. 

## Como ver os valores previstos que a regressão estima?

df_previsao <- dados_paises |> 
  select(country, dem_score14, hdi) |> 
  mutate(democracia_prevista = fitted(modelo_democracia),
         erro_previsao = residuals(modelo_democracia))

View(df_previsao)

## Arábia Saudita teve um erro de previsão muito diferente do valor observado.
## Semelhante acontece com a India: tem uma democracia maior (observado) do que a prevista gerada pelo modelo. 

## A média do erro é zero, ou muito próximo
mean(df_previsao$erro_previsao)

## Espera-se que a regressão seja linear nos parâmetros, não necessariamente nas variáveis. 

## Gráfico para ver a CEF da relação entre duas variáveis. 

ggplot(dados_paises, aes(x = gdp08, y = dem_score14)) +
  geom_point(alpha = 0.3) + 
  geom_smooth(se = FALSE, spam = 0.5) + 
  geom_smooth(method = "lm", se = FALSE,
              colour = "red",
              formula = y ~ log(x))


## Gráfico ilustrativo com logaritmo no GDP

ggplot(dados_paises, aes(x = log(gdp08), y = dem_score14)) +
  geom_point(alpha = 0.3) + 
  geom_smooth(se = FALSE, spam = 0.5) + 
  geom_smooth(method = "lm", se = FALSE,
              colour = "red")

## O log do PIB e o índice de democracia parece uma relação linear. 

## Especificando o log(y) dentro da regressão

modelo_log <- lm(dem_score14 ~ log(gdp08),
                 data = dados_paises)

stargazer::stargazer(modelo_log, type = "text")
summary(modelo_log)

## Com log, interpreta-se: a mudança em 1% do PIB é associada a um aumento no índice de democracia em 0,038

log(exp(1))

# O que muda? Ao invés de ser a transformação na unidade, a transformação fica na %. 

