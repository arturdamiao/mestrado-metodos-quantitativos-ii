library(tidyverse)
library(poliscidata)
library(stargazer)
library(plotly)

dados_paises <- world %>% 
  select(country, dem_score14, hdi, gini10) %>% 
  drop_na()

modelo_hdi <- lm(dem_score14 ~ hdi,
                 data = dados_paises)

modelo_gini <- lm(dem_score14 ~ gini10,
                  data = dados_paises)

modelo_multiplo <- lm(dem_score14 ~ hdi + gini10,
                      data = dados_paises)

summary(modelo_hdi)
summary(modelo_gini)
summary(modelo_multiplo)

# Forma melhor de visualizar todos os modelos juntos.
stargazer::stargazer(modelo_hdi, modelo_gini, modelo_multiplo,
                     type = "text")

# Visualizar os resultados

dados_paises %>% 
  ggplot(aes(gini10, dem_score14)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm")


## Resultados em 3D

dados_paises %>% 
  plot_ly(x = ~hdi, y = ~gini10, z = ~dem_score14,
          text = ~country, hoverinfo = "text+x+y+z",
          type = "scatter3d",
          mode = "markers",
          marker = list(size = 3, color = "blue")) %>% 
  layout(title = "Pontos em 3D",
       scene = list(xaxis = list(title = "IDH"),
                    yaxis = list(title = "Gini"),
                    zaxis = list(title = "Democracia")),
       paper_bgcolor = 'lightyellow') # fundo amarelo

## Pacote {ggeffects}

library(ggeffects)

predicoes <- ggpredict(modelo_multiplo,
                       terms = c("hdi", "gini10"))
# No resultado de gini, mantém os valores constantes.
# Pega a média do Gini e adiciona um desvio padrão a mais e a menos

plot(predicoes) +
  theme_minimal() +
  labs(x = "IDH",
       y = "Democracia",
       color = "Gini")

# Simulando dados de renda das pessoas

set.seed(123) # permite ter o mesmo resultado nas aleatorizações

populacao <- tibble(
  anos_de_estudo = rnorm(10000, mean = 12, sd = 3),
  erro = rnorm(10000, mean = 0, sd = 500),
  renda = 500 + 150 * anos_de_estudo + erro
) 

amostra <- populacao %>% 
  sample_n(200)

modelo_amostra <- lm(renda ~ anos_de_estudo,
                     data = amostra)


stargazer::stargazer(modelo_amostra, type  = "text")
summary(modelo_amostra)

betas_estimados <- numeric(1000)


for(i in 1:1000) {
  # sampleando a amostra
  amostra_loop <- populacao %>% 
    sample_n(200)
  
  # criando o modelo de regressão
  modelo_loop <- lm(renda ~ anos_de_estudo,
                    data = amostra_loop)
  
  #analisando o segundo elemento do modelo
  betas_estimados[i] <- coef(modelo_loop)[2]
  
}

# Verificar se a média está próxima de 150
mean(betas_estimados)

# Visualizando o resultado
## O estimador da regressão linear é não viesado.
## Também é resultado do Teorema Central do Limite
## Transformando betas_estimados em um dataframe 
## Facilita no ggplot2
df_betas <- tibble(betas_estimados)

## Plotando o resultado
df_betas %>% 
  ggplot(aes(betas_estimados)) + 
  geom_histogram(bins = 30) + 
  geom_vline(xintercept = 150,
             color = "red",
             linetype = "dashed",
             linewidth = 1.5)

