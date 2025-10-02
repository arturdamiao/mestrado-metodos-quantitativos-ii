# 0 - Ambientação ----

## install.packages("pacman")
pacman::p_load(
  tidyverse,
  poliscidata
)

# 1 - Utilizando o pacote poliscidata -----

## 1.1 - Importando e visualizando os dados ----
banco <- poliscidata::gss

# Para visualizar o banco de dados, usar a função View()
View(banco)

# Função summary para resumo o conjunto de dados
summary(banco)

# Função glimpse para ver o conjunto de dados
glimpse(banco)

# Função head para ver as primeiras linhas do conjunto
 <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
## Criar muitos objetos não é eficiente. Ocupa memória, confunde
## o ambiente de trabalho. Com um único objeto:

banco_finalizado <- banco |>
  dplyr::select(age, born, degree, sex, authoritarianism) |>
  dplyr::filter(
    born == "YES",
    sex == "Female"
  ) |>
  dplyr::mutate(
    ano_nascimento = 2012 - age
  ) |>
  dplyr::rename(
    yearh_birth = ano_nascimento
  ) |>
  dplyr::mutate(
    age = dplyr::case_when(
      age < 20 ~ "Menos de 20",
      age >= 20 & age < 30 ~ "De 20 a 29",
      age >= 30 ~ "30+"
    )
  )

# 2 - Visualização de dados (pacote nes) ----
## Utilizando ggplot2, que segue a Gramática dos Gráficos

banco <- poliscidata::nes |> 
  ggplot2::remove_missing(
    vars = "pres_vote12"
  )

## 2.1 Gráfico de barras (varíavel categórica) ----
ggplot2::ggplot(banco, aes(pres_vote12)) + 
  geom_bar() + 
  theme_classic()

## 2.2 Boxplot, histograma e densidade (variáveis númericas) ----

### 2.2.1 Gráfico de densidade ----
ggplot2::ggplot(banco, aes(conservatism)) +
  geom_density() + 
  theme_classic()

### 2.2.2 Histograma ----
ggplot2::ggplot(banco, aes(conservatism)) +
  geom_histogram() + 
  theme_classic()

### 2.2.3 Boxplot ----
ggplot2::ggplot(banco, aes(conservatism)) +
  geom_boxplot() + 
  theme_classic()

## 2.3 Gráfico bivariados ----
### Voto presidencial e conservadorismo

### 2.3.1 Boxplot com duas variáveis ----
ggplot2::ggplot(banco,
                aes(x = pres_vote12,y = conservatism)) +
  geom_boxplot()

### 2.3.2 Histograma com duas variáveis ----
# O argumento fill coloca as duas variáveis 
# O argumento alpha permite transparência no gráfico
ggplot2::ggplot(
  banco,
  aes(
    x = conservatism,
    fill = pres_vote12
  )
) +
  geom_histogram(alpha = 0.5)

### 2.3.3 Densidade com duas variáveis ----
ggplot2::ggplot(
  banco,
  aes(
    x = conservatism,
    fill = pres_vote12
  )
) +
  geom_density(alpha = 0.5)

### 2.3.4 Gráfico de pontos (conservadorismo e afinidade com obama)
ggplot(banco,
       aes(x = conservatism, y = obama_therm)) + 
  geom_point()

# O geom_jitter é o mesmo gráfico de pontos, mas espalha um pouco
# mais para facilitar a visualização. 
ggplot(banco,
       aes(x = conservatism, y = obama_therm)) + 
  geom_jitter(alpha = 0.1)
## A mancha escura indica que há muitos pontos concentrados. 

## É possível ver com uma linha de regressão também. 
ggplot(banco,
       aes(x = conservatism, y = obama_therm)) + 
  geom_jitter(alpha = 0.1) + 
  geom_smooth(method = "lm")


