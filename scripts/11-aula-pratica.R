# Aula 11

# Instalando pacotes

install.packages("pacman")

pacman::p_load(
  rio,
  tidyverse,
  data.table,
  sjlabelled,
  haven,
  janitor,
  lubridate,
  knitr,
  broom
)

# Lendo os dados

lat_bar23 <- read_rds("Latinobarometro_2023_Eng_Rds_v1_0.rds")

# Transformando as variáveis em categóricas

lat_bar23 <- lat_bar23 %>% 
  mutate(across(where(haven::is.labelled),
                      haven::as_factor))

lat_bar23 <- lat_bar23 %>% 
  janitor::clean_names() %>% 
  # Transformar primeiro em texto garante o número correto
  mutate(idade = as.numeric(as.character(edad)),
         econ_12_meses = p6stgbs,
         aprovacao_presidente = p15stgbs,
         ideologia = p16st,
         votaria_governo = perpart,
         religiao = s1) %>% 
  dplyr::select(idade, ideologia, econ_12_meses, aprovacao_presidente,
                votaria_governo, religiao, idenpa)

br_23 <- lat_bar23 %>% 
  mutate(pais = as.character(idenpa)) %>% 
  filter(pais == " Brasil") %>% 
  mutate(y = ifelse(grepl("Govern",
                          votaria_governo), 1, 0),
         ideologia = as.character(ideologia),
         ideologia = as.numeric(gsub("\\ [A-Z]+",
                                     "", ideologia)))

# Rodando a regressão

reg_logistica <- glm(y ~ idade + ideologia, 
                     data = br_23,
                     family = "binomial")

summary(reg_logistica)

# Atente-se ao valor Z, e não valor T. 

# O intercepto da logística (quando os preditores são 0) não é mais onde 
# a reta cruza o eixo Y. 

# Função da logística:
## Intercepto
1/(1+exp(--0.020801))

# Probabilidade de votar no governo quando todas as outras variáveis
# são zero

## Idade
1

# Regressão só com ideologia ----

reg_logistica_ideologia <- glm(y ~ ideologia, 
                     data = br_23,
                     family = "binomial")

summary(reg_logistica_ideologia)

1/(1+exp(-0.1707))

# Andando na escala
1/(1+exp(-(0.1707 + 1*coef(reg_logistica_ideologia)[2])))
1/(1+exp(-(0.1707 + 2*coef(reg_logistica_ideologia)[2])))
1/(1+exp(-(0.1707 + 9*coef(reg_logistica_ideologia)[2])))
1/(1+exp(-(0.1707 + 10*coef(reg_logistica_ideologia)[2])))

# O efeito não é sempre o mesmo 
# Aumentar uma unidade para a esquerda diminuir em 
# 0.038. Mas de 9 para 10 o valor é diferente. O efeito 
# não é constante.


# Método raiz

curve(1/(1+exp(-x)), from =  -5, to = 5 )

# não peguei

# Para calcular as razões de chance
## Para ser significativa, o intervalo de confiança
## não cruza o 1
broom::tidy(reg_logistica_ideologia,
            exponentiate = TRUE,
            conf.int = TRUE)


# Com LM
modelo_lm <- lm(y ~ ideologia, 
                data = br_23)

summary(modelo_lm)
