####  Projeto (Linguagem R) ####

# Configurando o diretório de trabalho
setwd("~/Desktop/DataScience/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/18.Mini-Projeto-2_-_Analise_Financeira_com_Linguagem_SQL_e_Regressao_Linear_em_R")
getwd()


## Carregando pacotes
library(dplyr)
library(ggplot2)

library(caret)        # dividir dados em treino e teste
library(randomForest)


## Perguntas de Negócio (Linguagem SQL)

# 1-  Qual a distribuição da idade dos pacientes que frequentam o hospital?
# 2-  Qual faixa etária tem o maior gasto total no hospital?
# 3-  Qual grupo baseado em diagnóstico (Aprdrg) tem o maior gasto total no hospital?
# 4-  A raça do paciente tem relação com o total gasto em internações no hospital?
# 5-  A combinação de idade e gênero dos pacientes influencia no gasto total em internações no hospital?
# 6-  Como o tempo de permanência é o fator crucial para pacientes internados, desejamos descobrir se o tempo 
#     de permanência pode ser previsto a partir de idade, gênero e raça.
# 7-  Quais variáveis têm maior impacto nos custos de internação hospitalar?


## Carregando dados
dados <- read.csv("dataset.csv")
head(dados)

## Renomeando Colunas
names(dados) <- c("idade", "genero", "tempo_permanencia", "tipo_raca", "gasto_total", "tipo_diagnostico")
head(dados)


## Verificando e removendo valores ausentes nas colunas
colSums(is.na(dados))
dados <- na.omit(dados)




##  1) Qual a distribuição da idade dos pacientes que frequentam o hospital?

# Visualizando através do summary com variável do tipo numérico (o resultado é um resumo estatatístico)
summary(dados$idade)
table(dados$idade)

# Convertemos a variável para o tipo fator e então obtemos o sumário que precisamos. (mesma coisa que o table())
resumo_idade <- summary(as.factor(dados$idade))
resumo_idade

# Visualizando através de gráfico
hist(dados$idade)
hist(dados$idade, 
     main = "Histograma da Distribuição da Idade dos Pacientes que Frequentam o Hospital",
     xlab = "Idade", 
     border = "black", 
     col = c("light green", "dark green"), 
     xlim = c(0,20), 
     ylim = c(0,350))

rm(resumo_idade)

# -> Resposta: Crianças entre 0 e 1 ano são as que mais frequentam o hospital.




##  2) Qual faixa etária tem o maior gasto total no hospital?

# Visualizando através de tabela
idade_com_maior_gasto <- dados %>% 
  group_by(idade) %>% 
  summarise(idade_gasto = sum(gasto_total)) %>% 
  arrange(-idade_gasto)
idade_com_maior_gasto

# Visualizando através de gráfico (gráfico de barras) (adicionando levels exibe em ordem decrescente)
ggplot(idade_com_maior_gasto, aes(x = factor(idade, levels = idade), y = idade_gasto)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Distribuição do Gasto Total por Idade",
       x = "Idade",
       y = "Gasto Total") +
  theme_minimal()


rm(idade_com_maior_gasto)

# -> Resposta: A Faixa etária com maior gasto é 0.




##  3) Qual grupo baseado em diagnóstico (Aprdrg) tem o maior gasto total no hospital?

# Visualizando através de tabela
tipo_diag_maior_gasto <- dados %>% 
  group_by(tipo_diagnostico) %>% 
  summarise(diag_gasto = sum(gasto_total)) %>% 
  arrange(-diag_gasto) %>% 
  head(10)
tipo_diag_maior_gasto
maior_gasto <- tipo_diag_maior_gasto %>% 
  filter(diag_gasto == max(diag_gasto))
maior_gasto

# Visualizando através de gráfico (gráfico de barras) os 10 tipos de diagnóstico mais caros
ggplot(tipo_diag_maior_gasto, aes(x = factor(tipo_diagnostico, levels = tipo_diagnostico), y = diag_gasto)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(title = "Top 10 Tipos de Diagnóstico Mais Caros",
       x = "Tipo de Diagnóstico",
       y = "Gasto Total") +
  theme_minimal()


rm(maior_gasto)
rm(tipo_diag_maior_gasto)

# -> Resposta: O Grupo com maior gasto total no hospital é o 640




##  4) A raça do paciente tem relação com o total gasto em internações no hospital?

# Tipo 1
# Verificando a Correlação de Pearson entre duas variáveis (tipo_raca e gasto_total)
cor.test(dados$tipo_raca, dados$gasto_total, method = "pearson")  # -0.0181643

# - O valor de correlação de aproximadamente 0.01 e um valor p acima de 0.05 indica uma correlação fraca entre raça do paciente e
#   gasto em internações.


# Tipo 2
# Verificando a Correlação através do método ANOVA
modelo_anova <- aov(gasto_total ~ factor(tipo_raca), data = dados)
anova(modelo_anova)


rm(modelo_anova)

# - O valor de p de 0.9429 está acima de 0.05.
#   Portanto, a conclusão de que não há uma relação significativa entre a raça do paciente e o gasto em internações está correta




##  5) A combinação de idade e gênero dos pacientes influencia no gasto total em internações no hospital?

# Verificando a Correlação através do método ANOVA
modelo_anova <- aov(gasto_total ~ idade + genero, data = dados)
modelo_anova2 <- aov(gasto_total ~ factor(idade) + factor(genero), data = dados)
anova(modelo_anova)
anova(modelo_anova2)


rm(modelo_anova)
rm(modelo_anova2)

# -> Resposta: No modelo_anova ambas as variáveis (idade e genero) o valor-p é menor que 0.05, portanto há um efeito significativo na
#              combinação de idade e gênero nos custos hospitalares.
#              No modelo_anova2 apenas a variável idade tem valor-p superior a 0.05, portanto não há um efeito significativo da idade
#              e gênero nos custos hospitalares.




##  6) Como o tempo de permanência é um fator crucial para pacientes internados, desejamos descobrir se o tempo 
#      de permanência pode ser previsto a partir de idade, gênero e raça.

# Dividindo os dados em treino e teste
set.seed(150)
indices <- createDataPartition(dados$tempo_permanencia, p = 0.80, list = FALSE)  
dados_treino <- dados[indices, ]
dados_teste <- dados[-indices, ]
rm(indices)


#  H0: Não há relação linear entre variáveis dependente e independentes.
#  H1: Há relação linear entre variáveis dependente e independentes.

# Criando Modelo Preditivo
modelo <- lm(tempo_permanencia ~ idade + genero + tipo_raca, data = dados_treino)
summary(modelo)


rm(modelo)

# -> Resposta: O modelo de regressão linear atual, usando idade, gênero e raça como preditores, obteve um valor de R-squared muito baixo e 
#              um valor de p abaixo de 0.05 e com isso não demonstrou significância estatística e portanto possui baixo poder preditivo




##  7) Quais variáveis têm maior impacto nos custos de internação hospitalar?

# Criando Modelo Para Seleção De Variáveis
modelo <- randomForest(gasto_total ~ ., data = dados, 
                       ntree = 100, nodesize = 10, importance = T)

print(modelo$importance)
varImpPlot(modelo)
importancia_ordenada <- modelo$importance[order(-modelo$importance[, 1]), , drop = FALSE] 
df_importancia <- data.frame(
  Variavel = rownames(importancia_ordenada),
  Importancia = importancia_ordenada[, 1]
)
ggplot(df_importancia, aes(x = reorder(Variavel, -Importancia), y = Importancia)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Importância das Variáveis", x = "Variável", y = "Importância") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))


rm(modelo)
rm(importancia_ordenada)
rm(df_importancia)
# -> Resposta: As variáveis com maior impacto nos custos de internação hospitalar são tempo_permanencia e tipo_diagnostico



## Modelo Ideal (para verificar quais variáveis tem maior impacto nos custos de internação hospitalares)

modelo_random <- randomForest(gasto_total ~ tempo_permanencia + tipo_diagnostico, data = dados_treino, 
                              ntree = 100, nodesize = 10)
print(modelo_random)

modelo_lm <- lm(gasto_total ~ tempo_permanencia + tipo_diagnostico, data = dados_treino)
summary(modelo_lm)


## Comparando eficácia dos modelos

# Erro Quadrático Médio (MSE)
predicoes_random <- predict(modelo_random, newdata = dados_teste)
mse_random <- mean((dados_teste$gasto_total - predicoes_random)^2)
print(paste("Erro Quadrático Médio (MSE) - Random Forest:", round(mse_random, 2)))

predicoes_lm <- predict(modelo_lm, newdata = dados_teste)
mse_lm <- mean((dados_teste$gasto_total - predicoes_lm)^2)
print(paste("Erro Quadrático Médio (MSE) - Regressão Linear:", round(mse_lm, 2)))


# R-squared (R²)
rsquared_random <- 1 - mse_random / var(dados_teste$gasto_total) 
print(paste("R-squared (R²) - Random Forest:", rsquared_random))  # 0.71 

rsquared_lm <- summary(modelo_lm)$r.squared
print(paste("R-squared (R²) - Regressão Linear:", rsquared_lm))   # 0.42


# Validação Cruzada

# Defina o controle para 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Realize a validação cruzada para o modelo Random Forest
cv_results_random <- train(gasto_total ~ tempo_permanencia + tipo_diagnostico, data = dados,
                           method = "rf", trControl = ctrl)

# Realize a validação cruzada para o modelo de Regressão Linear
cv_results_lm <- train(gasto_total ~ tempo_permanencia + tipo_diagnostico, data = dados,
                       method = "lm", trControl = ctrl)

# Exiba os resultados da validação cruzada
print("Resultados da Validação Cruzada - Random Forest:")
print(cv_results_random)

print("Resultados da Validação Cruzada - Regressão Linear:")
print(cv_results_lm)
