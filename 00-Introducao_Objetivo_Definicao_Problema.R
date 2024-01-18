####  Objetivo e Definição do Problema de Negócio  ####

# Configurando o diretório de trabalho
setwd("~/Desktop/DataScience/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/18.Mini-Projeto-2_-_Analise_Financeira_com_Linguagem_SQL_e_Regressao_Linear_em_R")
getwd()


#### Análise Financeira de uma Rede Hospitalar


## Objetivo

# - O objetivo deste mini projeto é unir duas poderosas ferramentas de análise de dados para resolver um problema
#   de negócio:

#  -> Linguagem SQL - Será usada para análise exploratória de dados através do pacote sqldf
#  -> Linguagem R   - Será usada para análise estatística


## Definição do Problema de Negócio e Coleta de Dados

# - Uma rede de hospitais gostaria de compreender as variáveis relacionadas aos gastos com internações hospitalares
#  de pacientes.

# - Usaremos dados de uma pesquisa nacional de custos hospitalares realizada pela US Agency for Healthcare que consiste
#   em registros hospitalares de amostras de pacientes internados. Os dados fornecidos são restritos à cidade de Wisconsin
#   e referem-se a pacitens na faixa etária de 0 a 17 anos.

# - Vamos separar o trabalho em duas etapas:

#  -> Etapa 1 - Exploração dos dados utilizando Lingaugem SQL e responder 10 perguntas de negócio.
#  -> Etapa 2 - Realização de análise estatística com Linguagem R através do Teste ANOVA e Regressão Linear +
#               responder 7 perguntas de negócio.

#  -> Link da fonte de dados: https://healthdata.gov/


## Perguntas de Negócio (Linguagem SQL)

# 1-  Quantas raças estão representadas no dataset?
# 2-  Qual a idade média dos pacientes?
# 3-  Qual a moda da idade dos pacientes?
# 4-  Qual a variância da coluna idade?
# 5-  Qual o gasto total com internações hospitalares por idade?
# 6-  Qual idade gera o maior gasto total com internações hospitalares?
# 7-  Qual o gasto total com internações hospitalares por gênero?
# 8-  Qual a média de gasto com internações hospitalares por raça do paciente?
# 9-  Para pacientes acima de 10 anos, qual a média de gasto total com internações hospitalares?
# 10- Considerando a pergunta 10, qual idade tem média de gastos superior a 3000 ?



## Perguntas de Negócio (Linguagem R)

# 1-  Qual a distribuição da idade dos pacientes que frequentam o hospital?
# 2-  Qual faixa etária tem o maior gasto total no hospital?
# 3-  Qual grupo baseado em diagnóstico (Aprdrg) tem o maior gasto total no hospital?
# 4-  A raça do paciente tem relação com o total gasto em internações no hospital?
# 5-  A combinação de idade e gênero dos pacientes influencia no gasto total em internações no hospital?
# 6-  Como o tempo de permanência é o fator crucial para pacientes internados, desejamos descobrir se o tempo 
#     de permanência pode ser previsto a partir de idade, gênero e raça.
# 7-  Quais variáveis têm maior impacto nos custos de internação hospitalar?








# Projeto utilizando as linguagems SQL e R para realizar uma análise financeira de dados de uma Rede Hospitalar


