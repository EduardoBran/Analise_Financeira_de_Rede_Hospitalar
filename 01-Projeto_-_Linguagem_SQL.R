####  Projeto (Linguagem SQL) ####

# Configurando o diretório de trabalho
setwd("~/Desktop/DataScience/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/18.Mini-Projeto-2_-_Analise_Financeira_com_Linguagem_SQL_e_Regressao_Linear_em_R")
getwd()


## Carregando pacotes
library(sqldf)
library(dplyr)
library(dbplyr)
library(RMySQL)


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


## Carregando dados
dados <- read.csv("dataset.csv")
head(dados)

## Renomeando Colunas
names(dados) <- c("idade", "genero", "tempo_permanencia", "tipo_raca", "gasto_total", "tipo_diagnostico")
head(dados)


## verificando e removendo valores ausentes nas colunas
colSums(is.na(dados))
dados <- na.omit(dados)



## Conectando ao banco de dados (banco criado utilizando o passo a passo ao final do script)
con <- dbConnect(MySQL(), user = 'eduardo', password = '1234', dbname = 'banco', host = 'localhost', local_infile = TRUE)

# Consultar e mostrar as tabelas
dbGetQuery(con, "SHOW TABLES;")


## Adicionando a tabela csv ao banco de dados (necessario 1 vez)

# Escreve os dados no banco de dados
nome_da_tabela <- 'rede_hospitalar2'
dbWriteTable(con, name = nome_da_tabela, value = dados, overwrite = TRUE)

# Verifica se os dados foram adicionados corretamente
consulta <- dbGetQuery(con, paste("SELECT * FROM", nome_da_tabela))
head(consulta)
rm(consulta)

head(dbGetQuery(con, paste("SELECT idade FROM", nome_da_tabela)))



## Nome da Tabela
nome_da_tabela <- 'rede_hospitalar2'




##  1) Quantas raças estão representadas no dataset? (3 tipos de resposta)

# Tipo 1
query <- paste("SELECT COUNT(DISTINCT tipo_raca) as quantidade_de_racas FROM", nome_da_tabela)
dbGetQuery(con, query)

# Tipo 2 (utilizando pacote dbplyr)
tabela_virtual <- tbl(con, from = nome_da_tabela)    # cria uma tabela virtual com dbplyr
consulta_racas <- tabela_virtual %>%
  summarise(quantidade_de_racas = n_distinct(tipo_raca)) %>%
  collect()

collect(consulta_racas)  # visualiza a consulta

# Tipo 3 (Linguagem R)
length(unique(dados$tipo_raca))


#  -> Resposta: 6 tipos de raça.




##  2) Qual a idade média dos pacientes? (3 tipos de resposta)

# Tipo 1
query <- paste("SELECT AVG(idade) as media_idade FROM", nome_da_tabela)
dbGetQuery(con, query)

# Tipo 2 (utilizando pacote dbplyr)
tabela_virtual <- tbl(con, from = nome_da_tabela)    # cria uma tabela virtual com dbplyr
consulta_media_idade <- tabela_virtual %>%
  summarise(media_idade = mean(idade)) %>%
  collect()

consulta_media_idade  # visualiza a consulta

# Tipo 3 (Linguagem R)
media_idade <- dados %>% 
  summarise(media_idade = mean(idade))

media_idade

#  -> Resposta: Média Idade é de aproximadamente 5.10.




##  3) Qual a moda da idade dos pacientes?

# Tipo 1
query <- paste("SELECT idade, COUNT(idade) as frequencia",
               "FROM", nome_da_tabela,
               "GROUP BY idade",
               "ORDER BY frequencia DESC",
               "LIMIT 1;")
dbGetQuery(con, query)

# Tipo 2 (utilizando pacote dbplyr)
tabela_virtual <- tbl(con, from = nome_da_tabela)    # cria uma tabela virtual com dbplyr
moda_dbplyr <- tabela_virtual %>%
  group_by(idade) %>%
  summarise(frequencia = n()) %>%
  slice_max(order_by = frequencia, n = 1) %>%
  select(idade) %>%
  collect() %>%
  pull()

moda_dbplyr

# Tipo 3 (Linguagem R)
tabela_contagem <- table(dados$idade)  # # Calcular a tabela de contagem
moda <- as.numeric(names(tabela_contagem[tabela_contagem == max(tabela_contagem)]))
moda

#  -> Resposta: Moda Idade é de 0.




##  4) Qual a variância da coluna idade?

# Tipo 1
query_variancia <- paste("SELECT VARIANCE(idade) as variancia_idade FROM", nome_da_tabela)
dbGetQuery(con, query_variancia)

# Tipo 2 (utilizando pacote dbplyr)
tabela_virtual <- tbl(con, from = nome_da_tabela)    # cria uma tabela virtual com dbplyr
consulta_variancia <- tabela_virtual %>%
  summarise(variancia_idade = var(idade)) %>%
  collect()
consulta_variancia

# Tipo 3 (Linguagem R)
variancia <- dados %>%
  summarise(variancia = var(idade, na.rm = TRUE))
variancia

#  -> Resposta: Variância de Idade é de aproximadamente 48.3.




##  5) Qual o gasto total com internações hospitalares por idade?

# Tipo 1
query <- paste("SELECT idade, SUM(gasto_total) as total_por_idade",
               "FROM", nome_da_tabela,
               "GROUP BY idade;")
dbGetQuery(con, query)

# Tipo 2 (utilizando pacote dbplyr)
tabela_virtual <- tbl(con, from = nome_da_tabela)    # cria uma tabela virtual com dbplyr
consulta_gasto_idade <- tabela_virtual %>%
  group_by(idade) %>% 
  summarise(total_por_idade = sum(gasto_total)) %>%
  collect()
consulta_gasto_idade

# Tipo 3 (Linguagem R)
gasto_total_idade <- dados %>% 
  group_by(idade) %>% 
  summarise(total_por_idade = sum(gasto_total))

gasto_total_idade




##  6) Qual idade gera o maior gasto total com internações hospitalares? (utiliza dataset do exercicio 5)

# Tipo 1
query_idade_max_gasto <- paste("SELECT idade, SUM(gasto_total) as total_por_idade",
                               "FROM", nome_da_tabela,
                               "GROUP BY idade",
                               "ORDER BY total_por_idade DESC",
                               "LIMIT 1;")
dbGetQuery(con, query_idade_max_gasto)

# Tipo 2 (utilizando pacote dbplyr)
tabela_virtual <- tbl(con, from = nome_da_tabela)    # cria uma tabela virtual com dbplyr
idade_max_gasto_dbplyr <- tabela_virtual %>%
  group_by(idade) %>% 
  summarise(total_por_idade = sum(gasto_total)) %>%
  filter(total_por_idade == max(total_por_idade)) %>%
  collect() %>%
  select(idade, total_por_idade)
idade_max_gasto_dbplyr

# Tipo 3 (Linguagem R)
idade_com_mais_gasto <- gasto_total_idade %>% 
  filter(total_por_idade == max(total_por_idade)) %>%
  select(idade, total_por_idade)
idade_com_mais_gasto




##  7) Qual o gasto total com internações hospitalares por gênero?

# Tipo 1
query <- paste("SELECT genero, SUM(gasto_total) as total_por_genero",
               "FROM", nome_da_tabela,
               "GROUP BY genero;")
dbGetQuery(con, query)

# Tipo 2 (utilizando pacote dbplyr)
tabela_virtual <- tbl(con, from = nome_da_tabela)    # cria uma tabela virtual com dbplyr
consulta_gasto_genero <- tabela_virtual %>%
  group_by(genero) %>% 
  summarise(total_por_genero = sum(gasto_total)) %>%
  collect()
consulta_gasto_genero

# Tipo 3 (Linguagem R)
gasto_total_genero <- dados %>% 
  group_by(genero) %>% 
  summarise(total_por_genero = sum(gasto_total))

gasto_total_genero




##  8) Qual a média de gasto com internações hospitalares por raça do paciente?

# Tipo 1
query <- paste("SELECT tipo_raca, AVG(gasto_total) as media_gasto_raca",
               "FROM", nome_da_tabela,
               "GROUP BY tipo_raca;")
dbGetQuery(con, query)

# Tipo 2 (utilizando pacote dbplyr)
tabela_virtual <- tbl(con, from = nome_da_tabela)    # cria uma tabela virtual com dbplyr
consulta_gasto_raca <- tabela_virtual %>%
  group_by(tipo_raca) %>% 
  summarise(media_por_raca = mean(gasto_total)) %>%
  collect()
consulta_gasto_raca

# Tipo 3 (Linguagem R)
media_gasto_raca <- dados %>% 
  group_by(tipo_raca) %>% 
  summarise(media_por_raca = mean(gasto_total))
media_gasto_raca


head(dados)




#  9) Para pacientes acima de 10 anos, qual a média de gasto total com internações hospitalares?

# Tipo 1
query_media_gasto_acima_10y <- paste("SELECT AVG(gasto_total) as media_gasto_acima_10y",
                                     "FROM", nome_da_tabela,
                                     "WHERE idade > 10;")
dbGetQuery(con, query_media_gasto_acima_10y)

# Tipo 2
tabela_virtual <- tbl(con, from = nome_da_tabela)  # cria uma tabela virtual com dbplyr
media_gasto_acima_10y_dbplyr <- tabela_virtual %>%
  filter(idade > 10) %>%
  summarise(media_gasto_acima_10y = mean(gasto_total)) %>%
  collect()
media_gasto_acima_10y_dbplyr

# Tipo 3 (Linguagem R)
media_gasto_acima_10y <- dados %>% 
  filter(idade > 10) %>% 
  summarise(media_gasto_acima_10y = mean(gasto_total))
media_gasto_acima_10y




#  10) Considerando a pergunta 10, qual idade(s) tem média de gastos superior a 3000 ?

# Tipo 1
query_media_gasto_acima_3000 <- paste("SELECT idade, AVG(gasto_total) as media_gasto",
                                      "FROM", nome_da_tabela,
                                      "GROUP BY idade",
                                      "HAVING media_gasto > 3000",
                                      "ORDER BY media_gasto DESC;")
dbGetQuery(con, query_media_gasto_acima_3000)

# Tipo 2 (utilizando pacote dbplyr)
tabela_virtual <- tbl(con, from = nome_da_tabela)
consulta_media_gasto <- tabela_virtual %>%
  group_by(idade) %>%
  summarise(media_gasto = mean(gasto_total)) %>%
  filter(media_gasto > 3000) %>%
  collect()
consulta_media_gasto

# Tipo 3 (Linguagem R)
media_gasto_idade_acima_3000 <- dados %>%
  group_by(idade) %>%
  summarise(media_gasto = mean(gasto_total)) %>%
  filter(media_gasto > 3000)
media_gasto_idade_acima_3000







#### Configurar um ambiente MySQL no seu sistema Ubuntu. Siga estes passos:

## Acesso ao MySQL:
#   Acesse o MySQL usando o seguinte comando:
# sudo mysql -u root -p
# Digite a senha que você definiu durante a instalação.

## Criação de um Banco de Dados e um Usuário:
#   Dentro do MySQL, crie um banco de dados e um usuário para seu projeto:

# CREATE DATABASE nome_do_banco;
# CREATE USER 'eduardo' IDENTIFIED BY '1234';
# GRANT ALL PRIVILEGES ON banco* TO 'eduardo';
# FLUSH PRIVILEGES;

# use nome_do_banco;

## Sair do MySQL:
#   Saia do MySQL usando:
# exit;

## RMySQL Configuração:
#   De volta ao RStudio, agora você pode configurar a conexão usando o código anterior, substituindo 'seu_usuario',
#   'sua_senha', 'nomedoBancoDeDados' e 'localhost' pelos valores que você definiu no MySQL.

# library(RMySQL)
# con <- dbConnect(MySQL(), user = 'seu_usuario', password = 'sua_senha', dbname = 'nomedoBancoDeDados', host = 'localhost')

# Esse processo deve criar uma tabela no banco de dados MySQL e permitir que você acesse os dados por meio da conexão con no RStudio.
# Certifique-se de adaptar todos os detalhes de acordo com as configurações específicas do seu ambiente.




