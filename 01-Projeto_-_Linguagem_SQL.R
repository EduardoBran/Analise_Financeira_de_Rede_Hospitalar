####  Projeto (Linguagem SQL) ####

# Configurando o diretório de trabalho
setwd("~/Desktop/DataScience/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/17.Mini-Projeto-1_-_Analise_Exploratoria_de_Dados_Socioeconomicos")
getwd()


## Carregando pacotes
library(sqldf)
library(dplyr)
library(dbplyr)


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


# verificando e removendo valores ausentes nas colunas
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


