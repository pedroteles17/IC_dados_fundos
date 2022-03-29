library(tidyverse)
library(readxl)
library(xts)
library(tidyr)
library(taxonomizr) #LastNotNa

# Cria a funcao 'limpa base de dados' que vai receber uma 'base de dados suja' e fazer sua limpeza:
## Definir corretamente o nome de cada coluna
## Elimninar linhas e colunas desncessarias
## Transformar a coluna de datas em um objeto da classe Date
## Garantir que os valores numericos tenham o tipo (numeric) correto
limpa_bd <- function(bd_suja){
  ## Selecionamos apenas o numero identificador de cada fundo
  nome_cols <- do.call('c', lapply(str_split(bd_suja[3,-1], pattern = "\n"), function(x) x[length(x)]))
  nome_cols <- c('Data', nome_cols)
  
  colnames(bd_suja) <- nome_cols
  
  bd_suja <- bd_suja[-1:-3, -2] # Linhas 1 a 3 e coluna 2 nao possuem dados
  
  ## A coluna de datas esta um valores numericos. Passamos para data
  bd_suja$Data <- as.Date(as.numeric(bd_suja$Data), origin = "1899-12-30")
  
  ## Garantimos que todas as colunas, menos a de data, esteja na classe numerica
  bd_suja[,-1] <- as.data.frame(apply(bd_suja[,-1], 2, as.numeric))
  
  ## Apos esses passos, temos uma base de dados limpa
  bd_limpa <- bd_suja
  
  return(bd_limpa)
}

# Como descrito no README, temos tres bases de dados distintas que se referem ao mesmo indicador
## Para evitar a repeticao desnecessaria de codigo, criamos uma funcao que limpa as bases (utilizando a funcao acima) e junta todas elas.
## Essa funcao recebe a base de dados dos fundos ativos, dos cancelados e dos 10 extras e retorna uma unica df com todos os fundos
junta_bd <- function(nome_indicador, fundos_10 = NULL){
  ## Importamos dados dos fundos ativos
  fundos_ativos <- read_excel(paste0('rawData/Dados/', nome_indicador, '_ativos.xlsx'), na = '-')
  
  ## Importamos dados dos fundos cancelados
  fundos_cancelados <- read_excel(paste0('rawData/Dados/', nome_indicador, '_cancelados.xlsx'), na = '-')
  
  ### Limpamos a base de dados dos fundos ativos
  fundos_ativos <- limpa_bd(fundos_ativos)
  
  ### Limpamos a base de dados dos fundos cancelados
  fundos_cancelados <- limpa_bd(fundos_cancelados)
  
  ### Apenas uma verificacao de qualidade da limpeza de dados
  print(intersect(colnames(fundos_ativos), colnames(fundos_cancelados)))
  
  ## Criamos uma df unica com os dados de captacao liquida de todos os fundos
  fundos <- merge(merge(fundos_ativos, fundos_cancelados, by = 'Data', all = TRUE), fundos_10, by = 'Data', all = TRUE)
  
  return(fundos)
}

# Dados 10 ----

## Importamos dados para os 10 fundos que restaram
dados_10 <- read_excel('rawData/Dados/dados_10.xlsx', na = '-')

lista_dados_10 <- vector('list', length = 6)
for (i in 1:6) {
  lista_dados_10[[i]] <- dados_10[, c(1, (2 + 11 * (i - 1)):(1 + i*11))]
}

dados_10 <- lapply(lista_dados_10, limpa_bd)
names(dados_10) <- c('cota', 'pl', 'capt_liq', 'capt', 'resgate', 'n_cotistas')

rm(lista_dados_10)

# Cota 
cota <- junta_bd('cota', dados_10[['cota']])

# Captacao Liquida 
capt_liq <- junta_bd('capt_liq', dados_10[['capt_liq']])

# Resgate
resgate <- junta_bd('resgate', dados_10[['resgate']])

# Captacao
captacao <- junta_bd('capt', dados_10[['capt']])

# Numero de Cotistas
n_cotistas <- junta_bd('num_cotistas', dados_10[['n_cotistas']])

# Patrimonio Liquido
patrim_liq <- junta_bd('pl', dados_10[['pl']])

# Taxa de Administracao ----
## A Economatica nao possuia dados de tx de adm para os 10 ativos. Assim, fazemos o merge apenas baseado nos fundos ativos e cancelados
## Alem disso, nessa bd o separador decimal e a virgula. Corrigimos isso

## Importamos dados da tx de adm dos fundos ativos
tx_adm_ativos <- read_excel('rawData/Dados/tx_adm_ativos.xlsx', na = '-')
tx_adm_ativos <- as.data.frame(apply(tx_adm_ativos, 2, function(x) gsub(",", ".", gsub("\\.", "", x))))

## Importamos dados dos fundos cancelados
tx_adm_cancelados <- read_excel('rawData/Dados/tx_adm_cancelados.xlsx', na = '-')
tx_adm_cancelados <- as.data.frame(apply(tx_adm_cancelados, 2, function(x) gsub(",", ".", gsub("\\.", "", x))))

### Limpamos a base de dados dos fundos ativos
tx_adm_ativos <- limpa_bd(tx_adm_ativos)

### Limpamos a base de dados dos fundos cancelados
tx_adm_cancelados <- limpa_bd(tx_adm_cancelados)

### Apenas uma verificacao de qualidade da limpeza de dados
print(intersect(colnames(tx_adm_ativos), colnames(tx_adm_cancelados)))

## Criamos uma df unica com os dados de captacao liquida de todos os fundos
tx_adm <- merge(tx_adm_ativos, tx_adm_cancelados, by = 'Data', all = TRUE)

rm(tx_adm_ativos, tx_adm_cancelados)

# Codigo e Dados Cadastrais ----
## Dataframe contendo o codigo de cada fundo na base da economatica
codigo_ativos <- read_excel('rawData/Dados/codigo_ativos.xlsx', na = '-') %>% dplyr::select(-1)
colnames(codigo_ativos) <- codigo_ativos[3,]
codigo_ativos <- codigo_ativos[-1:-3, ]

codigo_cancelados <- read_excel('rawData/Dados/codigo_cancelados.xlsx', na = '-') %>% dplyr::select(-1)
colnames(codigo_cancelados) <- codigo_cancelados[3,]
codigo_cancelados <- codigo_cancelados[-1:-3, ]

codigo <- rbind(codigo_ativos, codigo_cancelados)
colnames(codigo)[9] <- 'Codigo'
codigo <- codigo %>% dplyr::select(CNPJ, Codigo) # Selecionamos apenas essas duas colunas para facilitar o merge
rm(codigo_ativos, codigo_cancelados)

## Dataframe contendo os dados cadastrais de cada fundo
dados_cadast <- read_excel('rawData/Dados/dados_cadastrais.xlsx', na = '-') %>% dplyr::select(-1)
colnames(dados_cadast) <- dados_cadast[3,]
dados_cadast <- dados_cadast[-1:-3, -2] # Eliminamos a 2 coluna dado que ela esta repetida

## Merge das duas bases com dados cadastrais
dados_cadast <- merge(dados_cadast, codigo, by = 'CNPJ')

dados_cadast <- dplyr::distinct(dados_cadast, Codigo, .keep_all = TRUE)

colnames(dados_cadast) <- c('cnpj', 'nome', 'pais_sede', 'tipo_ativo', 'ativo_cancelado',
                            'classific_anbima', 'gestor_carteira', 'empresa_gestora',
                            'administradora', 'benchmark', 'invest_qualificado', 'alavancado',
                            'data_inicio', 'data_fim', 'prazo_emis_cota', 'prazo_conv_resg',
                            'prazo_pag_resg', 'aplic_min_inic', 'situac_atual', 'data_inicio_situac_atual', 
                            'classe', 'forma_condominio', 'fundo_cotas', 'fundo_exclusivo',
                            'fundo', 'classific_cvm', 'subclasse_cvm', 'codigo')

# A base de dados cadastrais possuem algumas falhas. Corrigimos elas abaixo
dados_cadast$data_inicio <- as.Date(as.numeric(dados_cadast$data_inicio), origin = "1899-12-30")
dados_cadast$data_fim <- as.Date(as.numeric(dados_cadast$data_fim), origin = "1899-12-30")
dados_cadast$data_inicio_situac_atual <- as.Date(as.numeric(dados_cadast$data_inicio_situac_atual), origin = "1899-12-30")

dados_cadast$prazo_emis_cota <- replace(dados_cadast$prazo_emis_cota, dados_cadast$prazo_emis_cota == 'Até 12h, D0; depois disso, D+1', 'D+001')
dados_cadast$prazo_emis_cota <- replace(dados_cadast$prazo_emis_cota, dados_cadast$prazo_emis_cota == 'D=0', 'D+000')
dados_cadast$prazo_emis_cota <- replace(dados_cadast$prazo_emis_cota, dados_cadast$prazo_emis_cota == 'd=0', 'D+000')

dados_cadast$prazo_conv_resg <- replace(dados_cadast$prazo_conv_resg, dados_cadast$prazo_conv_resg == 'Até 12h, D0; depois disso, D+1', 'D+001')
dados_cadast$prazo_conv_resg <- replace(dados_cadast$prazo_conv_resg, dados_cadast$prazo_conv_resg == 'D+30 dias corridos', 'D+030')
dados_cadast$prazo_conv_resg <- replace(dados_cadast$prazo_conv_resg, dados_cadast$prazo_conv_resg == 'D=1', 'D+001')

dados_cadast$prazo_pag_resg <- replace(dados_cadast$prazo_pag_resg, dados_cadast$prazo_pag_resg == '2 dias úteis da conversão', 'D+002')
dados_cadast$prazo_pag_resg <- replace(dados_cadast$prazo_pag_resg, dados_cadast$prazo_pag_resg == '4 dias0', 'D+004')
dados_cadast$prazo_pag_resg <- replace(dados_cadast$prazo_pag_resg, dados_cadast$prazo_pag_resg == 'D=4', 'D+004')

dados_cadast$benchmark <- replace(dados_cadast$benchmark, dados_cadast$benchmark == 'IBRX', 'IBRX-100')

# OBS: unique(dados_cadast$classe) retorna Fundo Multimercado e Fundo de Renda Fixa tambem

# Eliminamos a lista com os dados dos 10 fundos
rm(dados_10)

# Mensalizacao dos dados ----

# Temos alguns tipos de dados que precisamos somar os valores de cada mes e outros que precisamos apenas do ultimo valor
lista_soma <- list(capt_liq, captacao, resgate)
lista_ultimo <- list(cota, n_cotistas, patrim_liq, tx_adm)

# Transformamos o dado de diario para mensal somando os valores dentro de cada mes
nome_valores <- c('capt_liq', 'captacao', 'resgate')
for (i in seq_along(lista_soma)) {
  df <- lista_soma[[i]]
  
  df <- xts(df[,-1], df$Data)
  
  # Para cada coluna aplicamos a funcao apply.monthly
  df_mes <- do.call('cbind', lapply(df, function(x) apply.monthly(x, function(y) sum(y, na.rm = TRUE))))
  
  df_mes <- data.frame(Data = index(df_mes), df_mes)
  
  names(df_mes) <- sapply(str_remove_all(colnames(df_mes),"X"),"[")
  
  df_mes[df_mes == 0] <- NA
  
  df_mes <- df_mes %>% pivot_longer(!Data, names_to = 'Fundo', values_to = nome_valores[i])
  
  # Atualizamos nossa lista com o valor mensal
  lista_soma[[i]] <- df_mes
}

# Transformamos o dado de diario para mensal selecionando o ultimo valor dentro de cada mes
nome_valores <- c('cota', 'n_cotistas', 'patrim_liq', 'tx_adm')
for (i in seq_along(lista_ultimo)) {
  df <- lista_ultimo[[i]]
  
  df <- xts(df[,-1], df$Data)
  
  # Para cada coluna aplicamos a funcao apply.monthly para selecionar a ultima observacao de cada mes diferente de NA
  df_mes <- do.call('cbind', lapply(df, function(x) apply.monthly(x, function(y) lastNotNa(y, NA))))
  
  df_mes <- data.frame(Data = index(df_mes), df_mes)
  
  names(df_mes) <- sapply(str_remove_all(colnames(df_mes),"X"),"[")
  
  df_mes <- df_mes %>% pivot_longer(!Data, names_to = 'Fundo', values_to = nome_valores[i])
  
  # Atualizamos nossa lista com o valor mensal
  lista_ultimo[[i]] <- df_mes
}

# Temos 2 listas com nossos dados. Primeiro, fazemos merge dentro de cada lista. Por fim, fazemos o merge das duas dfs
dados1 <- lista_soma %>% reduce(full_join, by = c('Data', 'Fundo'))
dados2 <- lista_ultimo %>% reduce(full_join, by = c('Data', 'Fundo'))

dados <- merge(dados1, dados2, by = c('Data', 'Fundo'), all = TRUE)

dados_diarios <- list(capt_liq, captacao, cota, n_cotistas, patrim_liq, resgate, tx_adm)

# Eliminamos listas e dfs
rm(df_mes, df, dados1, dados2, lista_soma, lista_ultimo, codigo, capt_liq, captacao, cota, n_cotistas, patrim_liq, resgate, tx_adm)

# Eliminamos valores e funcoes
rm(i, nome_valores, junta_bd, limpa_bd)

# Eliminamos a lista com os dados diarios. Caso voce queira os dados diarios, basta nao rodar a linha abaixo
#rm(dados_diarios)

save.image(file = 'dados_tratados.RData')

#load('dados_tratados.RData')
