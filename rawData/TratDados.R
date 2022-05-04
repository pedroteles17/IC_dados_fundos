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

# Jutamos os dados diários em uma lista
dados_diarios <- list(capt_liq, captacao, cota, n_cotistas, patrim_liq, resgate, tx_adm)
names(dados_diarios) <- c('capt_liq', 'capt', 'cota', 'n_cotistas', 'patrim_liq', 'resg', 'tx_adm')

##################################################################
##                     Calculo Retorno Cota                     ##
##################################################################

# Garantir que as datas sao as mesmas que o indice de mercado
ind_rf <- read.csv('ind_rf.csv')
ind_rf$Data <- as.Date(ind_rf$Data)

prices <- dados_diarios[['cota']]
## If an asset hasn't any price data, we eliminate it from our database
prices <- prices[, colSums(is.na(prices)) != nrow(prices)]

## We iterate to fill the NAs in the middle of the sample
prices_locf <- data.frame(matrix(ncol = ncol(prices) - 1, nrow = nrow(prices)))
for (i in 2:ncol(prices)) {
  ind_date <- prices[, 1, drop = FALSE]
  # Select only the date column and the asset in position i
  suport1 <- prices[, c(1, i)]
  # Create a vector that informs the position of the non NA observations
  NonNAindex <- which(!is.na(suport1[, 2]))
  # Filter so we can work only with the dates before an asset, possibly, delists.
  # This will avoid problems with the na.locf function
  suport2 <- suport1[1:max(NonNAindex), ]
  # Use the na.locf function to replace NAs with the last available information
  suport3 <- na.locf(suport2)
  # Add the asset prices to the data frame using it's date column
  prices_locf[[i - 1]] <- merge(ind_date, suport3, by = "Data", all.x = TRUE)[, 2]
}

prices_locf <- merge(ind_rf[, 1, drop = FALSE], prices_locf, by = "Data", all.x = TRUE)

## Calculate assets returns from the price data
returns <- as.data.frame(lapply(prices_locf, function(x) diff(x) / x[-length(x)])) %>%
  set_names(colnames(prices)[-1]) %>%
  dplyr::mutate(Data = prices$Data[-1], .before = 1)

dados_diarios[['cota']] <- returns

##################################################################
##                 Preencher Patrimônio Líquido                 ##
##################################################################

## If an asset hasn't any price data, we eliminate it from our database
## Select price data only when there is market index data
pl <- dados_diarios[['patrim_liq']]
pl <- pl[, colSums(is.na(pl)) != nrow(pl)]

## We iterate to fill the NAs in the middle of the sample
pl_locf <- data.frame(matrix(ncol = ncol(pl) - 1, nrow = nrow(pl)))
for (i in 2:ncol(pl)) {
  ind_date <- pl[, 1, drop = FALSE]
  # Select only the date column and the asset in position i
  suport1 <- pl[, c(1, i)]
  # Create a vector that informs the position of the non NA observations
  NonNAindex <- which(!is.na(suport1[, 2]))
  # Filter so we can work only with the dates before an asset, possibly, delists.
  # This will avoid problems with the na.locf function
  suport2 <- suport1[1:max(NonNAindex), ]
  # Use the na.locf function to replace NAs with the last available information
  suport3 <- na.locf(suport2)
  # Add the asset pl to the data frame using it's date column
  pl_locf[[i - 1]] <- merge(ind_date, suport3, by = "Data", all.x = TRUE)[, 2]
}

pl <- pl_locf %>%
  set_names(colnames(pl)[-1]) %>%
  dplyr::mutate(Data = pl$Data, .before = 1) 

pl <- merge(ind_rf[, 1, drop = FALSE], pl, by = "Data", all.x = TRUE)

dados_diarios[['patrim_liq']] <- pl


##################################################################
##          Captacao, Resgate e Captacao Liquida                ##
##################################################################

test_capt_liq <- dados_diarios[['capt_liq']]

for (flow_type in c('capt', 'resg', 'capt_liq')) {
  ## If an asset hasn't any price data, we eliminate it from our database
  flow <- test_capt_liq
  flow <- flow[, colSums(is.na(flow)) != nrow(flow)]
  
  ## We iterate to fill the NAs in the middle of the samflowe
  flow_locf <- data.frame(matrix(ncol = ncol(flow) - 1, nrow = nrow(flow)))
  for (i in 2:ncol(flow)) {
    ind_date <- flow[, 1, drop = FALSE]
    # Select only the date column and the asset in position i
    suport1 <- flow[, c(1, i)]
    # When the fund starts and ends?
    fund_number <- colnames(suport1)[2]
    datas <- dados_cadast %>% dplyr::filter(codigo == fund_number) %>% dplyr::select(data_inicio, data_fim)
    data_inicio <- as.Date(datas$data_inicio)
    data_fim <- as.Date(datas$data_fim)
    data_fim <- if(is.na(data_fim)) suport1$Data[length(suport1$Data)] else data_fim
    # We need one day before the start of the fund to ensure that the firts inflow is considered (cumsum)
    if(data_inicio >= "2000-01-01"){
      one_before_begin_date <- ind_date %>% dplyr::filter(Data < data_inicio)
      data_inicio <- one_before_begin_date$Data[length(one_before_begin_date$Data)]
    }
    # Filter so we can work only with the dates before an asset delists.
    suport1 <- suport1 %>% dplyr::filter(Data >= data_inicio & Data <= data_fim)
    # Use the na.locf function to reflowace NAs with the last available information
    suport1[is.na(suport1)] <- 0
    # soma cumulativa para depois fazermos o merge com as datas do indice
    suport1[,2] <- cumsum(suport1[,2])
    # Add the asset flow to the data frame using it's date column
    flow_locf[[i - 1]] <- merge(ind_date, suport1, by = "Data", all.x = TRUE)[, 2]
  }
  
  flow_locf <- flow_locf %>%
    set_names(colnames(flow)[-1]) %>%
    dplyr::mutate(Data = flow$Data, .before = 1) 
  
  flow_locf <- merge(ind_rf[, 1, drop = FALSE], flow_locf, by = "Data", all.x = TRUE)
  
  flow_locf[,-1] <- lapply(flow_locf[,-1], function(x) append(NA, diff(x)))
  
  dados_diarios[[flow_type]] <- flow_locf
}

# Salvamos os dados tratados. Para ler: readRDS('dados_tratados_mes.rds')
saveRDS(dados_cadast, file = 'dados_cadastrais.rds')
saveRDS(dados_diarios, file = 'dados_tratados_diarios.rds')
