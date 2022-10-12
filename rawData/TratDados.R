library(tidyverse)
library(readxl)
library(xts)
library(tidyr)

fill_na <- function(x){
  last_non_na <- max(which(!is.na(x)))
  
  x_filled <- na.locf(x[1:max(which(!is.na(x)))], na.rm = FALSE)
  
  x_filled <- append(x_filled, rep(NA, length(x) - last_non_na))
  
  return(x_filled)
}

# Clean the raw database
limpa_bd <- function(bd_suja) {
  
  ## We select only the funds' code (last word in the phrase)
  nome_cols <- do.call("c", lapply(str_split(colnames(bd_suja)[-1], pattern = "\n"), function(x) x[length(x)]))

  bd_limpa <- bd_suja %>% 
    set_names(c("Data", nome_cols)) %>% # Change column names
    select(!2) %>% # Drop empity column
    mutate(across(everything(), ~as.numeric(.x))) %>%  # Everything as numeric 
    mutate(Data = as.Date(Data, origin="1899-12-30")) # Date as date 

  return(bd_limpa)
}

# Como descrito no README, temos tres bases de dados distintas que se referem ao mesmo indicador
## Para evitar a repeticao desnecessaria de codigo, criamos uma funcao que limpa as bases (utilizando a funcao acima) e junta todas elas.
## Essa funcao recebe a base de dados dos fundos ativos, dos cancelados e dos 10 extras e retorna uma unica df com todos os fundos
junta_bd <- function(nome_indicador, fundos_10 = NULL) {
  ## Importamos dados dos fundos ativos
  fundos_ativos <- read_excel(paste0("rawData/Dados/", nome_indicador, "_ativos.xlsx"), na = "-", skip = 3, col_types = 'text')

  ## Importamos dados dos fundos cancelados
  fundos_cancelados <- read_excel(paste0("rawData/Dados/", nome_indicador, "_cancelados.xlsx"), na = "-", skip = 3, col_types = 'text')

  ### Limpamos a base de dados dos fundos ativos
  fundos_ativos <- limpa_bd(fundos_ativos)

  ### Limpamos a base de dados dos fundos cancelados
  fundos_cancelados <- limpa_bd(fundos_cancelados)
  
  ## Criamos uma df unica 
  fundos <- merge(fundos_ativos, fundos_cancelados, by = "Data", all = TRUE)

  if(!is.null(fundos_10)){
    fundos <- merge(fundos, fundos_10, by = "Data", all = TRUE)
  }
  
  return(fundos)
}

# Dados 10 ----

## Importamos dados para os 10 fundos que restaram
dados_10 <- read_excel("rawData/Dados/dados_10.xlsx", na = "-", skip = 3,
                       col_types = 'text')

lista_dados_10 <- vector("list", length = 6)
for (i in 1:6) {
  lista_dados_10[[i]] <- dados_10[, c(1, (2 + 11 * (i - 1)):(1 + i * 11))]
}

dados_10 <- lapply(lista_dados_10, limpa_bd)
names(dados_10) <- c("cota", "pl", "capt_liq", "capt", "resgate", "n_cotistas")

rm(lista_dados_10)

# Cota
cota <- junta_bd("cota", dados_10[["cota"]])

# Captacao Liquida
capt_liq <- junta_bd("capt_liq", dados_10[["capt_liq"]])

# Resgate
resgate <- junta_bd("resgate", dados_10[["resgate"]])

# Captacao
captacao <- junta_bd("capt", dados_10[["capt"]])

# Numero de Cotistas
n_cotistas <- junta_bd("num_cotistas", dados_10[["n_cotistas"]])

# Patrimonio Liquido
patrim_liq <- junta_bd("pl", dados_10[["pl"]])

# Taxa de Administracao ----
## A Economatica nao possuia dados de tx de adm para os 10 ativos. Assim, fazemos o merge apenas baseado nos fundos ativos e cancelados
tx_adm <- junta_bd("tx_adm")


#################################################################
##                      Registration Data                      ##
#################################################################

## Dataframe contendo o codigo de cada fundo na base da economatica
codigo_ativos <- read_excel("rawData/Dados/codigo_ativos.xlsx", na = "-", skip = 3) %>% 
  dplyr::select(-1) %>%
  dplyr::select(c('CNPJ', 'Código'))

codigo_cancelados <- read_excel("rawData/Dados/codigo_cancelados.xlsx", na = "-", skip = 3) %>% 
  dplyr::select(-1) %>%
  dplyr::select(c('CNPJ', 'Código'))

codigo <- rbind(codigo_ativos, codigo_cancelados)
colnames(codigo)[2] <- "Codigo"

rm(codigo_ativos, codigo_cancelados)

## Dataframe contendo os dados cadastrais de cada fundo
dados_cadast <- read_excel("rawData/Dados/dados_cadastrais.xlsx", na = "-", skip = 3) %>% 
  dplyr::select(-c(1, 3))

## Merge das duas bases com dados cadastrais
dados_cadast <- merge(dados_cadast, codigo, by = "CNPJ")

dados_cadast <- dplyr::distinct(dados_cadast, Codigo, .keep_all = TRUE)

colnames(dados_cadast) <- c(
  "cnpj", "nome", "pais_sede", "tipo_ativo", "ativo_cancelado",
  "classific_anbima", "gestor_carteira", "empresa_gestora",
  "administradora", "benchmark", "invest_qualificado", "alavancado",
  "data_inicio", "data_fim", "prazo_emis_cota", "prazo_conv_resg",
  "prazo_pag_resg", "aplic_min_inic", "situac_atual", "data_inicio_situac_atual",
  "classe", "forma_condominio", "fundo_cotas", "fundo_exclusivo",
  "fundo", "classific_cvm", "subclasse_cvm", "codigo"
)

# A base de dados cadastrais possuem algumas falhas. Corrigimos elas abaixo
dados_cadast <- dados_cadast %>% 
  mutate(data_inicio = as.Date(data_inicio),
         data_fim = as.Date(data_fim),
         data_inicio_situac_atual = as.Date(data_inicio_situac_atual)) %>% 
  mutate(prazo_emis_cota = replace(prazo_emis_cota, prazo_emis_cota == "Até 12h, D0; depois disso, D+1", "D+001"),
         prazo_emis_cota = gsub("D=0", "D+000",  prazo_emis_cota),
         prazo_emis_cota = gsub("d=0", "D+000", prazo_emis_cota)) %>% 
  mutate(prazo_conv_resg = replace(prazo_conv_resg, prazo_conv_resg == "Até 12h, D0; depois disso, D+1", "D+001"),
         prazo_conv_resg = replace(prazo_conv_resg, prazo_conv_resg == "D+30 dias corridos", "D+030"),
         prazo_conv_resg = gsub("D=1", "D+001", prazo_conv_resg)) %>% 
  mutate(prazo_pag_resg = replace(prazo_pag_resg, prazo_pag_resg == "2 dias úteis da conversão", "D+002"),
         prazo_pag_resg = gsub("4 dias0", "D+004", prazo_pag_resg),
         prazo_pag_resg = gsub("D=4", "D+004", prazo_pag_resg)) %>% 
  mutate(benchmark = gsub("IBRX", "IBRX-100", benchmark))

# OBS: unique(dados_cadast$classe) retorna Fundo Multimercado e Fundo de Renda Fixa tambem

# Jutamos os dados diários em uma lista
dados_diarios <- list(capt_liq, captacao, cota, n_cotistas, patrim_liq, resgate, tx_adm)
names(dados_diarios) <- c("capt_liq", "capt", "cota", "n_cotistas", "patrim_liq", "resg", "tx_adm")

##################################################################
##                     Calculo Retorno Cota                     ##
##################################################################

# Garantir que as datas sao as mesmas que o indice de mercado
ind <- read.csv("indice.csv")
ind$Data <- as.Date(ind$Data)

rf <- read.csv("rf.csv")
rf$Data <- as.Date(rf$Data)

ind_rf <- ind %>%
  mutate(rf$Risk_free) %>%
  set_names("Data", "ret_ibx", "ret_rf")

rm(ind, rf)

prices <- dados_diarios[["cota"]]

returns <- prices %>% 
  select_if(~sum(!is.na(.)) > 0) %>% # drop empty columns
  mutate(across(!Data, ~fill_na(.x))) %>% # fill na with last observation
  right_join(ind_rf, by = 'Data') %>% # same dates as in the index data
  dplyr::select(!c("ret_ibx", "ret_rf")) %>% 
  mutate(across(!Data, ~append(NA, diff(.x) / .x[-length(.x)]))) # get returns

dados_diarios[["cota"]] <- returns


#################################################################
##                        Save the data                        ##
#################################################################

# Salvamos os dados tratados. Para ler: readRDS('dados_tratados_mes.rds')
saveRDS(dados_cadast, file = "dados_cadastrais.rds")
saveRDS(dados_diarios, file = "dados_tratados_diarios.rds")
