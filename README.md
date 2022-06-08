# Introdução
Esse repositório contém dados diários para todos os fundos de ação livre existentes ou que deixaram de existir no Brasil. Os dados foram extraídos da Economática e serão utilizados em um artigo acadêmico.

# Tratamento dos Dados
Uma explicação acerca do processo de tratamento de dados pode ser encontrado no arquivo README na pasta 'rawData'.

O resultado do tratamento de dados são dois arquivos:

1- 'dados_tratados_diario.rds': dados diários relacionados a cada fundo
2- 'dados_cadastrais.rds': dados cadastrais relacionados a cada fundo

# indice.csv e rf.csv

indice.csv: csv com o retorno diário do índice de mercado (IBX)
rf.csv: csv com o retorno diário da taxa livre de risco (CDI)

# prepare_data_model.R

Preparamos os dados para modelagem. 

Criamos três variáveis que podem ser utilizadas como variáveis dependentes. Uma para regressão ("regres_flows") e três para classificação ("closed","outperf", "classif_flows").

Além disso, temos múltiplas variáveis independentes que podem ser utilizadas para modelar as variáveis dependentes.