# Introdução
Esse repositório contém dados diários e mensais para todos os fundos de ação livre existentes ou que deixaram de existir no Brasil. Os dados foram extraídos da Economática e serão utilizados em um artigo acadêmico.

# Tratamento dos Dados
Uma explicação acerca do processo de tratamento de dados pode ser encontrado no arquivo README na pasta 'rawData'.

O resultado do tratamento de dados é o 'dados_tratados'. Esse é um arquivo .Rdata que possui três componentes principais.

1 - dados: Dataframe no formato long (painel) contendo dados mensais de diversos indicadores para todos os fundos de ação livre que existem ou já existiram.
2 - dados_cadast: Dataframe contendo dados cadastrais de cada fundo. Optamos por não fazer o merge com o 'dados' porque iria aumentar significativamente o tamanho do arquivo. Esses dados precisam ser levados em consideração com cuidado dado que não temos informação sobre eles na escala temporal, apenas cross sectional.
3 - dados_diarios: Lista contendo os dados diários para cada indicador. É possível transformar em uma tabela única no formato long, mas teriam milhões de linhas.  
