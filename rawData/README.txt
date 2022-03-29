Estrutura dos dados raw (Fundos Ação Livre Brasil):

Importamos todos os nossos dados da base da Economatica. Entretanto, essa base tinha algumas restrições para extração desses dados. Uma delas é que o máximo de ativos em cada extração é de 2000. 

Assim, como nosso universo de fundos é substancialmente maior que 2000, tivemos que quebrar nossa requisição em múltiplas requisições menores.

A primeira quebra foi filtrar a primeira vez apenas por fundos cancelados. Assim, temos os dados referentes a esses fundos nas bases de dados com fim "_cancelados".

Em seguida, filtramos pelos fundos ativos. Nesse caso, temos 2009 fundos. Ou seja, tivemos que excluir 10 fundos da nossa amostra para sermos capazes de fazer a requisição.

Os dados referentes a esses 10 fundos estão em uma planilha separada batizada de "dados_10".

Os dados restantes, referentes aos fundos ativos, estão nos arquivos com fim "_ativos".