# Repositório com código para limpeza e processamento de dados do SI-PNI disponibilizados no OPENDATASUS

Os dados processados com este código são atualizados semanalmente no repositório https://github.com/covid19br/dados-vacinas

O script para download, limpeza e processamento dos dados é executado em Bash script.
Para executá-lo, utilize ./roda_base.sh

Este script realiza o download dos dados disponibilizados em:
https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao

Após o downlaod, é feita a seleção apenas das colunas de interesse.
A base de SP tem suas linhas ordenadas de acordo com o campo id_descricao, e então dividido em 4 arquivos que serão processados separadamente.

O código para a primeira parte de processamento de dados está presente no script vaccine_functions.R

O código para calculo das coberturas de doses por estado por classificação presente no SI-PNI está presente no script calculo_cobertura_por_estado.R

O código para cálculo das coberturas de doses por estado por ordem de aplicação por ID está presente no script calcular_ordem_por_estado.R

Para reproduzir o código, mantenha a mesma estrutura de pastas deste repositório ( "dados/", "figuras/" e "output/", e seus subdiretórios)

No caso de dúvidas, entre em contato: Marcelo Eduardo Borges (meborges.b [at] gmail [dot] com) 

OBSERVATÓRIO COVID-19 BR<br />
https://covid19br.github.io/<br />
<br />
