---
editor_options:
  chunk_output_type: inline
output:html_document
---

# BCB_Data

## Sobre o Projeto

Este pequeno aplicativo serve para acessar rápida e facilmente a API do Banco
Central do Brasil. Ele permite a visualização em um gráfico simples, e o 
download da série selecionada no intervalo escolhido.

Aproveite! Críticas e sugestões são bem vindas!

## Os dados

As informações que podem ser obtidas do Banco Central foram mapeadas a partir do
[Sistema de Gerenciamento de Séries Temporais](https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries).

A busca de séries é realizada neste conjunto de informações, que deve ser 
atualizada mensalmente.

## Como utilizar

Você pode utilizar a versão online ou clonar este repositório. Caso você clone 
o repositório, basta abrir o R na pasta selecionada e rodar o seguinte código:



```{r eval=FALSE}

shiny::runApp()

```


## Restrições

Não existem restrições para a utilização. Entretanto, a aplicação que está 
online tem um limite de 25 horas de uso por mês devido à política do 
shinnyapps.io. Use com parcimônia a versão online!
