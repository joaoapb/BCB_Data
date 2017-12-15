# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# CONSULTA DE DADOS DO BCB
#   versão: 0.1
#   data: 13/12/2017
#   autor: João Augusto P. Batista
#
#   Estrutura da requisição:
#     http://api.bcb.gov.br/dados/serie/bcdata.sgs.{codigo_serie}/dados?
#     formato=csv&dataInicial={dataInicial}&dataFinal={dataFinal}
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# PACOTES ----
library(dplyr)
library(lubridate)


# LISTA ----
# TODO (@João Augusto): criar método para atualização da lista de séries
# Carrega a lista de séries disponíveis
bacenData <- read.csv("00_dados/lista_de_series_SGS.csv",
                       header = TRUE, sep = ";")

bacenData$nome = tolower(iconv(bacenData$Nome.completo, "WINDOWS-1252","UTF-8"))
bacenData$Nome.completo = tolower(iconv(bacenData$Nome.completo, "WINDOWS-1252",
                                        "UTF-8"))

# FUNÇÕES ----
CountCharOccurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return(nchar(s) - nchar(s2))
}

SearchSeries <- function(txt='', desativada=NULL) {
  if (txt == '') {
    tmp = bacenData
  } else {
    # texto a ser buscado em letras minúsculas
    txt = tm::removeWords(tolower(txt), tm::stopwords('pt'))

    # divide o texto nos espaços
    txt.spc = strsplit(txt, ' ', fixed = T)

    txt.search = data.frame(
      'termo'          = NA,
      'relevancia'     = NA,
      stringsAsFactors = FALSE
    )

    # gera a lista de termos a serem buscados
    if (length(txt.spc) > 2) {
      for (i in 1:length(txt.spc)) {
        tmp <- combinat::combn(x = txt.spc[i], m = i,
                               simplify = FALSE)
        for (t in tmp) {
          txt.search <-
            txt.search %>%
            bind_rows(
              data.frame(
                'termo' = paste(unlist(t), collapse = ' '),
                'relevancia' = i,
                stringsAsFactors = FALSE))
        }
      }
    } else {
      txt.spc = c(txt.spc, paste(txt.spc, collapse = ' '))
      for (t in txt.spc) {
        txt.search <-
          txt.search %>%
          bind_rows(
            data.frame(
              'termo' = t,
              'relevancia' = CountCharOccurrences(' ', t) + 1,
              stringsAsFactors = FALSE))
      }
    }
    # remove a primeira linha
    txt.search = txt.search[-1,]

    # ordena pela relevancia
    txt.search = txt.search %>% arrange(desc(relevancia))

    # busca os termos na base
    tmp = bacenData %>% mutate(relevancia = 0)

    for (i in 1:nrow(txt.search)) {
      rel = txt.search$relevancia[i]
      termo = txt.search$termo[i]

      tmp =
        tmp %>%
        mutate(relevancia = ifelse(stringr::str_detect(nome, termo),
                                   relevancia + rel,
                                   relevancia))
    }

    tmp <- tmp %>%
      filter(relevancia > 0,
             Desativada %in% ifelse(
               desativada == FALSE, c('N'),
               ifelse(
                 is.null(desativada), c('N', 'S'), 'S'))) %>%
      select(Codigo, Nome.completo, Unidade, Periodicidade,
             Inicio_dd.MM.aaaa, Desativada, relevancia) %>%
      arrange(desc(relevancia))
  }


  return(tmp)
}

# Função que recebe o código da série e retorna a série em um dataframe
GetSeriesData <- function(series, formato='csv', datainicial='2000-01-01',
                          datafinal='2017-12-31'){

  # override do formato
  formato = 'csv'

  # ajusta as datas
  datainicial = lubridate::ymd(datainicial)
  datafinal = lubridate::ymd(datafinal)

  datainicial = paste0(
    day(datainicial), "/",
    month(datainicial), "/",
    year(datainicial)
  )

  datafinal = paste0(
    day(datafinal), "/",
    month(datafinal), "/",
    year(datafinal)
  )

  # converte a série em character
  series <- as.character(series)

  # inicia o db
  db = NULL
  # loop por todas as séries passadas
  for (serie in series) {
    # prepara a url
    url = paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.', serie,
                 '/dados?formato=', formato,
                 '&dataInicial=', datainicial,
                 '&dataFinal=', datafinal)
    # executa o download, lendo direto da URL em um data.frame
    tmp <- read.csv2(url, stringsAsFactors = FALSE)

    # pega as informações de unidade, código e nome da série
    unidade = bacenData$Unidade[which(bacenData$Codigo  == serie)]
    nome = bacenData$Nome.completo[which(bacenData$Codigo  == serie)]

    # cria as colunas com informações de unidade, código e nome da série
    tmp <-
      tmp %>%
      mutate(cod  = serie,
             und  = unidade,
             nome = nome) %>%
      select(cod, nome, und, data, valor)

    # junta ao db
    db <-
      db %>%
      bind_rows(tmp)
  }

  db <-
    db %>%
    mutate(data = lubridate::dmy(data)) %>%
    arrange(data)

  return(db)
}


# GetMethodology <- function(serie=NULL) {
#   # essa função busca o texto da série
#
#   if (!is.null(serie)) {
#     return(FALSE)
#   }
#
#   serie = as.character(serie)
#
#   # pega o nome da série
#   nome = bacenData %>%
#     filter(Codigo == serie) %>%
#     mutate(Nome.completo = as.character(Nome.completo)) %>%
#     select(Nome.completo) %>%
#     as.character()
#   nome = gsub('  ', ' ', tolower(nome), fixed = T)
#   nome = gsub(' ', '-', tolower(nome), fixed = T)
#   nome = gsub('(', '', tolower(nome), fixed = T)
#   nome = gsub(')', '', tolower(nome), fixed = T)
#   nome = iconv(nome, to = "ASCII//TRANSLIT")
#
#   # prepara a URL
#   url = paste0('https://dadosabertos.bcb.gov.br/dataset/', serie, '-', nome)
#
#   # consulta a serie
#   require(rvest)
#   pagina = read_html(url)
#
#   # executa o parse
#   # pega a descrição
#   desc = pagina %>% html_nodes('.notes') %>% html_text()
#
#   return(desc)
# }
#
#

