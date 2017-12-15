
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)

source('01_scripts/20171213_SGS series.R')

shinyServer(function(input, output, session) {

  getdata <- reactive({
    serie <- as.character(
      bacenData %>%
        filter(Nome.completo == input$serieSelect) %>%
        select(Codigo))

    nome <- as.character(
      bacenData %>%
        mutate(Nome.completo = as.character(Nome.completo)) %>%
        filter(Nome.completo == input$serieSelect) %>%
        select(Nome.completo))

    db <- GetSeriesData(serie, datainicial = input$cal.inicio,
                        datafinal = input$cal.fim)

    return(list(serie, nome, db))
  })
  observeEvent(input$bt.busca, {
    db <- SearchSeries(input$busca, desativada = FALSE)

    output$spacer1 <- renderUI({
      br()
    })
    output$serieSelect <- renderUI({
      selectInput("serieSelect", label = NULL, choices = db$Nome.completo)
      })
    output$box.gerafraf <- renderUI({
      box(
        width = 12,
        column(6,
               dateInput('cal.inicio', 'Data de Início', value = '01/01/2000',
                         format = "dd/mm/yyyy", startview = "month",
                         weekstart = 0, language = "pt")),
        column(6,
               dateInput('cal.fim', 'Data de Término', value = NULL,
                         format = "dd/mm/yyyy", startview = "month",
                         weekstart = 0, language = "pt")),
        column(6,
          actionButton('bt.geragraf','Gerar Gráfico', icon = icon('line-chart'))
          ),
        column(6,
               downloadButton('bt.download','Download dos dados',
                              icon = icon('download'))
        ))
    })
  })

  observeEvent(input$bt.geragraf, {

    db <- getdata()

    # metodologia <- GetMethodology(serie)

    output$txt.titulo <- renderText({
      paste0(db[[1]], ' - ', db[[2]])
    })
    output$grafico_serie <- renderPlot({
      ggplot(db[[3]]) +
        geom_line(
          aes(x = data,
              y = valor,
              group = 1)
        ) +
        theme_hc()
    })
    # output$metodologia <- renderText(metodologia)
  })

  output$bt.download <- downloadHandler(
    filename = function() {
      paste0('bcb_', input$cal.inicio, '_', input$cal.fim, '.csv')
    },
    content = function(con) {
      db <- getdata()[3]
      write.csv2(db, con, row.names = FALSE)
    }
  )

})

rm(list = ls())
