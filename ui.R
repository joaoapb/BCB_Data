
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(shinydashboard)
library(dplyr)
#
# bacenData <- data.table::fread("00_dados/lista_de_series_SGS.csv",
#                                header = TRUE, sep = ";",
#                                stringsAsFactors = FALSE, data.table = FALSE)
# bacenData <-
#   bacenData %>%
#   arrange(`Nome completo`) %>%
#   mutate(Label = paste(Codigo, `Nome completo`, sep = ' - '))

# header <- dashboardHeader(title = "Busca de dados")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Banco Central do Brasil", tabName = "bcb",
             icon = icon("dashboard"))
  ),
  disable = TRUE
)

header <- dashboardHeader(disable = TRUE)

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "bcb",
            tags$div(
              tags$h1('BUSCA DE DADOS DO BANCO CENTRAL DO BRASIL')
            ),
            box(
              width = 12,
              textInput(
                "busca", "Busque séries aqui"),
              actionButton('bt.busca','Buscar', icon = icon('search')),
              uiOutput('spacer1'),
              uiOutput("serieSelect")),
            uiOutput('box.gerafraf'),
            box(
              width = 12,
              textOutput('txt.titulo'),
              br(),
              plotOutput('grafico_serie'),
              br()
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)
