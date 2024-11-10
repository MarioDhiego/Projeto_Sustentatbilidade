
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(plotly)
library(dplyr)

# Interface do Usuário (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Sustentabilidade Ambiental", titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Projeto",         tabName = "defprojeto", icon = icon("book")),
      menuItem("SÓCIO-ECONÔMICO", tabName = "socioeconomico", icon = icon("users")),
      menuItem("COLETA SELETIVA", tabName = "coleta",         icon = icon("recycle")),
      menuItem("CIRETRAN",        tabName = "ciretran",       icon = icon("car")),
      selectInput("municipio", "MUNICÍPIOS:",
                  choices = c("Altamira", "Marabá", "Castanhal", "Santarem", "Parauapebas"),
                  selected = "Altamira")
    )
  ),

  dashboardBody(
    tabItems(
      # Aba Socio-Econômico
      tabItem(tabName = "socioeconomico",
              fluidRow(
                box(title = "GÊNERO", status = "primary", solidHeader = TRUE,
                    plotlyOutput("sexoPlot", height = 300)),
                box(title = "RAÇA/COR", status = "primary", solidHeader = TRUE,
                    plotlyOutput("racaPlot", height = 300)),
                box(title = "IDADE", status = "primary", solidHeader = TRUE,
                    plotlyOutput("idadePlot", height = 300)),
                box(title = "GRAU DE ESCOLARIDADE", status = "primary", solidHeader = TRUE,
                    plotlyOutput("escolaridadePlot", height = 300)),
                box(title = "ESTADO CIVIL", status = "primary", solidHeader = TRUE,
                    plotlyOutput("estadoCivilPlot", height = 300)),
                box(title = "CARGO/FUNÇÃO", status = "primary", solidHeader = TRUE,
                    plotlyOutput("cargoPlot", height = 300))
              )
      ),

      # Aba Coleta Seletiva
      tabItem(tabName = "coleta",
              fluidRow(
                box(title = "NO SEU BAIRRO TÊM COLETA SELETIVA?", status = "success", solidHeader = TRUE,
                    plotlyOutput("bairroColetaPlot", height = 300)),
                box(title = "RECEBEU INFORMES SOBRE COLETA SELETIVA?", status = "success", solidHeader = TRUE,
                    plotlyOutput("informesPlot", height = 300)),
                box(title = "COSTUMA SEPARAR O LIXO?", status = "success", solidHeader = TRUE,
                    plotlyOutput("separarLixoPlot", height = 300)),
                box(title = "SABE SEPARAR CORRETAMENTE O LIXO?", status = "success", solidHeader = TRUE,
                    plotlyOutput("separarCorretamentePlot", height = 300))
              )
      ),

      # Aba CIRETRAN
      tabItem(tabName = "ciretran",
              #h2("Informações do Ciretran"),
              fluidRow(
                box(title = "Facilidade de Encontrar Lixeiras", status = "warning", solidHeader = TRUE,
                    plotlyOutput("lixeiraFacilidadePlot", height = 300)),
                box(title = "Uso de Garrafa e Caneca", status = "warning", solidHeader = TRUE,
                    plotlyOutput("usoGarrafaCanecaPlot", height = 300)),
                box(title = "Lixeiras de Coleta Seletiva", status = "warning", solidHeader = TRUE,
                    plotlyOutput("lixeiraColetaSeletivaPlot", height = 300)),
                box(title = "Destino Final do Lixo", status = "warning", solidHeader = TRUE,
                    plotlyOutput("destinoFinalLixoPlot", height = 300))
              )
      )
    )
  ,
  footer = dashboardFooter(
    left = tags$b("DETRAN-PA"), 
    right = tags$b("BELÉM-PA, 2024 v.1")
  )
)
)




# Server
server <- function(input, output, session) {

  # Dados de exemplo Socio-Econômico
  set.seed(123)
  setwd("C:/Users/usuario/Documents/Projeto_Sustentatbilidade/Projeto_Coleta_Seletiva")
  data <- readxl::read_excel("BANCO_PROJETO_SUSTENTABILIDADE.xlsx")
 
  # Filtrar os dados Socio-Econômico com base no município selecionado
  filtered_data <- reactive({
    subset(data, MUNICIPIO == input$municipio)
  })

  # Gráficos Socio-Econômico
  output$sexoPlot <- renderPlotly({
    p1 <- ggplot(filtered_data(), aes(x = reorder(SEXO, SEXO, function(x) -length(x)))) +
      geom_bar(aes(fill = SEXO), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_gray()
    ggplotly(p1)
  })

  output$racaPlot <- renderPlotly({
    p2 <- ggplot(filtered_data(), aes(x = reorder(RACA, RACA, function(x) -length(x)))) +
      geom_bar(aes(fill = RACA), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_gray()
    ggplotly(p2)
  })

  output$idadePlot <- renderPlotly({
    p3 <- ggplot(filtered_data(), aes(x = IDADE)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_gray()
    ggplotly(p3)
  })

  output$escolaridadePlot <- renderPlotly({
    p4 <- ggplot(filtered_data(), aes(x = reorder(ESCOLARIDADE, ESCOLARIDADE, function(x) -length(x)))) +
      geom_bar(aes(fill = ESCOLARIDADE), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_gray()
    ggplotly(p4)
  })

  output$estadoCivilPlot <- renderPlotly({
    p5 <- ggplot(filtered_data(), aes(x = reorder(ESTADO_CIVIL, ESTADO_CIVIL, function(x) -length(x)))) +
      geom_bar(aes(fill = ESTADO_CIVIL), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_gray()
    ggplotly(p5)
  })

  output$cargoPlot <- renderPlotly({
    p6 <- ggplot(filtered_data(), aes(x = reorder(CARGO_FUNCAO, CARGO_FUNCAO, function(x) -length(x)))) +
      geom_bar(aes(fill = CARGO_FUNCAO), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p6)
  })


  # Filtrar os dados de Coleta Seletiva com base no município selecionado
  filtered_coleta_data <- reactive({
    subset(data, MUNICIPIO == input$municipio)
  })

  # Gráficos Coleta Seletiva
  output$bairroColetaPlot <- renderPlotly({
    p9 <- ggplot(filtered_coleta_data(), aes(x = reorder(P9, P9, function(x) -length(x)))) +
      geom_bar(aes(fill = P9), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p9)
  })

  output$informesPlot <- renderPlotly({
    p10 <- ggplot(filtered_coleta_data(), aes(x = reorder(P10, P10, function(x) -length(x)))) +
      geom_bar(aes(fill = P10), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p10)
  })

  output$separarLixoPlot <- renderPlotly({
    p11 <- ggplot(filtered_coleta_data(), aes(x = reorder(P11, P11, function(x) -length(x)))) +
      geom_bar(aes(fill = P11), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p11)
  })

  output$separarCorretamentePlot <- renderPlotly({
    p12 <- ggplot(filtered_coleta_data(), aes(x = reorder(P12, P12, function(x) -length(x)))) +
      geom_bar(aes(fill = P12), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p12)
  })

  # Filtrar os dados de CIRETRAN com base no município selecionado
  filtered_cietran_data <- reactive({
    subset(data, MUNICIPIO == input$municipio)
  })

  # Gráficos CIRETRAN
  output$lixeiraFacilidadePlot <- renderPlotly({
    p16 <- ggplot(filtered_cietran_data(), aes(x = reorder(P16, P16, function(x) -length(x)))) +
      geom_bar(aes(fill = P16), color = "black") +
      labs(title = "Facilidade de Encontrar Lixeiras", x = "", y = "Nº de Respostas") +
      theme_minimal()
    ggplotly(p16)
  })

  output$usoGarrafaCanecaPlot <- renderPlotly({
    p17 <- ggplot(filtered_cietran_data(), aes(x = reorder(P17, P17, function(x) -length(x)))) +
      geom_bar(aes(fill = P17), color = "black") +
      labs(title = "Uso de Garrafa e Caneca", x = "", y = "Nº de Respostas") +
      theme_minimal()
    ggplotly(p17)
  })

  output$lixeiraColetaSeletivaPlot <- renderPlotly({
    p19 <- ggplot(filtered_cietran_data(), aes(x = reorder(P19,P19, function(x) -length(x)))) +
      geom_bar(aes(fill = P19), color = "black") +
      labs(title = "Lixeiras de Coleta Seletiva", x = "", y = "Nº de Respostas") +
      theme_minimal()
    ggplotly(p19)
  })

  output$destinoFinalLixoPlot <- renderPlotly({
    p20 <- ggplot(filtered_cietran_data(), aes(x = reorder(P20, P20, function(x) -length(x)))) +
      geom_bar(aes(fill = P20), color = "black") +
      labs(title = "Destino Final do Lixo", x = "", y = "Nº de Respostas") +
      theme_minimal()
    ggplotly(p20)
  })
}

# Executar o aplicativo
shinyApp(ui, server)
