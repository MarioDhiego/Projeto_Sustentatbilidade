
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)

# Interface do Usuário (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Sustentabilidade Ambiental", titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("SÓCIO-ECONÔMICO", tabName = "socioeconomico", icon = icon("users")),
      menuItem("COLETA SELETIVA", tabName = "coleta", icon = icon("recycle")),
      menuItem("CIRETRAN", tabName = "ciretran", icon = icon("car")),
      selectInput("municipio", "Municípios:",
                  choices = c("Altamira", "Santarém", "Marabá"),
                  selected = "Altamira"),
      selectInput("cargo", "Cargo:",
                  choices = c("Estagiário", "Terceirizado", "Auxiliar", "Assistente", "Analista", "Comissionado"),
                  selected = "Estagiário")
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
  )
)

# Server
server <- function(input, output, session) {
  
  # Dados de exemplo Socio-Econômico
  set.seed(123)
  data <- data.frame(
    Municipio = rep(c("Altamira", "Marabá", "Santarém"), each = 9),
    Sexo = sample(c("FEMININO", "MASCULINO"), 27, replace = TRUE),
    Raca = sample(c("PARDO", "NEGRO", "INDÍGENA", "BRANCO"), 27, replace = TRUE),
    Idade = sample(18:60, 27, replace = TRUE),
    Escolaridade = sample(c("EFI", "EFC", "EMI", "ESC", "POSGRADUAÇÃO"), 27, replace = TRUE),
    EstadoCivil = sample(c("SOLTEIRO", "CASADO", "UNIÃO ESTÁVEL", "OUTROS"), 27, replace = TRUE),
    Cargo = sample(c("Estagiário", "Terceirizado", "Auxiliar", "Assistente", "Analista", "Comissionado"), 27, replace = TRUE)
  )
  
  # Dados de exemplo Coleta Seletiva
  coleta_data <- data.frame(
    Municipio = rep(c("Altamira", "Marabá", "Santarém"), each = 9),
    BairroColeta = sample(c("SIM", "NÃO", "NÃO SEI INFORMAR"), 27, replace = TRUE),
    Informes = sample(c("SIM", "NÃO"), 27, replace = TRUE),
    SepararLixo = sample(c("SIM", "NÃO"), 27, replace = TRUE),
    SepararCorretamente = sample(c("SIM", "NÃO", "NÃO SEI INFORMAR"), 27, replace = TRUE)
  )
  
  # Dados de exemplo CIRETRAN
  ciretran_data <- data.frame(
    Municipio = rep(c("Altamira", "Marabá", "Santarém"), each = 9),
    LixeiraFacilidade = sample(c("SIM", "NÃO"), 27, replace = TRUE),
    UsoGarrafaCaneca = sample(c("SIM", "NÃO", "AS VEZES"), 27, replace = TRUE),
    LixeiraColetaSeletiva = sample(c("SIM", "NÃO"), 27, replace = TRUE),
    DestinoFinalLixo = sample(c("SIM", "NÃO"), 27, replace = TRUE)
  )
  
  # Filtrar os dados com base no município e cargo selecionado
  filtered_data <- reactive({
    data %>%
      filter(Municipio == input$municipio & Cargo == input$cargo)
  })
  
  filtered_coleta_data <- reactive({
    coleta_data %>%
      filter(Municipio == input$municipio)
  })
  
  filtered_ciretran_data <- reactive({
    ciretran_data %>%
      filter(Municipio == input$municipio)
  })
  
  # Gráficos Socio-Econômico
  output$sexoPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Sexo)) +
      geom_bar(aes(fill = Sexo), color = "black") +
      labs(title = "Distribuição por Sexo", x = "Sexo", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$racaPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Raca)) +
      geom_bar(aes(fill = Raca), color = "black") +
      labs(title = "Distribuição por Raça", x = "Raça/Cor", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$idadePlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Idade)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "Distribuição por Idade", x = "Idade", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$escolaridadePlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Escolaridade)) +
      geom_bar(aes(fill = Escolaridade), color = "black") +
      labs(title = "Distribuição por Escolaridade", x = "Escolaridade", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$estadoCivilPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = EstadoCivil)) +
      geom_bar(aes(fill = EstadoCivil), color = "black") +
      labs(title = "Distribuição por Estado Civil", x = "Estado Civil", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$cargoPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Cargo)) +
      geom_bar(aes(fill = Cargo), color = "black") +
      labs(title = "Distribuição por Cargo", x = "Cargo", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Gráficos Coleta Seletiva
  output$bairroColetaPlot <- renderPlotly({
    p <- ggplot(filtered_coleta_data(), aes(x = BairroColeta)) +
      geom_bar(aes(fill = BairroColeta), color = "black") +
      labs(title = "No Seu Bairro Tem Coleta Seletiva?", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$informesPlot <- renderPlotly({
    p <- ggplot(filtered_coleta_data(), aes(x = Informes)) +
      geom_bar(aes(fill = Informes), color = "black") +
      labs(title = "Recebeu Informes Sobre Coleta Seletiva?", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$separarLixoPlot <- renderPlotly({
    p <- ggplot(filtered_coleta_data(), aes(x = SepararLixo)) +
      geom_bar(aes(fill = SepararLixo), color = "black") +
      labs(title = "Costuma Separar o Lixo?", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$separarCorretamentePlot <- renderPlotly({
    p <- ggplot(filtered_coleta_data(), aes(x = SepararCorretamente)) +
      geom_bar(aes(fill = SepararCorretamente), color = "black") +
      labs(title = "Sabe Separar Corretamente o Lixo?", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Gráficos CIRETRAN
  output$lixeiraFacilidadePlot <- renderPlotly({
    p <- ggplot(filtered_ciretran_data(), aes(x = LixeiraFacilidade)) +
      geom_bar(aes(fill = LixeiraFacilidade), color = "black") +
      labs(title = "Facilidade de Encontrar Lixeiras", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$usoGarrafaCanecaPlot <- renderPlotly({
    p <- ggplot(filtered_ciretran_data(), aes(x = UsoGarrafaCaneca)) +
      geom_bar(aes(fill = UsoGarrafaCaneca), color = "black") +
      labs(title = "Uso de Garrafa e Caneca", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$lixeiraColetaSeletivaPlot <- renderPlotly({
    p <- ggplot(filtered_ciretran_data(), aes(x = LixeiraColetaSeletiva)) +
      geom_bar(aes(fill = LixeiraColetaSeletiva), color = "black") +
      labs(title = "Lixeiras de Coleta Seletiva", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$destinoFinalLixoPlot <- renderPlotly({
    p <- ggplot(filtered_ciretran_data(), aes(x = DestinoFinalLixo)) +
      geom_bar(aes(fill = DestinoFinalLixo), color = "black") +
      labs(title = "Destino Final do Lixo", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
}

# Execução do aplicativo
shinyApp(ui = ui, server = server)
