

#------------------------------------------------------------------------------#
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Interface do Usuário(UI)
ui <- dashboardPage(
  dashboardHeader(title = "Sustentabilidade Ambiental", titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("SÓCIO-ECONÔMICO", tabName = "socioeconomico", icon = icon("users")),
      menuItem("COLETA SELETIVA", tabName = "coleta", icon = icon("recycle")),
      menuItem("CIRETRAN'S", tabName = "cietran", icon = icon("car")),
      selectInput("municipio", "Municípios:",
                  choices = c("Altamira", "Santarem", "Maraba", "Paraupebas", "Breves"),
                  selected = "Altamira")
    )
  ),
#------------------------------------------------------------------------------#
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
#------------------------------------------------------------------------------#
# Aba Coleta Seletiva
      tabItem(tabName = "coleta",
              #h2("Dados sobre Coleta Seletiva"),
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
#------------------------------------------------------------------------------#
# Aba Cietran
      tabItem(tabName = "cietran",
              h2("Informações do Ciretran"),
              fluidRow(
                box(title = "Estatísticas de Trânsito", status = "warning", solidHeader = TRUE,
                    plotOutput("transitoPlot", height = 300))
              )
      )
    )
  )
)
#------------------------------------------------------------------------------#
# Server
server <- function(input, output, session) {

# Dados de exemplo Socio-Econômico
  set.seed(123)
  data <- data.frame(
    Municipio = sample(c("Altamira",
                         "Santarem",
                         "Maraba",
                         "Paraupebas",
                         "Breves",
                         "Castanhal",
                         "Bragança"), 1000, replace = TRUE),
    Sexo = sample(c("MASCULINO", "FEMININO"), 1000, replace = TRUE),
    Raca = sample(c("PARDO", "NEGRO", "INDÍGENA", "BRANCO"), 1000, replace = TRUE),
    Idade = sample(18:70, 1000, replace = TRUE),
    Escolaridade = sample(c("EFI",
                            "EFC",
                            "EMI",
                            "EMC",
                            "ESI",
                            "ESC"), 1000, replace = TRUE),
    EstadoCivil = sample(c("SOLTEIRO",
                           "CASADO",
                           "UNIÃO",
                           "OUTROS"), 1000, replace = TRUE),
    Cargo = sample(c("Estagiário",
                     "Terceirizado",
                     "Auxiliar",
                     "Assistente",
                     "Analista",
                     "Comissionado"), 1000, replace = TRUE)
  )

# Filtrar os dados Socio-Econômico com base no município selecionado
  filtered_data <- reactive({
    subset(data, Municipio == input$municipio)
  })

#------------------------------------------------------------------------------#
# Gráficos Socio-Econômico
  output$sexoPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(Sexo, Sexo, function(x) -length(x)))) +
      geom_bar(aes(fill = Sexo), color = "black") +
      #coord_flip() +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_gray()
    ggplotly(p)
  })
#------------------------------------------------------------------------------#
  output$racaPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(Raca, Raca, function(x) -length(x)))) +
      geom_bar(aes(fill = Raca), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_gray()
    ggplotly(p)
  })
#------------------------------------------------------------------------------#
  output$idadePlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Idade)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_gray()
    ggplotly(p)
  })
#------------------------------------------------------------------------------#
  output$escolaridadePlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(Escolaridade, Escolaridade, function(x) -length(x)))) +
      geom_bar(aes(fill = Escolaridade), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_gray()
    ggplotly(p)
  })
#------------------------------------------------------------------------------#
  output$estadoCivilPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(EstadoCivil, EstadoCivil, function(x) -length(x)))) +
      geom_bar(aes(fill = EstadoCivil), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_gray()
    ggplotly(p)
  })
#------------------------------------------------------------------------------#
  output$cargoPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = reorder(Cargo, Cargo, function(x) -length(x)))) +
      geom_bar(aes(fill = Cargo), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
#------------------------------------------------------------------------------#
# Dados de exemplo Coleta Seletiva
  coleta_data <- data.frame(
    Municipio = sample(c("Altamira",
                         "Santarem",
                         "Maraba",
                         "Paraupebas",
                         "Breves",
                         "Castanhal",
                         "Bragança"), 500, replace = TRUE),
    BairroColeta = sample(c("Sim", "Não", "Não Sei Informar"), 500, replace = TRUE),
    Informes = sample(c("Sim", "Não"), 500, replace = TRUE),
    SepararLixo = sample(c("Sim", "Não"), 500, replace = TRUE),
    SepararCorretamente = sample(c("Sim", "Não", "Não Sei Informar"), 500, replace = TRUE)
  )
#------------------------------------------------------------------------------#
# Filtrar os dados de Coleta Seletiva com base no município selecionado
  filtered_coleta_data <- reactive({
    subset(coleta_data, Municipio == input$municipio)
  })
#------------------------------------------------------------------------------#
# Gráficos Coleta Seletiva
  output$bairroColetaPlot <- renderPlotly({
    p <- ggplot(filtered_coleta_data(), aes(x = reorder(BairroColeta, BairroColeta, function(x)-length(x)))) +
      geom_bar(aes(fill = BairroColeta), color = "black") +
      labs(title = "", x = "", y = "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })





#------------------------------------------------------------------------------#
  output$informesPlot <- renderPlotly({
    p <- ggplot(filtered_coleta_data(), aes(x = reorder(Informes, Informes, function(x)-length(x)))) +
      geom_bar(aes(fill = Informes), color = "black") +
      labs(title = "", x = "", "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
#------------------------------------------------------------------------------#
  output$separarLixoPlot <- renderPlotly({
    p <- ggplot(filtered_coleta_data(), aes(x = SepararLixo)) +
      geom_bar(aes(fill = SepararLixo), color = "black") +
      labs(title = "", x = "", "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
#------------------------------------------------------------------------------#
  output$separarCorretamentePlot <- renderPlotly({
    p <- ggplot(filtered_coleta_data(), aes(x = SepararCorretamente)) +
      geom_bar(aes(fill = SepararCorretamente), color = "black") +
      labs(title = "", x = "", "Nº de Funcionários") +
      theme_minimal()
    ggplotly(p)
  })
#------------------------------------------------------------------------------#
# Exemplo de gráfico para CIRETRAN
  output$transitoPlot <- renderPlot({
    plot(cars, main = "Estatísticas de Trânsito", col = "red")
  })
}
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Run the application
shinyApp(ui = ui, server = server)

#------------------------------------------------------------------------------#
