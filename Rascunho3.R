
# ======================================================================================================#
# Pacotes Utilizados
library(readr)
library(readxl)
library(dplyr)
library(data.table)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(forcats)
library(DT)  # Para renderizar a mini-tabela
# ======================================================================================================#

# Função para criar gráficos dinâmicos reutilizáveis
gerar_grafico <- function(data, x_var, fill_var, title, order = "asc") {
  # Verificar se a coluna está presente nos dados
  if (!(x_var %in% colnames(data))) {
    stop(paste("A coluna", x_var, "não foi encontrada nos dados"))
  }
  
  # Ordenação opcional das categorias
  data[[x_var]] <- if (order == "asc") {
    forcats::fct_infreq(data[[x_var]]) # Ordem crescente
  } else if (order == "desc") {
    forcats::fct_rev(forcats::fct_infreq(data[[x_var]])) # Ordem decrescente
  } else {
    factor(data[[x_var]]) # Ordem original
  }
  
  # Criação do gráfico com ggplot2
  ggplot(data, aes_string(x = x_var, fill = fill_var)) +
    geom_bar(color = "black") +
    geom_text(
      stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
      position = position_stack(vjust = 0.5), color = "white"
    ) +
    labs(title = title, x = "", y = "Nº de Entrevistados") +
    theme_minimal()
}

# ======================================================================================================#
# Interface do Usuário (UI)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Projeto Sustentabilidade Ambiental",
    titleWidth = 390,
    tags$li(
      class = "dropdown",
      a(href = "https://www.facebook.com/detranPARA", class = "fa fa-facebook", target = "_blank")
    ),
    tags$li(
      class = "dropdown",
      a(href = "https://www.instagram.com/detranpa_", class = "fa fa-instagram", target = "_blank")
    ),
    tags$li(
      class = "dropdown",
      a(href = "https://twitter.com/DETRAN_PA", class = "fa fa-twitter", target = "_blank")
    ),
    tags$li(
      class = "dropdown",
      a(href = "https://github.com/MarioDhiego", icon("github"), "Suporte", target = "_blank")
    )
  ),
  dashboardSidebar(
    minified = FALSE,
    collapsed = FALSE,
    tags$img(
      src = "atitudes.jpg",
      width = 230,
      height = 100
    ),
    sidebarMenu(
      menuItem("Sobre Projeto", tabName = "sobre", icon = icon("book")),
      menuItem("Análises", tabName = "analises", icon = icon("chart-bar")),
      selectInput("municipio", "MUNICÍPIO:",
                  choices = NULL, # Carregado no server
                  selected = NULL
      ),
      actionButton("reset_button", "Reiniciar Filtros", class = "btn-success")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "sobre",
        fluidRow(
          box(title = "Sobre o Projeto", width = 12, status = "info",
              "Esse é um aplicativo para análise de dados sobre sustentabilidade ambiental.")
        )
      ),
      tabItem(
        tabName = "analises",
        fluidRow(
          box(
            title = "Distribuição por Gênero", width = 7, status = "success", solidHeader = TRUE,
            collapsible = TRUE, plotlyOutput("sexoPlot") %>% withSpinner(color = "#28a745")
          )
          ,
          box(
            title = "Medidas Resumo", width = 5, status = "success", solidHeader = TRUE,
            collapsible = TRUE, DT::dataTableOutput("tabelaGenero") %>% withSpinner(color = "#28a745")
          )
        )
      )
    )
  )
)

# ======================================================================================================#
# Servidor

server <- function(input, output, session) {
  # Carregar os dados
  data <- reactive({
    req(file.exists("BANCO_PROJETO_SUSTENTABILIDADE.xls"))
    readxl::read_excel("BANCO_PROJETO_SUSTENTABILIDADE.xls")
  })
  
  # Atualizar opções dos filtros dinâmicos
  observe({
    dados <- data()
    updateSelectInput(session, "municipio", choices = unique(dados$MUNICIPIO), selected = unique(dados$MUNICIPIO)[1])
  })
  
  # Filtrar dados reativos
  filtered_data <- reactive({
    req(input$municipio)
    data() %>%
      filter(MUNICIPIO == input$municipio)
  })
  
  # Verificar se a coluna SEXO existe nos dados
  observe({
    dados <- data()
    if (!"SEXO" %in% colnames(dados)) {
      showNotification("A coluna 'SEXO' não foi encontrada nos dados.", type = "error")
    }
  })
  
  # Gráficos de Gênero
  output$sexoPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    req("SEXO" %in% colnames(filtered_data()))  # Verificar se a coluna SEXO está presente
    ggplotly(gerar_grafico(filtered_data(), "SEXO", "SEXO", "Distribuição por Gênero", order = "asc"))
  })
  
  # Tabela de Gênero com a média de IDADE e total
  output$tabelaGenero <- DT::renderDataTable({
    req(nrow(filtered_data()) > 0)
    
    # Verificar se as colunas SEXO e IDADE estão presentes
    dados <- filtered_data()
    if (!"SEXO" %in% colnames(dados) | !"IDADE" %in% colnames(dados)) {
      return(NULL)  # Retorna NULL se as colunas SEXO ou IDADE não estiverem presentes
    }
    
    # Contar o total por gênero e calcular a média de idade
    genero_count <- dados %>%
      group_by(SEXO) %>%
      summarise(
        Total = n(),
        Media_Idade = round(mean(IDADE, na.rm = TRUE), 1)  # Arredondando a média de idade para 1 casa decimal
      ) %>%
      rename("Gênero" = SEXO)
    
    # Adicionar a linha de total
    total_row <- tibble(
      "Gênero" = "Total",
      "Total" = sum(genero_count$Total),
      "Media_Idade" = round(mean(dados$IDADE, na.rm = TRUE), 1)  # Média de idade total arredondada
    )
    
    # Unir a linha de total com o resto da tabela
    genero_count <- bind_rows(genero_count, total_row)
    
    # Verificar se o resultado da contagem é vazio
    if (nrow(genero_count) == 0) {
      return(NULL)  # Retorna NULL se a contagem não gerar resultados
    }
    
    DT::datatable(
      genero_count,
      options = list(pageLength = 5),
      rownames = FALSE
    )
  })
  
  # Botão de reset
  observeEvent(input$reset_button, {
    updateSelectInput(session, "municipio", selected = unique(data()$MUNICIPIO)[1])
  })
}

# ======================================================================================================#
# Executar o app
shinyApp(ui, server)
