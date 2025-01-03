# ======================================================================================================#
# Pacotes Utilizados
library(readr)
library(readxl)
library(dplyr)
library(curl)
library(plyr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(leaflet)
library(tidygeocoder)
library(likert)
library(scales) 
library(htmlwidgets)
library(htmltools)
library(RColorBrewer)
library(table1)
library(flextable)
library(rstatix)
library(haven)
library(DiagrammeR) 
library(rlang)
library(forcats)
library(DT)  
library(openrouteservice)
library(osmdata)
library(httr2)
library(glue)
library(rjson)
library(googleway)
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
      style = "margin-right: 15px; display: inline-block;",  
a(href = "https://www.facebook.com/detranPARA", class = "fa fa-facebook fa-lg", target = "_blank", title = "Facebook", style = "color: #3b5998; transition: color 0.3s;"),
    tags$style(HTML("
      .fa-facebook:hover {
        color: #8b9dc3;
      }
    "))),
    tags$li(
      class = "dropdown",
      style = "margin-right: 15px; display: inline-block;", 
  a(href = "https://www.instagram.com/detranpa_", class = "fa fa-instagram", target = "_blank", title = "InstaGram",style = "color: #e1306c; transition: color 0.3s;"),
      tags$style(HTML("
      .fa-instagram:hover {
        color: #fd1d1d;
      }
    "))),
    tags$li(
      class = "dropdown",
      style = "margin-right: 15px; display: inline-block;", 
      a(href = "https://twitter.com/DETRAN_PA", 
        class = "fa fa-twitter", 
        target = "_blank", 
        title = "Twitter",
        style = "color: #1da1f2; transition: color 0.3s;"),
      tags$style(HTML("
      .fa-twitter:hover {
        color: #0d95e8;
      }
    "))),
    tags$li(
      class = "dropdown",
      style = "margin-right: 15px; display: inline-block;", 
      a(href = "https://github.com/MarioDhiego", 
        icon("github"), "Suporte", 
        target = "_blank", 
        title = "Suporte",
        style = "color: #333; transition: color 0.3s;"),
      tags$style(HTML("
      .fa-github:hover {
        color: #6e6e6e;
      }
    ")))),
  dashboardSidebar(
    minified = FALSE,
    collapsed = FALSE,
    tags$img(
      src = "detran1.jpeg",
      width = 230,
      height = 150),
    sidebarMenu(
      menuItem("PROJETO", tabName = "defprojeto", icon = icon("book"),
               menuSubItem("Sobre Projeto", tabName="sobre1", icon=icon("book")),
               menuSubItem("Localização", tabName="local1", icon=icon("video"))),
      menuItem("SÓCIOECONÔMICO", tabName = "analises", icon = icon("chart-bar")),
      menuItem("PERCEPÇÃO", tabName = "escalalikert", icon = icon("book"),
               menuSubItem("Percepção Geral", tabName = "likertgeral", icon = icon("book")),
               menuSubItem("Percepção Por Gênero", tabName = "likertgenero", icon = icon("book"))),
      selectInput("municipio", "MUNICÍPIO:",
                  choices = NULL, # Carregado no server
                  selected = NULL),
      actionButton("reset_button", "Reiniciar Filtros", class = "btn-success"))),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "sobre",
        fluidRow(
          box(title = "Sobre o Projeto", width = 12, status = "info",
              "Esse é um aplicativo para análise de dados sobre sustentabilidade ambiental.")
        )),
      tabItem(
        tabName = "analises",
        fluidRow(
          tabBox(
            title = "SócioEconômico", width = 12,
            tabPanel("Gênero",
                     fluidRow(
                       box(
                         title = "Distribuição por Gênero", width = 7, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, plotlyOutput("sexoPlot") %>% withSpinner(color = "#28a745")
                       ),
                       box(
                         title = "Medidas Resumo", width = 5, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, DT::dataTableOutput("tabelaGenero") %>% withSpinner(color = "#28a745")
                       ))),
            tabPanel("Raça",
                     fluidRow(
                       box(
                         title = "Distribuição por Raça", width = 7, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, plotlyOutput("racaPlot") %>% withSpinner(color = "#17a2b8")
                       ),
                       box(
                         title = "Medidas Resumo", width = 5, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, DT::dataTableOutput("tabelaRaca") %>% withSpinner(color = "#17a2b8")
                       ))),
            tabPanel("Escolaridade",
                     fluidRow(
                       box(
                         title = "Distribuição por Escolaridade", width = 7, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, plotlyOutput("escolaridadePlot") %>% withSpinner(color = "#ffc107")
                       ),
                       box(
                         title = "Medidas Resumo", width = 5, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, DT::dataTableOutput("tabelaEscolaridade") %>% withSpinner(color = "#ffc107")
                       ))),
            # Nova sub-aba para Estado Civil
            tabPanel("Estado Civil",
                     fluidRow(
                       box(
                         title = "Distribuição por Estado Civil", width = 7, 
                         status = "success", 
                         solidHeader = TRUE,
                         collapsible = TRUE, 
                         plotlyOutput("estadoCivilPlot") %>% 
                           withSpinner(color = "#007bff")
                       ),
                       box(
                         title = "Medidas Resumo", width = 5, 
                         status = "success", solidHeader = TRUE,
                         collapsible = TRUE, 
                         DT::dataTableOutput("tabelaEstadoCivil") %>% 
                           withSpinner(color = "#007bff")
                       )))))),
      tabItem(
        tabName = "likertgeral",
        tabPanel("Escala Likert Geral",
                 icon = icon("address-card"),
                 fluidRow(
                   box( width = 12,
                        title = "Percepção de Sustentabilidade Geral",
                        status = "success",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        headerBorder = TRUE,
                        tags$div(
                          style = "display: flex; justify-content: center; align-items: center;",
                          plotlyOutput("likertPlot1",
                                       width = "auto",
                                       height = "auto")
                        ))))),
      tabItem(
        tabName = "likertgenero",
        tabPanel("Escala Likert Gênero",
                 icon = icon("address-card"),   
                 fluidRow(
                   box(
                     width = 12,
                     title = "Percepção de Sustentabilidade por Gênero",
                     style = "text-align: center",
                     status = "success",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     headerBorder = TRUE,
                     tags$div(
                       style = "display: flex; justify-content: center; align-items: center;",
                       plotlyOutput("likertPlot2",
                                    width = 850,
                                    height = 900)
                     ))))),
      tabItem(
        tabName = "local1",
        tabPanel(
          title = "Mapa",
          fluidRow(
            column(12,
                   h3("Mapa de Municípios"),
                   leafletOutput("mapa_municipios",
                                 height = 700)  # Mapa interativo
            )))))))

# ======================================================================================================#
# Servidor

server <- function(input, output, session) {
  
  Dados_Clima <- readxl::read_excel("Dados_Clima.xls")
  
  # Carregar os dados
  data <- reactive({
    req(file.exists("BANCO_PROJETO_SUSTENTABILIDADE.xls"))
    readxl::read_excel("BANCO_PROJETO_SUSTENTABILIDADE.xls")
  })
  
  # Atualizar opções dos filtros dinâmicos
  observe({
    dados <- data()
    updateSelectInput(session, 
                      "municipio", 
                      choices = unique(dados$MUNICIPIO), 
                      selected = unique(dados$MUNICIPIO)[1])
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
    municipio_selecionado <- input$municipio  # Pega o nome do município selecionado
    
    # Modificar o título para incluir o nome do município
    titulo <- paste("Distribuição por Gênero em", municipio_selecionado)
    
    ggplotly(gerar_grafico(filtered_data(), "SEXO", "SEXO", titulo, order = "asc"))
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
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # Gráfico de Raça
  output$racaPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    req("RACA" %in% colnames(filtered_data()))  # Verificar se a coluna RACA está presente
    municipio_selecionado <- input$municipio  # Pega o nome do município selecionado
    
    # Modificar o título para incluir o nome do município
    titulo <- paste("Distribuição por Raça em", municipio_selecionado)
    
    ggplotly(gerar_grafico(filtered_data(), "RACA", "RACA", titulo, order = "asc"))
  })
  
  # Tabela de Raça com a média de IDADE e total
  output$tabelaRaca <- DT::renderDataTable({
    req(nrow(filtered_data()) > 0)
    
    # Verificar se as colunas RACA e IDADE estão presentes
    dados <- filtered_data()
    if (!"RACA" %in% colnames(dados) | !"IDADE" %in% colnames(dados)) {
      return(NULL)  # Retorna NULL se as colunas RACA ou IDADE não estiverem presentes
    }
    
    # Contar o total por raça e calcular a média de idade
    raca_count <- dados %>%
      group_by(RACA) %>%
      summarise(
        Total = n(),
        Media_Idade = round(mean(IDADE, na.rm = TRUE), 1)  # Arredondando a média de idade para 1 casa decimal
      ) %>%
      rename("Raça" = RACA)
    
    # Adicionar a linha de total
    total_row <- tibble(
      "Raça" = "Total",
      "Total" = sum(raca_count$Total),
      "Media_Idade" = round(mean(dados$IDADE, na.rm = TRUE), 1)  # Média de idade total arredondada
    )
    
    # Unir a linha de total com o resto da tabela
    raca_count <- bind_rows(raca_count, total_row)
    
    # Verificar se o resultado da contagem é vazio
    if (nrow(raca_count) == 0) {
      return(NULL)  # Retorna NULL se a contagem não gerar resultados
    }
    
    DT::datatable(
      raca_count,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # Gráfico de Escolaridade
  output$escolaridadePlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    req("ESCOLARIDADE" %in% colnames(filtered_data()))  # Verificar se a coluna ESCOLARIDADE está presente
    municipio_selecionado <- input$municipio  # Pega o nome do município selecionado
    
    # Modificar o título para incluir o nome do município
    titulo <- paste("Distribuição por Escolaridade em", municipio_selecionado)
    
    ggplotly(gerar_grafico(filtered_data(), "ESCOLARIDADE", "ESCOLARIDADE", titulo, order = "asc"))
  })
  
  # Tabela de Escolaridade com a média de IDADE e total
  
  output$tabelaEscolaridade <- DT::renderDataTable({
    req(nrow(filtered_data()) > 0)
    
    # Verificar se as colunas ESCOLARIDADE e IDADE estão presentes
    dados <- filtered_data()
    if (!"ESCOLARIDADE" %in% colnames(dados) | !"IDADE" %in% colnames(dados)) {
      return(NULL)  # Retorna NULL se as colunas ESCOLARIDADE ou IDADE não estiverem presentes
    }
    
    # Contar o total por escolaridade e calcular a média de idade
    escolaridade_count <- dados %>%
      group_by(ESCOLARIDADE) %>%
      summarise(
        Total = n(),
        Media_Idade = round(mean(IDADE, na.rm = TRUE), 1)  # Arredondando a média de idade para 1 casa decimal
      ) %>%
      rename("Escolaridade" = ESCOLARIDADE)
    
    # Substituições nos valores da coluna "Escolaridade"
    escolaridade_count <- escolaridade_count %>%
      mutate(Escolaridade = case_when(
        Escolaridade == "EFI" ~ "Ensino Fundamental Incompleto",
        Escolaridade == "EMI" ~ "Ensino Médio Incompleto",
        Escolaridade == "EMC" ~ "Ensino Médio Completo",
        Escolaridade == "ESC" ~ "Ensino Superior Completo",
        Escolaridade == "ESI" ~ "Ensino Superior Incompleto",
        Escolaridade == "PÓS" ~ "Pós-Graduação",
        TRUE ~ Escolaridade  # Mantém o valor original para outros casos
      ))
    
    
    
    # Adicionar a linha de total
    total_row <- tibble(
      "Escolaridade" = "Total",
      "Total" = sum(escolaridade_count$Total),
      "Media_Idade" = round(mean(dados$IDADE, na.rm = TRUE), 1)  # Média de idade total arredondada
    )
    
    # Unir a linha de total com o resto da tabela
    escolaridade_count <- bind_rows(escolaridade_count, total_row)
    
    # Verificar se o resultado da contagem é vazio
    if (nrow(escolaridade_count) == 0) {
      return(NULL)  # Retorna NULL se a contagem não gerar resultados
    }
    
    DT::datatable(
      escolaridade_count,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  # Gráfico de Estado Civil
  output$estadoCivilPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    req("ESTADO_CIVIL" %in% colnames(filtered_data()))  # Verificar se a coluna ESTADO_CIVIL está presente
    municipio_selecionado <- input$municipio  # Pega o nome do município selecionado
    
    # Modificar o título para incluir o nome do município
    titulo <- paste("Município de ", municipio_selecionado)
    
    ggplotly(gerar_grafico(filtered_data(), "ESTADO_CIVIL", "ESTADO_CIVIL", titulo, order = "asc"))
  })
  
  # Tabela de Estado Civil com a média de IDADE e total
  output$tabelaEstadoCivil <- DT::renderDataTable({
    req(nrow(filtered_data()) > 0)
    
    # Verificar se as colunas ESTADO_CIVIL e IDADE estão presentes
    dados <- filtered_data()
    if (!"ESTADO_CIVIL" %in% colnames(dados) | !"IDADE" %in% colnames(dados)) {
      return(NULL)  # Retorna NULL se as colunas ESTADO_CIVIL ou IDADE não estiverem presentes
    }
    
    # Contar o total por estado civil e calcular a média de idade
    estado_civil_count <- dados %>%
      group_by(ESTADO_CIVIL) %>%
      summarise(
        Total = n(),
        Media_Idade = round(mean(IDADE, na.rm = TRUE), 1)  # Arredondando a média de idade para 1 casa decimal
      ) %>%
      rename("Estado Civil" = ESTADO_CIVIL)
    
    # Adicionar a linha de total
    total_row <- tibble(
      "Estado Civil" = "Total",
      "Total" = sum(estado_civil_count$Total),
      "Media_Idade" = round(mean(dados$IDADE, na.rm = TRUE), 1)  # Média de idade total arredondada
    )
    
    # Unir a linha de total com o resto da tabela
    estado_civil_count <- bind_rows(estado_civil_count, total_row)
    
    # Verificar se o resultado da contagem é vazio
    if (nrow(estado_civil_count) == 0) {
      return(NULL)  # Retorna NULL se a contagem não gerar resultados
    }
    
    DT::datatable(
      estado_civil_count,
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # Botão de reset
  observeEvent(input$reset_button, {
    updateSelectInput(session, "municipio",
                      selected = unique(data()$MUNICIPIO)[1])
  })
#------------------------------------------------------------------------------#  
# Escala Likert

output$likertPlot1 <- renderPlotly({
    
# Filtrando os dados com base nos inputs
dados_filtrados <- Dados_Clima %>%
filter(MUNICIPIO == input$municipio)
    
# Garantir que as colunas estão no formato de fator com níveis adequados
dados_filtrados[, 1:9] <- lapply(dados_filtrados[, 1:9], 
                                 factor, 
                                 levels = 1:3,
                                 labels = c("Sim", 
                                            "Não", 
                                            "Não Sei Informar"),
                                 ordered = TRUE)
    
# Carregar a tabela com os nomes das colunas
nomes <- read_excel("Dados_Clima.xls", sheet = 3)
colnames(dados_filtrados)[1:9] <- nomes$Nomes
    
# Gerar o gráfico da escala Likert
dados_grafico <- likert(as.data.frame(dados_filtrados[1:9]))
    
# Paleta de cores para o gráfico
paleta <- brewer.pal(n=5, "RdBu")
paleta[3] <- "lightblue"
    
# Criar o Gráfico Likert
g1 <- likert.bar.plot(dados_grafico,
                      plot.percent.low = TRUE, 
                      plot.percent.high = TRUE, 
                      plot.percent.neutral = TRUE,
                      strip = TRUE,
                      strip.left = TRUE,
                      ReferenceZero = 3,
                      wrap = 25,
                      centered = TRUE,
                      text.size = 4, 
                      hjust = 0.5,
                      legend = "Escala Likert",
                      legend.position = "right", # top 
                      auto.key = list(columns = 1, reverse.rows = TRUE),
                      ordered = TRUE) +
  labs(x = "", y = "FREQUÊNCIA (%)") +
  scale_fill_manual(values = paleta) +
  guides(fill = guide_legend(title = "Escala Likert")) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5)  # Centraliza o título
    )
# Obter as dimensões da janela do navegador
largura <- session$clientData$output_likertPlot1_width
altura <- session$clientData$output_likertPlot1_height

ggplotly(g1) %>%
  layout(
    width = ifelse(is.null(largura), 800, largura),   # Largura dinâmica
    height = ifelse(is.null(altura), 750, altura),    # Altura dinâmica
    margin = list(l = 60, r = 80, t = 50, b = 100)   # Ajuste das margens internas
  )
  })
#------------------------------------------------------------------------------#
# Escala Likert GENERO
  output$likertPlot2 <- renderPlotly({
    
    # Filtrando os dados com base nos inputs
    dados_filtrados <- Dados_Clima %>%
      filter(MUNICIPIO == input$municipio)
    
    Dados_Clima[,1:9] <- lapply(Dados_Clima[,1:9], 
                                factor, 
                                levels = 1:3,
                                labels = c("Sim", 
                                           "Não", 
                                           "Não Sei Informar"),
                                order = TRUE)
    
    nomes <- read_excel("Dados_Clima.xls", 
                        sheet = 3)
    colnames(Dados_Clima)[1:9] <- nomes$Nomes
    table1(~., data = Dados_Clima, overall = "n(%)", decimal.mark = ",")
    
    caption  <- "Pesquisa Sustentabilidade"
    footnote <- "Fonte: CGP/DETRAN-PA"
    
    # Escala Likert p/ Gênero
    dados_grafico2 <- likert(as.data.frame(Dados_Clima[1:9]),
                             grouping = Dados_Clima$GENERO)
# Paleta de Cores
paleta <- brewer.pal(n=5, "RdBu")
paleta[3] <- "lightblue"
    
# Gráfico Likert
g2 <- likert.bar.plot(dados_grafico2,wrap = 60,ReferenceZero = 3,centered = TRUE,text.size = 4, hjust = 1,
                     legend = "Escala Likert",legend.position = "right",ordered = TRUE) +
      ggtitle("") +
      labs(x = "", 
           y = "FREQUÊNCIA (%)") +
      scale_fill_manual(values = paleta, 
                        breaks = levels(Dados_Clima$Q9)) +
      guides(fill = guide_legend(title = "Escala Likert")) +
      theme_bw(base_size = 11)+
      theme(
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5)  # Centers the title
      )
    ggplotly(g2)
  })
 
#------------------------------------------------------------------------------#
# Mapa de Geolocalização  
  
  output$mapa_municipios <- renderLeaflet({
    # Definindo os endereços para origem, intermediário, destino e novo ponto
    
    endereco_novo <- "Av Augusto Montenegro km 3, Belem, PA, Brazil"
    endereco_destino <- "Av Barao do Rio Branco 1287, Castanhal, PA, Brazil"
    endereco_velho <- "Av Benjamin Constant 285, Cameta, PA, Brazil"
    endereco_origem <- "Duque de Caxias 85, Altamira, PA, Brazil"
    endereco_intermediario <- "Av Cuiaba 890, Santarem, PA, Brazil"
   
    
    
    # Geocodificando os endereços
    novo_ponto <- tidygeocoder::geo(address = endereco_novo)             # Belém
    destino <- tidygeocoder::geo(address = endereco_destino)             # Castanhal
    velho_ponto <- tidygeocoder::geo(address = endereco_velho)           # Cametá
    origem <- tidygeocoder::geo(address = endereco_origem)               # Altamira
    intermediario <- tidygeocoder::geo(address = endereco_intermediario) # Marabá
    
    
    
    # Criando uma tabela com as coordenadas
    tab <- dplyr::bind_rows(origem, 
                            intermediario, 
                            destino, 
                            novo_ponto, 
                            velho_ponto)
    
    # Função para geocodificação reversa e obter o nome da cidade
    get_city_name <- function(lat, long) {
      url <- paste0("https://nominatim.openstreetmap.org/reverse?lat=", lat, "&lon=", long, "&format=json")
      response <- httr::GET(url)
      content <- httr::content(response, "parsed")
      city_name <- content$address$city
      return(city_name)
    }
    
    # Gerando os popups dinamicamente
    tab$city_name <- sapply(1:nrow(tab), function(i) get_city_name(tab$lat[i], tab$long[i]))
    
    # Criando a URL para calcular a rota
    url <- glue::glue(
      "http://router.project-osrm.org/route/v1/driving/{novo_ponto$long},{novo_ponto$lat};{destino$long},{destino$lat};{velho_ponto$long},{velho_ponto$lat};{origem$long},{origem$lat};{intermediario$long},{intermediario$lat}"
    )
    # Obtendo os dados da rota
    rota <- rjson::fromJSON(file = url)
    
    # Verificando se a rota foi encontrada e decodificando a geometria
    if (!is.null(rota$routes) && length(rota$routes) > 0) {
      tab_rota <- googleway::decode_pl(rota$routes[[1]]$geometry)
      
      # Plotando a rota e os pontos no mapa
      leaflet_map <- leaflet::leaflet(data = tab) %>%
        leaflet::addTiles() %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
        #addProviderTiles(providers$Esri.WorldStreetMap)%>%
        leaflet::addPolylines(
          lng = tab_rota$lon,
          lat = tab_rota$lat
        ) %>%
        leaflet::addMarkers(
          lng = tab$long,
          lat = tab$lat,
          popup = ~paste("Cidade: ", city_name)
        )
    }
  
  

    
    })
}

# ======================================================================================================#
# Executar o app
shinyApp(ui, server)
