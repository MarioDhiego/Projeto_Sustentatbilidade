

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(likert)
library(RColorBrewer)

# Interface do Usuário (UI)
shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(
      title = span(tags$b("COLETA SELETIVA"), 
                   style = "font-size: 18px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"),
      tags$li(class = "dropdown",
              a(href = "https://www.facebook.com/detranPARA",
                class = "fa fa-facebook",
                target = "_blank"
              )),
      tags$li(class = "dropdown",
              a(href = "https://www.instagram.com/detranpa_",
                class = "fa fa-instagram",
                target = "_blank"
              )
      ),
      tags$li(class = "dropdown",
              a(href = "https://twitter.com/DETRAN_PA",
                class = "fa fa-twitter",
                target = "_blank"
              )),
      tags$li(
        class="dropdown",
        tags$a(href="https://github.com/MarioDhiego",
               icon("github"), "Suporte", target = "_blank"))
    ),
    skin = "blue",
    scrollToTop = TRUE,
    options = list(sidebarExpandOnHover = TRUE),
    
    sidebar = dashboardSidebar(minified = FALSE,
                               collapsed = FALSE,
                               tags$img(src = "atitudes.jpg", 
                                        width = 230, 
                                        height = 100),
                               
                               sidebarMenu(
                                 menuItem("PROJETO", tabName = "projeto", icon = icon("info-circle")),
                                 menuItem("SÓCIO-ECONÔMICO", tabName = "socioeconomico", icon = icon("users")),
                                 menuItem("COLETA SELETIVA", tabName = "coleta", icon = icon("recycle")),
                                 menuItem("CIRETRAN", tabName = "ciretran", icon = icon("car")),
                                 selectInput("municipio", "Municípios:",
                                             choices = c("Altamira", "Santarem", "Maraba", "Paraupebas", "Breves"),
                                             selected = "Altamira")
                               )
    ),
    
    body = dashboardBody(
      tags$head(
        tags$style(HTML("
          .metodologia-texto {
            text-align: justify
            font-size: 18px; /* Ajuste o tamanho da fonte aqui */
          }
        "))
      ),
      tabItems(
        # Aba Projeto
        tabItem(tabName = "projeto",
                fluidRow(
                  box(title = "Objetivo", status = "info", solidHeader = TRUE,
                      p("Proporcionar informação e treinamento sobre o uso racional e sustentável dos recursos, descarte adequado de material inservível e que possa ser reciclado.")
                  )
                ),
                fluidRow(
                  box(title = "Metodologia", status = "info", solidHeader = TRUE,
                      p(class = "metodologia-texto",
                        "Um programa de conscientização sobre o descarte e coleta seletiva de lixo deve levar em consideração a utilização de recursos que visem atingir metas importantes no serviço público, alcançando assim as agências do DETRAN-PA que gerem mais resíduos e que possam ser reciclados. Nesse sentido, serão priorizadas as Ciretrans do tipo A, pois geralmente possuem mais servidores e prestam uma maior quantidade de serviços, sendo então a geração de lixo diretamente proporcional a esta demanda."
                      )
                  )
                )   
        ),
        
        # Aba Socio-Econômico
        tabItem(tabName = "socioeconomico",
                fluidRow(
                  box(title = "GÊNERO", 
                      status = "primary", 
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      plotlyOutput("sexoPlot", height = 300)),
                  box(title = "RAÇA/COR", 
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      plotlyOutput("racaPlot", height = 300)),
                  box(title = "IDADE", 
                      status = "primary", 
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      plotlyOutput("idadePlot", height = 300)),
                  box(title = "GRAU DE ESCOLARIDADE", 
                      status = "primary", 
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      plotlyOutput("escolaridadePlot", height = 300)),
                  box(title = "ESTADO CIVIL", 
                      status = "primary", 
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      plotlyOutput("estadoCivilPlot", height = 300)),
                  box(title = "CARGO/FUNÇÃO", 
                      status = "primary", 
                      solidHeader = TRUE,
                      collapsible = TRUE,
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
                  column(width = 12,
                         box(title = "Análise das Respostas", 
                             status = "warning", 
                             solidHeader = TRUE,
                             plotlyOutput("likertPlot", height = 600))
                  )
                )
        )
      )
      ),
    
    footer = dashboardFooter(
      left = HTML("CopyRight <b>&copy; Todos os Direitos Reservados.</b>"), 
      right = tags$b("Belém-PA, 2024 v.1")
    )
  ),
  # Server
  server = function(input, output, session) {
    
    # Dados de exemplo Socio-Econômico
    set.seed(123)
    data <- data.frame(
      Municipio = sample(c("Altamira", "Santarem", "Maraba", "Paraupebas", "Breves", "Castanhal", "Bragança"), 1000, replace = TRUE),
      Sexo = sample(c("MASCULINO", "FEMININO"), 1000, replace = TRUE),
      Raca = sample(c("PARDO", "NEGRO", "INDÍGENA", "BRANCO"), 1000, replace = TRUE),
      Idade = sample(18:70, 1000, replace = TRUE),
      Escolaridade = sample(c("EFI", "EFC", "EMI", "EMC", "ESI", "ESC"), 1000, replace = TRUE),
      EstadoCivil = sample(c("SOLTEIRO", "CASADO", "UNIÃO", "OUTROS"), 1000, replace = TRUE),
      Cargo = sample(c("Estagiário", "Terceirizado", "Auxiliar", "Assistente", "Analista", "Comissionado"), 1000, replace = TRUE)
    )
    
    # Dados de exemplo Coleta Seletiva
    coleta_data <- data.frame(
      Municipio = sample(c("Altamira", "Santarem", "Maraba", "Paraupebas", "Breves", "Castanhal", "Bragança"), 500, replace = TRUE),
      BairroColeta = sample(c("Sim", "Não", "Não Sei Informar"), 500, replace = TRUE),
      Informes = sample(c("Sim", "Não"), 500, replace = TRUE),
      SepararLixo = sample(c("Sim", "Não"), 500, replace = TRUE),
      SepararCorretamente = sample(c("Sim", "Não", "Não Sei Informar"), 500, replace = TRUE)
    )
    
    # Dados de exemplo CIRETRAN
    ciretran_data <- data.frame(
      Municipio = sample(c("Altamira", "Santarem", "Maraba", "Paraupebas", "Breves", "Castanhal", "Bragança"), 500, replace = TRUE),
      LixeiraFacilidade = sample(c("Sim", "Não"), 500, replace = TRUE),
      UsoGarrafaCaneca = sample(c("Sim", "Não", "Às Vezes"), 500, replace = TRUE),
      LixeiraColetaSeletiva = sample(c("Sim", "Não"), 500, replace = TRUE),
      DestinoFinalLixo = sample(c("Sim", "Não"), 500, replace = TRUE)
    )
    
    # Filtrar os dados Socio-Econômico com base no município selecionado
    filtered_data <- reactive({
      subset(data, Municipio == input$municipio)
    })
    
    # Filtrar os dados de Coleta Seletiva com base no município selecionado
    filtered_coleta_data <- reactive({
      subset(coleta_data, Municipio == input$municipio)
    })
    
    filtered_ciretran_data <- reactive({
      subset(ciretran_data, Municipio == input$municipio)
    })
    
    
    
    # Plot do gráfico de gênero
    output$sexoPlot <- renderPlotly({
      filtered_data <- filtered_data()
      sexo_count <- table(filtered_data$Sexo)
      sexo_df <- data.frame(Sexo = names(sexo_count), Count = as.numeric(sexo_count))
      plot_ly(sexo_df, x = ~Sexo, y = ~Count, type = 'bar', name = 'Gênero', 
              marker = list(color = 'rgba(55, 128, 191, 0.6)', 
                            line = list(color = 'rgba(55, 128, 191, 1.0)', width = 2))
              ) %>%
        layout(title = 'Distribuição por Gênero', xaxis = list(title = 'Gênero'), yaxis = list(title = 'Count'))
    })
    
    # Plot do gráfico de raça/cor
    output$racaPlot <- renderPlotly({
      filtered_data <- filtered_data()
      raca_count <- table(filtered_data$Raca)
      raca_df <- data.frame(Raca = names(raca_count), Count = as.numeric(raca_count))
      plot_ly(raca_df, x = ~Raca, y = ~Count, type = 'bar', name = 'Raça/Cor', 
              marker = list(color = 'rgba(50, 171, 96, 0.6)', 
                            line = list(color = 'rgba(50, 171, 96, 1.0)', width = 2))) %>%
        layout(title = 'Distribuição por Raça/Cor', xaxis = list(title = 'Raça/Cor'), yaxis = list(title = 'Count'))
    })
    
    # Plot do gráfico de idade
    output$idadePlot <- renderPlotly({
      filtered_data <- filtered_data()
      idade_hist <- hist(filtered_data$Idade, breaks = 10, plot = FALSE)
      idade_df <- data.frame(Idade = idade_hist$mids, Count = idade_hist$counts)
      plot_ly(idade_df, x = ~Idade, y = ~Count, type = 'bar', name = 'Idade', 
              marker = list(color = 'rgba(219, 64, 82, 0.6)', 
                            line = list(color = 'rgba(219, 64, 82, 1.0)', width = 2))) %>%
        layout(title = 'Distribuição por Idade', xaxis = list(title = 'Idade'), yaxis = list(title = 'Count'))
    })
    
    # Plot do gráfico de grau de escolaridade
    output$escolaridadePlot <- renderPlotly({
      filtered_data <- filtered_data()
      escolaridade_count <- table(filtered_data$Escolaridade)
      escolaridade_df <- data.frame(Escolaridade = names(escolaridade_count), Count = as.numeric(escolaridade_count))
      plot_ly(escolaridade_df, x = ~Escolaridade, y = ~Count, type = 'bar', name = 'Escolaridade', 
              marker = list(color = 'rgba(128, 0, 128, 0.6)', 
                            line = list(color = 'rgba(128, 0, 128, 1.0)', width = 2))) %>%
        layout(title = 'Distribuição por Grau de Escolaridade', xaxis = list(title = 'Escolaridade'), yaxis = list(title = 'Count'))
    })
    
    # Plot do gráfico de estado civil
    output$estadoCivilPlot <- renderPlotly({
      filtered_data <- filtered_data()
      estado_civil_count <- table(filtered_data$EstadoCivil)
      estado_civil_df <- data.frame(EstadoCivil = names(estado_civil_count), Count = as.numeric(estado_civil_count))
      plot_ly(estado_civil_df, x = ~EstadoCivil, y = ~Count, type = 'bar', name = 'Estado Civil', 
              marker = list(color = 'rgba(0, 128, 128, 0.6)', 
                            line = list(color = 'rgba(0, 128, 128, 1.0)', width = 2))) %>%
        layout(title = 'Distribuição por Estado Civil', xaxis = list(title = 'Estado Civil'), yaxis = list(title = 'Count'))
    })
    
    # Plot do gráfico de cargo/função
    output$cargoPlot <- renderPlotly({
      filtered_data <- filtered_data()
      cargo_count <- table(filtered_data$Cargo)
      cargo_df <- data.frame(Cargo = names(cargo_count), Count = as.numeric(cargo_count))
      plot_ly(cargo_df, x = ~Cargo, y = ~Count, type = 'bar', name = 'Cargo/Função', 
              marker = list(color = 'rgba(255, 165, 0, 0.6)', 
                            line = list(color = 'rgba(255, 165, 0, 1.0)', width = 2))) %>%
        layout(title = 'Distribuição por Cargo/Função', xaxis = list(title = 'Cargo/Função'), yaxis = list(title = 'Count'))
    })
    
    # Plot do gráfico de bairro com coleta seletiva
    output$bairroColetaPlot <- renderPlotly({
      filtered_coleta_data <- filtered_coleta_data()
      bairro_coleta_count <- table(filtered_coleta_data$BairroColeta)
      bairro_coleta_df <- data.frame(BairroColeta = names(bairro_coleta_count), Count = as.numeric(bairro_coleta_count))
      plot_ly(bairro_coleta_df, x = ~BairroColeta, y = ~Count, type = 'bar', name = 'Coleta Seletiva no Bairro', 
              marker = list(color = 'rgba(0, 255, 0, 0.6)', 
                            line = list(color = 'rgba(0, 255, 0, 1.0)', width = 2))) %>%
        layout(title = 'Coleta Seletiva no Bairro', xaxis = list(title = 'Coleta Seletiva no Bairro'), yaxis = list(title = 'Count'))
    })
    
    # Plot do gráfico de recebimento de informes
    output$informesPlot <- renderPlotly({
      filtered_coleta_data <- filtered_coleta_data()
      informes_count <- table(filtered_coleta_data$Informes)
      informes_df <- data.frame(Informes = names(informes_count), Count = as.numeric(informes_count))
      plot_ly(informes_df, x = ~Informes, y = ~Count, type = 'bar', name = 'Recebimento de Informes', 
              marker = list(color = 'rgba(255, 105, 180, 0.6)', 
                            line = list(color = 'rgba(255, 105, 180, 1.0)', width = 2))) %>%
        layout(title = 'Recebimento de Informes sobre Coleta Seletiva', xaxis = list(title = 'Recebimento de Informes'), yaxis = list(title = 'Count'))
    })
    
    # Plot do gráfico de separação de lixo
    output$separarLixoPlot <- renderPlotly({
      filtered_coleta_data <- filtered_coleta_data()
      separar_lixo_count <- table(filtered_coleta_data$SepararLixo)
      separar_lixo_df <- data.frame(SepararLixo = names(separar_lixo_count), Count = as.numeric(separar_lixo_count))
      plot_ly(separar_lixo_df, x = ~SepararLixo, y = ~Count, type = 'bar', name = 'Separação de Lixo', 
              marker = list(color = 'rgba(255, 69, 0, 0.6)', 
                            line = list(color = 'rgba(255, 69, 0, 1.0)', width = 2))) %>%
        layout(title = 'Separação de Lixo', xaxis = list(title = 'Separar Lixo'), yaxis = list(title = 'Count'))
    })
    
    # Plot do gráfico de separação correta do lixo
    output$separarCorretamentePlot <- renderPlotly({
      filtered_coleta_data <- filtered_coleta_data()
      separar_corretamente_count <- table(filtered_coleta_data$SepararCorretamente)
      separar_corretamente_df <- data.frame(SepararCorretamente = names(separar_corretamente_count), Count = as.numeric(separar_corretamente_count))
      plot_ly(separar_corretamente_df, x = ~SepararCorretamente, y = ~Count, type = 'bar', name = 'Separação Correta do Lixo', 
              marker = list(color = 'rgba(100, 149, 237, 0.6)', 
                            line = list(color = 'rgba(100, 149, 237, 1.0)', width = 2))) %>%
        layout(title = 'Separação Correta do Lixo', xaxis = list(title = 'Separação Correta'), yaxis = list(title = 'Count'))
    })
    
    # Gráficos CIRETRAN
    output$likertPlot <- renderPlotly({
      # Preparar os dados para o gráfico Likert
      likert_data <- filtered_ciretran_data() %>%
        select(LixeiraFacilidade, UsoGarrafaCaneca, LixeiraColetaSeletiva, DestinoFinalLixo) %>%
        mutate(across(everything(), factor, levels = c("Sim", "Às Vezes", "Não")))
      
      
      paleta <- brewer.pal(5, "RdBu")
      paleta[3] <- "#DFDFDF"
      
      likert_result <- likert(likert_data)
      
      # Criar o gráfico
      p <- likert.bar.plot(likert_result,  text.size=4) +
        theme(axis.text.y=element_text(size="12"))+
        labs(x="", y = "Frequência (%)", size=12)+
        ggtitle("Pesquisa sobre Sustentabilidade")+
        scale_fill_manual(values = paleta,
                          breaks = levels(likert_data))+
        guides(fill = guide_legend(title = "Resposta"))+
        theme_minimal()+
        theme(panel.grid = element_blank(),
              plot.background = element_rect(fill = "white"))
      ggplotly(p)
    })
  }
)
