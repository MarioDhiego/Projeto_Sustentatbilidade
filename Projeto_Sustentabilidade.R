library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(leaflet)
library(plotly)
library(dplyr)
library(scales) # Para a função percent()

# Interface do Usuário (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Sustentabilidade Ambiental", titleWidth = 350,
                  tags$li(class = "dropdown",
                          a(href = "https://www.facebook.com/detranPARA",
                            class = "fa fa-facebook",
                            target = "_blank"
                          )),
                  tags$li(class = "dropdown",
                          a(href = "https://www.instagram.com/detranpa_",
                            class = "fa fa-instagram",
                            target = "_blank"
                          )),
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
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("PROJETO", tabName = "defprojeto", icon = icon("book"),
               menuSubItem("Sobre Projeto", tabName="sobre1", icon=icon("book")),
               menuSubItem("Vídeo Institucional", tabName="video1", icon=icon("video"))
               ),
      menuItem("SÓCIO-ECONÔMICO", tabName = "socioeconomico", icon = icon("users")),
      menuItem("COLETA SELETIVA", tabName = "coleta", icon = icon("recycle")),
      menuItem("CIRETRAN", tabName = "ciretran", icon = icon("car")),
      selectInput("municipio", "MUNICÍPIOS:",
                  choices = c("Altamira", "Marabá", "Castanhal", "Santarem", "Parauapebas"),
                  selected = "Altamira")
    )
  ),
  dashboardBody(
    fluidPage(
      tags$head(
        tags$link(rel = "shortcut icon", href = "icons8-favicon-94.png", type = "image/x-icon"), 
        tags$link(
          rel = "stylesheet",
          href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
        )
      )
    ),
#------------------------------------------------------------------------------#
tabItems(
  tabItem(tabName="sobre1",
          tabBox(id="t1", width=12,
                 tabPanel("JUSTIFICATIVA",
                      icon = icon("address-card"),
                      fluidRow(
                        column(width = 8,
                               position = "left",
                               tags$img(
                                 id = "foto1",
                                 src = "sustentabilidade.jpg",
                                 controls = "controls",
                                 width = 550,height = 400),
                               tags$br(),
                               tags$a("Photo by Asdecom"),
                               align = "left"
                                ),
                          column(width = 4,
                                 tags$br(),
                                 tags$p(
                                   style = "text-align:justify;font-si20pt",
                                   strong("Após reunião com o presidente eleito Luiz Inácio Lula da Silva, no dia 11 de Janeiro de 2023, em Brasília, o governador do Pará e presidente do Consórcio Interestadual de Desenvolvimento Sustentável da Amazônia Legal (CAL), Helder Barbalho, informou que a capital paraense, Belém, é a cidade brasileira escolhida como candidata oficial do país para sediar uma edição da Conferência das Nações Unidas sobre as Mudanças Climáticas (COP 30) em 2025. Assim, várias ações serão necessárias para que o Estado do Pará caminhe de acordo com esta agenda de governo."
                                    )
                                  ),
                                  tags$br(),
                                  tags$p(
                                    style = "text-align: justify;font-si20pt",
                                    strong("Conseqüentemente, surge a necessidade da difusão do conceito de Responsabilidade Socioambiental, que está ligado a ações que respeitam o meio ambiente e a políticas que tenham como um dos principais objetivos a sustentabilidade. Logo, todos são responsáveis pela preservação ambiental: entidades governamentais, empresas privadas e todos que compõem a sociedade civil organizada."
                                    )
                                  ),
                                  tags$br(),
                                  tags$p(
                                    style = "text-align: justify;font-si20pt",
                                    strong(""
                                    )
                                  )
                                )
                              )), 
         
                     
                     tabPanel("LEGISLAÇÃO VIGENTE",
                              icon = icon("layer-group"),
                              fluidRow(
                                column(
                                  width = 4,
                                  position = "center",
                                  solidHeader = TRUE,
                                  tags$br(),
                                  tags$p(
                                    style = "text-align: justify;font-si20pt",
                                    strong(
                                      "A Lei Estadual nº 5.899, de 01/08/1995, considera, no Estado do Pará, a coleta seletiva e a reciclagem de lixo como atividades ecológicas de relevância social e de interesse público."
                                    )
                                  ),
                                  tags$br(),
                                  tags$p(
                                    style = "text-align: justify;font-si20pt",
                                    strong(
                                      "Lei Ordinária Estadual n° 6.918, 10/10/2006, dispõe sobre a Política Estadual de Reciclagem de Materiais e dá outras providências."
                                    )
                                  ),
                                  tags$br(),
                                  tags$p(
                                    style = "text-align: justify;font-si20pt",
                                    strong(
                                      "Decreto Estadual nº 801, 15/02/2008, institui a separação de resíduos sólidos recicláveis, na fonte geradora, em todos os órgãos da Administração Estadual."
                                    )
                                  ),
                                  tags$br(),
                                  tags$p(
                                    style = "text-align: justify;font-si20pt",
                                    strong(
                                      "Lei Estadual n° 9.149, 23/11/2020, dispõe sobre a substituição e recolhimento de sacolas plásticas em estabelecimentos comerciais localizados no Estado do Pará."
                                    )
                                  )
                                )
                              )),
                     tabPanel("SOFTWARE'S", icon=icon("computer"),
                              fluidRow(
                                column(width=4,
                                       position="center",solidHeader = TRUE,
                                       tags$br(),
                                       tags$p(style="text-align: justify;font-si20pt",
                                              strong("Para Criação do Anuário em Formato Web com Dasboard Interativos, foi Desenvolvido um script em Linguagem de Programação R-PROJECT Versão 4.2.2, no formato de Projeto de Software Livre de Código Aberto (open source), ou seja, pode ser utilizado sem custos de licença (R DEVELOPMENT CORE TEAM, 2022)")),
                                       tags$br(),
                                       tags$img(
                                         id="foto2",
                                         src="R.jpg",
                                         controls="controls",
                                         width=180,height=150),
                                       tags$br(),
                                       tags$a("Software R",
                                              href = "https://cran.r-project.org/bin/windows/base/R-4.3.2-win.exe"),
                                       tags$br(),
                                ),
                                column(width=4,
                                       position="center",solidHeader = TRUE,
                                       tags$br(),
                                       tags$p(style="text-align: justify;font-si20pt",
                                              strong("Foi utilizado um Ambiente de Desenvolvmento Integrado (IDE) Chamado Rstudio Versão 1.4.1.7, utilizando um Processo de Extração-Transformação-Carga(ETL) com uso de Várias bibliotecas (library), para o Ambiente Windows")),
                                       tags$br(),
                                       tags$img(
                                         id="foto3",
                                         src="RStudio.png",
                                         controls="controls",
                                         width=180,height=150),
                                       tags$br(),
                                       tags$a("RStudio",
                                              href = "https://download1.rstudio.org/electron/windows/RStudio-2023.09.1-494.exe"),
                                       tags$br(),
                                )
                              )
                     ),
                     tabPanel("MATERIAL E MÉTODOS", 
                              icon=icon("book"),
                              fluidRow(
                                column(width = 4, 
                                       position = "center",
                                       tags$br(),
                                       tags$br("Metodologia"),
                                       tags$br(),
                                       tags$p(
                                         style = "text-align:justify;font-si20pt",
                                         strong(
                                           "A Metodologia Adotada para o Planejamento e execução do Projeto foi apoiada na Estratégia de Proatividade e Parceria Desenvolvida pela GRSP (CARDITA e DI PIETRO, 2010)."
                                         )
                                       ),
                                       tags$br(),
                                       tags$p(
                                         style = "text-align:justify;font-si20pt",
                                         strong(
                                           "A Estratégia de Proatividade e Parceria (EPP) consiste em um Modelo Desenvolvido para Tratar das questões de Segurança no Trânsito."
                                         )
                                       ),
                                       tags$br(),
                                       tags$p(style = "text-align:justify;font-si20pt",
                                              strong(" As Etapas a Serem Desenvolvidas Durante Aplicação do Projeto são:")),
                                       tags$br(),
                                       tags$p(style = "text-align:justify;font-si20pt",
                                              strong("1) Articulação Intersetorial e Formação")),
                                       tags$p(style = "text-align: justify;font-si20pt",
                                              strong("2) Qualificação, Integração e Análise de Dados")),
                                       tags$p(style = "text-align: justify;font-si20pt",
                                              strong("3) Ações Integradas de Segurança no Trânsito")),
                                       tags$p(style = "text-align: justify;font-si20pt",
                                              strong("4) Monitoramento, Avaliação de Desenpenhp e Reconhecimento")),
                                       tags$p(style = "text-align: justify;font-si20pt",
                                              strong("5) Revisão Geral Anual")),
                                       tags$p(style = "text-align: justify;font-si20pt",
                                              strong("6) Renovação e Expansão"))
                                ),
                                tags$br(),
                                column(
                                  width = 4,
                                  position = "center",
                                  tags$br("Pareamento"),
                                  tags$br(),
                                  tags$p(
                                    style = "text-align:justify;font-si20pt",
                                    strong(
                                      "Para o Relacionamento das Múltiplas Bases de Dados(pareamento), utilizou-se o Método Probabilístico de Relacionamento de Registro desenvolvido por Fellegi e Sunter (1969)."
                                    )
                                  ),
                                  tags$br(),
                                  tags$p(
                                    style = "text-align:justify;font-si20pt",
                                    strong(
                                      "A principal dificuldade do pareamento é a não existência de um identificador único que permita vincular um Boletim de Ocorrência à uma Autorização de Internação Hospitalar ou Declaração de Òbito."
                                    )
                                  ),
                                  tags$br(),
                                  tags$p(
                                    style = "text-align:justify;font-si20pt",
                                    strong(
                                      "O Processo de Padronização de Variáveis, utilizando o Método Probabilístico, foi realizado para homogeneizar as variáveis das diferentes bases de dados, visando minimizar erros no processo de pareamento, com a alocação de registros da mesma vítima num bloco lógico para evitar: erros fonéticos, perda de informação, etc."
                                    )
                                  )
                                )
                              )
                     ),
                     tabPanel(
                       "CRÉDITOS",
                       icon = icon("phone"),
                       fluidRow(
                         column(
                           width = 4,
                           position = "center",
                           solidHeader = TRUE,
                           tags$br(),
                           tags$p(
                             style = "text-align: justify;font-si20pt",
                             strong("DEPARTAMENTO DE TRÂNSITO DO ESTADO DO PARÁ - DETRAN/PA")
                           ),
                           tags$p(style = "text-align: justify;font-si20pt",
                                  strong("RENATA MIRELA COELHO")),
                           tags$p(style = "text-align: justify;font-si20pt",
                                  strong("AVENIDA: AUGUSTO MONTENEGRO KM 03 S/N")),
                           tags$p(style = "text-align: justify;font-si20pt",
                                  strong("CEP: 66635-918 - PARQUE VERDE - BELÉM - PARÁ")),
                           tags$a("https://www.detran.pa.gov.br",
                                  href = "https://www.detran.pa.gov.br"),
                           tags$br(),
                           tags$br(),
                           tags$p(
                             style = "text-align: justify;font-si20pt",
                             strong(
                               "Esta publicação deve ser citada como: Departamento de Trânsito do Estado do Pará (DETRAN-PA), Anuário Estatístico de Acidentes de Trânsito, 2023 (LC/PUB.2023/1-P), Belém, 2023."
                             )
                           ),
                           tags$br(),
                           tags$p(
                             style = "text-align: justify;font-si20pt",
                             strong(
                               "A autorização para a reprodução total ou parcial deste trabalho deve ser solicitada ao Departamento de Trânsito do Estado do Pará, Gerência de Análise e Estatística de Trânsito, gerest@detran.pa.gov.br. Os Estados membros das Nações Unidas e suas instituições governamentais podem reproduzir este trabalho sem autorização prévia. Solicita-se apenas que mencionem a fonte e informem ao DETRAN-PA de tal reprodução."
                             )
                           ),
                           tags$br(),
                           
                         ),
                         column(width = 4,
                                position = "center",
                                solidHeader = TRUE,
                                tags$br(),
                                leafletOutput("mapa"),
                         )
                       )        
                     ),
                     tabPanel("SUGESTÕES",
                              fluidRow(
                                column(
                                  width = 4,
                                  position = "center",
                                  tags$br(),
                                  tags$p(
                                    style = "text-align: justify;font-si20pt",
                                    strong(
                                      "Reclamações, sugestões, críticas e elogios relacionados ao Anuário
Estatístico de Acidentes de Trânsito do DETRAN-PA podem ser registrados na Gerência de Análise Estatística de Trânsito, por intermédio do "
                                    )
                                  ),
                                  tags$a("estatisticadetransito@detran.pa.gov.br",
                                         href = "gerest@detran.pa.gov.br"),
                                )
                              ))
              )
      ),
      
      
      
#------------------------------------------------------------------------------#
      # Aba Socio-Econômico
      tabItem(tabName = "socioeconomico",
              fluidRow(
                box(title = "Distribuição po Gênero", status = "primary", solidHeader = TRUE,
                    plotlyOutput("sexoPlot", height = 300)),
                box(title = "Distribuição por Raça/Cor", status = "primary", solidHeader = TRUE,
                    plotlyOutput("racaPlot", height = 300)),
                box(title = "Distribuição por Idade", status = "primary", solidHeader = TRUE,
                    plotlyOutput("idadePlot", height = 300)),
                box(title = "Distribuição por Grau de Escolaridade", status = "primary", solidHeader = TRUE,
                    plotlyOutput("escolaridadePlot", height = 300)),
                box(title = "Distribuição por Estado Civil", status = "primary", solidHeader = TRUE,
                    plotlyOutput("estadoCivilPlot", height = 300)),
                box(title = "Distribuição por Cargo/Função", status = "primary", solidHeader = TRUE,
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
    ),
    footer = dashboardFooter(
      left = tags$b("DETRAN-PA"), 
      right = tags$b("BELÉM-PA, 2024 v.1")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Carregar os dados do Excel
  data <- readxl::read_excel("BANCO_PROJETO_SUSTENTABILIDADE.xlsx")
  
  # Filtrar dados com base no município selecionado
  filtered_data <- reactive({
    subset(data, MUNICIPIO == input$municipio)
  })
  
  # Função para criar gráficos de barras com percentuais
  plot_with_percent <- function(data, x_var, fill_var, title) {
    ggplot(data, aes_string(x = x_var, fill = fill_var)) +
      geom_bar(color = "black") +
      geom_text(stat = 'count', aes(label = scales::percent(..count../sum(..count..))),
                position = position_stack(vjust = 0.5), color = "white") +
      labs(title = title, x = "", y = "Nº de Respostas") +
      theme_minimal()
  }
  
  # Socio-Econômico
  output$sexoPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "SEXO", "SEXO", ""))
  })
  
  output$racaPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "RACA", "RACA", ""))
  })
  
  output$idadePlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = IDADE)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "", x = "Idades", y = "Nº de Respostas") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$escolaridadePlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "ESCOLARIDADE", "ESCOLARIDADE", ""))
  })
  
  output$estadoCivilPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "ESTADO_CIVIL", "ESTADO_CIVIL", ""))
  })
  
  output$cargoPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "CARGO_FUNCAO", "CARGO_FUNCAO", ""))
  })
  
  # Coleta Seletiva
  output$bairroColetaPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "P9", "P9", ""))
  })
  
  output$informesPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "P10", "P10", ""))
  })
  
  output$separarLixoPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "P11", "P11", ""))
  })
  
  output$separarCorretamentePlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "P12", "P12", ""))
  })
  
  # CIRETRAN
  output$lixeiraFacilidadePlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "P16", "P16", ""))
  })
  
  output$usoGarrafaCanecaPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "P17", "P17", ""))
  })
  
  output$lixeiraColetaSeletivaPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "P19", "P19", ""))
  })
  
  output$destinoFinalLixoPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "P20", "P20", ""))
  })
}

# Executar o aplicativo
shinyApp(ui, server)
