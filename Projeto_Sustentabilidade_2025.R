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
library(webshot)
library(kableExtra)
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
    geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),position = position_stack(vjust = 0.5), color = "white") +
    labs(title = title, x = "", y = "Nº de Entrevistados") +
    theme_minimal()}
# ======================================================================================================#
# Interface do Usuário (UI)
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Projeto Sustentabilidade Ambiental",
    titleWidth = 390,
    tags$li(class = "dropdown",style = "margin-right: 15px; display: inline-block;",  
a(href = "https://www.facebook.com/detranPARA", 
  class = "fa fa-facebook fa-lg", 
  target = "_blank", 
  title = "Facebook", 
  style = "color: #3b5998; transition: color 0.3s;"),
    tags$style(HTML(".fa-facebook:hover {color: #8b9dc3;}"))),
    tags$li(class = "dropdown",style = "margin-right: 15px; display: inline-block;", 
  a(href = "https://www.instagram.com/detranpa_", class = "fa fa-instagram", target = "_blank", title = "InstaGram",style = "color: #e1306c; transition: color 0.3s;"),
      tags$style(HTML(".fa-instagram:hover {color: #fd1d1d;}"))),
    tags$li(class = "dropdown",style = "margin-right: 15px; display: inline-block;", 
      a(href = "https://twitter.com/DETRAN_PA",class = "fa fa-twitter",target = "_blank",title = "Twitter",
        style = "color: #1da1f2; transition: color 0.3s;"),tags$style(HTML(".fa-twitter:hover {color: #0d95e8;}"))),
    tags$li(class = "dropdown",style = "margin-right: 15px; display: inline-block;", 
      a(href = "https://github.com/MarioDhiego", icon("github"), "Suporte", target = "_blank", title = "Suporte",style = "color: #333; transition: color 0.3s;"),
      tags$style(HTML(".fa-github:hover {color: #6e6e6e;}")))

),
  dashboardSidebar(minified = FALSE,collapsed = FALSE,tags$img(src = "detran1.jpeg",width = 230,height = 150),
    sidebarMenu(
      menuItem("PROJETO", 
               tabName = "defprojeto", icon = icon("book"),
               menuSubItem("Sobre Projeto", tabName = "sobre1", icon = icon("book")),
               menuSubItem("Localização", tabName = "local1", icon = icon("video"))),
      menuItem("CIRETRAN'S",
               tabName = "catCiretran", icon = icon("book"),
               menuSubItem("TIPO A", tabName = "tipoA", icon = icon("book")),
               menuSubItem("TIPO B", tabName = "tipoB", icon = icon("book")),
               menuSubItem("HOMOLOGADAS", tabName = "tipoC", icon = icon("book")),
               menuSubItem("POSTO DE ATENDIMENTO", tabName = "tipoD", icon = icon("book"))
               ),
      
      menuItem("PALESTRAS",
               tabName = "palestra", icon = icon("book"),
               menuSubItem("ALTAMIRA", tabName = "ciretran1", icon = icon("video")),
               menuSubItem("MARABÁ", tabName = "ciretran2", icon = icon("video")),
               menuSubItem("BELÉM", tabName = "ciretran3", icon = icon("video")),
               menuSubItem("CASTANHAL", tabName = "ciretran4", icon = icon("video"))),
      
      menuItem("SÓCIOECONÔMICO", tabName = "analises", icon = icon("chart-bar")),
      
      menuItem("PERCEPÇÃO", tabName = "escalalikert", icon = icon("book"),
               menuSubItem("Percepção Geral", tabName = "likertgeral", icon = icon("book")),
               menuSubItem("Percepção Por Gênero", tabName = "likertgenero", icon = icon("book"))),
      
      selectInput("municipio", "MUNICÍPIO:",choices = NULL, selected = NULL),
      
      actionButton("reset_button", "Reiniciar Filtros", class = "btn-success")
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tipoA",
        tabPanel("TIPOS DE CIRETRAN'S",
                 icon = icon("address-card"),
                 fluidRow(
                   box(
                     width = 12,
                     title = tags$div("CIRETRAN DO TIPO A",
                                      style = "text-align: center"
                     ),
                     status = "success",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     headerBorder = TRUE,
                     div(
                       style = "text-align: center;",
                       class = "elemente",
                       DiagrammeROutput("ciretrantipoA")
                     )
                   )
                 )
        )
      ),
      tabItem(
        tabName = "tipoB",
        tabPanel("TIPOS DE CIRETRAN'S",
                 icon = icon("address-card"),
                 fluidRow(
                   box(
                     width = 12,
                     title = "CIRETRAN DO TIPO B",
                     style = "text-align: center",
                     status = "success",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     headerBorder = TRUE,
                     div(
                       class = "elemente",
                       DiagrammeROutput("ciretrantipoB")
                     )
                   )
                 )
        )
      ),
      tabItem(
        tabName = "tipoC",
        tabPanel("TIPOS DE CIRETRAN'S", icon = icon("address-card"), fluidRow(
          box(
            width = 12,
            title = "CIRETRAN HOMOLOGADAS",
            style = "text-align: center",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            headerBorder = TRUE,
            div(
              style = "text-align: center;",
              class = "elemente",
              DiagrammeROutput("ciretrantipoC")
            )
          )
        ))
      ),
      tabItem(
        tabName = "tipoD",
        tabPanel("TIPOS DE CIRETRAN'S", icon = icon("address-card"), fluidRow(
          box(
            width = 12,
            title = "POSTO DE ATENDIMENTO",
            style = "text-align: center",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            headerBorder = TRUE,
            div(
              style = "text-align: center;",
              class = "elemente",
              DiagrammeROutput("ciretrantipoD")
            )
          )
        ))
      ),

      tabItem(
        tabName = "ciretran1",
        tabBox(
          id = "t3", width = 12,
          tabPanel("PALESTRAS REALIZADAS",
                   tags$br(),
                   h3("MUNICÍPIO DE ALTAMIRA", align = "center"),
                   tags$br(),
                   icon = icon("address-card"),
                   fluidRow(
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestra1",
                         src = "Palestra1_ATM.jpg",
                         controls = "controls",
                         width = 430, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: GT/CRH/DETRAN"),
                       align = "left"
                     ),
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestra2",
                         src = "Palestra2_ATM.jpg",
                         controls = "controls",
                         width = 430, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: GT/CGP/DETRAN"),
                       align = "left"
                     ),
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestra3",
                         src = "Palestra3_ATM.jpg",
                         controls = "controls",
                         width = 430, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: GT/CGP/DETRAN"),
                       align = "left"
                     )
                   )
          )
        )
      ), 
      tabItem(
        tabName = "ciretran2",
        tabBox(
          id = "t4", width = 12,
          tabPanel("PALESTRAS REALIZADAS",
                   tags$br(),
                   h3("MUNICÍPIO DE MARABÁ", align = "center"),
                   tags$br(),
                   icon = icon("address-card"),
                   fluidRow(
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestramaraba1",
                         src = "Palestra3_MAB.jpg",
                         controls = "controls",
                         width = 430, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: GT/CGP/DETRAN"),
                       align = "left"
                     ),
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestramaraba2",
                         src = "Palestra1_MAB.jpg",
                         controls = "controls",
                         width = 430, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: GT/CGP/DETRAN"),
                       align = "left"
                     ),
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestramaraba3",
                         src = "Palestra2_MAB.jpg",
                         controls = "controls",
                         width = 430, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: GT/CGP/DETRAN"),
                       align = "left"
                     )
                   )
          )
        )
      ),
      tabItem(
        tabName = "ciretran3",
        tabBox(
          id = "t5", width = 12,
          tabPanel("PALESTRAS REALIZADAS",
                   tags$br(),
                   h3("MUNICÍPIO DE BELÉM", align = "center"),
                   tags$br(),
                   icon = icon("address-card"),
                   fluidRow(
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestrabelem1",
                         src = "Palestra_Belem3.jpeg",
                         controls = "controls",
                         width = 430, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: ASDECOM"),
                       align = "left"
                     ),
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestrabelem2",
                         src = "Palestra_Belem2.jpeg",
                         controls = "controls",
                         width = 430, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: ASDECOM"),
                       align = "left"
                     ),
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestrabelem3",
                         src = "Palestra_Belem1.jpeg",
                         controls = "controls",
                         width = 430, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: ASDECOM"),
                       align = "left"
                     )
                   )
          )
        )
      ),
      tabItem(
        tabName = "ciretran4",
        tabBox(
          id = "t6", width = 12,
          tabPanel("PALESTRAS REALIZADAS",
                   tags$br(),
                   h3("MUNICÍPIO DE CASTANHAL", align = "center"),
                   tags$br(),
                   icon = icon("address-card"),
                   fluidRow(
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestracastanhal1",
                         src = "castanhal1.jpg",
                         controls = "controls",
                         width = 420, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: ASDECOM"),
                       align = "left"
                     ),
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestracastanhal2",
                         src = "castanhal2.jpg",
                         controls = "controls",
                         width = 420, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: ASDECOM"),
                       align = "left"
                     ),
                     column(
                       width = 4,
                       position = "left",
                       tags$img(
                         id = "palestracastanhal3",
                         src = "castanhal3.jpg",
                         controls = "controls",
                         width = 420, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo: ASDECOM"),
                       align = "left"
                     )
                   )
          )
        )
      ),
      tabItem(
        tabName = "sobre1",
        tabBox(
          id = "t1", width = 12,
          tabPanel("REFERENCIAL",
                   icon = icon("address-card"),
                   fluidRow(
                     column(
                       width = 8,
                       position = "left",
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$img(
                         id = "foto1",
                         src = "sustentabilidade.jpg",
                         controls = "controls",
                         width = 650, height = 450
                       ),
                       tags$br(),
                       tags$a("Photo by Asdecom"),
                       align = "left"
                     ),
                     column(
                       width = 4,
                       tags$br(),
                       h3("JUSTIFICATIVA", align = "center"),
                       tags$br(),
                       tags$p(
                         style = "text-align:justify;font-si20pt",
                         strong("Após reunião com o presidente eleito Luiz Inácio Lula da Silva, no dia 11 de Janeiro de 2023, em Brasília, o governador do Pará e presidente do Consórcio Interestadual de Desenvolvimento Sustentável da Amazônia Legal (CAL), Helder Barbalho, informou que a capital paraense, Belém, é a cidade brasileira escolhida como candidata oficial do país para sediar uma edição da Conferência das Nações Unidas sobre as Mudanças Climáticas (COP 30) em 2025. Assim, várias ações serão necessárias para que o Estado do Pará caminhe de acordo com esta agenda de governo.")
                       ),
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("Conseqüentemente, surge a necessidade da difusão do conceito de Responsabilidade Socioambiental, que está ligado a ações que respeitam o meio ambiente e a políticas que tenham como um dos principais objetivos a sustentabilidade. Logo, todos são responsáveis pela preservação ambiental: entidades governamentais, empresas privadas e todos que compõem a sociedade civil organizada.")
                       ),
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("")
                       )
                     )
                   )
          ),
          tabPanel("BASE LEGAL",
                   icon = icon("layer-group"),
                   fluidRow(
                     column(
                       width = 4,
                       position = "center",
                       tags$br(),
                       h3("LEGISLAÇÃO ESTADUAL", align = "center"),
                       tags$br(),
                       solidHeader = TRUE,
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("A Lei Estadual nº 5.899, de 01/08/1995, considera, no Estado do Pará, a coleta seletiva e a reciclagem de lixo como atividades ecológicas de relevância social e de interesse público.")
                       ),
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("A Lei Ordinária Estadual n° 6.918, 10/10/2006, dispõe sobre a Política Estadual de Reciclagem de Materiais e dá outras providências.")
                       ),
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("O Decreto Estadual nº 801, 15/02/2008, institui a separação de resíduos sólidos recicláveis, na fonte geradora, em todos os órgãos da Administração Estadual.")
                       ),
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("A Lei Estadual n° 9.149, 23/11/2020, dispõe sobre a substituição e recolhimento de sacolas plásticas em estabelecimentos comerciais localizados no Estado do Pará.")
                       )
                     ),
                     column(
                       width = 4,
                       position = "center",
                       tags$br(),
                       h3("ODS", align = "center"),
                       tags$br(),
                       solidHeader = TRUE,
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("O Programa de Sustentabilidade Ambiental do DETRAN-PA está pautado nos 17 Objetivos de Desenvolvimento Sustentável da ONU, chamado ODS , no qual é considerado um apelo global para acabar com a pobreza, proteger o meio ambiente e o clima. Sendo alinhado principalmente nos Objetivos 11 e 12.")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("Objetivo 11.6: Até 2030, reduzir o impacto ambiental negativo per capita das cidades, inclusive prestando especial atenção à qualidade do ar, gestão de resíduos municipais e outros;")
                       ),
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("Objetivo 12.5: Até 2030, reduzir substancialmente a geração de resíduos por meio da prevenção, redução, reciclagem e reuso;")
                       ),
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("Objetivo 12.7: Promover práticas de compras públicas sustentáveis, de acordo com as políticas e prioridades nacionais;")
                       ),
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("Objetivo 12.8:  Até 2030, garantir que as pessoas, em todos os lugares, tenham informação relevante e conscientização para o desenvolvimento sustentável e estilos de vida em harmonia com a natureza;")
                       )
                     )
                   )
          ),
          tabPanel("MATERIAL E MÉTODOS",
                   icon = icon("book"),
                   fluidRow(
                     column(
                       width = 4,
                       position = "center",
                       tags$br(),
                       h3("OBJETIVO GERAL", align = "center"),
                       tags$br(),
                       tags$p(
                         style = "text-align:justify;font-si20pt",
                         strong(
                           "Implantar o Projeto de Sustentabilidade Ambiental nas Agências Regionais do DETRAN-PA, cujo enfoque é proporcionar informação e treinamento sobre o uso racional e sustentável dos recursos, descarte adequado de material inservível e que possa ser reciclado."
                         )
                       ),
                       tags$br(),
                       h3("OBJETIVOS ESPECÍFICOS", align = "center"),
                       tags$p(
                         style = "text-align:justify;font-si20pt",
                         strong(" ")
                       ),
                       tags$p(
                         style = "text-align:justify;font-si20pt",
                         strong("1) Treinamento e Capacitação para todos os servidores das Ciretrans, sobre a importância da cultura de preservação do meio ambiente;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("2) Análise e Compreensão das Ciretrans tipo A, que mais geram resíduos que possam ser reciclados;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("3) Sensibilizar os servidores e usuários para que adotem estas práticas em suas casas, locais de lazer e outros locais de trabalho;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("4) Colaborar com possíveis estudos acadêmicos sobre o descarte consciente de resíduos, políticas de reciclagem e responsabilidade socioambiental.")
                       ),
                       tags$p(style = "text-align: justify;font-si20pt")
                     ),
                     column(
                       width = 4,
                       position = "center",
                       tags$br(),
                       h3("ETAPAS OPERACIONAIS", align = "center"),
                       tags$br(),
                       tags$p(
                         style = "text-align:justify;font-si20pt",
                         strong("1) Visita Técnica as CIRETRAN'S do Tipo A;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("2) Reunião de Alinhamento para apresentação do projeto ao Gerente da CIRETRAN;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("3) Aplicação do Questionário de Percepção sobre Sustentabilidade Ambiental;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("4) Palestra Sobre o Uso Racional dos Recursos/Descarte Seletivo;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("5) Reunião Técnica com a Secretaria Municipal Meio Ambiente;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("6) Pesquiva de Levantamento: Cooperativas/Associações de Reciclagem;")
                       ),
                       tags$br(),
                       h3("MÉTRICA", align = "center"),
                       tags$br(),
                       tags$p(
                         style = "text-align:justify;font-si20pt",
                         strong(
                           "Para a coleta dos dados foi utilizado um instrumento semiestruturado composto por 21 itens que versam sobre sustentabilidade ambiental. A estrutura do questionário contém três subescalas, que medem características socioeconômicas, coleta seletiva e destino do lixo."
                         )
                       )
                     ),
                     column(
                       width = 4,
                       position = "center",
                       tags$br(),
                       h1("Relatório", align = "center"),
                       tags$br(),
                       
                       # Exibir o PDF dentro do app
                       tags$iframe(
                         src = "Report_Sustentabilidade.pdf",  # O arquivo deve estar na pasta 'www'
                         width = "100%",
                         height = "550px"
                       )
                     )
                     
                     
                     
                     
                     
                     
                   )
          ),
          tabPanel("RECURSO COMPUTACIONAL",
                   icon = icon("computer"),
                   fluidRow(
                     column(
                       width = 4,
                       position = "center",
                       tags$br(),
                       h3("SOFTWARE", align = "center"),
                       tags$br(),
                       solidHeader = TRUE,
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong(
                           "Para Criação do Painel em Formato Web com Dasboard Interativos, foi Desenvolvido um script em Linguagem de Programação R-PROJECT Versão 4.4.1, no formato de Projeto de Software Livre de Código Aberto (open source), ou seja, pode ser utilizado sem custos de licença (R DEVELOPMENT CORE TEAM, 2024)"
                         )
                       ),
                       tags$br(),
                       tags$img(
                         id = "foto2",
                         src = "R.jpg",
                         controls = "controls",
                         width = 180, height = 150
                       ),
                       tags$br(),
                       tags$a("Software R",
                              href = "https://cran.r-project.org/bin/windows/base/R-4.3.2-win.exe"
                       ),
                       tags$br(),
                     ),
                     column(
                       width = 4,
                       position = "center", solidHeader = TRUE,
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("Foi utilizado um Ambiente de Desenvolvmento Integrado (IDE) Chamado Rstudio Versão 1.4.1.7, utilizando um Processo de Extração-Transformação-Carga(ETL) com uso de Várias bibliotecas (library), para o Ambiente Windows")
                       ),
                       tags$br(),
                       tags$br(),
                       tags$img(
                         id = "foto3",
                         src = "RStudio.png",
                         controls = "controls",
                         width = 190, height = 170
                       ),
                       tags$br(),
                       tags$a("RStudio",
                              href = "https://download1.rstudio.org/electron/windows/RStudio-2023.09.1-494.exe"
                       ),
                       tags$br(),
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
                h3("DIREITOS AUTORAIS", align = "center"),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong("DEPARTAMENTO DE TRÂNSITO DO ESTADO DO PARÁ")
                ),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong("RENATA MIRELA COELHO")
                ),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong("AVENIDA: AUGUSTO MONTENEGRO KM 03 S/N")
                ),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong("CEP: 66635-918 - PARQUE VERDE - BELÉM - PARÁ")
                ),
                tags$a("https://www.detran.pa.gov.br",
                       href = "https://www.detran.pa.gov.br"
                ),
                tags$br(),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "Esta publicação deve ser citada como: Departamento de Trânsito do Estado do Pará (DETRAN-PA), Programa de Sustentatabilidade Ambiental Por Todo Pará, 2025 (LC/PUB.2025/1-P), Belém, 2025."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "A autorização para a reprodução total ou parcial deste trabalho deve ser solicitada ao Departamento de Trânsito do Estado do Pará, Gerência de Treinamento, getren@detran.pa.gov.br. Os Estados membros das Nações Unidas e suas instituições governamentais podem reproduzir este trabalho sem autorização prévia. Solicita-se apenas que mencionem a fonte e informem ao DETRAN-PA de tal reprodução."
                  )
                ),
                tags$br(),
              ),
              column(
                width = 4,
                position = "center",
                solidHeader = TRUE,
                tags$br(),
                leafletOutput("mapa"),
              )
            )
          ),
          tabPanel(
            "RESPONSÁVEL TÉCNICO",
            fluidRow(
              column(
                width = 5,
                position = "center",
                tags$br(),
                h3("EQUIPE TÉCNICA", align = "center"),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  tags$br(),
                  strong(
                    "Projeto de Sustentabilidade Ambiental POR TODO O PARÁ da Autarquia de Trânsito, Desenvolvido na Coordenadoria de Gestão de Pessoa, sob a tutela da Gerência de Treinamento (Sra. Vera Brazil).
",
                    tags$br(),
                    tags$a("", href = "cristovao.simoes@detran.pa.gov.br")
                  ),
                  tags$br(),
                  strong(
                    "O Projeto é Executado sob a Supervisão Técnica do Servidor, Assistende de Trânsito, Sr:",
                    tags$a("CRISTOVÃO SIMÕES DA MOTA (Antropólogo)", href = "cristovao.simoes@detran.pa.gov.br")
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "Reclamações, sugestões, críticas e elogios relacionados ao Projeto de Sustentabilidade Ambiental Por Todo o Pará do DETRAN-PA podem ser registrados na Gerência de Treinamento"
                  )
                )
              )
            )
          ),
          tabItem(
            tabName = "video1",
            tabBox(
              id = "t2",
              width = 12,
              tabPanel(
                "Video Institucional",
                icon = icon("video"),
                fluidRow(
                  column(
                    width = 8,
                    position = "center",
                    tags$br("Projeto "),
                    tags$video(
                      id = "videoID",
                      type = "video/mp4",
                      src = "video_detran.mp4",
                      width = 750,
                      height = 500,
                      controls = "controls"
                    ),
                    tags$br(),
                    tags$a("Video: by Asdecom"),
                    align = "left"
                  ),
                  column(
                    width = 4,
                    tags$br(),
                    tags$p(
                      style = "text-align:justify;font-si20pt",
                      strong(
                        "O Departamento de Trânsito do Estado do Pará obteve o Projeto “Strengthening Road Traffic Enforcement
in Brazil” aprovado e financiado pela (United Road Safety Fund), com duração de 12 meses, se constituindo
o único selecionado do Brasil, que somado as propostas de alguns outros países, formam o conjunto de projetos
nacionais administrados pelo Fundo, coordenado e supervisionados por diversas Agências e Comissões
Regionais das Nações Unidas."
                      )
                    ),
                    tags$br(),
                    tags$p(
                      style = "text-align: justify;font-si20pt",
                      strong(
                        "Concomitantemente, o Projeto Brasileiro é supervisionado pela Comissão Econômica das Nações
Unidas para América Latina e Caribe (CEPAL), coordenado e implementado pelo DETRAN-PA
em parceria com Conselho Estadual de Trânsito do Estado do Pará (CETRAN-PA), e tem como objetivo
contribuir para a redução de mortes e lesões no Trânsito através das atividades de Educação, Engenharia e
Fiscalização em nível Estadual."
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ), 
      tabItem(
        tabName = "sobre",
        fluidRow(
          box(title = "Sobre o Projeto", width = 12, status = "info",
              "Esse é um aplicativo para análise de dados sobre sustentabilidade ambiental."))),
      tabItem(
        tabName = "analises",
        fluidRow(
          tabBox(title = "SócioEconômico", width = 12,
                 tabPanel("Municípios",
                          fluidRow(
                            box(title = "Total Geral", width = 7, status = "success", solidHeader = TRUE,collapsible = TRUE, 
                                DT::dataTableOutput("tabelamunicipio") %>% 
                                  withSpinner(color = "#007bff")))),
            tabPanel("Gênero",
                     fluidRow(
                       box(title = "Distribuição por Gênero", width = 7, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, plotlyOutput("sexoPlot") %>% withSpinner(color = "#28a745")),
                       box(title = "Medidas Resumo", width = 5, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, DT::dataTableOutput("tabelaGenero") %>% withSpinner(color = "#28a745")))),
            tabPanel("Raça",
                     fluidRow(
                       box(title = "Distribuição por Raça", width = 7, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, plotlyOutput("racaPlot") %>% withSpinner(color = "#17a2b8")),
                       box(title = "Medidas Resumo", width = 5, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, DT::dataTableOutput("tabelaRaca") %>% withSpinner(color = "#17a2b8")))),
            tabPanel("Escolaridade",
                     fluidRow(
                       box(title = "Distribuição por Escolaridade", width = 7, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, plotlyOutput("escolaridadePlot") %>% withSpinner(color = "#ffc107")),
                       box(title = "Medidas Resumo", width = 5, status = "success", solidHeader = TRUE,
                         collapsible = TRUE, DT::dataTableOutput("tabelaEscolaridade") %>% withSpinner(color = "#ffc107")))),
            # Nova sub-aba para Estado Civil
            tabPanel("Estado Civil",
                     fluidRow(
                       box(title = "Distribuição por Estado Civil", width = 7, status = "success", solidHeader = TRUE,collapsible = TRUE, 
                         plotlyOutput("estadoCivilPlot") %>% 
                           withSpinner(color = "#007bff")
                       ),
                       box(title = "Medidas Resumo", width = 5, status = "success", solidHeader = TRUE,collapsible = TRUE, 
                         DT::dataTableOutput("tabelaEstadoCivil") %>% 
                           withSpinner(color = "#007bff")))),
            tabPanel("CNH",
                     fluidRow(
                       box(title = "Distribuição por CNH", width = 7, status = "success", solidHeader = TRUE,collapsible = TRUE, 
                           plotlyOutput("cnhPlot") %>% 
                             withSpinner(color = "#007bff")
                       ),
                       box(title = "Medidas Resumo", width = 5, status = "success", solidHeader = TRUE,collapsible = TRUE, 
                           DT::dataTableOutput("tabelacnh") %>% 
                             withSpinner(color = "#007bff")))),
            
            tabPanel("MEIO_TRANSPORTE",
                     fluidRow(
                       box(title = "Distribuição por Meio de Transporte", width = 7, status = "success", solidHeader = TRUE,collapsible = TRUE, 
                           plotlyOutput("transportePlot") %>% 
                             withSpinner(color = "#007bff")
                       ),
                       box(title = "Medidas Resumo", width = 5, status = "success", solidHeader = TRUE,collapsible = TRUE, 
                           DT::dataTableOutput("tabelatransporte") %>% 
                             withSpinner(color = "#007bff")))),
            tabPanel("CARGO_FUNCAO",
                     fluidRow(
                       box(title = "Distribuição por Cargo", width = 7, status = "success", solidHeader = TRUE,collapsible = TRUE, 
                           plotlyOutput("cargoPlot") %>% 
                             withSpinner(color = "#007bff")
                       ),
                       box(title = "Medidas Resumo", width = 5, status = "success", solidHeader = TRUE,collapsible = TRUE, 
                           DT::dataTableOutput("tabelacargo") %>% 
                             withSpinner(color = "#007bff"))))
            
            
            )
        )
        ),
      tabItem(
        tabName = "likertgeral",
        tabPanel("Escala Likert Geral",
                 icon = icon("address-card"),
                 fluidRow(
                   box(width = 12,title = "Percepção de Sustentabilidade Geral", status = "success", solidHeader = TRUE, collapsible = TRUE, headerBorder = TRUE,
                  tags$div(style = "display: flex; justify-content: center; align-items: center;",
                           plotlyOutput("likertPlot1",width = "auto",height = "auto")))))),
      tabItem(
        tabName = "likertgenero",
        tabPanel("Escala Likert Gênero",
                 icon = icon("address-card"),   
                 fluidRow(
  box(width = 12,title="Percepção de Sustentabilidade por Gênero",style="text-align: center", status ="success",solidHeader=TRUE,collapsible=TRUE, headerBorder=TRUE,
                     tags$div(style = "display: flex; justify-content: center; align-items: center;",
                       plotlyOutput("likertPlot2",width = 850,height = 900)))))),
      tabItem(
        tabName = "local1",
        tabPanel(
          title = "Mapa",
          fluidRow(
            column(12,
                   h3("Mapa de Municípios"),
                   leafletOutput("mapa_municipios",
                                 height = 700)  ))))))

,
footer = dashboardFooter(
  left = "COPYRIGHT© 2025 DETRAN-PA - Todos os direitos Reservados.",
  right = "Belém - PA"
)

)
# ======================================================================================================#
# Servidor
server <- function(input, output, session) {

#-------------------------------------------------------------------------------#
#CIRETRAN TIPO A  
  output$ciretrantipoA <- renderDiagrammeR({
    DiagrammeR::DiagrammeR(
      "
      graph TB
      A[DETRAN-PA]-->B[CIRETRAN A]
      B-->C[LEI Nº7594/2011]
      B-->D[LEI Nº432/2019]
      C-->E(SANTARÉM)
      E-->F(CASTANHAL)
      F-->G(MARABÁ)
      G-->H(ABAETETUBA)
      C-->I(ALTAMIRA)
      I-->J(CAPANEMA)
      J-->K(PARAGOMINAS)
      K-->L(TUCURUÍ)
      C-->M(REDENÇÃO)
      M-->N(ITAITUBA)
      N-->O(PARAUAPEBAS)
      O-->P(BREVES)
      D-->Q(BRAGANÇA)
      Q-->R(SÃO FÉLIX DO XINGU)"
    )
  })
#-------------------------------------------------------------------------------#
#CIRETRAN TIPO B  
output$ciretrantipoB <- renderDiagrammeR({
    mermaid(
      "graph TB
  A[LEI Nº7594/2011]-->B[DETRAN-PA]
  B-->C[CIRETRAN TIPO B]
  C-->D(SOURE)
  D-->E(ALENQUER)
  E-->F(ALMEIRIM/M.DOURADO)
  F-->G(MONTE ALEGRE)
  G-->H(ÓBIDOS)
  C-->I(ORIXIMINÁ)
  I-->J(IGUARAPÉ-AÇÚ)
  J-->K(SÃO MIGUEL)
  K-->L(SANTA LUZIA)
  L-->M(TOMÉ-AÇÚ)
  C-->N(ITUPIRANGA)
  N-->O(JACUNDÁ)
  O-->P(RONDON)
  P-->Q(SÃO GERALDO)
  Q-->R(BARCARENA)
  C-->S(IGARAPÉ-MIRI)
  S-->T(MEDICILÂNDIA)
  T-->U(URUARÁ)
  U-->V(CAPITÃO POÇO)
  V-->W(OURILÂNDIA DO NORTE)
  C-->X(DOM ELISEU)
  X-->Y(MÃE DO RIO)
  Y-->Z(NOVO REPARTIMENTO)
  Z-->A1(CONCEIÇÃO DO ARAGUAIA)
  A1-->A2(SANTANA DO ARAGUAIA)
  C-->A3(TUCUMÃ)
  A3-->A4(NOVO PROGRESSO)
  A4-->A5(CANÃA DOS CARAJÁS)
  A5-->A6(CURIONÓPOLIS)
  A6-->A7(RURÓPOLIS)
   C-->A8(ANANINDEUA)
   A8-->A9(CAMETÁ)
   A9-->A10(VIGIA)
   A10-->A11(SALINÓPOLIS)
   A11-->A12(TAILÂNDIA)
   C--> A13(SANTA ISABEL)
   A13--> A14(ELDORADO DOS CARAJÁS)
  ",  width = 1000,
      align = "center"
    )
  })
#-------------------------------------------------------------------------------#
#CIRETRAN TIPO C  
output$ciretrantipoC <- renderDiagrammeR({
  mermaid("
graph TD
A[DETRAN-PA]-->B[CONADM]
B-->C[HOMOLOGADAS]
C-->D[2008]
D-->E(ULIANÓPOLIS)
C-->F[2009]
F-->G(RURÓPOLIS)
G-->H(MARITUBA)
C-->I[2013]
I-->J(SÃO DOMINGOS)
C-->K[2019]
K-->L(JURURTI)
L-->M(VISEU)
", width = 1000)
})
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
# POSTO DE ATENDIMENTO
output$ciretrantipoD <- renderDiagrammeR({
  mermaid("
graph TD
A[DETRAN-PA]-->B[POSTO DE ATENDIMENTO]
B-->C[BELÉM]
C-->D(SHOPPING BOULEVARD)
D-->E(SHOPPING BOSQUE GRÃO PARÁ)
E-->F(SHOPPING METRÓPOLE)
F-->G(SHOPPING PÁTIO BELÉM)
G-->H(ESTAÇÃO CIDADANIA SÃO BRÁS)
H-->I(PARQUE SHOPPING)
B-->J[MARABÁ]
J-->K(SHOPPING PÁTIO MARABÁ)
B-->L[PARAUAPEBAS]
L-->M(SHOPPING KARAJÁS)
B-->N[SANTARÉM]
N-->O(TERMINAL HIDROVIÁRIO)
", width = 1000)
})
#-------------------------------------------------------------------------------#
  
  
  
  
  
  
  
  detran_location <- data.frame(
    lat = -1.37843,
    lon = -48.44034
  )
  
  output$mapa <- renderLeaflet({
    df <- read.csv(textConnection(
      "Nome, lat, lon,
      DETRAN-PA, -1.37843, -48.44034"
    ))
    leaflet::leaflet(df) %>%
      addTiles() %>%
      # addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      # addProviderTiles(providers$Esri.WorldStreetMap)%>%
      addMarkers(~lon, ~lat,
                 label = ~ htmlEscape(Nome),
                 labelOptions = labelOptions(
                   noHide = FALSE,
                   textsize = "15px"
                 )
      ) %>%
      addProviderTiles(providers$OpenSeaMap) %>%
      setView(
        lng = detran_location$lon,
        lat = detran_location$lat,
        zoom = 15
      )
  })
  
Dados_Clima <- readxl::read_excel("Dados_Clima.xls")
Dados_geral <- readxl::read_excel("BANCO_PROJETO_SUSTENTABILIDADE.xls")

# Carregar os dados
  data <- reactive({
    req(file.exists("BANCO_PROJETO_SUSTENTABILIDADE.xls"))
    readxl::read_excel("BANCO_PROJETO_SUSTENTABILIDADE.xls")})
  
# Atualizar opções dos filtros dinâmicos
  observe({
    dados <- data()
    updateSelectInput(session, 
                      "municipio", 
                      choices = unique(dados$MUNICIPIO), 
                      selected = unique(dados$MUNICIPIO)[1])})
# Filtrar dados reativos
  filtered_data <- reactive({
    req(input$municipio)
    data() %>%
      filter(MUNICIPIO == input$municipio)})
# Verificar se a coluna SEXO existe nos dados
  observe({
    dados <- data()
    if (!"SEXO" %in% colnames(dados)) {
      showNotification("A coluna 'SEXO' não foi encontrada nos dados.", type = "error")}})
  
  
  
#------------------------------------------------------------------------------#  
# Gráficos de Gênero
  output$sexoPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    req("SEXO" %in% colnames(filtered_data()))  # Verificar se a coluna SEXO está presente
    municipio_selecionado <- input$municipio  # Pega o nome do município selecionado
    
# Modificar o título para incluir o nome do município
    titulo <- paste("Distribuição por Gênero em", municipio_selecionado)
    ggplotly(gerar_grafico(filtered_data(), "SEXO", "SEXO", titulo, order = "asc"))
  })

  output$tabelaGenero <- DT::renderDataTable({
    req(nrow(filtered_data()) > 0)
    
    dados <- filtered_data()
    
    # Verificar se as colunas SEXO e IDADE estão presentes
    if (!all(c("SEXO", "IDADE") %in% colnames(dados))) {
      return(NULL)
    }
    
    # Verificar se a coluna IDADE não está totalmente vazia
    if (all(is.na(dados$IDADE))) {
      media_idade_geral <- NA
    } else {
      media_idade_geral <- round(mean(dados$IDADE, na.rm = TRUE), 2)
    }
    
    # Contar o total por gênero e calcular a média de idade
    genero_count <- dados %>%
      group_by(SEXO) %>%
      summarise(
        Total = n(),
        Media_Idade = round(mean(IDADE, na.rm = TRUE), 1)
      ) %>%
      mutate(Percentual = round((Total / sum(Total)) * 100, 1)) %>%
      rename("Gênero" = SEXO)
    
    # Adicionar linha de total
    genero_count <- bind_rows(genero_count, tibble(
      "Gênero" = "Total",
      "Total" = sum(genero_count$Total),
      "Media_Idade" = media_idade_geral,
      "Percentual" = 100
    ))
    
    # Criar a tabela com estilo para destacar a linha total
    DT::datatable(
      genero_count,
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        'Gênero',
        target = 'row',
        backgroundColor = DT::styleEqual("Total", "lightgrey"),
        fontWeight = DT::styleEqual("Total", "bold")
      )
  })
  
#------------------------------------------------------------------------------#
  

#------------------------------------------------------------------------------#
# Gráfico de Raca
  output$racaPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    req("RACA" %in% colnames(filtered_data()))  # Verificar se a coluna RACA está presente
    municipio_selecionado <- input$municipio  # Pega o nome do município selecionado
    
# Modificar o título para incluir o nome do município
    titulo <- paste("Distribuição por Raça em", municipio_selecionado)
    ggplotly(gerar_grafico(filtered_data(), "RACA", "RACA", titulo, order = "asc"))
  })

  output$tabelaRaca <- DT::renderDataTable({
    req(nrow(filtered_data()) > 0)
    
    dados <- filtered_data()
    
    # Verificar se as colunas RACA e IDADE estão presentes
    if (!all(c("RACA", "IDADE") %in% colnames(dados))) {
      return(NULL)
    }
    
    # Verificar se a coluna IDADE não está totalmente vazia
    if (all(is.na(dados$IDADE))) {
      media_idade_geral <- NA
    } else {
      media_idade_geral <- round(mean(dados$IDADE, na.rm = TRUE), 1)
    }
    
    # Contar o total por raça e calcular a média de idade
    raca_count <- dados %>%
      group_by(RACA) %>%
      summarise(
        Total = n(),
        Media_Idade = round(mean(IDADE, na.rm = TRUE), 1)
      ) %>%
      mutate(Percentual = round((Total / sum(Total)) * 100, 1)) %>%
      rename("Raça" = RACA)
    
    # Adicionar linha de total
    raca_count <- bind_rows(raca_count, tibble(
      "Raça" = "Total",
      "Total" = sum(raca_count$Total),
      "Media_Idade" = media_idade_geral,
      "Percentual" = 100
    ))
    
    # Criar a tabela com estilo para destacar a linha total
    DT::datatable(
      raca_count,
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        'Raça',
        target = 'row',
        backgroundColor = DT::styleEqual("Total", "lightgrey"),
        fontWeight = DT::styleEqual("Total", "bold")
      )
  })
  
#------------------------------------------------------------------------------#
 
  
  
   
#------------------------------------------------------------------------------#  
# Gráfico de Escolaridade
  output$escolaridadePlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    req("ESCOLARIDADE" %in% colnames(filtered_data()))  # Verificar se a coluna ESCOLARIDADE está presente
    municipio_selecionado <- input$municipio  # Pega o nome do município selecionado
    
# Modificar o título para incluir o nome do município
    titulo <- paste("Distribuição por Escolaridade em", municipio_selecionado)
    ggplotly(gerar_grafico(filtered_data(), "ESCOLARIDADE", "ESCOLARIDADE", titulo, order = "asc"))
  })

# Tabela de Escolaridade x Media
  output$tabelaEscolaridade <- DT::renderDataTable({
    req(filtered_data(), nrow(filtered_data()) > 0)
    
    dados <- filtered_data()
    
    if (!all(c("ESCOLARIDADE", "IDADE") %in% colnames(dados))) {
      return(NULL)
    }
    
    media_idade_geral <- round(mean(dados$IDADE, na.rm = TRUE), 1)
    
    escolaridade_count <- dados %>%
      group_by(ESCOLARIDADE) %>%
      summarise(
        Total = n(),
        Media_Idade = round(mean(IDADE, na.rm = TRUE), 1),
        .groups = "drop"  # Evita mensagens de agrupamento
      ) %>%
      mutate(Percentual = round((Total / sum(Total)) * 100, 1)) %>%
      rename("Escolaridade" = ESCOLARIDADE) %>%
      mutate(Escolaridade = case_when(
        Escolaridade == "EFI" ~ "Ensino Fundamental Incompleto",
        Escolaridade == "EMI" ~ "Ensino Médio Incompleto",
        Escolaridade == "EMC" ~ "Ensino Médio Completo",
        Escolaridade == "ESC" ~ "Ensino Superior Completo",
        Escolaridade == "ESI" ~ "Ensino Superior Incompleto",
        Escolaridade == "PÓS" ~ "Pós-Graduação",
        TRUE ~ Escolaridade
      ))
    
    total_row <- tibble(
      "Escolaridade" = "Total",
      "Total" = sum(escolaridade_count$Total),
      "Media_Idade" = media_idade_geral,
      "Percentual" = 100
    )
    
    escolaridade_count <- bind_rows(escolaridade_count, total_row)
    
    DT::datatable(
      escolaridade_count,
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Escolaridade",
        target = "row",
        backgroundColor = DT::styleEqual("Total", "lightgray"),
        fontWeight = DT::styleEqual("Total", "bold")
      )
  })
#------------------------------------------------------------------------------#  
  
  
  
  
#------------------------------------------------------------------------------#
# Gráfico de Estado Civil
  output$estadoCivilPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    req("ESTADO_CIVIL" %in% colnames(filtered_data()))  # Verificar se a coluna ESTADO_CIVIL está presente
    municipio_selecionado <- input$municipio  # Pega o nome do município selecionado
    
    # Modificar o título para incluir o nome do município
    titulo <- paste("Município de ", municipio_selecionado)
    
    ggplotly(gerar_grafico(filtered_data(), "ESTADO_CIVIL", "ESTADO_CIVIL", titulo, order = "asc"))
  })
  
  # Tabela de Estado Civil com média de IDADE, total e percentual
  output$tabelaEstadoCivil <- DT::renderDataTable({
    req(nrow(filtered_data()) > 0)
    
    dados <- filtered_data()
    
    # Verificar se as colunas ESTADO_CIVIL e IDADE estão presentes
    if (!all(c("ESTADO_CIVIL", "IDADE") %in% colnames(dados))) {
      return(NULL)
    }
    
    # Calcular a média geral de idade antes para otimização
    media_idade_geral <- round(mean(dados$IDADE, na.rm = TRUE), 1)
    
    # Contar o total por estado civil e calcular a média de idade
    estado_civil_count <- dados %>%
      group_by(ESTADO_CIVIL) %>%
      summarise(
        Total = n(),
        Media_Idade = round(mean(IDADE, na.rm = TRUE), 1)  # Arredondando para 1 casa decimal
      ) %>%
      mutate(Percentual = round((Total / sum(Total)) * 100, 1)) %>%  # Adiciona o percentual
      rename("Estado Civil" = ESTADO_CIVIL)
    
    # Adicionar a linha de total
    total_row <- tibble(
      "Estado Civil" = "Total",
      "Total" = sum(estado_civil_count$Total),
      "Media_Idade" = media_idade_geral,
      "Percentual" = 100  # O total sempre será 100%
    )
    
    # Unir a linha de total com o resto da tabela
    estado_civil_count <- bind_rows(estado_civil_count, total_row)
    
    DT::datatable(
      estado_civil_count,
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Estado Civil",
        target = "row",
        backgroundColor = DT::styleEqual("Total", "lightgray"),
        fontWeight = DT::styleEqual("Total", "bold")
      )
  })
  

#------------------------------------------------------------------------------#
  
  

#------------------------------------------------------------------------------#  
  # Gráfico de CNH
  output$cnhPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    req("CNH" %in% colnames(filtered_data()))  # Verificar se a coluna ESTADO_CIVIL está presente
    municipio_selecionado <- input$municipio  # Pega o nome do município selecionado
    
    # Modificar o título para incluir o nome do município
    titulo <- paste("Município de ", municipio_selecionado)
    
    ggplotly(gerar_grafico(filtered_data(), "CNH", "CNH", titulo, order = "asc"))
  })
  
  # Tabela de CNH com média de IDADE, total e percentual
  output$tabelacnh <- DT::renderDataTable({
    req(nrow(filtered_data()) > 0)
    
    dados <- filtered_data()
    
    # Verificar se as colunas necessárias estão presentes
    if (!all(c("CNH", "IDADE") %in% colnames(dados))) {
      return(NULL)
    }
    
    # Calcular a média geral de idade antes para otimização
    media_idade_geral <- round(mean(dados$IDADE, na.rm = TRUE), 1)
    
    # Contar o total por CNH e calcular a média de idade
    cnh_count <- dados %>%
      group_by(CNH) %>%
      summarise(
        Total = n(),
        Media_Idade = round(mean(IDADE, na.rm = TRUE), 1)  
      ) %>%
      mutate(Percentual = round((Total / sum(Total)) * 100, 1))  
    
    # Adicionar a linha de total
    total_row <- tibble(
      CNH = "Total",
      Percentual = 100,  # O total sempre será 100%
      Total = sum(cnh_count$Total),
      Media_Idade = media_idade_geral
    )
    
    # Unir a linha de total com o resto da tabela
    cnh_count <- bind_rows(cnh_count, total_row)
    
    DT::datatable(
      cnh_count,
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "CNH",
        target = "row",
        backgroundColor = DT::styleEqual("Total", "lightgray"),
        fontWeight = DT::styleEqual("Total", "bold")
      )
  })
#------------------------------------------------------------------------------#
  
  
  
#------------------------------------------------------------------------------# 
  # Gráfico de Transporte
  output$transportePlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    req("MEIO_TRANSPORTE" %in% colnames(filtered_data()))  # Verificar se a coluna ESTADO_CIVIL está presente
    municipio_selecionado <- input$municipio  # Pega o nome do município selecionado
    
    # Modificar o título para incluir o nome do município
    titulo <- paste("Município de ", municipio_selecionado)
    
    ggplotly(gerar_grafico(filtered_data(), "MEIO_TRANSPORTE", "MEIO_TRANSPORTE", titulo, order = "asc"))
  })
  

# Tabela de Meio de Transporte com média de IDADE, total e percentual
  output$tabelatransporte <- DT::renderDataTable({
    req(nrow(filtered_data()) > 0)
    
    dados <- filtered_data()
    
    # Verificar se as colunas necessárias estão presentes
    if (!all(c("MEIO_TRANSPORTE", "IDADE") %in% colnames(dados))) {
      return(NULL)
    }
    
    # Calcular a média geral de idade antes para otimização
    media_idade_geral <- round(mean(dados$IDADE, na.rm = TRUE), 1)
    
    # Contar o total por meio de transporte e calcular a média de idade
    transporte_count <- dados %>%
      group_by(MEIO_TRANSPORTE) %>%
      summarise(
        Total = n(),
        Media_Idade = round(mean(IDADE, na.rm = TRUE), 1)  # Arredondando para 1 casa decimal
      ) %>%
      mutate(Percentual = round((Total / sum(Total)) * 100, 1))  # Adiciona o percentual
    
    # Adicionar a linha de total
    total_row <- tibble(
      MEIO_TRANSPORTE = "Total",
      Total = sum(transporte_count$Total),
      Media_Idade = media_idade_geral,
      Percentual = 100  # O total sempre será 100%
    )
    
    # Unir a linha de total com o resto da tabela
    transporte_count <- bind_rows(transporte_count, total_row)
    
    DT::datatable(
      transporte_count,
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "MEIO_TRANSPORTE",
        target = "row",
        backgroundColor = DT::styleEqual("Total", "lightgray"),
        fontWeight = DT::styleEqual("Total", "bold")
      )
  })
  
#------------------------------------------------------------------------------#  
  
  
#------------------------------------------------------------------------------# 
# Gráfico de Cargo
  # Gráfico de Cargo
  output$cargoPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    req("CARGO_FUNCAO" %in% colnames(filtered_data()))  # Verifica se a coluna está presente
    
    municipio_selecionado <- input$municipio %||% "Não Especificado"  # Evita erro se for NULL
    
    # Modificar o título para incluir o nome do município
    titulo <- paste("Município de", municipio_selecionado)
    
    ggplotly(gerar_grafico(filtered_data(), "CARGO_FUNCAO", "CARGO_FUNCAO", titulo, order = "asc"))
  })
  
  output$tabelacargo <- DT::renderDataTable({
    req(nrow(filtered_data()) > 0)
    dados <- filtered_data()
    
    # Verificar se as colunas necessárias estão presentes
    if (!all(c("CARGO_FUNCAO", "IDADE") %in% colnames(dados))) {
      return(DT::datatable(data.frame(Mensagem = "Colunas necessárias não estão presentes.")))
    }
    
    # Calcular a média geral de idade
    media_idade_geral <- round(mean(dados$IDADE, na.rm = TRUE), 1)
    
    # Calcular total por cargo, média de idade e percentual
    cargo_count <- dados %>%
      group_by(CARGO_FUNCAO) %>%
      summarise(
        Total = n(),
        Media_Idade = round(mean(IDADE, na.rm = TRUE), 1)
      ) %>%
      ungroup() %>%
      mutate(Percentual = round((Total / sum(Total)) * 100, 1))
    
    # Adicionar linha de total diretamente
    cargo_count <- cargo_count %>%
      add_row(
        CARGO_FUNCAO = "Total",
        Total = sum(cargo_count$Total),
        Media_Idade = media_idade_geral,
        Percentual = 100
      )
    
    # Criar tabela DT
    DT::datatable(
      cargo_count,
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>%
    DT::formatStyle(
      "CARGO_FUNCAO",
      target = "row",
      backgroundColor = DT::styleEqual("Total", "lightgray"),
      fontWeight = DT::styleEqual("Total", "bold")
    )
  })
  
#------------------------------------------------------------------------------#
# Tabela Municipios
  output$tabelamunicipio <- renderDT({
    
    # Calcular o total e o percentual por município
    tabela_municipio <- Dados_geral %>%
      group_by(MUNICIPIO) %>%
      summarise(
        Total = n(),
        Percentual = round((n() / nrow(Dados_geral)) * 100, 2)
      ) %>%
      arrange(desc(Total))
    
    # Adicionar uma linha para o total geral
    total_geral <- tibble(
      MUNICIPIO = "Total Geral",
      Total = sum(tabela_municipio$Total),
      Percentual = 100
    )
    
    # Combinar os dados e adicionar a linha do total geral
    tabela_completa <- bind_rows(tabela_municipio, total_geral)
    
    # Renderizar a tabela interativa e aplicar o estilo
    DT::datatable(tabela_completa, 
                  filter = "top", 
                  plugins = 'natural',
                  extensions = 'Buttons',
                  options=list(dom = 'Blfrtip',buttons = c('copy','csv','excel','pdf','print'),
                               engthMenu = list(c(5,50,100,250,-1)), c(5,50,100,250,"All"),
                               pageLength = 10, 
                               autoWidth = TRUE,
                               scrollX = TRUE),
                  rownames = FALSE,
                  class = 'cell-border compact stripe hover row-border order-column dt-body-right',
                  style = 'bootstrap',
                  editable = 'cell',
                  colnames = c('Municípios', 'Nº de Entrevistas', '%'),
                  caption = 'Tabela 1. Municípios Realizados o Projeto de Sustentabilidade.'
                  ) %>%
    DT::formatStyle(
        "MUNICIPIO",
        target = "row",
        backgroundColor = DT::styleEqual("Total Geral", "lightgray"),
        fontWeight = DT::styleEqual("Total Geral", "bold")
      )
  })
  
  
  
  
  
  
  
  
  
  
  
  
#------------------------------------------------------------------------------# 
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
g1 <- likert.bar.plot(dados_grafico,strip = TRUE,strip.left = TRUE,ReferenceZero = 3,wrap = 25,centered = TRUE,text.size = 4, hjust = 0.5,
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
    novo_ponto <- tidygeocoder::geo(address = endereco_novo, method = "osm")             # Belém
    destino <- tidygeocoder::geo(address = endereco_destino, method = "osm")             # Castanhal
    velho_ponto <- tidygeocoder::geo(address = endereco_velho, method = "osm")           # Cametá
    origem <- tidygeocoder::geo(address = endereco_origem, method = "osm")               # Altamira
    intermediario <- tidygeocoder::geo(address = endereco_intermediario, method = "osm") # Marabá
    
    
    
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
