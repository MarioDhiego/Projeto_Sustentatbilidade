#======================================================================================================#
# Pacotes Utilizados
library(readr)
library(readxl)
library(dplyr)
library(plyr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(leaflet)
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
#======================================================================================================#
#======================================================================================================#
# Interface do Usuário (UI)
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Projeto Sustentabilidade Ambiental", 
                  titleWidth = 390,
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
  dashboardSidebar(minified = FALSE,
                   collapsed = FALSE,
                   tags$img(src = "atitudes.jpg", 
                            width = 230, 
                            height = 100),
    sidebarMenu(
      menuItem("PROJETO", tabName = "defprojeto", icon = icon("book"),
               menuSubItem("Sobre Projeto", tabName="sobre1", icon=icon("book")),
               menuSubItem("Vídeo Institucional", tabName="video1", icon=icon("video"))
               ),
      menuItem("CIRETRAN'S", tabName = "catCiretran", icon = icon("book"),
               menuSubItem("TIPO A", tabName = "tipoA", icon = icon("book")),
               menuSubItem("TIPO B", tabName = "tipoB", icon = icon("book"))
      ),
      menuItem("PALESTRAS", tabName = "palestra", icon = icon("book"),
               menuSubItem("ALTAMIRA", tabName="ciretran1", icon=icon("video")),
               menuSubItem("MARABÁ", tabName="ciretran2", icon=icon("video")),
               menuSubItem("BELÉM", tabName="ciretran3", icon=icon("video")),
              menuSubItem("CASTANHAL", tabName="ciretran4", icon=icon("video"))
               ),
      menuItem("SÓCIO-ECONÔMICO", tabName = "socioeconomico", icon = icon("users")),
      menuItem("COLETA SELETIVA", tabName = "coleta", icon = icon("recycle")),
      menuItem("DESTINO LIXO", tabName = "ciretran", icon = icon("recycle")),
      menuItem("PERCEPÇÃO", tabName = "escalalikert", icon = icon("book"),
               menuSubItem("Percepção Geral", tabName = "likertgeral", icon = icon("book")),
               menuSubItem("Percepção Por Gênero", tabName = "likertgenero", icon = icon("book")),
               menuSubItem("Percepção Por Município", tabName = "likertmunic", icon = icon("book"))
               
               ),
      selectInput("municipio", "MUNICÍPIOS:",
                  choices = c("Altamira",
                              "Belém",
                              "Marabá", 
                              "Castanhal"),
                  selected = "Altamira"),
      selectInput("cnh", "POSSUI CNH:",
                  choices = c("Sim", 
                              "Não"),
                  selected = "Sim"),
      selectInput("destino", "DESTINO LIXO:",
                   choices = c("Aterro Sanitário", 
                               "Compostagem",
                               "Incineração",
                               "Lixão",
                               "Reciclagem"),
                   selected =  "Aterro Sanitário"),
      # Botão para reiniciar os filtros
      actionButton("reset_button", "REINICIAR",
                   style = "background-color: #28a745; 
                   color: white; 
                   border-radius: 5px; 
                   padding: 10px; 
                   font-size: 12px"),
      tableOutput("tabela_filtrada")
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
  tabItem(
    tabName = "tipoA",
           tabPanel("TIPOS DE CIRETRAN'S",
                    icon = icon("address-card"),   
                    fluidRow(
                      box(
                        width = 12,
                        title = tags$div("CIRETRAN DO TIPO A",
                                         style = "text-align: center"),
                        status = "success",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        headerBorder = TRUE,
                        div(class = "elemente",
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
                 div(class = "elemente",
                     DiagrammeROutput("ciretrantipoB")
                 )
                 
               )
             )
    )
  ),
  tabItem(
    tabName = "ciretran1",
    tabBox(id = "t3", width = 12,
           tabPanel("PALESTRAS REALIZADAS",
                     tags$br(),
                     h3("MUNICÍPIO DE ALTAMIRA", align = "center"),
                    tags$br(),
                     icon = icon("address-card"),
                     fluidRow(
                       column(width = 4,
                              position = "left",
                              tags$img(
                                id = "palestra1",
                                src = "Palestra1_ATM.jpg",
                                controls = "controls",
                                width = 430, height = 450),
                              tags$br(),
                              tags$a("Photo: GT/CRH/DETRAN"),
                              align = "left"
                              ),
                       column(width = 4,
                              position = "left",
                              tags$img(
                                id = "palestra2",
                                src = "Palestra2_ATM.jpg",
                                controls = "controls",
                                width = 430,height = 450),
                              tags$br(),
                              tags$a("Photo: GT/CGP/DETRAN"),
                              align = "left"),
                       column(width = 4,
                              position = "left",
                              tags$img(
                                id = "palestra3",
                                src = "Palestra3_ATM.jpg",
                                controls = "controls",
                                width = 430,height = 450),
                              tags$br(),
                              tags$a("Photo: GT/CGP/DETRAN"),
                              align = "left")
                     )
           )
    )
  ),
  tabItem(
    tabName = "ciretran2",
    tabBox(id = "t4", width = 12,
           tabPanel("PALESTRAS REALIZADAS",
                    tags$br(),
                     h3("MUNICÍPIO DE MARABÁ", align = "center"),
                    tags$br(),
                     icon = icon("address-card"),
                     fluidRow(
                       column(width = 4,
                              position = "left",
                              tags$img(
                                id = "palestramaraba1",
                                src = "Palestra3_MAB.jpg",
                                controls = "controls",
                                width = 430,height = 450),
                              tags$br(),
                              tags$a("Photo: GT/CGP/DETRAN"),
                              align = "left"
                       ),
                       column(width = 4,
                              position = "left",
                              tags$img(
                                id = "palestramaraba2",
                                src = "Palestra1_MAB.jpg",
                                controls = "controls",
                                width = 430,height = 450),
                              tags$br(),
                              tags$a("Photo: GT/CGP/DETRAN"),
                              align = "left"),
                       column(width = 4,
                              position = "left",
                              tags$img(
                                id = "palestramaraba3",
                                src = "Palestra2_MAB.jpg",
                                controls = "controls",
                                width = 430,height = 450),
                              tags$br(),
                              tags$a("Photo: GT/CGP/DETRAN"),
                              align = "left")
                     )
           )
    )
  ),
  tabItem(
    tabName = "ciretran3",
    tabBox(id = "t5", width = 12,
           tabPanel("PALESTRAS REALIZADAS",
                    tags$br(),
                    h3("MUNICÍPIO DE BELÉM", align = "center"),
                    tags$br(),
                    icon = icon("address-card"),
                    fluidRow(
                      column(width = 4,
                             position = "left",
                             tags$img(
                               id = "palestrabelem1",
                               src = "Palestra_Belem3.jpeg",
                               controls = "controls",
                               width = 430,height = 450),
                             tags$br(),
                             tags$a("Photo: ASDECOM"),
                             align = "left"
                      ),
                      column(width = 4,
                             position = "left",
                             tags$img(
                               id = "palestrabelem2",
                               src = "Palestra_Belem2.jpeg",
                               controls = "controls",
                               width = 430,height = 450),
                             tags$br(),
                             tags$a("Photo: ASDECOM"),
                             align = "left"),
                      column(width = 4,
                             position = "left",
                             tags$img(
                               id = "palestrabelem3",
                               src = "Palestra_Belem1.jpeg",
                               controls = "controls",
                               width = 430,height = 450),
                             tags$br(),
                             tags$a("Photo: ASDECOM"),
                             align = "left")
                    )
           )
    )
  ),
  tabItem(
    tabName = "ciretran4",
    tabBox(id = "t6", width = 12,
           tabPanel("PALESTRAS REALIZADAS",
                    tags$br(),
                    h3("MUNICÍPIO DE CASTANHAL", align = "center"),
                    tags$br(),
                     icon = icon("address-card"),
                     fluidRow(
                       column(width = 4,
                              position = "left",
                              tags$img(
                                id = "palestracastanhal1",
                                src = "",
                                controls = "controls",
                                width = 420,height = 450),
                              tags$br(),
                              tags$a("Photo: ASDECOM"),
                              align = "left"
                       ),
                       column(width = 4,
                              position = "left",
                              tags$img(
                                id = "palestracastanhal2",
                                src = "",
                                controls = "controls",
                                width = 420,height = 450),
                              tags$br(),
                              tags$a("Photo: ASDECOM"),
                              align = "left"),
                       column(width = 4,
                              position = "left",
                              tags$img(
                                id = "palestracastanhal3",
                                src = "",
                                controls = "controls",
                                width = 420,height = 450),
                              tags$br(),
                              tags$a("Photo: ASDECOM"),
                              align = "left")
                     )
           )
    )
  ),
  tabItem(tabName="sobre1",
          tabBox(id="t1", width=12,
                 tabPanel("REFERENCIAL",
                      icon = icon("address-card"),
                      fluidRow(
                        column(width = 8,
                               position = "left",
                               tags$br(),
                               tags$br(),
                               tags$br(),
                               tags$img(
                                 id = "foto1",
                                 src = "sustentabilidade.jpg",
                                 controls = "controls",
                                 width = 650,height = 450),
                               tags$br(),
                               tags$a("Photo by Asdecom"),
                               align = "left"
                                ),
                          column(width = 4,
                                 tags$br(),
                                 h3("JUSTIFICATIVA", align = "center"),
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
               strong("A Lei Estadual nº 5.899, de 01/08/1995, considera, no Estado do Pará, a coleta seletiva e a reciclagem de lixo como atividades ecológicas de relevância social e de interesse público.")),
             tags$br(),
             tags$p(
               style = "text-align: justify;font-si20pt",
               strong("A Lei Ordinária Estadual n° 6.918, 10/10/2006, dispõe sobre a Política Estadual de Reciclagem de Materiais e dá outras providências."
                )),
             tags$br(),
             tags$p(
               style = "text-align: justify;font-si20pt",
               strong("O Decreto Estadual nº 801, 15/02/2008, institui a separação de resíduos sólidos recicláveis, na fonte geradora, em todos os órgãos da Administração Estadual."
                  )),
             tags$br(),
             tags$p(
               style = "text-align: justify;font-si20pt",
               strong("A Lei Estadual n° 9.149, 23/11/2020, dispõe sobre a substituição e recolhimento de sacolas plásticas em estabelecimentos comerciais localizados no Estado do Pará."
                                    )
                                  )
                                ),
           column(
               width = 4,
               position = "center",
               tags$br(),
               h3("ODS" , align = "center"),
               tags$br(),
               solidHeader = TRUE,
               tags$br(),
               tags$p(
                 style = "text-align: justify;font-si20pt",
                 strong("O Programa de Sustentabilidade Ambiental do DETRAN-PA está pautado nos 17 Objetivos de Desenvolvimento Sustentável da ONU, chamado ODS , no qual é considerado um apelo global para acabar com a pobreza, proteger o meio ambiente e o clima. Sendo alinhado principalmente nos Objetivos 11 e 12."
                 )
               ),
               tags$p(
                 style = "text-align: justify;font-si20pt",
                 strong("Objetivo 11.6: Até 2030, reduzir o impacto ambiental negativo per capita das cidades, inclusive prestando especial atenção à qualidade do ar, gestão de resíduos municipais e outros;")),
               tags$br(),
               tags$p(
                 style = "text-align: justify;font-si20pt",
                 strong("Objetivo 12.5: Até 2030, reduzir substancialmente a geração de resíduos por meio da prevenção, redução, reciclagem e reuso;"
                 )),
               tags$br(),
               tags$p(
                 style = "text-align: justify;font-si20pt",
                 strong("Objetivo 12.7: Promover práticas de compras públicas sustentáveis, de acordo com as políticas e prioridades nacionais;"
                 )),
               tags$br(),
               tags$p(
                 style = "text-align: justify;font-si20pt",
                 strong("Objetivo 12.8:  Até 2030, garantir que as pessoas, em todos os lugares, tenham informação relevante e conscientização para o desenvolvimento sustentável e estilos de vida em harmonia com a natureza;"
                 )
               )
           )
        )),
tabPanel("MATERIAL E MÉTODOS", 
         icon=icon("book"),
         fluidRow(
           column(width = 4, 
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
                  tags$p(style = "text-align:justify;font-si20pt",
                         strong(" ")),
                  tags$p(style = "text-align:justify;font-si20pt",
                         strong("1) Treinamento e Capacitação para todos os servidores das Ciretrans, sobre a importância da cultura de preservação do meio ambiente;")),
                  tags$p(style = "text-align: justify;font-si20pt",
                         strong("2) Análise e Compreensão das Ciretrans tipo A, que mais geram resíduos que possam ser reciclados;")),
                  tags$p(style = "text-align: justify;font-si20pt",
                         strong("3) Sensibilizar os servidores e usuários para que adotem estas práticas em suas casas, locais de lazer e outros locais de trabalho;")),
                  tags$p(style = "text-align: justify;font-si20pt",
                         strong("4) Colaborar com possíveis estudos acadêmicos sobre o descarte consciente de resíduos, políticas de reciclagem e responsabilidade socioambiental.")),
                  tags$p(style = "text-align: justify;font-si20pt")
           ),
          column(
            width = 4,
            position = "center",
            tags$br(),
            h3("ETAPAS OPERACIONAIS" ,align = "center"),
            tags$br(),
            tags$p(style = "text-align:justify;font-si20pt",
                   strong("1) Visita Técnica as CIRETRAN'S do Tipo A;")),
            tags$p(style = "text-align: justify;font-si20pt",
                   strong("2) Reunião de Alinhamento para apresentação do projeto ao Gerente da CIRETRAN;")),
            tags$p(style = "text-align: justify;font-si20pt",
                   strong("3) Aplicação do Questionário de Percepção sobre Sustentabilidade Ambiental;")),
            tags$p(style = "text-align: justify;font-si20pt",
                   strong("4) Palestra Sobre o Uso Racional dos Recursos/Descarte Seletivo;")),
            tags$p(style = "text-align: justify;font-si20pt",
                   strong("5) Reunião Técnica com a Secretaria Municipal Meio Ambiente;")),
            tags$p(style = "text-align: justify;font-si20pt",
                   strong("6) Pesquiva de Levantamento: Cooperativas/Associações de Reciclagem;")),
            tags$br(),
            h3("MÉTRICA", align = "center"),
            tags$br(),
            tags$p(
              style = "text-align:justify;font-si20pt",
              strong(
                "Para a coleta dos dados foi utilizado um instrumento semiestruturado composto por 21 itens que versam sobre sustentabilidade ambiental. A estrutura do questionário contém três subescalas, que medem características socioeconômicas, coleta seletiva e destino do lixo."
              )
            )
          )
         )
),
tabPanel("RECURSO COMPUTACIONAL", icon=icon("computer"),
         fluidRow(
           column(width=4,
                  position="center",
                  tags$br(),
                  h3("SOFTWARE" ,align = "center"),
                  tags$br(),
                  solidHeader = TRUE,
                  tags$br(),
                  tags$p(style="text-align: justify;font-si20pt",
                         strong(
"Para Criação do Painel em Formato Web com Dasboard Interativos, foi Desenvolvido um script em Linguagem de Programação R-PROJECT Versão 4.4.1, no formato de Projeto de Software Livre de Código Aberto (open source), ou seja, pode ser utilizado sem custos de licença (R DEVELOPMENT CORE TEAM, 2024)")),
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
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$p(style="text-align: justify;font-si20pt",
                                              strong("Foi utilizado um Ambiente de Desenvolvmento Integrado (IDE) Chamado Rstudio Versão 1.4.1.7, utilizando um Processo de Extração-Transformação-Carga(ETL) com uso de Várias bibliotecas (library), para o Ambiente Windows")),
                                       tags$br(),
                                       tags$br(),
                                       tags$img(
                                         id="foto3",
                                         src="RStudio.png",
                                         controls="controls",
                                         width=190,height=170),
                                       tags$br(),
                                       tags$a("RStudio",
                                              href = "https://download1.rstudio.org/electron/windows/RStudio-2023.09.1-494.exe"),
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
                           h3("DIREITOS AUTORAIS" ,align = "center"),
                           tags$br(),
                           tags$p(
                             style = "text-align: justify;font-si20pt",
                             strong("DEPARTAMENTO DE TRÂNSITO DO ESTADO DO PARÁ")
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
                               "Esta publicação deve ser citada como: Departamento de Trânsito do Estado do Pará (DETRAN-PA), Programa de Sustentatabilidade Ambiental Por Todo Pará, 2024 (LC/PUB.2024/1-P), Belém, 2024."
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
                         column(width = 4,
                                position = "center",
                                solidHeader = TRUE,
                                tags$br(),
                                leafletOutput("mapa"),
                         )
                       )        
                     ),
                     tabPanel("RESPONSÁVEL TÉCNICO",
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
                                    strong("Projeto de Sustentabilidade Ambiental POR TODO O PARÁ da Autarquia de Trânsito, Desenvolvido na Coordenadoria de Gestão de Pessoa, sob a tutela da Gerência de Treinamento (Sra. Vera Brazil). 
", 
                                           tags$br(),
tags$a("",                                                                                                                                                                                                                                          href = "cristovao.simoes@detran.pa.gov.br")
                                    ),
tags$br(),
strong(
  "O Projeto é Executado sob a Supervisão Técnica do Servidor, Assistende de Trânsito, Sr:", 
  tags$a("CRISTOVÃO SIMÕES DA MOTA (Atropólogo)",                                                                                                                                                                                                                                          href = "cristovao.simoes@detran.pa.gov.br")
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
                              )),
tabItem(tabName = "video1",
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
                tags$br() ,
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
        ))
              )
      ),
#------------------------------------------------------------------------------#
# Aba Socio-Econômico
tabItem(tabName = "socioeconomico",
        fluidRow(
          box(title = "Distribuição po Gênero", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("sexoPlot", height = 300)),
          box(title = "Distribuição por Raça/Cor", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("racaPlot", height = 300)),
          box(title = "Distribuição por Idade", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("idadePlot", height = 300)),
          box(title = "Distribuição por Grau de Escolaridade", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("escolaridadePlot", height = 300)),
          box(title = "Distribuição por Estado Civil", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("estadoCivilPlot", height = 300)),
          box(title = "Distribuição por Cargo/Função", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("cargoPlot", height = 300)),
                #box(title = "Distribuição por CNH", status = "primary", solidHeader = TRUE,
                #    plotlyOutput("cnhPlot", height = 300)),
          box(title = "Distribuição por Meio de Transporte", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("transportePlot", height = 300))
              )
      ),
      # Aba Coleta Seletiva
      tabItem(tabName = "coleta",
              fluidRow(
          box(title = "NO SEU BAIRRO TÊM COLETA SELETIVA?", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("bairroColetaPlot", height = 300)),
          box(title = "RECEBEU INFORMES SOBRE COLETA SELETIVA?", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("informesPlot", height = 300)),
          box(title = "COSTUMA SEPARAR O LIXO?", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("separarLixoPlot", height = 300)),
          box(title = "SABE SEPARAR CORRETAMENTE O LIXO?", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("separarCorretamentePlot", height = 300))
              )
      ),
      # Aba CIRETRAN
      tabItem(tabName = "ciretran",
              fluidRow(
          box(title = "Facilidade de Encontrar Lixeiras", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("lixeiraFacilidadePlot", height = 300)),
          box(title = "Uso de Garrafa e Caneca", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("usoGarrafaCanecaPlot", height = 300)),
          box(title = "Lixeiras de Coleta Seletiva", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("lixeiraColetaSeletivaPlot", height = 300)),
          box(title = "Sabe o Destino Final do Lixo Ciretran", 
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("destinoFinalLixoPlot", height = 300))
          #,
          #box(title = "VC Sugere o Destino Final do Lixo", 
          #    status = "warning", 
          #    solidHeader = TRUE,
          #    collapsible = TRUE,
          #    plotlyOutput("sugeredestinoLixoPlot", height = 300))
              )
      ),
# Aba Escala Likert

tabItem(
  tabName = "likertgeral",
  tabPanel("Escala Likert",
           icon = icon("address-card"),
        fluidRow(
          box( width = 10,
            title = tags$div("PERCEPÇÃO SUSTENTABILIDADE GERAL",
                             style = "text-align: center"),
              status = "success", 
              solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("likertPlot1",
                           width = 800,
                           height = 700)
              )
        )
  )
),

tabItem(
  tabName = "likertgenero",
  tabPanel("Escala Likert Gênero",
           icon = icon("address-card"),   
           fluidRow(
             box(
               width = 10,
               title = "PERCEPÇÃO SUSTENTABILIDADE POR GÊNERO",
               style = "text-align: center",
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               headerBorder = TRUE,
               div(class = "elemente",
                   plotlyOutput("likertPlot2",
                                width = 800,
                                height = 700
                                )
               )
               
             )
           )
  )
),


tabItem(
  tabName = "likertmunic",
  tabPanel("Escala Likert Município",
           icon = icon("address-card"),   
           fluidRow(
             box(
               width = 10,
               title = "Percepção Sustentabilidade Por Município",
               style = "text-align: center",
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               headerBorder = TRUE,
               div(class = "elemente",
                   plotlyOutput("likertPlot3",
                                width = 800,
                                height = 700
                   )
               )
               
             )
           )
  )
)

    ),
)
)
   
#==============================================================================#
# Server
server <- function(input, output, session) {
  output$ciretrantipoA <- renderDiagrammeR({
    mermaid("
graph TB
A[DETRAN-PA]-->B[CIRETRAN A]
B-->C[LEI Nº7594/2011]
B-->D[LEI Nº432/2019]
C-->E[SANTARÉM]
E-->F[CASTANHAL]
F-->G[MARABÁ]
G-->H[ABAETETUBA]
C-->I[ALTAMIRA]
I-->J[CAPANEMA]
J-->K[PARAGOMINAS]
K-->L[TUCURUÍ]
C-->M[REDENÇÃO]
M-->N[ITAITUBA]
N-->O[PARAUAPEBAS]
O-->P[BREVES]
D-->Q[BRAGANÇA]
Q-->R[SÃO FÉLIX]
", width = 1000, 
            align = 'center')
  })
  
  output$ciretrantipoB <- renderDiagrammeR({
    mermaid("graph TB
  A[LEI Nº7594/2011]-->B[DETRAN-PA]
  B-->C[CIRETRAN TIPO B]
  C-->D[SOURE]
  D-->E[ALENQUER]
  E-->F[ALMEIRIM]
  F-->G[MONTE ALEGRE]
  G-->H[ÓBIDOS]
  C-->I[ORIXIMINÁ]
  I-->J[IGUARAPÉ-AÇÚ]
  J-->K[SÃO MIGUEL]
  K-->L[SANTA LUZIA]
  L-->M[TOMÉ-AÇÚ]
  C-->N[ITUPIRANGA]
  N-->O[JACUNDÁ]
  O-->P[RONDON]
  P-->Q[SÃO GERALDO]
  Q-->R[BARCARENA]
  C-->S[IGARAPÉ-MIRI]
  S-->T[MEDICILÂNDIA]
  T-->U[URUARÁ]
  U-->V[CAPITÃO POÇO]
  V-->W[OURILÂNDIA DO NORTE]
  C-->X[DOM ELISEU]
  X-->Y[MÃE DO RIO]
  Y-->Z[NOVO REPARTIMENTO]
  Z-->A1[CONCEIÇÃO DO ARAGUAIA]
  A1-->A2[SANTANA DO ARAGUAIA]
  C-->A3[TUCUMÃ]
  A3-->A4[NOVO PROGRESSO]
  A4-->A5[CANÃA DOS CARAJÁS]
  A5-->A6[CURIONÓPOLIS]
  A6-->A7[RURÓPOLIS]
   C-->A8[ANANINDEUA]
   A8-->A9[CAMETÁ]
   A9-->A10[VIGIA]
   A10-->A11[SALINÓPOLIS]
   A11-->A12[TAILÂNDIA]
  ", width = 1000)
  })

  detran_location <- data.frame(
    lat = -1.37843,
    lon = -48.44034
  )
  
  output$mapa <- renderLeaflet({
    df <- read.csv(textConnection(
      "Nome, lat, lon,
      DETRAN-PA, -1.37843, -48.44034" ))
    leaflet::leaflet(df) %>%
      addTiles() %>%
      #addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      #addProviderTiles(providers$Esri.WorldStreetMap)%>%
      addMarkers(~lon, ~lat, label= ~htmlEscape(Nome),
                 labelOptions = labelOptions(noHide = FALSE,
                                             textsize = "15px")) %>%
      addProviderTiles(providers$OpenSeaMap) %>%
      setView(lng = detran_location$lon,
              lat = detran_location$lat,
              zoom = 15)
  })
  
#------------------------------------------------------------------------------#
# Carregar os dados do Excel
setwd("C:/Users/mario.valente/Documents/github_2024/Projeto_Sustentatbilidade-main")

data <- readxl::read_excel("BANCO_PROJETO_SUSTENTABILIDADE.xlsx")
Dados_Clima <- readxl::read_excel("Dados_Clima.xlS")
#------------------------------------------------------------------------------#

  # Filtrar dados com base no município selecionado
  filtered_data <- reactive({
    subset(data, MUNICIPIO == input$municipio & CNH == input$cnh & P21 == input$destino)
  })
 
  filtered_ciretran_data <- reactive({
    subset(Dados_Clima, MUNICIPIO == input$municipio & CNH == input$cnh & P21 == input$destino)
  })
  
  # Função para criar o gráfico com porcentagens e ordenação
  plot_with_percent <- function(data, x_var, fill_var, title, order = "asc") {
    # Reordenando os níveis da variável categórica
    data[[x_var]] <- if (order == "asc") {
      forcats::fct_infreq(data[[x_var]]) # Crescente
    } else if (order == "desc") {
      forcats::fct_rev(forcats::fct_infreq(data[[x_var]])) # Decrescente
    } else {
      factor(data[[x_var]]) # Ordem original
    }
  
# Função para criar gráficos de barras com percentuais
 #plot_with_percent <- function(data, x_var, fill_var, title) {
    ggplot(data, aes_string(x = x_var, fill = fill_var)) +
      geom_bar(color = "black") +
      geom_text(stat = 'count', aes(label = scales::percent(..count../sum(..count..))),
                position = position_stack(vjust = 0.5), color = "white") +
      labs(title = title, x = "", y = "Nº de Entrevistados") +
      theme_gray()
  }
  # Socio-Econômico
  output$sexoPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "SEXO", "SEXO", "", order = "asc"),
             )
    })
  
  output$racaPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "RACA", "RACA", "", order = "asc" ))
  })
  
  output$idadePlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = IDADE)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "", x = "Idades", y = "Nº de Entrevistados") +
      theme_gray()
    ggplotly(p)
  })
  
  output$escolaridadePlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "ESCOLARIDADE", "ESCOLARIDADE", ""))
  })
  
  output$estadoCivilPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "ESTADO_CIVIL", "ESTADO_CIVIL", ""))
  })
  
  output$cargoPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "CARGO_FUNCAO", "CARGO_FUNCAO", "")+
               coord_flip()
             )
    })
  
  output$cnhPlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "CNH", "CNH", ""))
  })
  
  output$transportePlot <- renderPlotly({
    ggplotly(plot_with_percent(filtered_data(), "MEIO_TRANSPORTE", "MEIO_TRANSPORTE", "")+
             coord_flip()
             )
  })
#------------------------------------------------------------------------------#  
  
  
#------------------------------------------------------------------------------#  
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
#===============================================================================#
  

#===============================================================================#
# GRÁFICO Escala Likert
  my.render.cont <- function(x) {
    with(stats.apply.rounding(stats.default(x), digits=2), 
         c("", "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
  }
  my.render.cat <- function(x) {
    c("", sapply(stats.default(x), 
                 function(y) with(y,
                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
  }
  
  pvalue <- function(x, ...) {
    # Construct vectors of data y, and groups (strata) g
    y <- unlist(x)
    g <- factor(rep(1:length(x), times=sapply(x, length)))
    if (is.numeric(y)) {
      # For numeric variables, perform a standard 2-sample t-test
      p <- t.test(y ~ g)$p.value
    } else {
      # For categorical variables, perform a chi-squared test of independence
      p <- chisq.test(table(y, g))$p.value
    }
    # Format the p-value, using an HTML entity for the less-than sign.
    # The initial empty string places the output on the line below the variable label.
    c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
  }
  

#-------------------------------------------------------------------------------
# Escala Likert
  output$likertPlot1 <- renderPlotly({
    
  dados_filtrados <- Dados_Clima %>%
    filter(MUNICIPIO == input$municipio & CNH == input$cnh & DESTINO_LIXO == input$destino)

  Dados_Clima[,1:9] <- lapply(Dados_Clima[,1:9], 
                                  factor, 
                                  levels = 1:2,
                                  labels = c("Sim", 
                                             "Não"),
                                  order = TRUE)
  
  nomes <- read_excel("Dados_Clima.xls", sheet = 3)
  colnames(Dados_Clima)[1:9] <- nomes$Nomes
  table1(~., data = Dados_Clima, overall = "n(%)", decimal.mark = ",")
  
  caption  <- "Pesquisa Sustentabilidade"
  footnote <- "Fonte: CGP/DETRAN-PA"
  
  
  dados_grafico <- likert(as.data.frame(Dados_Clima[1:9]))
  
  paleta <- brewer.pal(5, "RdBu")
  paleta[3] <- "#DFDFDF"
  
  
  g1 <- likert.bar.plot(dados_grafico,
                        text.size = 3, 
                        hjust = 1) +
    labs(x = "", 
         y = "FREQUÊNCIA (%)") +
    ggtitle("") +
    scale_fill_manual(values = paleta, 
                      breaks = levels(Dados_Clima$`Sabe Qual o Destino do Lixo da Ciretran?` )) +
    guides(fill = guide_legend(title = "Escala Likert")) +
    theme_gray(base_size = 12) +
    theme(
      axis.text.y = element_text(size = 7),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white"),
      plot.title = element_text(hjust = 0.5)  # Centers the title
    )
  ggplotly(g1)
  })
#-------------------------------------------------------------------------------#
  
  
#-------------------------------------------------------------------------------
# Escala Likert GENERO

  output$likertPlot2 <- renderPlotly({
    
    dados_filtrados2 <- Dados_Clima %>%
      filter(MUNICIPIO == input$municipio & CNH == input$cnh & DESTINO_LIXO == input$destino)
    
    Dados_Clima[,1:9] <- lapply(Dados_Clima[,1:9], 
                                factor, 
                                levels = 1:2,
                                labels = c("Sim", 
                                           "Não"),
                                order = TRUE)
    
    nomes <- read_excel("Dados_Clima.xls", sheet = 3)
    colnames(Dados_Clima)[1:9] <- nomes$Nomes
    table1(~., data = Dados_Clima, overall = "n(%)", decimal.mark = ",")
    
    caption  <- "Pesquisa Sustentabilidade"
    footnote <- "Fonte: CGP/DETRAN-PA"
    
    
    dados_grafico2 <- likert(as.data.frame(Dados_Clima[1:9]),
                             grouping = Dados_Clima$GENERO
    )
    
    paleta <- brewer.pal(5, "RdBu")
    paleta[3] <- "#DFDFDF"
    
    
    g2 <- likert.bar.plot(dados_grafico2,
                          text.size = 3, 
                          hjust = 1) +
      labs(x = "", 
           y = "FREQUÊNCIA (%)") +
      ggtitle("") +
      scale_fill_manual(values = paleta, 
                        breaks = levels(Dados_Clima$Q20 )) +
      guides(fill = guide_legend(title = "Escala Likert")) +
      theme_gray(base_size = 12) +
      theme(
        axis.text.y = element_text(size = 7),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5)  # Centers the title
      )
    ggplotly(g2)
  })
  
  #-------------------------------------------------------------------------------#
  
  
  #-------------------------------------------------------------------------------
  # Escala Likert MUNICIPIO
  
  output$likertPlot3 <- renderPlotly({
    
    dados_filtrados2 <- Dados_Clima %>%
      filter(MUNICIPIO == input$municipio & CNH == input$cnh & DESTINO_LIXO == input$destino)
    
    Dados_Clima[,1:9] <- lapply(Dados_Clima[,1:9], 
                                factor, 
                                levels = 1:2,
                                labels = c("Sim", 
                                           "Não"),
                                order = TRUE)
    
    nomes <- read_excel("Dados_Clima.xls", sheet = 3)
    colnames(Dados_Clima)[1:9] <- nomes$Nomes
    table1(~., data = Dados_Clima, overall = "n(%)", decimal.mark = ",")
    
    caption  <- "Pesquisa Sustentabilidade"
    footnote <- "Fonte: CGP/DETRAN-PA"
    
    
    dados_grafico3 <- likert(as.data.frame(Dados_Clima[1:9]),
                             grouping = Dados_Clima$MUNICIPIO)
    
    paleta <- brewer.pal(5, "RdBu")
    paleta[3] <- "#DFDFDF"
    
    
    g3 <- likert.bar.plot(dados_grafico3,
                          text.size = 3, 
                          hjust = 1) +
      labs(x = "", 
           y = "FREQUÊNCIA (%)") +
      ggtitle("") +
      scale_fill_manual(values = paleta, 
                  breaks = levels(Dados_Clima$`Seu Bairro Têm Coleta Seletiva?` )) +
      guides(fill = guide_legend(title = "Escala Likert")) +
      theme_gray(base_size = 12) +
      theme(
        axis.text.y = element_text(size = 7),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5)  # Centers the title
      )
    ggplotly(g3)
  })
  
#-------------------------------------------------------------------------------#
  
  
  

#------------------------------------------------------------------------------#
# Tabela Escala Likert
output$tablikertPlot <- renderTable({
  Dados_Clima[,1:9] <- lapply(Dados_Clima[,1:9], 
                              factor, 
                              levels = 1:2,
                              labels = c("Sim", 
                                         "Não"),
                              order = TRUE)
  
  
  #Definir Tipos de variáveis
  my.render.cont <- function(x) {
    with(stats.apply.rounding(stats.default(x), digits=2), 
         c("", "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))}
  my.render.cat <- function(x) {
    c("", sapply(stats.default(x), 
                 function(y) with(y,
                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))}
  
  # Função Calcular P-valor das variáveis:contínuas/categóricas.
  pvalue <- function(x, ...) {
    y <- unlist(x)
    g <- factor(rep(1:length(x), times=sapply(x, length)))
    if (is.numeric(y)) {
      p <- t.test(y ~ g)$p.value
    } else {
      p <- chisq.test(table(y, g))$p.value
    }
    c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
  }  
  
  #Tabela dos Itens 
  caption  <- "Percepção sobre Sustentabilidade"
  footnote <- "Fonte: CGP/DETRAN-PA"
  
  table1(~., 
         data = Dados_Clima,
         #ctable = TRUE,
         overall = "n(%)",
         #overall = F,
         #decimal.mark = ",",
         caption = caption, 
         footnote = footnote,
         #topclass="Rtable1-grid Rtable1-shade Rtable1-times",
         topclass = "Rtable1-zebra",
         #render.continuous=my.render.cont,
         #render.categorical=my.render.cat
         #extra.col=list(`P-value`=pvalue)
  )
})
  observeEvent(input$reset_button, {
    updateSelectInput(session, "municipio", selected = "Altamira")
    updateSelectInput(session, "cnh", selected = "Sim")
    updateSelectInput(session, "destino", selected = "Aterro Sanitário")
  })
}
  


# Executar o aplicativo
shinyApp(ui, server)
