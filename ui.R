

header <- dashboardHeader(title = "Projeto de Estatística")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Métricas", tabName = "m", icon = icon("chart-line")),
        menuItem('Comparando Criptomoedas', tabName = 'comp', icon = icon('chart-bar'))
    )
)

body <- dashboardBody(
    tabItems(
        ##PAGINA METRICAS
        tabItem(tabName = 'm',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='primary', color = 'blue',
                        selectInput('stock', 'Criptomoeda', cripto_list, multiple=FALSE),
                        uiOutput("timedate"),
                        actionButton('go', 'Submeter')
                        )
                ),
                fluidRow(
                    box(title = "Informações sobre a Criptomoeda", width = 12, solidHeader = TRUE,
                        DTOutput('info')
                    )
                ),
                fluidRow(
                    box(title = "Grafico em Linha (Preco de Fechamento)", width = 12, solidHeader = TRUE,
                        plotOutput('sh')
                    )
                ),
                fluidRow(
                    box(title = "Histograma (Volume de Negociacoes)", width = 12, solidHeader = TRUE,
                        plotOutput('sh2')
                    )
                ),
                fluidRow(
                    box(title = "Boxplot (Abertura, Fechamento, Maxima e Minima)", width = 12, solidHeader = TRUE,
                        plotOutput('sh3')
                    )
                ),
                
        ),
        ##PAGINA COMPARANDO CRIPTOMOEDAS
        tabItem(tabName = 'comp',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='primary',
                        selectInput('stock1', 'Criptomoeda 1', cripto_list, multiple=FALSE),
                        selectInput('stock2', 'Criptomoeda 2', cripto_list, multiple=FALSE),
                        uiOutput("timedate1"),
                        actionButton('go2', 'Submeter ')
                    )
                ),
                fluidRow(
                    box(title = "Informações sobre as Criptomoedas", width = 12, solidHeader = TRUE,
                        DTOutput('info1')
                    )
                ),
                fluidRow(
                    box(title = "Grafico em Linha (Comparacao do Preco de Fechamento)", width = 12, solidHeader = TRUE,
                        plotOutput('sh4')
                    )
                ),
                fluidRow(
                    box(title = "Grafico de Barras Medias (Comparacao do Volume Negociado)", width = 12, solidHeader = TRUE,
                        plotOutput('sh5')
                    )
                ),
        )
        
    )
)

ui <- dashboardPage(
    skin = 'blue',
    header, sidebar, body)
