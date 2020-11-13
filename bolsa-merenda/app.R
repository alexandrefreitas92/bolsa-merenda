library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)
library(openxlsx)
library(sf)
library(abjutils)
library(brazilmaps)
library(geojson)
library(leafletR)
library(rgdal)
library(rjson)

##### Read db

# Pagamentos - Municipal
pagamentos_alunos_mun <- read_csv("data/pagamentos_alunos_mun.csv")
pagamentos_alunos_mun$X1 <- NULL

# Base mapas
pagamentos_por_regional_ext_objeto <- read_csv("data/pagamentos_alunos_ext_reg.csv") %>%
  select(Diretoria.Regional, perc_familias_recebeu) %>%
  mutate(Diretoria.Regional_2 = rm_accent(Diretoria.Regional))

pagamentos_por_regional_pobres_objeto <- read_csv("data/pagamentos_alunos_pobres_reg.csv")%>%
  select(Diretoria.Regional, perc_familias_recebeu) %>%
  mutate(Diretoria.Regional_2 = rm_accent(Diretoria.Regional))


# mg dados
mg_dados <- openxlsx::read.xlsx("data/mg_dados.xlsx", sheet = 1)

mg_dados <- mg_dados %>%
  select(Código.IBGE, Município, Mesorregião, Diretoria.Regional) %>%
  mutate(Município = str_to_upper(rm_accent(Município)))

# Mapa
url <- 'https://raw.githubusercontent.com/xedar13/bolsa-merenda/master/dashboard/data/regionais_mg.geojson'
geojson <- rjson::fromJSON(file=url)

g <- list(
  fitbounds = "locations",
  visible = FALSE
)

###################################
#                                 #
#           Dashboard             #
#                                 #
###################################

# Dashboard
header <- dashboardHeader(title = "Bolsa Merenda",
                          tags$li(div(href = 'http://social.mg.gov.br',
                                    tags$img(src = 'images/sedese_menor.png',
                                        title = "Sedese", height = "50px"),
                                    style="text-align: center;"),
                                  class = "dropdown"))
#margin-right:10px;
#header$children[[2]]$children <-  tags$a(href='http://social.mg.gov.br',
#                                           tags$img(src='images/sedese_menor.png',height='60',width='200'))


#                            span(img(src="www/images/sedese01.png")), disable = FALSE)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dados Gerais",
             tabName = "dados_gerais", 
             icon = icon("chart-bar"))
  )
)
body <- dashboardBody(
  tags$head(tags$style(HTML(' /* body */
                                .content-wrapper, .right-side {
                                background-color: white;
                                }
        /* main sidebar */
        .skin-black .main-sidebar {
                              background-color: #383d40;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: white;
                              }
        /* other links in the sidebarmenu */

        .skin-black .main-sidebar .sidebar .sidebar-menu a{
                              background-color: white;
                              color: black;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: light-gray;
                              }
        /* toggle button when hovered  */                    
         .skin-black .main-header .navbar .sidebar-toggle:hover{
                              background-color: light-gray;
                              }
'))),
  tabItems(
    tabItem(tabName = "dados_gerais",
            fluidRow(
              infoBox("Valor transferido", "R$ 89.606.200", icon = icon("donate"), width = 3, color = "olive"),
              infoBox("Número de Potenciais Beneficiários", "469.675", icon = icon("user-friends"), width = 3, color = "yellow"),
              infoBox(HTML(paste("Percentual de famílias extremamente",br(), "pobres atendidas")), "90,4%", icon = icon("chart-line"), width = 3, color = "purple"),
              infoBox(HTML(paste("Percentual de famílias",br(), "pobres atendidas")), "20,6%", icon = icon("chart-line"), width = 3, color = "navy")
            ),
            fluidRow(
              column(plotlyOutput("mapa_ext_pobres"), width = 6),
              column(plotlyOutput("mapa_pobres"), width = 6)
            ),
            fluidRow(
              column(h3("Tabela com informações municipalizadas"),width =  10, offset = 1, align = "center", DT::DTOutput('table_municipio'),style = "background: white; margin-top: 20px")
            )
    )
  )
)

# Define UI ----
ui <- dashboardPage(header,sidebar,body, skin = "black")

# Define server logic ----
server <- function(input, output) {
  
  output$mapa_ext_pobres <- renderPlotly({
    mapa_ext_pobres <- plot_ly()
    mapa_ext_pobres <- mapa_ext_pobres %>%
      add_trace(type = "scattergeo", 
                mode = "text",
                geojson=geojson,
                locations=pagamentos_por_regional_ext_objeto$Diretoria.Regional_2,
                text = paste("<b>",pagamentos_por_regional_ext_objeto$Diretoria.Regional,"<b>", sep = ""),
                locationmode = "geojson-id",
                featureidkey="properties.Drtr_Rg",
                textfont = list(size = 8, color = "black"),
                hoverinfo = "skip"
      )
    mapa_ext_pobres <- mapa_ext_pobres %>%
      add_trace(
        type="choropleth",
        geojson=geojson,
        locations=pagamentos_por_regional_ext_objeto$Diretoria.Regional_2,
        z=pagamentos_por_regional_ext_objeto$perc_familias_recebeu,
        colorscale= list(c(0, "rgb(255, 255, 255)"), c(1, "rgb(255,127,0)")),
        zmin = 0,
        #  zmid = 50,
        zmax = 100,
        text = pagamentos_por_regional_ext_objeto$Diretoria.Regional,
        featureidkey="properties.Drtr_Rg",
        hovertemplate = paste("%{text}: %{z:.1f}%<extra></extra>"),
        marker = list(line = list(color = "#000"))
      )
    mapa_ext_pobres <- mapa_ext_pobres %>% colorbar(title = "<b>Porcentagem<br> </b>",
                                                    nticks = 10, ticklen = 10)
    mapa_ext_pobres <- mapa_ext_pobres %>% layout(
      title = "\nPorcentagem de famílias extremamente pobres atendidas - 16/10/2020",
      geo = g
    )  
  })#
  
  output$mapa_pobres <- renderPlotly({
    mapa_pobres <- plot_ly()
    mapa_pobres <- mapa_pobres %>%
      add_trace(type = "scattergeo", 
                mode = "text",
                geojson=geojson,
                locations=pagamentos_por_regional_pobres_objeto$Diretoria.Regional_2,
                text = paste("<b>",pagamentos_por_regional_pobres_objeto$Diretoria.Regional,"<b>", sep = ""),
                locationmode = "geojson-id",
                featureidkey="properties.Drtr_Rg",
                textfont = list(size = 8),
                hoverinfo = "skip"
      )
    mapa_pobres <- mapa_pobres %>%
      add_trace(
        type="choropleth",
        geojson=geojson,
        locations=pagamentos_por_regional_pobres_objeto$Diretoria.Regional_2,
        z=pagamentos_por_regional_pobres_objeto$perc_familias_recebeu,
        colorscale= list(c(0, "rgb(255, 255, 255)"), c(1, "rgb(255,127,0)")), #rgb(153, 51, 153)
        zmin = 0,
        #  zmid = 50,
        zmax = 100,
        text = pagamentos_por_regional_pobres_objeto$Diretoria.Regional,
        featureidkey="properties.Drtr_Rg",
        hovertemplate = paste("%{text}: %{z:.1f}%<extra></extra>"),
        marker = list(line = list(color = "#000"))
      )
    mapa_pobres <- mapa_pobres %>% colorbar(title = "<b>Porcentagem<br> </b>",
                                            nticks = 10, ticklen = 10)
    mapa_pobres <- mapa_pobres %>% layout(
      title = "\nPorcentagem de famílias pobres atendidas - 16/10/2020",
      geo = g
    ) 
  })
  
  output$table_municipio <- DT::renderDT({
    datatable(pagamentos_alunos_mun, 
              rownames = FALSE,
              colnames = c('Diretoria Regional', 'Código IBGE','Município', 
                           'Número de estudantes extremamente pobres',
                           'Número de estudantes extremamente pobres que receberam',
                           'Percentual de estudantes extremamente pobres que receberam',
                           'Número de estudantes pobres',
                           'Número de estudantes pobres que receberam',
                           'Percentual de estudantes pobres que receberam'
              )
    ) %>%
      formatPercentage('perc_alunos_recebeu', 2) %>%
      formatPercentage('perc_alunos_recebeu_pobres', 2)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
