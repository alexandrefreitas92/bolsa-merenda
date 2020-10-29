library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)
library(openxlsx)
library(sf)
library(abjutils)
library(brazilmaps)

##### Read db

# Pagamentos - Municipal
pagamentos_alunos_mun <- read_csv("data/pagamentos_alunos_mun.csv")
pagamentos_alunos_mun$X1 <- NULL

# Base mapas
pagamentos_por_regional_ext_objeto <- read_csv("data/pagamentos_alunos_ext_reg.csv")
pagamentos_por_regional_ext_objeto$X1 <- NULL

pagamentos_por_regional_pobres_objeto <- read_csv("data/pagamentos_alunos_pobres_reg.csv")
pagamentos_por_regional_pobres_objeto$X1 <- NULL

# mg dados
mg_dados <- openxlsx::read.xlsx("data/mg_dados.xlsx", sheet = 1)

mg_dados <- mg_dados %>%
  select(Código.IBGE, Município, Mesorregião, Diretoria.Regional) %>%
  mutate(Município = str_to_upper(rm_accent(Município)))

############ dados de coordenadas geográficas
minas_cidades <- get_brmap(geo="City", geo.filter = list(State=31))

mapa_reg <- right_join(mg_dados, minas_cidades, by = c("Código.IBGE" = "City")) %>%
  group_by(Diretoria.Regional) %>%
  summarise(geometry= st_union(geometry),
            centroide = st_centroid(geometry))

mapa_reg <- mapa_reg %>%
  separate(centroide, into = c("long", "lat"), sep = ",") %>%
  mutate(long = as.numeric(str_replace(long, "c\\(", "")),
         lat = as.numeric(str_replace(lat, "\\)", "")))

# Create maps structure
mapa_pagamentos_por_regional_ext_objeto <- mapa_reg %>%
  inner_join(pagamentos_por_regional_ext_objeto) %>%
  mutate(rotulo = sprintf("%s (%.1f%%)", str_to_upper(Diretoria.Regional), perc_familias_recebeu))

mapa_pagamentos_por_regional_pobres_objeto <- mapa_reg %>%
  inner_join(pagamentos_por_regional_pobres_objeto) %>%
  mutate(rotulo = sprintf("%s (%.1f%%)", str_to_upper(Diretoria.Regional), perc_familias_recebeu))


###################################
#                                 #
#           Dashboard             #
#                                 #
###################################

# Dashboard
header <- dashboardHeader(title = "Bolsa Merenda")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dados Gerais",
             tabName = "dados_gerais", 
             icon = icon("chart-bar"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dados_gerais",
            fluidRow(
              infoBox("Valor transferido", "R$ 89.606.200", icon = icon("donate"), width = 3, color = "olive"),
              infoBox("Número de Beneficiários", "469.675", icon = icon("user-friends"), width = 3, color = "yellow"),
              infoBox(HTML(paste("Percentual de famílias extremamente",br(), "pobres atendidas")), "90,4%", icon = icon("chart-line"), width = 3, color = "purple"),
              infoBox(HTML(paste("Percentual de famílias",br(), "pobres atendidas")), "20,6%", icon = icon("chart-line"), width = 3, color = "navy")
            ),
            fluidRow(
              column(plotOutput("mapa_ext_pobres"), width = 6),
              column(plotOutput("mapa_pobres"), width = 6)
            ),
            fluidRow(
              column(h3("Tabela com informações municipalizadas"),width =  10, offset = 1, align = "center", DT::DTOutput('table_municipio'))
            )
    )
  )
)

# Define UI ----
ui <- dashboardPage(header,sidebar,body)

# Define server logic ----
server <- function(input, output) {
     
  output$mapa_ext_pobres <- renderPlot({
     mapa_ext_pobres <- mapa_pagamentos_por_regional_ext_objeto %>%
       ggplot(aes(fill=perc_familias_recebeu, geometry = geometry)) +
       geom_sf(size = 0.5, color = "gray40") +
       scale_fill_gradient(low = "white", high = "darkorange1", name = "%", limits = c(80,100), breaks = seq(80,100,5)) +
       theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(),
             axis.text = element_blank(), axis.ticks = element_blank()) +
       labs(title = "Percentual de famílias extremamente pobres já contempladas pelo Bolsa Merenda \nnas regionais SEDESE de Minas Gerais",
            subtitle = "16/10/2020") +
       xlab("") +
       ylab("") +
       geom_text(aes(x=long, y=lat, label=str_wrap(rotulo, 9)), size=2.4, fontface="bold"
       )
     
    plot(mapa_ext_pobres)
   })#

  output$mapa_pobres <- renderPlot({
    mapa_pobres <- mapa_pagamentos_por_regional_pobres_objeto %>%
      ggplot(aes(fill=perc_familias_recebeu, geometry = geometry)) +
      geom_sf(size = 0.5, color = "gray40") +
      scale_fill_gradient(low = "white", high = "darkorange1", name = "%", limits = c(0,60), breaks = seq(0,60,10)) +
      theme(panel.grid = element_line(colour = "transparent"), panel.background = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank()) +
      labs(title = "Percentual de famílias pobres já contempladas pelo Bolsa Merenda \nnas regionais SEDESE de Minas Gerais",
           subtitle = "16/10/2020") +
      xlab("") +
      ylab("") +
      geom_text(aes(x=long, y=lat, label=str_wrap(rotulo, 9)), size=2.4, fontface="bold"
      )
        
      plot(mapa_pobres)
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
