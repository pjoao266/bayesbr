# Carregando pacotes ----
rm(list=ls())
require(shiny)
library(bayesbr)
library(fdrtool)
require(highcharter)
library(DT)
library(coda)
require(shinydashboard)
require(dashboardthemes)
require(shinyalert)
require(shinyjs)
require(openxlsx)
require(dplyr)
require(stringr)
require(ggplot2)
require(magrittr)


#--------------------------------------------------------------------
options(scipen = 999,
        shiny.maxRequestSize=100*1024^2,
        dplyr.summarise.inform = FALSE)

# Lendo Scripts ----
source("scr/sideBar.R", encoding = "UTF-8")

source('scr/ajuste_module.R', encoding = "UTF-8")
source('scr/dados_module.R', encoding = "UTF-8")
source('scr/resultados_module.R', encoding = "UTF-8")

source('scr/menuInterfaces_module.R')

# Design --######################################################################


brbg <- hsv(0.5, .35, seq(.25, .95, length.out = 12))


header <- dashboardHeader(title = "",
                            tags$li(a(target='_blank',
                                      href = 'https://CRAN.R-project.org/package=bayesbr',
                                      img(src = 'logo.png',
                                          title = "bayesbr Logo", height = "70px"),
                                      style = "padding-top:5px; padding-bottom:5px;"),
                                    class = "dropdown",style='height:80px !important;'))

sidebar <- dashboardSidebar(uiOutput("mainsidebar"))


body <- dashboardBody(
    tags$head(
        tags$link(rel = "icon", type = "image/gif", href = "logo.ico")
    ),
    tags$script(HTML('
                   $(document).ready(function() {
                   $("header").find("nav").append(\'<span class="myClass">  </span>\');
                   })
                   ')),
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    useShinyjs(),
    useShinyalert(),
    theme_blue_gradient,
    uiOutput("mainbody")
)

ui <- dashboardPage(
    title = "bayesbr app",
    header,
    sidebar,
    body
)

# Servidor ####################################################################

server <- function(input, output, session) {

    addClass(selector = "body", class = "sidebar-collapse")
    addClass(selector = ".sidebar-toggle", class = "sidebar-toggle-none")

    ### Interface #################################################################

    output$mainsidebar <- renderUI({
        sidebarMenuOutput("sidebarpanel")

    })

    # sidebar login
    output$sidebarpanel <- makeSideBar()


    ### Body ######################################################################

    output$mainbody <- renderUI({
        uiOutput("body")
    })

    output$body <- renderUI({
        tabItems(
            tabItem(
                tabName = "capa",
                img(src="capa.png", align = "center", class="img-responsive"),
                class="active"
            ),

            ajusteUI("ajuste"),
            dadosUI('dados'),
            resultadosUI('resultados')
        )
    })

    # OUTPUTS -----

    # CARREGANDO MÓDULOS ----
    menuInterfaces = callModule(menuInterfacesServer,
                                id='tabela')
    dados = callModule(
        module = dadosServer,
        id = 'dados'
    )
    ajuste = callModule(
        module = ajusteServer,
        id='ajuste',dados = dados$dados_banco,path_banco = dados$path,
        m_viz = dados$m_viz,
        menu_atual = menuInterfaces$menu_atual
    )
    callModule(
        module = resultadosServer,
        id = 'resultados', modelo = ajuste$modelo,
        menu_atual = menuInterfaces$menu_atual
    )


}

shinyApp(ui, server)
