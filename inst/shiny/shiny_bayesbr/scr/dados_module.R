dadosUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "dados",
    uiOutput(ns('layout_data'))
  )

}

dadosServer <- function(input, output, session) {
  ##### Variaveis ####
  dados_values = reactiveValues()
  ns = session$ns

  ##### Funções ####

  ###### UI's###############
  output$layout_data = renderUI({
    tagList(
      fluidPage(
        box(width = 12,title = 'Data Select',
            fluidRow(
              column(fileInput(ns('dados'),label = 'Select a database',
                               accept = c('.csv','.xlsx','.rds','.txt')),width = 4),
              column(width = 8,
                     p(span(style='color: #00537f;',
                            'Remark: The data must contain the variables in the columns and the observations in the rows.'))
                       )),
            fluidRow(
              column(fileInput(ns('m_viz'),label = 'Select a neighborhood matrix',
                               accept = c('.csv','.xlsx','.rds','.txt')),width = 4),
              column(width = 8,
                     p(span(style='color: #00537f;',
                            'Remark: The matrix must be symetric, binary and value 1 represents neighborhood.'))
              )
            )
        ),
        box(width = 12,title ='Viewing data',
            dataTableOutput(ns('banco'))),
        box(width = 12,title = 'Graphs',
            fluidRow(
            column(width = 4,
                   selectInput(ns('var1'),label = 'Choose a variable',choices='')),
            column(width = 4,
                   selectInput(ns('tipo'),label='Type of graph',choices='')),

            conditionalPanel("input.tipo == 'Dispersion'",ns = ns,
                             column(width = 4,
                                    selectInput(ns('var2'),label='Choose another variable',choices='')))
            ),
            fluidRow(
              column(width = 12,
                     highchartOutput(ns('graph')))
            )

            )
      )
    )
  })



  ####### Observe's ##########
  observeEvent(input$dados,{
    req(input$dados)
    caminho = input$dados$datapath
    dados_values$datapath = input$dados$name
    ext = tools::file_ext(caminho) %>% str_to_lower()
    if(ext == 'xlsx'){
      dados_values$banco = read.xlsx(input$dados$datapath)
    }
    else if(ext == 'csv' || ext == 'txt'){
      dados_values$banco = read.csv2(caminho)
    }
    else if(ext == 'rds'){
      dados_values$banco = readRDS(caminho)
    }
    else{
      shinyalert(text="The selected database doesn't have a valid extension",
                 type = 'error')
    }
    banco = dados_values$banco

    updateSelectInput(session,'var1',choices = c('',colnames(banco)))
    updateSelectInput(session,'var2',choices = c('',colnames(banco)))

    updateSelectInput(session,'tipo',
                      choices = c('Histogram','Boxplot','Dispersion'),
                      selected = 'Histogram')
  })

  observeEvent(input$m_viz,{
    req(input$m_viz,dados_values$banco)
    banco = dados_values$banco
    caminho = input$m_viz$datapath
    ext = tools::file_ext(caminho) %>% str_to_lower()
    if(ext == 'xlsx'){
      m_neighborhood = read.xlsx(input$m_viz$datapath)
    }
    else if(ext == 'csv' || ext == 'txt'){
      m_neighborhood = read.csv2(caminho)
    }
    else if(ext == 'rds'){
      m_neighborhood = readRDS(caminho)
    }
    else{
      shinyalert(text="The selected neighborhood matrix doesn't have a valid extension",
                 type = 'error')
    }


    m_viz_valido = T
    tam = nrow(m_neighborhood)

    elements = m_neighborhood %>% unique() %>% lapply(unique) %>% do.call(c,.) %>%
      unique()
    Dw = diag(apply(m_neighborhood,1,sum))
    if(nrow(m_neighborhood) != ncol(m_neighborhood)){
      m_viz_valido = F
      shinyalert(text="The selected neighborhood matrix must contain the same number of rows and columns.",
                 type = 'error')
    }
    else if(nrow(m_neighborhood) != nrow(banco)){
      m_viz_valido = F
      shinyalert(text="The selected neighborhood matrix must contain a dimension equal to the number of observations in the database.",
                 type = 'error')
    }
    else if(!isTRUE(all.equal(m_neighborhood,t(m_neighborhood)))){
      m_viz_valido = F
      shinyalert(text="The selected neighborhood matrix must be the same as its transpose matrix.",
                 type = 'error')
    }
    else if(!isTRUE(all.equal(diag(m_neighborhood),diag(matrix(0,tam,tam))))){
      m_viz_valido = F
      shinyalert(text="The selected neighborhood matrix must contain only 0 in its main diagonal.",
                 type = 'error')
    }
    else if(length(elements) != 2 | !(1 %in% elements & 0 %in% elements)){
      m_viz_valido = F
      shinyalert(text="The selected neighborhood matrix must only contain values 0 or 1.",
                 type = 'error')
    }
    else if(0 %in% diag(Dw)){
      m_viz_valido = F
      shinyalert(text="Every observation in the selected neighborhood matrix must contain at least one neighbor.",
                 type = 'error')
    }

    if(m_viz_valido){
      shinyalert(text="The selected neighborhood matrix is valid.",
                 type = 'success')
      dados_values$m_viz = m_neighborhood
    }

  })
  ########## REACTIVE'S #############

  ########### OUTPUT'S ###############
  output$banco = renderDataTable({
    req(dados_values$banco)
    banco = dados_values$banco
    colunas = ncol(banco)-1
    datatable(banco,rownames = F,
              escape = T,extensions = 'Scroller',
              options =
                list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
                     pageLength = 15,
                     scroller = TRUE,
                     scrollX = TRUE,
                     scrollY = 300,
                     columnDefs = list(list(className = 'dt-center', targets = c(0:colunas))),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#00537f', 'color': '#ffffff'});",
                       "}")
                ))
  })

  output$graph = renderHighchart({
    req(input$var1,input$tipo,dados_values$banco)
    banco = dados_values$banco

    if(input$tipo == 'Histogram'){

      var = banco[,input$var1]
      hc <- hchart(
        var,
        color = "#00537f", name = input$var1
      )
    }
    else if(input$tipo == 'Boxplot'){
      dat = data_to_boxplot(data = banco,!!as.name(input$var1),name=input$var1)

      hc = highchart() %>%
        hc_xAxis(type = "category") %>%
        hc_add_series_list(dat)

    }
    else if(input$tipo == 'Dispersion'){
        req(input$var2)
       hc = banco %>%
         hchart('scatter', hcaes(x = !!as.name(input$var1),
                                 y = !!as.name(input$var2)))

    }
      hc %>% hc_colors('#00537f')
  })

  banco = reactive(dados_values$banco)
  path_banco = reactive(dados_values$datapath)
  m_viz = reactive(dados_values$m_viz)
  return(list(dados_banco = banco, path  = path_banco, m_viz = m_viz))
}
