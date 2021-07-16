resultadosUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "resultados",
    uiOutput(ns('layout_resultados'))
  )

}

resultadosServer <- function(input, output, session, modelo,menu_atual) {
  ##### Variaveis ####
  resultados_values = reactiveValues()
  ns = session$ns

  ##### Funções ####
  envelope_shiny = function(x,sim = 1000,conf = 0.95,resid.type  = c("","quantile","sweighted", "pearson","ordinary")){
    resid.type = match.arg(resid.type)
    if(resid.type==""){
      resid.type = x$residuals.type
    }
    res = residuals(x,resid.type)
    if(!is.numeric(sim)){
      warning("The parameter sim must be a number, sim will be set equal to its default value 1000",call.=TRUE)
      sim = 1000
    }
    else{
      if(sim<10){
        warning("The minimum value that the parameter sim can take is 10, sim will be set equal to 10",call.=TRUE)
        sim = 10
      }
      if(sim%%1>0){
        warning("Parameter sim must be an integer, sim will be defined ignoring decimal places",call.=TRUE)
        sim = as.integer(sim)
      }
    }
    if(!is.numeric(conf)){
      warning("The conf parameter must be a probability, conf will be set equal to its default value 0.95",call.=TRUE)
      conf = 0.95
    }
    else{
      if(conf>1 | conf<0){
        warning("The parameter conf must be a probability, that is, be contained between 0 and 1, conf will be set equal to its default value 0.95",call.=TRUE)
        conf = 0.95
      }
      if(conf==0){
        warning("It is not possible to perform a confidence interval with parameter conf equal to 0, conf will be set equal to its default 0.95",call.=TRUE)
        conf = 0.95
      }
      if(conf==1){
        warning("It is not possible to perform a confidence interval with parameter conf equal to 1, conf will be set equal to its default 0.95",call.=TRUE)
        conf = 0.95
      }
    }
    n = length(res)
    samples = matrix(nrow = sim, ncol=n)
    q1 = c()
    q2 = c()
    q3 = c()
    confstar = (1 - conf)/2
    for(i in 1:sim){
      sample =  rhalfnorm(n)
      sample = sort(sample)
      samples[i,] = sample
    }
    for(i in 1:n){
      sample = samples[,i]
      q1 = c(q1,as.numeric(quantile(sample,0+confstar)))
      q2 = c(q2,as.numeric(quantile(sample,0.500)))
      q3 = c(q3,as.numeric(quantile(sample,1-confstar)))
    }
    res = sort(abs(res))
    index = 1:n
    p = (index - 0.5)/n
    q = qhalfnorm(p)
    ggplot() + geom_point(mapping = aes(y = res,x = q),color='#00537f') +
      geom_line(aes(y=q1,x=q),color='#517aa1') +
      geom_line(aes(y=q2,x=q),color='#517aa1',linetype = "dashed")+
      geom_line(aes(y=q3,x=q),color='#517aa1') +
      scale_y_continuous(breaks = seq(0,6,0.5)) +
      ylab("Residuals abs. ord.") +
      xlab("Half-normal quantiles") +
      ggtitle("Half-normal residual plots with simulated envelope",subtitle = paste0("With ",conf*100,"% confindence"))
  }

  ###### UI's###############
  output$layout_resultados = renderUI({
    tagList(
      fluidPage(
        box(width=12, title='Model summary',
            verbatimTextOutput(ns('summary'))),
        box(width=12, title='Posterior results',
            fluidRow(
              column(width= 4,
                     selectInput(ns('tipo_par'),label = 'Param',choices = '')),
              column(width= 4,
                     selectInput(ns('chain_histo'),label = 'Graph',choices = ''))
            ),
            fluidRow(
              column(12,plotOutput(ns('posteriori_graphs')))
            )
            ),
        box(width=12, title='Envelope residuals',
            fluidRow(
              column(width= 4,
                     selectInput(ns('tipo_res'),label = 'Type of residual',choices = '')),
              column(width= 4,
                     numericInput(ns('sim'),label = 'Number of simulations',value=NULL)),
              column(width= 4,
                     numericInput(ns('conf'),label = 'Confidence level of envelope graph',value = NULL)),


            ),
            fluidRow(
              column(12,plotOutput(ns('envelope')))
            )
            )

      )
      )
  })


  ####### Observe's ##########
  observeEvent(menu_atual(),{
    if(menu_atual() == 'resultados'){
      modelo = modelo()
      if(!is.null(modelo)){
        resultados_values$modelo = modelo

        updateSelectInput(session,'tipo_res',
                          choices = c("","quantile", "sweighted","pearson","ordinary"),
                          selected = '')
        updateNumericInput(session,'sim',
                           value = 1000, step = 1, min = 10)
        updateNumericInput(session,'conf',
                           value = 0.95, step = 0.01, min = 0.01,
                           max=0.99)

        ops = c("",'loglik','mean',
                'precision')
        if(modelo$info$spatial_theta){
          ops = c(ops,'spatial effect for mean')
        }
        if(modelo$info$spatial_zeta){
          ops = c(ops,'spatial effect for precision')
        }
        n_beta = modelo$info$samples$beta %>% length()
        n_gamma = modelo$info$samples$gamma %>% length()
        ops = c(ops,paste0('coefficient for mean: ',modelo$info$names$names_x))
        ops = c(ops,paste0('coefficient for precision: ',modelo$info$names$names_w))

        updateSelectInput(session,'tipo_par',
                          choices = ops,
                          selected = '')


      }
    }
  })
  observeEvent(input$tipo_par,{
    req(input$tipo_par)

    if(input$tipo_par %in% c('mean','precision','spatial effect for mean',
                             'spatial effect for precision')){
      updateSelectInput(session,'chain_histo',
                        choices = c('','HPD Intervals'),
                        selected = '')
    }else{
      updateSelectInput(session,'chain_histo',
                        choices = c('','Traceplot','Posterior Density'),
                        selected = '')
    }

  })
  observeEvent(input$sim,{
    req(resultados_values$modelo,input$sim)
    if(input$sim<10){
      shinyalert(text = 'number of simulations must be greater than 10.',type='error')
      updateNumericInput(session,'sim',value = 10)
    }else updateNumericInput(session,'sim',value = round(input$sim,0) )
  })

  observeEvent(input$conf,{
    req(resultados_values$modelo,input$conf)
    if(input$conf>0.99){
      shinyalert(text = 'confidence level must be between 0 and 1.',type='error')
      updateNumericInput(session,'conf',value = 0.99)
    }
    if(input$conf<0.01){
      shinyalert(text = 'confidence level must be between 0 and 1.',type='error')
      updateNumericInput(session,'conf',value = 0.01)
    }
  })


  ########## REACTIVE'S #############

  ########### OUTPUT'S ###############
  output$summary = renderPrint({
    req(resultados_values$modelo)
    summary(resultados_values$modelo)
  })

  output$posteriori_graphs = renderPlot({
    req(resultados_values$modelo,input$tipo_par,input$chain_histo)
    modelo = resultados_values$modelo

    if(input$tipo_par %in% c('mean','precision','spatial effect for mean',
                            'spatial effect for mean')){
      if(input$tipo_par == 'mean'){
        amostra = modelo$info$samples$theta
      }
      else if(input$tipo_par == 'precision'){
        amostra = modelo$info$samples$zeta
      }
      else if(input$tipo_par == 'spatial effect for mean'){
        amostra = modelo$info$samples$delta
      }
      else if(input$tipo_par == 'spatial effect for precision'){
        amostra = modelo$info$samples$xi
      }
    }
    else if(grepl(x = input$tipo_par,pattern = 'mean')){
      num = which(input$tipo_par ==
                    paste0('coefficient for mean: ',modelo$info$names$names_x))
      beta = modelo$info$samples$beta
      amostra = beta[[num]]
    }
    else if(grepl(x = input$tipo_par,pattern = 'precision')){
      num = which(input$tipo_par ==
                    paste0('coefficient for precision: ',modelo$info$names$names_w))
      gamma = modelo$info$samples$gamma
      amostra = gamma[[num]]
    }
    else{
      amostra = modelo$loglik
    }

    nome = input$tipo_par %>% str_remove_all('coefficient for mean: ') %>%
      str_remove_all('coefficient for precision: ')

    if(input$chain_histo == 'Traceplot'){
      n = length(amostra)
      data.frame(amostra) %>%
        ggplot() + geom_line(aes(y = amostra,x=1:n),color='#00537f') + ylab(nome) +
        xlab("Iterations without warmups") +
        ggtitle(paste0(nome," chains of the estimated model"))
      }
    else if(input$chain_histo == 'Posterior Density'){

      data.frame(amostra = amostra) %>% ggplot(data = ., aes(x = amostra)) +
        geom_density(bins=15,color='#00537f') + ylab(nome) +
        xlab("Density") +
        ggtitle(paste0(nome," density of the estimated model"))
    }
    else{
      hpd_inf = c()
      hpd_sup = c()
      mean_posterior = c()
      n = modelo$info$n
      for (i in 1:n) {
        sample_mcmc = as.mcmc(amostra[[i]])
        hpd = HPDinterval(sample_mcmc, prob=0.95)
        hpd_inf = c(hpd_inf, hpd[1])
        hpd_sup = c(hpd_sup, hpd[2])
        mean_posterior = c(mean_posterior, mean(amostra[[i]]))
      }

      data = data.frame(mean_posterior,hpd_inf,hpd_sup)
      limits <- aes(ymax = hpd_inf, ymin = hpd_sup)

      valor = ifelse(input$tipo_par == 'mean',0.5,0)
      if(input$tipo_par == 'precision'){
        valor = NULL
      }

      ggplot(data, aes(y = mean_posterior,
                       x = 1:n)) +
        geom_point(color='#00537f') +
        geom_errorbar(limits) +
        geom_hline(yintercept = valor,color ='red',size=1.3) +
        labs(title = paste0("HPD intervals for ",nome)) +
               xlab('observations') +
        ylab(nome)
    }

  })

  output$envelope = renderPlot({
    req(input$sim, input$conf, input$tipo_res,resultados_values$modelo)
    modelo = resultados_values$modelo

    envelope_shiny(modelo,sim = round(input$sim,0), conf = input$conf, resid.type = input$tipo_res)
  })
}
