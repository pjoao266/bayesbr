ajusteUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "ajuste",
    uiOutput(ns('layout_np'))
  )

}

ajusteServer <- function(input, output, session,dados,menu_atual,path_banco,m_viz) {
  ##### Variaveis ####
  ajuste_values = reactiveValues()
  ns = session$ns

  ##### Funções ####

  ###### UI's###############

  output$layout_np = renderUI({
    tagList(
      fluidPage(
        box(width = 12,title = 'Formula',
            fluidRow(
              column(selectInput(ns('var_resp'),label = 'Response variable',
                                 multiple = F,choices=c('')),width = 4),
              column(selectizeInput(ns('covars_theta'),label = 'Covariates for mean',
                                    multiple = T,choices=c(''),
                                    options = list(create = TRUE)),width = 4),
              column(selectizeInput(ns('covars_zeta'),label = 'Covariates for precision',
                                    multiple = T,choices=c(''),
                                    options = list(create = TRUE)),width = 4)
            ),
            fluidRow(class = 'spatial_class',
                     column(selectInput(ns('Spatial'),
                                        label = 'Spatial effect',
                                        choices = c('None','Mean','Precision','Both'),
                                        selected = 'None'),width = 4)
            ),
            fluidRow(
              column(actionButton(ns('salva'),label='Select'),offset = 10,width = 2)
            )
        ),
        box(width = 12,title = 'Prior distribution',
            fluidRow(
              column(3,
                     selectInput(ns('par'),'Parameter',choices = '',
                                 multiple = F)),
              column(3,
                     selectInput(ns('coeficiente_var'),'Covariates',choices = '',
                                 multiple = F)),
              column(3,
                     numericInput(ns('media_priori'),'Prior mean of the coefficient',
                                  value = NULL)),
              column(3,
                     numericInput(ns('var_priori'),'Prior variance of the coefficient',
                                  value = NULL))

            ),
            fluidRow(
              column(1,offset = 11,
                     actionButton(ns('aplicar'),label='Confirm'))
            )
        ),
        box(width = 12,title = 'MCMC setup',
            fluidRow(
              column(4,
                     numericInput(ns('iter'),'Iterations (with warm-up period)',value = NULL)),
              column(4,
                     numericInput(ns('warmup'),'Warm-up period',value = NULL)),
              column(4,
                     selectInput(ns('pars'),'Posterior distribution of parameters',
                                 choices = c(''),multiple = T))
            ),
            fluidRow(
              column(2,offset = 8,downloadButton(ns('download'),label='Download R code')),
              column(2,
                     actionButton(ns('executa'),label='Run'))
            )
        )
      )
    )
  })


  ####### Observe's ##########
  observeEvent(menu_atual(),{
    if(menu_atual() == 'ajuste'){
      banco = dados()
      if(!is.null(banco)){
        ajuste_values$path = path_banco()
        ajuste_values$banco = banco

        matriz_viz = m_viz()
        if(!is.null(matriz_viz)){
          ajuste_values$m_viz = matriz_viz
          runjs(paste0('$(".spatial_class").css({"display": "block"})'))
        }
        else{
          runjs(paste0('$(".spatial_class").css({"display": "none"})'))
        }
      }

    }
  })

  observeEvent(ajuste_values$banco,{
    req(ajuste_values$banco)
    banco = ajuste_values$banco
    updateSelectInput(session,'var_resp',choices = c('',colnames(banco)),
                      selected = '')

    vec = colnames(banco)
    for (i in 1:ncol(banco)) {
      name_i = colnames(banco)[i]
      vec = c(vec,paste0(name_i,'^2'),paste0(name_i,'^3'))
      for(j in (i+1):ncol(banco)){
        name_j = colnames(banco)[j]
        if(j<=ncol(banco) && i!=j) vec = c(vec,paste0(name_i,':',name_j))
      }
    }

    updateSelectInput(session,'covars_theta',
                      choices = c('Intercept',vec) %>% unique(),
                      selected = 'Intercept')

    updateSelectInput(session,'covars_zeta',
                      choices = c('Intercept',vec) %>% unique(),
                      selected = 'Intercept')
  })
  observeEvent(input$var_resp,{
    req(ajuste_values$banco,input$var_resp)
    banco = ajuste_values$banco
    var = banco[,input$var_resp] %>% as_vector()
    banco = banco[colnames(banco)!=input$var_resp]

    if(!is.null(ajuste_values$m_viz)){
      runjs(paste0('$(".spatial_class").css({"display": "block"})'))
    }

    if(!(min(var)>0 & max(var)<1)){
      shinyalert(text='The response variable should only contain values between 0 and 1.',
                 type = 'error')
    }
    else{

      vec = colnames(banco)
      for (i in 1:ncol(banco)) {
        name_i = colnames(banco)[i]
        vec = c(vec,paste0(name_i,'^2'),paste0(name_i,'^3'))
        for(j in (i+1):ncol(banco)){
          name_j = colnames(banco)[j]
          if(j<=ncol(banco) && i!=j) vec = c(vec,paste0(name_i,':',name_j))
        }
      }

      updateSelectInput(session,'covars_theta',
                        choices = c('Intercept',vec) %>% unique(),
                        selected = 'Intercept')

      updateSelectInput(session,'covars_zeta',
                        choices = c('Intercept',vec) %>% unique(),
                        selected = 'Intercept')
    }

  })

  observeEvent(input$salva,{
    req(ajuste_values$banco)
    priores = data.frame(par = NA,var=NA,mean=NA,variance=NA)

    covars_theta = input$covars_theta
    if(length(covars_theta) > 0){
      for (i in 1:length(covars_theta)) {
        priores = rbind(priores, c(par = 'coefficients for mean',var = covars_theta[i],mean=0,variance=10))
      }
    }

    covars_zeta = input$covars_zeta
    if(length(covars_zeta) > 0){
      for (i in 1:length(covars_zeta)) {
        priores = rbind(priores, c(par = 'coefficients for precision',var = covars_zeta[i],mean=0,variance=10))
      }
    }

    ajuste_values$priores = priores %>% drop_na()
    updateSelectInput(session,'par',choices = c('','coefficients for mean','coefficients for precision'),selected = '')
    updateNumericInput(session,'iter',value = 1000,min=10)
    updateNumericInput(session,'',value = 500,min=1)

    ops = c("mean","precision","coefficients for mean","coefficients for precision")
    updateSelectInput(session,'pars',choices = ops,selected = ops)
  })

  observeEvent(input$par,{
    req(ajuste_values$priores,ajuste_values$banco)
    priores = ajuste_values$priores %>% filter(par == input$par)
    ops = priores$var
    updateSelectInput(session,'coeficiente_var',choices = c('',ops),selected = '')
  })

  observeEvent(input$coeficiente_var,{
    req(ajuste_values$priores,ajuste_values$banco)
    priores = ajuste_values$priores %>% filter(par == input$par,
                                               var == input$coeficiente_var)

    updateNumericInput(session,'media_priori',value = priores$mean)
    updateNumericInput(session,'var_priori',value = priores$variance)

  })

  observeEvent(input$aplicar,{
    req(ajuste_values$priores,ajuste_values$banco)
    priores = ajuste_values$priores %>%
      mutate(mean = ifelse(par == input$par & var == input$coeficiente_var,
                           input$media_priori,mean),
             variance = ifelse(par == input$par & var == input$coeficiente_var,
                               input$var_priori,variance))
    ajuste_values$priores = priores
  })

  observeEvent(input$iter,{
    req(ajuste_values$priores,ajuste_values$banco,input$iter)
    if(input$iter<10){
      shinyalert(text = 'number of iterations must be greater than 10.',type='error')
      updateNumericInput(session,'iter',value = 10,min=10)
      updateNumericInput(session,'warmup',value = 5)
    }
    else{
      updateNumericInput(session,'iter',value = round(input$iter,0),min=10)
      updateNumericInput(session,'warmup',value = round((input$iter)/2,0) )
    }


  })

  observeEvent(input$warmup,{
    req(ajuste_values$priores,ajuste_values$banco,input$warmup)
    if(input$warmup<0){
      shinyalert(text = 'warmup iterations must be greater than 0.',type='error')
      updateNumericInput(session,'warmup',value = round((input$iter)/2,0) )
    }else if(input$warmup>input$iter){
      shinyalert(text = 'number of iterations must be greater than warmup period.',type='error')
      updateNumericInput(session,'warmup',value = round((input$iter)/2,0) )
    }else updateNumericInput(session,'warmup',value = round(input$warmup,0) )
  })

  observeEvent(input$executa,{
    req(ajuste_values$banco,input$var_resp,ajuste_values$priores)
    shinyalert(
      title = "Running model...",
      text = "Please wait a moment",
      closeOnEsc = FALSE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "",
      showConfirmButton = FALSE,
      showCancelButton = FALSE,
      timer = 0,
      imageUrl = "https://gifimage.net/wp-content/uploads/2017/10/colorful-loader-gif-transparent-13.gif",
      imageWidth = 100,
      imageHeight = 100,
      animation = TRUE
    )
    formula = str_c(input$var_resp,' ~ ')

    inter_theta = '0'
    covars_theta = input$covars_theta
    if("Intercept" %in% covars_theta){
      inter_theta = '1'
      covars_theta = covars_theta[covars_theta !="Intercept"]
    }
    covars_theta = ifelse(grepl('^',covars_theta,fixed = T),
                          paste0('I(',covars_theta,')'),covars_theta)

    if(length(covars_theta)>0){
      vars_theta = str_c(inter_theta,paste0(' + ',covars_theta,collapse = ''))
    }else{
      vars_theta = inter_theta
    }

    inter_zeta = '0'
    covars_zeta = input$covars_zeta
    if("Intercept" %in% covars_zeta){
      inter_zeta = '1'
      covars_zeta = covars_zeta[covars_zeta !="Intercept"]
    }
    covars_zeta = ifelse(grepl('^',covars_zeta,fixed = T),
                         paste0('I(',covars_zeta,')'),covars_zeta)

    if(length(covars_zeta)>0){
      vars_zeta = str_c(inter_zeta,paste0(' + ',covars_zeta,collapse = ''))
    }else{
      vars_zeta = inter_zeta
    }

    formula = str_c(formula,vars_theta,' | ',vars_zeta)

    priores_b = ajuste_values$priores %>% filter(par == 'coefficients for mean')
    priores_g = ajuste_values$priores %>% filter(par == 'coefficients for precision')

    priores_mean_b = priores_b$mean %>% as.numeric()
    priores_var_b = priores_b$variance %>% as.numeric()
    priores_mean_g = priores_g$mean %>% as.numeric()
    priores_var_g = priores_g$variance %>% as.numeric()

    pars = ifelse(input$pars=='mean','theta',
                  ifelse(input$pars=='precision','zeta',
                         ifelse(input$pars=='coefficients for mean','betas','gammas')))

    if(!is.null(ajuste_values$m_viz)){
      spatial_theta = F
      spatial_zeta = F

      if(input$Spatial == 'Mean'){
        spatial_theta = T
      }
      if(input$Spatial == 'Precision'){
        spatial_zeta = T
      }
      if(input$Spatial == 'Both'){
        spatial_theta = T
        spatial_zeta = T
      }

      ajuste_values$infos = list(formula = formula,
                                 priores_mean_b = priores_mean_b,
                                 priores_var_b = priores_var_b,
                                 priores_mean_g = priores_mean_g,
                                 priores_var_g = priores_var_g,
                                 pars = pars,
                                 warmup = input$warmup,
                                 iter = input$iter,
                                 spatial_theta = spatial_theta,
                                 spatial_zeta = spatial_zeta)


    }else{
      ajuste_values$infos = list(formula = formula,
                                 priores_mean_b = priores_mean_b,
                                 priores_var_b = priores_var_b,
                                 priores_mean_g = priores_mean_g,
                                 priores_var_g = priores_var_g,
                                 pars = pars,
                                 warmup = input$warmup,
                                 iter = input$iter,
                                 spatial_theta = NULL,
                                 spatial_zeta = NULL)

    }
    infos = ajuste_values$infos
    ajuste_values$modelo = bayesbr(infos$formula, data=ajuste_values$banco,
                                   iter=infos$iter,warmup = infos$warmup,
                                   pars = infos$pars, m_neighborhood = ajuste_values$m_viz,
                                   mean_betas = infos$priores_mean_b,
                                   mean_gammas = infos$priores_mean_g,
                                   variance_betas = infos$priores_var_b,
                                   variance_gammas = infos$priores_var_g,
                                   spatial_theta = infos$spatial_theta,
                                   spatial_zeta = infos$spatial_zeta)
    shinyjs::runjs("swal.close();")
    shinyalert(text='The regression model was successfully executed.',
               type = 'success')
  })
  output$download <- downloadHandler(
    filename = function() {
      paste("file.R", sep = "")
    },
    content = function(file) {
      req(ajuste_values$banco,input$var_resp,ajuste_values$priores,
          ajuste_values$modelo)
      caminho = ajuste_values$path
      ext = tools::file_ext(caminho) %>% str_to_lower()

      caminho = str_remove_all(string = caminho %>% str_to_lower(), '.rds')
      caminho = str_remove_all(string = caminho %>% str_to_lower(), '.csv')
      caminho = str_remove_all(string = caminho %>% str_to_lower(), '.txt')
      caminho = str_remove_all(string = caminho %>% str_to_lower(), '.xlsx')

      caminho = paste0(Sys.getenv("HOME"),"\\",caminho,'_shinyBayesbr.RDS')
      text = 'library(bayesbr)\n'
      saveRDS(file = caminho, ajuste_values$banco)

      text = str_c(text,"data = readRDS('",gsub("\\\\", "\\\\\\\\", caminho),"')")
      infos = ajuste_values$infos

      text = str_c(text,'\n\n','formula = "',infos$formula,'"')
      text = str_c(text,'\n\npars = c(',paste0("'",infos$pars,"'",collapse = ","),')')

      text = str_c(text,'\n\npriors_mean_b = c(',paste0(infos$priores_mean_b,collapse = ","),')')
      text = str_c(text,'\n\npriors_var_b = c(',paste0(infos$priores_var_b,collapse = ","),')')
      text = str_c(text,'\n\npriors_mean_g = c(',paste0(infos$priores_mean_g,collapse = ","),')')
      text = str_c(text,'\n\npriors_var_g = c(',paste0(infos$priores_var_g,collapse = ","),')')

      text = str_c(text,'\n\niter = ',infos$iter)
      text = str_c(text,'\n\nwarmup = ',infos$warmup)

      if(!is.null(ajuste_values$m_viz)){
        caminho = paste0(Sys.getenv("HOME"),"\\neighborhoodMatrix_shinyBayesbr.RDS")
        saveRDS(file = caminho, ajuste_values$m_viz)
        text = str_c(text,"\n\nm_neighborhood = readRDS('",gsub("\\\\", "\\\\\\\\", caminho),"')")

        text = str_c(text,'\nspatial_theta = ',infos$spatial_theta)
        text = str_c(text,'\nspatial_zeta = ',infos$spatial_zeta)


        text = str_c(text,'\n\nmodel = bayesbr(formula = formula, data = data,\n',
                     '  mean_betas = priors_mean_b,variance_betas = priors_var_b,\n',
                     '  mean_gammas = priors_mean_g, variance_gammas = priors_var_g,\n',
                     '  m_neighborhood = m_neighborhood,\n',
                     '  spatial_theta = spatial_theta, spatial_zeta = spatial_zeta,\n',
                     '  iter = iter, warmup = warmup, pars = pars)\n\nmodel')

      }else{
        text = str_c(text,'\n\nmodel = bayesbr(formula = formula, data = data,\n',
                     '  mean_betas = priors_mean_b,variance_betas = priors_var_b,\n',
                     '  mean_gammas = priors_mean_g, variance_gammas = priors_var_g,\n',
                     '  iter = iter, warmup = warmup, pars = pars)\n\nmodel')
      }


      writeLines(text, con = file)
    }
  )
  ########## REACTIVE'S #############
  modelo = reactive(ajuste_values$modelo)

  return(list(modelo = modelo))
}
