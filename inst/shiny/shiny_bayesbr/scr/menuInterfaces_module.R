menuInterfacesServer = function(input,output,session){

  menu_atual = reactive({
    input$tabs
  })

  return(list(menu_atual = menu_atual))
}
