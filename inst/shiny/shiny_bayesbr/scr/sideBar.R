makeSideBar <- function() renderMenu({
  ns = NS('tabela')
  shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  shinyjs::removeClass(selector = ".sidebar-toggle", class = "sidebar-toggle-none")
  dashboardSidebar(
    sidebarMenu(
      id = ns("tabs"),
      menuItem('Data',tabName = 'dados',icon=icon('database')),
      menuItem("Model fit", tabName = "ajuste", icon = icon("calculator")),
      menuItem("Results", tabName = "resultados", icon = icon("chart-line"))

    )
  )
})
