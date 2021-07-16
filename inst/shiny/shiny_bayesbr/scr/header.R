makeHeader <- function(logo, site, img, img_caption) {
  dashboardHeader(tags$li(a(href = site,
                            img(src = img,
                                title = img_caption,
                                height = "70px"
                               ),
                            style = "padding-top:5px; padding-right:5px; padding-bottom:0px;"
                            ),
                          class = "dropdown",
                          style = 'height:80px !important;'
                         )
                  )
}
