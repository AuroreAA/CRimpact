#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # NavBarPage --------------------------------------------------------------
    page_navbar(
      title = "Careless Responding Impact",
      window_title = "CRimpact",
      navbar_options = navbar_options(collapsible = FALSE, underline = TRUE),
      theme = bs_theme(bootswatch = "minty"), # morph or spacelab



      # Instructions --------------------------------------------------------------
      nav_panel(title = "Instructions",

                mod_distri_ui("distri_1")
      ),
      # Scales --------------------------------------------------------------
      nav_panel(title = "Scales",


                ## Number of categories ----

                layout_columns(
                  col_widths = c(2,10),
                  mod_sidebar_distri_ui("sidebar_distri_1"),



                  navset_tab(
                    id = "tabs_scales",
                    ## scale X ----

                    nav_panel("Scale X",
                              layout_columns(
                                col_widths = c(3, 9),
                                mod_sidebar_scale_ui("sidebar_scale_X"),
                                mod_scale_ui("scale_X")
                              ),

                    ),#end tabPanel
                    ## scale Y ----
                    nav_panel("Scale Y",
                              layout_columns(
                                col_widths = c(3, 9),
                                mod_sidebar_scale_ui("sidebar_scale_Y"),
                                mod_scale_ui("scale_Y")
                              )

                    )
                  ),#end tabSetPanel
                  fluidRow(

                    column(
                      width = 6,
                      offset = 4,

                      p(),
                      p()
                    )
                  )
                )
      ),

      # Results -----------------------------------------------------------------
      nav_panel("Impact",
                navset_tab(
                  id = "tabs_bias",
                  ## Maximum bias ----

                  nav_panel("Maximum Bias",
                            layout_columns(
                              col_widths = c(3, 9),
                              mod_sidebar_bias_ui("sidebar_bias_1"),
                              mod_crimpact_ui("crimpact_1")
                            ),

                  ),#end tabPanel
                  ## Bias ----
                  nav_panel("Bias",
                            layout_columns(
                              col_widths = c(3, 9),
                              mod_sidebar_bias_ui("sidebar_bias_2"),
                              mod_bias_ui("bias_1")
                            )

                  )
                ),#end tabSetPanel

      ),#END tabPanel

      # Help --------------------------------------------------------------------
      nav_panel("Info",
                layout_columns(
                  col_widths = c(3, 9),
                  mod_sidebar_info_ui("sidebar_info_1"),
                  mod_info_ui("info_1")
                )
      )#END tabPanel

    )

    # Footer ------------------------------------------------------------------
    # tags$footer(
    #   div(
    #     column(width = 6,   paste("2024", paste("Version",  utils::packageVersion("CRimpact")), sep = " | "),
    #            style = "width:500px; height:40px;color: #000000;
    #              padding: 10px;", align = "left"), #color: #242A75
    #     style =       "width:100%; height:40px;
    #   background-color: #E4E4E8;bottom:0;position:fixed;cursor:inherit;" #rgba(0,0,0,0.1)#position:fixed;cursor:inherit;
    #
    #   )

    # # Footer ------------------------------------------------------------------
    # tags$footer(
    #   div(
    #     column(
    #       width = 6,
    #       paste("2024", paste("Version", utils::packageVersion("CRimpact")), sep = " | "),
    #       style = "color: #000000; padding: 5px;",
    #       align = "left"
    #     ),
    #     style = "width:100%; height:30px; background-color: #E4E4E8;
    #          position:fixed; bottom:0; z-index:100;"
    #   )
    # )
  )#END Taglist
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CRimpact"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
