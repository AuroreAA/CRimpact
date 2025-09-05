#' bias UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import plotly
#' @rawNamespace import(ggplot2, except = last_plot)
mod_bias_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      full_screen = TRUE,

      card_header("Plot"),
      #plotOutput(ns("bias_plot"))
      plotlyOutput(ns("bias_plot"))
    )
  )
}

#' bias Server Functions
#'
#' @noRd
mod_bias_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent({
      r$type_cont_distribution
      r$K_X
      r$straightat},{
        r$prop_cont <- get_contam_distribution(type = r$type_cont_distribution,
                                                           K = r$K_X,
                                                           straightat = r$straightat)
      }
    )

    # Bias curve plot output -------
    output$bias_plot <- renderPlotly({
      req(r$prop_X)
      req(r$prop_Y)
      req(r$type_cont_distribution_X)
      req(r$type_cont_distribution_Y)

      req(r$rho_XY)
      req(r$J_X)
      req(r$J_Y)
      req(r$rhoscale_X)
      req(r$rhoscale_Y)
      req(r$straightat_X)
      req(r$straightat_Y)

      p <- bias_plot(
        FX = r$prop_X,
        HX = r$type_cont_distribution_X,
        rhoXY = r$rho_XY,
        rhoscaleX = r$rhoscale_X,
        JX = r$J_X,
        FY = r$prop_Y,
        HY = r$type_cont_distribution_Y,
        rhoscaleY = r$rhoscale_Y,
        JY = r$J_Y,
        KX = r$K_X,
        KY = r$K_Y,
        straightatX = r$straightat_X,
        straightatY = r$straightat_Y,
        eps = seq(from = 0, to = 0.5, by = 0.01),
        digits = 3
      )+
        theme_minimal()

      ggplotly(p, tooltip = "text")
    })
  })
}

## To be copied in the UI
# mod_bias_ui("bias_1")

## To be copied in the server
# mod_bias_server("bias_1")
