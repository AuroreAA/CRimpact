#' crimpact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @rawNamespace import(ggplot2, except = last_plot)
mod_crimpact_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      full_screen = TRUE,

      card_header("Plot"),
      plotlyOutput(ns("maxbias_plot"))
    )
  )
}

#' crimpact Server Functions
#'
#' @noRd
mod_crimpact_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Maxbias curve plot output -------

    output$maxbias_plot <- renderPlotly({
      req(r$prop_X)
      req(r$prop_Y)
      req(r$rho_XY)
      req(r$J_X)
      req(r$J_Y)
      req(r$rhoscale_X)
      req(r$rhoscale_Y)

      p <- maxbias_plot(
        FX = r$prop_X,
        rhoXY = r$rho_XY,
        rhoscaleX = r$rhoscale_X,
        JX = r$J_X,
        FY = r$prop_Y,
        rhoscaleY = r$rhoscale_Y,
        JY = r$J_Y,
        eps = seq(from = 0, to = 0.5, by = 0.01),
        digits = 3
      )+
        theme_minimal()


      ggplotly(p, tooltip = "text")

    })
  })
}

## To be copied in the UI
# mod_crimpact_ui("crimpact_1")

## To be copied in the server
# mod_crimpact_server("crimpact_1")
