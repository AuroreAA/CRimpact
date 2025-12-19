#' adversarial UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_adversarial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      full_screen = TRUE,

      card_header("Adversarial impact plot"),
      plotlyOutput(ns("maxbias_plot"))
    )
  )
}

#' adversarial Server Functions
#'
#' @noRd
mod_adversarial_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Get max bias -------

    observeEvent({
      r$model_XY
    },{
      req(r$model_XY)

      epsilon = seq(from = 0, to = 0.5, by = 0.01)
      r$max_bias <- max_bias(r$model_XY,
                                 epsilon = epsilon)
      r$breakdown <- breakdown(r$model_XY)
    })



    # Max Bias curve plot output -------
    output$maxbias_plot <- renderPlotly({
      req(r$max_bias)
      req(r$model_XY)
      req(r$measure)
      req(r$breakdown)
      req(r$measure)


      p <- max_bias_plot_new(
        r$max_bias,
        type = r$measure,
        direction = r$direction,
        digits = 3,
        breakdown = r$breakdown
      )+
        theme_minimal()

      ggplotly(p, tooltip = "text")
    })

  })
}

## To be copied in the UI
# mod_adversarial_ui("adversarial_1")

## To be copied in the server
# mod_adversarial_server("adversarial_1")
