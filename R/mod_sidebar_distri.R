#' sidebar_distri UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_distri_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      full_screen = FALSE,
      card_header("Scales X & Y"),
      max_height = 175,
      numericInput(ns("K"), label = "Number of answer categories", 5, min = 3,
                   max = 50, step = 1)
    )
  )
}

#' sidebar_distri Server Functions
#'
#' @noRd
mod_sidebar_distri_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Number of response categories
    observeEvent({
      input$K
    },{
      req(input$K)
      r$K_X <- r$K_Y <- input$K
    })
  })

}

## To be copied in the UI
# mod_sidebar_distri_ui("sidebar_distri_1")

## To be copied in the server
# mod_sidebar_distri_server("sidebar_distri_1")
