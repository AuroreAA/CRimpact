#' sidebar_adversarial UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_adversarial_ui <- function(id) {
  ns <- NS(id)
  tagList(

    golem::activate_js(), # already loaded in your golem by `bundle_resources()`
    card(
      full_screen = FALSE,
      max_height = 150,
      card_header("Model distribution"),
      numericInput(ns("rho_XY"), "True correlation",
                   -0.2, min = -1, max = 1, step = 0.1)
    ),
    card(
      full_screen = FALSE,
      max_height = 350,
      card_header("Adversarial impact"),
      selectInput(
        ns("measure"),
        label = "Measure",
        choices = c("Bias", "Correlation"),
        selected = "Correlation"
      ),
      selectInput(
        ns("direction"),
        label = "Direction",
        choices = c("Upward", "Downward", "default"),
        selected = "default"
      )
    )


  )
}

#' sidebar_adversarial Server Functions
#'
#' @noRd
mod_sidebar_adversarial_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$rho_XY,{
      req(input$rho_XY)


      if(abs(input$rho_XY) > 1){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Warning",
          text = "The between-scale correlation should be between -1 and 1.",
          type = "warning",
          html = FALSE
        )
      }
      validate(need(abs(input$rho_XY) <= 1,
                    "The between-scale correlation should be between -1 and 1."))

      r$rho_XY <- input$rho_XY
    })

    observeEvent({
      r$rho_XY
    },
    {
      req(r$rho_XY)
      #We update the selector
      updateNumericInput(session, "rho_XY", value = r$rho_XY)
    })
    # get model XY
    observeEvent({
      r$rho_XY
      r$scale_X
    },
    {
      req(r$rho_XY)
      req(r$scale_X)

      r$model_XY <- simple_model(X = r$scale_X, rho = r$rho_XY)
    }
    )


    observeEvent(input$measure,{
      req(input$measure)

      r$measure <- input$measure

    })


    observeEvent(r$measure,{
      req(r$measure)
      updateSelectInput(session, "measure",
                        selected = r$measure)
    })


    observeEvent(input$direction,{
      req(input$direction)

      r$direction <- input$direction

    })
  })
}

## To be copied in the UI
# mod_sidebar_adversarial_ui("sidebar_adversarial_1")

## To be copied in the server
# mod_sidebar_adversarial_server("sidebar_adversarial_1")
