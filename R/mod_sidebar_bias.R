#' sidebar_bias UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom golem invoke_js
mod_sidebar_bias_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("rho_XY"), "Between-scale correlation", -0.2, min = -1, max = 1, step = 0.1),

    div(id = ns("type_X"),
        HTML("<b>Contamination of X and Y</b>"),
        selectInput(
          ns("type_cont_distribution_X"),
          label = "Contaminated distribution",
          choices = c("random", "straightlining", "extreme"),
          selected = "straightlining"
        )
    ),
    div(id = ns("straight_X"),
        numericInput(ns("straightat_X"), "straightlining at", 1, min = 1, max = 10, step = 1)
    )
  )
}

#' sidebar_bias Server Functions
#'
#' @noRd
mod_sidebar_bias_server <- function(id, tab_name = "crimpact", r){
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

    observeEvent(r$rho_XY,{
      req(r$rho_XY)
      #We update the selector
      updateNumericInput(session, "rho_XY", value = r$rho_XY)
    })

    observeEvent(input$type_cont_distribution_X,{
      req(input$type_cont_distribution_X)

      if(input$type_cont_distribution_X != "straightlining"){
        golem::invoke_js("hideid",ns("straight_X"))
      }else{
        golem::invoke_js("showid",ns("straight_X"))
      }

      r$type_cont_distribution_X <- input$type_cont_distribution_X

    })

    observeEvent( r$type_cont_distribution_X ,{
      req(r$type_cont_distribution_X)
      r$type_cont_distribution_Y <- r$type_cont_distribution_X
    })

    observeEvent(input$straightat_X,{
      req(input$straightat_X)
      r$straightat_X <- input$straightat_X
    })

    observeEvent(r$straightat_X,{
      req( r$straightat_X )
      r$straightat_Y <- r$straightat_X
    })


    observeEvent(r$K_X,{
      req(r$K_X)
      #We update the selector
      updateNumericInput(session, "straightat_X", max = r$K_X)
    })

    observeEvent(r$K_Y,{
      req(r$K_Y)
      #We update the selector
      updateNumericInput(session, "straightat_Y", max = r$K_Y)
    })

    if (tab_name %in% c("crimpact")){
      golem::invoke_js("hideid",ns("straight_X"))
      golem::invoke_js("hideid",ns("type_X"))
    }
    if (tab_name %in% c("bias")){
      golem::invoke_js("showid",ns("straight_X"))
    }

  })
}

## To be copied in the UI
# mod_sidebar_bias_ui("sidebar_bias_1")

## To be copied in the server
# mod_sidebar_bias_server("sidebar_bias_1")
