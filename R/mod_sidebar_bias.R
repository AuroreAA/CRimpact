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
    numericInput(ns("rho_XY"), "Between scale correlation", -0.2, min = -1, max = 1, step = 0.1),

    div(id = ns("type_X"),
        HTML("<b>Contamination of X and Y</b>"),
        selectInput(
          ns("type_cont_distribution_X"),
          label = "Contaminated distribution",
          choices = c("random", "straight", "extreme"),
          selected = "random"
        )
    ),
    div(id = ns("straight_X"),
        numericInput(ns("straightat_X"), "Straight at", 1, min = 1, max = 10, step = 1)
    ),

    # div(id = ns("type_Y"),
    #     HTML("<b>Contamination of Y</b>"),
    #     selectInput(
    #       ns("type_cont_distribution_Y"),
    #       label = "Contaminated distribution",
    #       choices = c("random", "straight"),
    #       selected = "random"
    #     )
    # ),
    # div(id = ns("straight_Y"),
    #     numericInput(ns("straightat_Y"), "Straight at", 1, min = 1, max = 10, step = 1)
    # )
  )
}

#' sidebar_bias Server Functions
#'
#' @noRd
mod_sidebar_bias_server <- function(id, tab_name, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$rho_XY,{
      req(input$rho_XY)
      r$rho_XY <- input$rho_XY
    })

    observeEvent(r$rho_XY,{
      req(r$rho_XY)
      #We update the selector
      updateNumericInput(session, "rho_XY", value = r$rho_XY)
    })

    observeEvent(input$type_cont_distribution_X,{
      req(input$type_cont_distribution_X)

      if(input$type_cont_distribution_X != "straight"){
        golem::invoke_js("hideid",ns("straight_X"))
      }else{
        golem::invoke_js("showid",ns("straight_X"))
      }

      r$type_cont_distribution_X <- input$type_cont_distribution_X

    })

    # observeEvent(input$type_cont_distribution_Y,{
    #   req(input$type_cont_distribution_Y)
    #
    #   if(input$type_cont_distribution_Y == "random"){
    #     golem::invoke_js("hideid",ns("straight_Y"))
    #   }else{
    #     golem::invoke_js("showid",ns("straight_Y"))
    #   }
    #
    #   r$type_cont_distribution_Y <- input$type_cont_distribution_Y
    #
    # })
    observeEvent( r$type_cont_distribution_X ,{
      req(r$type_cont_distribution_X)
      r$type_cont_distribution_Y <- r$type_cont_distribution_X
    })

    observeEvent(input$straightat_X,{
      req(input$straightat_X)
      r$straightat_X <- input$straightat_X
    })


    # observeEvent(input$straightat_Y,{
    #   req(input$straightat_Y)
    #   r$straightat_Y <- input$straightat_Y
    # })

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
      # golem::invoke_js("hideid",ns("straight_Y"))
      golem::invoke_js("hideid",ns("type_X"))
      # golem::invoke_js("hideid",ns("type_Y"))
    }
    if (tab_name %in% c("bias")){
      golem::invoke_js("showid",ns("straight_X"))
      # golem::invoke_js("showid",ns("straight_Y"))
    }

  })
}

## To be copied in the UI
# mod_sidebar_bias_ui("sidebar_bias_1")

## To be copied in the server
# mod_sidebar_bias_server("sidebar_bias_1")
