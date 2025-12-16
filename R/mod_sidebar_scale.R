#' sidebar_scale UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom golem invoke_js
#' @import bslib
mod_sidebar_scale_ui <- function(id) {
  ns <- NS(id)
  tagList(
    golem::activate_js(), # already loaded in your golem by `bundle_resources()`
    card(
      full_screen = TRUE,
      numericInput(ns("J"), "Number of items", 10, min = 1, max = 50, step = 1),

      selectInput(
        ns("type_true_distribution"),
        label = "True distribution",
        #choices = c("centered","agreeing","polarized", "customized"),
        choices = c("centered", "skewed", "polarized", "latent"),
        selected = "centered"
      ),
      div(id = ns("custom"),
          numericInput(ns("shift"), "Shift value", 0, step = 0.1),
          numericInput(ns("scale"), "Dispersion value", 1, min = 0, step = 0.1),
          numericInput(ns("skewness"), "Assymetry value", 0, min = -0.95, max = 0.95, step = 0.05),
          p(
            tags$small(
              tags$em(
                "For details on the function parameters, please refer to the ",
                tags$a(
                  href = "https://cran.r-project.org/web/packages/latent2likert/refman/latent2likert.html#simulate_likert",
                  target = "_blank",
                  tags$code("simulate_likert()")
                ),
                " and ",
                tags$a(
                  href = "https://cran.r-project.org/web/packages/latent2likert/refman/latent2likert.html#rlikert",
                  target = "_blank",
                  tags$code("rlikert()")
                ),
                " functions from the ",
                tags$a(
                  href = "https://cran.r-project.org/web/packages/latent2likert/",
                  target = "_blank",
                  tags$code("latent2likert")
                ),
                " package."
              )
            )
          )


      ),

      numericInput(ns("rhoscale"), "Within-scale correlation", 0.8, min = 0, max = 1, step = 0.1),
      div(id = ns("sameX"),
          div(
            class = "d-flex justify-content-center",
            actionButton(ns("sameXY"), "Same values for Y")
          )
      )
    )
  )
}

#' sidebar_scale Server Functions
#'
#' @noRd
mod_sidebar_scale_server <- function(id, tab_name, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Parameters for scale X -----
    # Number of items
    observeEvent(input$J,{
      req(input$J)

      if(input$J<1){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Warning",
          text = "The number of items should be at least 1.",
          type = "warning",
          html = FALSE
        )
      }
      validate(need(input$J >= 1,
                    "The number of item should be at least 1." ))
      if(tab_name == "X"){
        r$J_X <- input$J
      }else{
        r$J_Y <- input$J
      }
    })


    # Type of distribution
    observeEvent(input$type_true_distribution,{
      req(input$type_true_distribution)

      if(input$type_true_distribution != "latent"){
        golem::invoke_js("hideid",ns("custom"))
      }else{
        golem::invoke_js("showid",ns("custom"))
      }

      if(tab_name == "X"){
        r$type_true_distribution_X <- input$type_true_distribution
      }else{
        r$type_true_distribution_Y <- input$type_true_distribution
      }

    })



    # Within-scale correlation between items in the X-scale
    observeEvent(input$rhoscale,{
      req(input$rhoscale)

      if(input$rhoscale > 1 | input$rhoscale <0){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Warning",
          text = "The within-scale correlation should be between 0 and 1.",
          type = "warning",
          html = FALSE
        )
      }
      validate(need(abs(input$rhoscale) <= 1,
                    "The within-scale correlation should be between -1 and 1."))


      if(tab_name == "X"){
        r$rhoscale_X <- input$rhoscale
      }else{
        r$rhoscale_Y <- input$rhoscale
      }
    })

    # For customize distribution only
    # mean parameter
    observeEvent(input$shift,{
      req(input$shift)
      if(tab_name == "X"){
        r$shift_X <- input$shift
      }else{
        r$shift_Y <- input$shift
      }
    })

    # sd parameter
    observeEvent(input$scale,{
      req(input$scale)
      if(input$scale <= 0){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Warning",
          text = "The dispersion should be strictly positive.",
          type = "warning",
          html = FALSE
        )
      }
      validate(need(input$scale > 0,
                    "The dispersion should be strictly positive." ))


      if(tab_name == "X"){
        r$scale_X <- input$scale
      }else{
        r$scale_Y <- input$scale
      }
    })

    # skewness parameter
    observeEvent(input$skewness,{
      req(input$skewness)
      if(tab_name == "X"){
        r$skewness_X <- input$skewness
      }else{
        r$skewness_Y <- input$skewness
      }
    })



    # To update Y scale if the same as X -----
    observeEvent(input$sameXY,{
      r$J_Y <- r$J_X
      r$type_true_distribution_Y <- r$type_true_distribution_X
      r$rhoscale_Y <- r$rhoscale_X
      r$shift_Y <- r$shift_X
      r$scale_Y <- r$scale_X
      r$skewness_Y <- r$skewness_X
      r$sameXY <- input$sameXY
    })


    observe({
      if(tab_name == "X"){
        golem::invoke_js("showid",ns("sameX"))
      }else{
        golem::invoke_js("hideid",ns("sameX"))
      }
    })


    observeEvent(r$J_Y,{
      req(r$J_Y)
      if(tab_name == "Y")
        updateNumericInput(session, "J", value = r$J_Y)
    })

    observeEvent(r$type_true_distribution_Y,{
      req(r$type_true_distribution_Y)
      if(tab_name == "Y")
        updateSelectInput(session, "type_true_distribution",
                          selected = r$type_true_distribution_Y)
    })

    observeEvent(r$shift_Y,{
      req(r$shift_Y)
      if(tab_name == "Y")
        updateNumericInput(session, "shift",
                           value = r$shift_Y)
    })
    observeEvent(r$scale_Y,{
      req(r$scale_Y)
      if(tab_name == "Y")
        updateNumericInput(session, "scale",
                           value = r$scale_Y)
    })
    observeEvent(r$skewness_Y,{
      req(r$skewness_Y)
      if(tab_name == "Y")
        updateNumericInput(session, "skewness",
                           value = r$skewness_Y)
    })
    observeEvent(r$rhoscale_Y,{
      req(r$rhoscale_Y)
      if(tab_name == "Y")
        updateNumericInput(session, "rhoscale",
                           value = r$rhoscale_Y)
    })


  })
}

## To be copied in the UI
# mod_sidebar_scale_ui("sidebar_scale_1")

## To be copied in the server
# mod_sidebar_scale_server("sidebar_scale_1")
