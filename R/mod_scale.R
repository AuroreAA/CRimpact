#' scale UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import DT
#' @import bsicons
#' @import plotly
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
mod_scale_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_fluid(

      layout_columns(
        col_widths = c(9, 3),

        card(
          full_screen = FALSE,
          card_header("Marginal probabilities"),
          max_height = 200,
          DT::DTOutput(ns("df_prop"))
        ),
        value_box(
          title = "Cronbach's alpha",
          value =  textOutput(ns("cronbach")),
          showcase = bsicons::bs_icon("align-bottom"),
          max_height = 200
        )
      ),
      card(
        full_screen = FALSE,
        max_height = 400,
        card_header("True distribution"),
        #plotOutput(ns("plot_prop"))
        plotlyOutput(ns("plot_prop"))
      )
    )
  )
}

#' scale Server Functions
#'
#' @noRd
mod_scale_server <- function(id, tab_name, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Get marginals -------
    observeEvent({
      r$type_true_distribution_X
      r$K_X
      r$shift_X
      r$sd_X
      r$skewness_X
    },
    {
      r$prob_X <- get_marginal(distribution = r$type_true_distribution_X,
                               K = r$K_X,
                               mean = r$shift_X, sd = r$sd_X,
                               skew = r$skewness_X, session = session)
    }
    )

    observeEvent({
      r$type_true_distribution_Y
      r$K_Y
      r$shift_Y
      r$sd_Y
      r$skewness_Y
    },
    {
      r$prob_Y <- get_marginal(distribution = r$type_true_distribution_Y,
                               K = r$K_Y,
                               mean = r$shift_Y, sd = r$sd_Y,
                               skew = r$skewness_Y, session = session )
    }
    )


    # Get simple scale -----
    observeEvent({
      r$prob_X
      r$K_X
      r$rhoscale_X
    },{
      req(r$prob_X)
      req(r$K_X)
      req(r$rhoscale_X)
      r$scale_X <- simple_scale(prob = r$prob_X,
                                K = r$K_X,
                                rho_w = r$rhoscale_X)
    })


    observeEvent({
      r$prob_Y
      r$K_Y
      r$rhoscale_Y
    },{
      req(r$prob_Y)
      req(r$K_Y)
      req(r$rhoscale_Y)
      r$scale_Y <- simple_scale(prob = r$prob_Y,
                                K = r$K_Y,
                                rho_w = r$rhoscale_Y)
    })


    # Internals reactive values -----------------------------------------------
    r_mod <- reactiveValues(
      dt = NULL,
      cronbach = NULL
    )

    observeEvent({
      r$prob_X
      r$prob_Y
    },{
      if (tab_name == "X"){
        r_mod$dt <- r$prob_X
      }else{
        r_mod$dt <- r$prob_Y
      }
      K <- length(r_mod$dt)
      r_mod$dt <- data.frame(t(r_mod$dt))
      colnames(r_mod$dt) <- 1:K
    }
    )


    # Cronbach's alpha -------

    observeEvent({
      r$J_X
      r$rhoscale_X
      r$J_Y
      r$rhoscale_Y
    },{
      if (tab_name == "X"){
        r_mod$cronbach <- cronbach(J = r$J_X, rho = r$rhoscale_X)
      }else{
        r_mod$cronbach <- cronbach(J = r$J_Y, rho = r$rhoscale_Y)
      }

    })

    output$cronbach <- renderText({
      req(r_mod$cronbach)
      round(r_mod$cronbach, digits = 2)
    })


    # PMF Table output -------

    output$df_prop <- DT::renderDT({
      req(r_mod$dt)

      formatRound(
        datatable(
          r_mod$dt, fillContainer = TRUE,
          filter = "none",
          class = "compact",
          rownames = FALSE,
          options = list(
            scroller = FALSE, scrollX = FALSE,
            lengthChange = FALSE,
            paging = FALSE,
            columnDefs = list(
              list(className = 'dt-center', targets = "_all")
            ),
            searching = FALSE,
            dom = 't',
            ordering = FALSE,
            stateSave = TRUE, stateDuration = -1)
        ),
        columns = colnames(r_mod$dt),
        digits = 2)
    },  server = FALSE)


    # Distribution plot output -------

    # output$plot_prop <- renderPlot({
    output$plot_prop  <- renderPlotly({
      req(r_mod$dt)

      df <- data.frame(K = colnames(r_mod$dt), y = t(r_mod$dt))

      p <- ggplot(data = df, aes(x = K, y = y)) +
        geom_bar(stat = "identity") +
        ylim(0, 1) +
        xlab("Response option") +
        ylab("Probability mass") +
        theme_minimal()

      ggplotly(p, tooltip = "text")


    })
  })
}

## To be copied in the UI
# mod_scale_ui("scale_1")

## To be copied in the server
# mod_scale_server("scale_1")
