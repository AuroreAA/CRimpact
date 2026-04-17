#' impact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_impact_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      full_screen = TRUE,
      card_header("Contaminated distribution"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = FALSE,
          max_height = 200,
          card_header("Marginal probabilities"),
          DT::DTOutput(ns("df_prop"))
        ),
        card(
          full_screen = FALSE,
          max_height = 200,
          card_header("Distribution"),
          plotlyOutput(ns("plot_prop"))
        )
      ),

      card_header("Careless impact plot"),
      plotlyOutput(ns("measure_plot"))
    ),
    p(),
    p()
  )
}

#' impact Server Functions
#'
#' @noRd
mod_impact_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns




    # Get contamination model -------
    r_mod <- reactiveValues(
      contamination = NULL,
      dt_prob = NULL,
      bias = NULL
    )

    observeEvent({
      r$response_behavior
      r$target
      r$model_XY
    },{
      req(r$response_behavior)
      req(r$target)
      req(r$model_XY)

      r_mod$contamination <- careless(r$model_XY, type = r$response_behavior,
                                      target = r$target)

    })

    # Get bias -------

    observeEvent({
      r_mod$contamination
      r$model_XY
    },{
      req(r_mod$contamination)
      req(r$model_XY)

      epsilon = seq(from = 0, to = 0.5, by = 0.01)
      r_mod$bias <- bias(r$model_XY,
                             epsilon = epsilon,
                             contamination = r_mod$contamination)
    })






    # PMF Table output -------
    observeEvent({
      r_mod$contamination

    },{
      req(r_mod$contamination)

      r_mod$dt_prob <- r_mod$contamination$X$prob
      K <- length(r_mod$dt_prob)
      r_mod$dt_prob <- data.frame(t(r_mod$dt_prob))
      colnames(r_mod$dt_prob) <- 1:K
    }
    )

    output$df_prop <- DT::renderDT({

      req(r_mod$dt_prob)

      formatRound(
        datatable(
          r_mod$dt_prob, fillContainer = TRUE,
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
        columns = colnames(r_mod$dt_prob),
        digits = 2)
    },  server = FALSE)

    # Distribution plot output -------

    # output$plot_prop <- renderPlot({
    output$plot_prop <- renderPlotly({
      req(r_mod$dt_prob)

      df <- data.frame(K = colnames(r_mod$dt_prob), y = t(r_mod$dt_prob))

      p <- ggplot(data = df, aes(x = K, y = y)) +
        geom_bar(stat = "identity") +
        ylim(0, 1) +
        xlab("Response option") +
        ylab("Probability mass") +
        theme_minimal()

      ggplotly(p, tooltip = "text")


    })


    # Careless impact plot output -------
    output$measure_plot <- renderPlotly({
      req(r_mod$bias)
      req(r$measure)



      p <- bias_plot(
        r_mod$bias,
        type = r$measure,
        digits = 3
      )+
        theme_minimal()

      ggplotly(p, tooltip = "text")
    })




  })
}

## To be copied in the UI
# mod_impact_ui("impact_1")

## To be copied in the server
# mod_impact_server("impact_1")
