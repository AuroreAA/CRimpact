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
          # plotOutput(ns("plot_prop"))
          plotlyOutput(ns("plot_prop"))
        )
      ),

      card_header("Bias plot"),
      #plotOutput(ns("bias_plot"))
      plotlyOutput(ns("bias_plot"))
    ),
    p(),
    p()
  )
}

#' bias Server Functions
#'
#' @noRd
mod_bias_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent({
      r$type_cont_distribution_X
      r$K_X
      r$straightat_X},{
        validate(need(!is.null(r$K_X), "The number of categories should be >1."))
        r$prop_cont <- get_contam_distribution(type = r$type_cont_distribution_X,
                                               K = r$K_X,
                                               straightat = r$straightat_X)
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

    # Distribution plot output -------

    r_mod <- reactiveValues(
      dt = NULL
    )

    observeEvent({
      r$prop_cont
    },{
      r_mod$dt <- r$prop_cont
      K <- length(r_mod$dt)
      r_mod$dt <- data.frame(t(r_mod$dt))
      colnames(r_mod$dt) <- 1:K
    }
    )


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
    output$plot_prop <- renderPlotly({
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
# mod_bias_ui("bias_1")

## To be copied in the server
# mod_bias_server("bias_1")
