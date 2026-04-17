#' distri UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_distri_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h5("Instructions"),
    tags$ol(
      tags$li("Define the response distributions for", tags$a("scales X and Y",
                                                              href = "#", onclick = "$('a[data-value=\"Scales\"]').click();"), "."),
      tags$li("Go to the",  tags$a("Impact tab", href = "#", onclick = "$('a[data-value=\"Impact\"]').click();"),
              " to measure the impact of careless responding.")

    ),

    tags$h5("Distribution Types"),
    p("Scales X and Y must contain the same number of answer categories."),
    tags$ul(
      tags$li(
        tags$b("Predefined distributions:"),
        tags$ul(
          tags$li(tags$i("centered:"), " Responses cluster around neutral categories."),
          tags$li(tags$i("skewed:"), " Responses lean toward agreement."),
          tags$li(tags$i("polarized:"), " Responses concentrate at both extremes, indicating strong divergent opinions.")
        )
      ),
      tags$li(tags$b("Latent:"), " Define your own distribution using parameters in ",

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
    ),
    tags$h5("Plots"),
    p("The plots are interactive, and display options are available in the top-right corner of each graph:"),
    tags$ul(
      tags$li(tags$b("Autoscale:"), "Click to automatically zoom in and fit the data to the view."),
      tags$li(tags$b("Reset axes:"), "Click to zoom out and restore the original axis ranges.")
    )
  )
}

#' distri Server Functions
#'
#' @noRd
mod_distri_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_distri_ui("distri_1")

## To be copied in the server
# mod_distri_server("distri_1")
