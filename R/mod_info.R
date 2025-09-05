#' info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      full_screen = TRUE,
      max_height = 400,
      # information on software versions
      h3("Version"),
      uiOutput(ns("version_info")),

      # citation information
      h3("Citation"),
      uiOutput(ns("citation_info"))
    )

  )
}

#' info Server Functions
#'
#' @noRd
mod_info_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # render information on software versions
    R_version <- paste(R.Version()[c("major", "minor")], collapse = ".")
    CRimpact_version <- toString(packageVersion("CRimpact"))
    references <- get_references("HTML")


    output$version_info <- renderUI({
      p("You are using", strong("R"), paste0("version ", R_version, ", package"),
        strong("CRimpact"), paste0("version ", CRimpact_version, "."))
    })

    # render citation information
    output$citation_info <- renderUI({
      tagList(
        # p("To cite the method, please use:"),
        # references$ROBMED,
        p("To cite our software, please use:"),
        references$RATINGBIAS,
        # references$ratingbias,
        references$CRimpact
      )
    })
  })
}

## To be copied in the UI
# mod_info_ui("info_1")

## To be copied in the server
# mod_info_server("info_1")
