#' sidebar_info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      full_screen = FALSE,
      max_height = 400,
      selectInput(ns("citation_format"), "Citation format",
                  choices = c("EndNote", "BibTeX"),
                  selected = "EndNote", multiple = FALSE),
      downloadButton(ns("download_references"), "Download references")
    )

  )
}

#' sidebar_info Server Functions
#'
#' @noRd
mod_sidebar_info_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # download references in selected format
    output$download_references <- downloadHandler(

      filename = function() {
        extension <- switch(input$citation_format,
                            "EndNote" = "enw",
                            "BibTeX" = "bib")
        paste("RATINGBIAS", extension, sep = ".")
      },
      content = function(file) {

        # write lines to file of selected reference format
        references <- get_references(input$citation_format)
        writeLines(references, con = file)
      },
      contentType = "text/plain"
    )
  })
}

## To be copied in the UI
# mod_sidebar_info_ui("sidebar_info_1")

## To be copied in the server
# mod_sidebar_info_server("sidebar_info_1")
