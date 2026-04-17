#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import DT
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  # Initialisation of reactive values
  r <- reactiveValues(
    # Scale X
    J_X = NULL,
    K_X = NULL,
    type_true_distribution_X = NULL,
    rhoscale_X = NULL,
    prob_X = NULL,
    shift_X = NULL,
    sd_X = NULL,
    skewness_X = NULL,

    scale_X = NULL,

    # Scale Y
    J_Y = NULL,
    K_Y = NULL,
    type_true_distribution_Y = NULL,
    rhoscale_Y = NULL,
    prob_Y = NULL,
    shift_Y = NULL,
    sd_Y = NULL,
    skewness_Y = NULL,

    scale_Y = NULL,

    # Bias
    rho_XY = NULL,
    type_cont_distribution_X = NULL,
    type_cont_distribution_Y = NULL,
    prob_cont = NULL,
    straightat_X = 1,
    straightat_Y = 1,
    sameXY = 0,

    # Impact
    response_behavior = NULL,
    target = NULL,
    direction = NULL,
    model_XY = NULL,
    measure = NULL,
    max_bias = NULL,
    breakdown = NULL
  )

  # Distribution
  mod_sidebar_distri_server("sidebar_distri_1", r)

  # Scale
  mod_sidebar_scale_server("sidebar_scale_X", tab_name = "X", r)
  mod_sidebar_scale_server("sidebar_scale_Y", tab_name = "Y", r)
  mod_scale_server("scale_X", tab_name = "X", r)
  mod_scale_server("scale_Y", tab_name = "Y", r)


  # Adversarial impact
  mod_sidebar_adversarial_server("sidebar_adversarial_1", r)
  mod_adversarial_server("adversarial_1", r)

  # Careless impact
  mod_sidebar_impact_server("sidebar_impact_1", r)
  mod_impact_server("impact_1", r)


  # Info
  mod_sidebar_info_server("sidebar_info_1")
  mod_info_server("info_1")

  observeEvent(r$sameXY,{
    if(r$sameXY > 0){
      nav_select(id = "tabs_scales", selected = "Scale Y", session = session)
    }

  })
}
