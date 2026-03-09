# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************



bias_new <- function(model, epsilon, contamination = "careless", ...) {

  # extract components from object for model distribution
  mu_X <- model$X$mean
  Sigma_X <- model$X$cov
  mu_Y <- model$Y$mean
  Sigma_Y <- model$Y$cov
  Sigma_XY <- model$cov_XY

  # if necessary, initialize object for contamination distribution
  if (is.character(contamination)) {
    contamination <- match.arg(
      contamination,
      choices = c("careless", "reversed", "adversarial")
    )
    if (contamination == "careless") contamination <- careless(model, ...)
    else if (contamination == "reversed") contamination <- reversed(model, ...)
    else contamination <- adversarial(model, ...)
  }

  # extract components of object for contamination distribution
  nu_X <- contamination$X$mean
  Psi_X <- contamination$X$cov
  nu_Y <- contamination$Y$mean
  Psi_Y <- contamination$Y$cov
  Psi_XY <- contamination$cov_XY

  # extract weight vectors
  a <- model$a
  b <- model$b

  # extract variances and covariance at model distribution F
  var_F_U <- model$U$var
  var_F_V <- model$V$var
  cov_F_UV <- model$cov_UV

  # calculate variances and covariance at contamination distribution H
  var_H_U <- drop(crossprod(a, Psi_X) %*% a)
  var_H_V <- drop(crossprod(b, Psi_Y) %*% b)
  cov_H_UV <- drop(crossprod(a, Psi_XY) %*% b)

  # calculate difference of means at distributions F and H
  diff_E_U <- model$U$mean - drop(crossprod(a, nu_X))
  diff_E_V <- model$V$mean - drop(crossprod(b, nu_Y))

  # calculate covariance and variances at contaminated distribution G
  var_G_U <- (1 - epsilon) * var_F_U + epsilon * var_H_U +
    epsilon * (1 - epsilon) * diff_E_U^2
  var_G_V <- (1 - epsilon) * var_F_V + epsilon * var_H_V +
    epsilon * (1 - epsilon) * diff_E_V^2
  cov_G_UV <- (1 - epsilon) * cov_F_UV + epsilon * cov_H_UV +
    epsilon * (1 - epsilon) * diff_E_U * diff_E_V

  # calculate correlation coefficient at contaminated distribution G
  rho_G <- cov_G_UV / (sqrt(var_G_U) * sqrt(var_G_V))

  # return results
  list(epsilon = epsilon, rho = model$rho, rho_contaminated = rho_G,
       bias = rho_G - model$rho)

}



#' Visualize bias curves at specific contaminating distribution
#'
#' bjklk
#'
#' @param bias hjk
#' @param digits digits Number of digits shown in the hover.
#' @param type hjk,
#'
#' @return A \code{ggplot} object
#' @export
#'
#' @importFrom scales percent
#'
#' @author Aurore Archimbaud
#'
#' @rawNamespace import(ggplot2, except = last_plot)
bias_plot_new <- function (bias, digits = 2, type = c("Correlation", "Bias"))
{
  # initializations
  type <- match.arg(type)
  eps <- bias$epsilon
  rho <- bias$rho

  if(type == "Correlation"){
    BS <- bias$rho_contaminated
  }else{
    BS <- bias$bias
  }

  if(!is.null(digits)){
    BS <- round(BS, digits)
  }

  gg <- ggplot(data = data.frame(bias = BS, contamination = eps),
               aes(y = bias, x = contamination, text = paste0(
                 "Contamination: ", scales::percent(contamination, accuracy = 1),
                 "<br>",
                 type, ": ", bias
               ),  group = 1)) +
    geom_line() +
    scale_x_continuous(labels = scales::percent)+
    xlab("Contamination fraction") + ylab(type)



  if(type == "Correlation"){
    gg <- gg +
      #geom_hline(yintercept = rho, linetype = "dashed", color = "lightcoral") +
      geom_line(data = data.frame(x = eps, y = rho),
                mapping = aes(x = eps, y = rho,
                              text = paste0(
                                "True correlation: ",
                                rep(scales::percent(rho, accuracy = 0.1),
                                    length(eps))
                              ), group = 1),
                linetype = "dashed", color = "lightcoral") +
      ylim(-1, 1)
  }
  if(type == "Bias"){
    y_breakdown <- seq(-(1+rho), 1-rho, 0.02)
    gg <- gg +
      # geom_line(data = data.frame(x = breakdown, y = y_breakdown),
      #           mapping = aes(x = x, y = y_breakdown,
      #                         text = paste0(
      #                           "Breakdown value: ",
      #                           rep(scales::percent(breakdown, accuracy = 0.1),
      #                               length(y_breakdown))
      #                         ), group = 1),
      #           linetype = "dashed", color = "lightcoral") +
      ylim(-(1+rho), 1-rho)
  }
  return(gg)
}


max_bias <- function(model, epsilon) {

  # extract relevant quantities at model distribution F
  E_F_U <- model$U$mean
  E_F_V <- model$V$mean
  var_F_U <- model$U$var
  var_F_V <- model$V$var
  cov_F_UV <- model$cov_UV

  # bounds of support
  M_U <- model$U$M
  M_V <- model$V$M

  # collect quantities that only depend on model distribution F
  m_F_U <- (1 - epsilon) * var_F_U + epsilon * (1 - epsilon) * E_F_U^2
  m_F_V <- (1 - epsilon) * var_F_V + epsilon * (1 - epsilon) * E_F_V^2
  m_F_UV <- (1 - epsilon) * cov_F_UV + epsilon * (1 - epsilon) * E_F_U * E_F_V

  # calculate relevant quantities for maximum upward and downward bias
  rho_upward <- (m_F_UV + epsilon * M_U * M_V) /
    (sqrt(m_F_U + epsilon * M_U^2) * sqrt(m_F_V + epsilon * M_V^2))
  rho_downward <- (m_F_UV - epsilon * M_U * M_V) /
    (sqrt(m_F_U + epsilon * M_U^2) * sqrt(m_F_V + epsilon * M_V^2))

  # return results
  list(epsilon = epsilon, rho = model$rho, rho_upward = rho_upward,
       bias_upward = rho_upward - model$rho, rho_downward = rho_downward,
       bias_downward = rho_downward - model$rho)

}



breakdown <- function(model) {

  # extract relevant quantities at model distribution F
  E_F_U <- model$U$mean
  E_F_V <- model$V$mean
  var_F_U <- model$U$var
  var_F_V <- model$V$var
  cov_F_UV <- model$cov_UV

  # # check if the model distribution F is valid for breakdown value calculations
  # # (true correlation of 1)
  # # Update: Actually, we define the breakdown value more generally.
  # rho_F <- cov_F_UV / (sqrt(var_F_U) * sqrt(var_F_V))
  # if (rho_F < 1 - sqrt(.Machine$double.eps)) {
  #   stop("Invalid model distribution for breakdown value calculation.\nThe ",
  #        "true correlation of the scores must be 1 (here it is ", rho_F,").")
  # }

  # bounds of support
  M_U <- model$U$M
  M_V <- model$V$M

  # calculate breakdown value
  prod_E_F <- E_F_U * E_F_U
  prod_M <- M_U * M_V
  # TODO: use better names
  tmp <- prod_E_F - cov_F_UV - prod_M
  sqrt_tmp <- sqrt(tmp^2 + 4 * prod_E_F * cov_F_UV)
  if (prod_E_F > 0) {
    (tmp + sqrt_tmp) / (2 * prod_E_F)
  } else if (prod_E_F < 0) {
    (tmp - sqrt_tmp) / (2 * prod_E_F)
  } else {
    cov_F_UV / (cov_F_UV + prod_M)
  }

}

max_bias_plot_new <- function(maxbias, digits = 2,
                              type = c("Correlation", "Bias"),
                              direction = c("Upward", "Downward", "Maximum"),
                              breakdown){

  # initializations
  type <- match.arg(type)
  direction <- match.arg(direction)
  eps <- maxbias$epsilon
  rho <- maxbias$rho
  # B <- numeric()
  # txt <- character()
  type_name <- ifelse(type == "Correlation", "rho", "bias")
  direction <- ifelse(direction == "Maximum", ifelse(rho < 0, "Upward", "Downward"),
                      direction)

  direction <- tolower(direction)
  B <- maxbias[[paste(type_name, direction, sep = "_")]]
  txt <- paste("Maximum", direction,  tolower(type))

  if(!is.null(digits)){
    B <- round(B, digits)
  }

  gg <- ggplot(data = data.frame(maxbias = B, contamination = eps),
               mapping = aes(y = maxbias, x = contamination,
                             text = paste0(
                               "Contamination: ", scales::percent(contamination, accuracy = 1),
                               "<br>",
                               type, ": ",  maxbias
                             ),  group = 1)) +
    geom_line() +
    scale_x_continuous(labels = scales::percent) +
    xlab("Contamination fraction") + ylab(txt)

  if(type == "Correlation"){
    gg <- gg +
      #geom_hline(yintercept = rho, linetype = "dashed", color = "lightcoral") +
      geom_line(data = data.frame(x = eps, y = rho),
                mapping = aes(x = eps, y = rho,
                              text = paste0(
                                "True correlation: ",
                                rep(scales::percent(rho, accuracy = 0.1),
                                    length(eps))
                              ), group = 1),
                linetype = "dashed", color = "lightcoral") +

      ylim(-1, 1)
  }
  if(type == "Bias"){
    # geom_vline(xintercept = breakdown,
    #
    #            linetype = "dashed", color = "lightcoral") +
    y_breakdown <- seq(-(1+rho), 1-rho, 0.02)
    gg <- gg +
      geom_line(data = data.frame(x = breakdown, y = y_breakdown),
                mapping = aes(x = x, y = y_breakdown,
                              text = paste0(
                                "Breakdown value: ",
                                rep(scales::percent(breakdown, accuracy = 0.1),
                                    length(y_breakdown))
                              ), group = 1),
                linetype = "dashed", color = "lightcoral")

  }
  return(gg)
}
