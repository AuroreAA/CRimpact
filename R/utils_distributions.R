# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


# utility function for marginal distribution
get_marginal <- function(distribution = c("uniform", "centered", "skewed",
                                          "polarized", "latent"),
                         K = 5L, target = c("all", "extreme"),
                         mean = 0, sd = 1, skew = 0, session) {

  # initializations
  distribution <- match.arg(distribution)
  if (isTRUE(K >= 2)) K <- as.integer(K)
  else stop("number of categories 'K' must be >= 2")

  # obtain marginal probabilities
  if (distribution == "uniform") {
    # uniform distribution on the extreme categories only or all categories
    target <- match.arg(target)
    prob <- switch(target,
                   all = rep.int(1 / K, times = K),
                   extreme = c(0.5, rep.int(0, times = K - 2), 0.5))
  } else {
    # based on a latent skew-normal distribution
    # (with special cases for predefined parameter settings as in paper)
    if (distribution == "centered") {
      mean <- 0
      sd <- 0.9
      skew <- 0
    } else if (distribution == "skewed") {
      mean <- 0
      sd <- 1.1
      skew <- -0.95
    } else if (distribution == "polarized") {
      mean <- 1.6
      sd <- 1
      skew <- 0
    }

    if((skew > 0.95 | skew < -0.95) & (identical(distribution, "latent"))){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Warning",
        text = "Assymetry value must be between -0.95 and 0.95.",
        type = "warning",
        html = FALSE
      )

      validate(need(skew <= 0.95 & skew >= -0.95,
                    "Assymetry value must be between -0.95 and 0.95."))
    }

    # obtain probabilities of discrete rating scale using the procedure of
    # Boari & Nai Ruscone (2015)
    parameters <- c(mu = mean, sd = sd, skew = skew)
    prob <- unname(latent2likert::simulate_likert(K, cp = parameters))
    # the polarized distribution is computed as a mixture with two components
    if (distribution == "polarized") prob <- 0.5 * prob + 0.5 * rev(prob)
  }

  # return marginal probabilities
  prob

}


# obtain relevant quantities for the model distribution from a data set
# get_model <- function(X, Y, cat_X = NULL, cat_Y = cat_X,
#                       a = rep.int(1/p, p), b = rep.int(1/q, q)) {
#
#   # initializations
#   X <- as.matrix(X)
#   Y <- as.matrix(Y)
#   n <- nrow(X)
#   if (nrow(Y) != n) {
#     stop("'X' and 'Y' must contain the same number of observations")
#   }
#   if (n < 2) {
#     stop("'X' and 'Y' contain too few observations for computing variances ",
#          "and covariances")
#   }
#   p <- ncol(X)
#   if (p == 0) stop("'X' does not contain any items")
#   q <- ncol(Y)
#   if (q == 0) stop("'Y' does not contain any items")
#   if (length(a) != p) stop("weight vector 'a' must be of length ", p)
#   if (length(b) != q) stop("weight vector 'b' must be of length ", q)
#
#   # number of rating categories (may differ for X and Y)
#   if (is.null(cat_X)) observed_cat_X <- sort(unique(as.vector(X)))
#   else observed_cat_X <- cat_X
#   K_X <- length(observed_cat_X)
#   if (is.null(cat_Y)) observed_cat_Y <- sort(unique(as.vector(Y)))
#   else observed_cat_Y <- cat_Y
#   K_Y <- length(observed_cat_Y)
#
#   # we use a centered scale of subsequent integers
#   cat_X <- seq_len(K_X) - (1 + K_X) / 2
#   cat_Y <- seq_len(K_Y) - (1 + K_Y) / 2
#
#   # bound of support
#   M_X <- cat_X[K_X]
#   M_Y <- cat_Y[K_Y]
#
#   # keys of items are taken as the signs of the weight vectors
#   keys_X <- sign(a)
#   keys_Y <- sign(b)
#
#   # # make sure that X and Y are on a centered scale of subsequent integers
#   # # TODO: there should be an argument whether this should be enforced
#   # # (for the big5 application, we should not do this due to some non-integer
#   # # values in the data, likely from imputation)
#   # if (!identical(observed_cat_X, cat_X)) {
#   #   names(cat_X) <- observed_cat_X
#   #   X <- matrix(cat_X[as.character(X)], nrow = n, ncol = p)
#   #   names(cat_X) <- NULL
#   # }
#   # if (!identical(observed_cat_Y, cat_Y)) {
#   #   names(cat_Y) <- observed_cat_Y
#   #   Y <- matrix(cat_Y[as.character(Y)], nrow = n, ncol = q)
#   #   names(cat_Y) <- NULL
#   # }
#
#   # compute means and covariance matrices to be used as true values
#   mean_X <- colMeans(X)
#   mean_Y <- colMeans(Y)
#   cov_X <- cov(X)
#   cov_Y <- cov(Y)
#   cov_XY <- cov(X, Y)
#
#   # bounds of support of linear combinations
#   M_U <- sum(abs(a)) * M_X
#   M_V <- sum(abs(b)) * M_Y
#
#   # compute quantities of linear combinations to be used as true values
#   mean_U <- drop(crossprod(a, mean_X))
#   mean_V <- drop(crossprod(b, mean_Y))
#   var_U <- drop(crossprod(a, cov_X) %*% a)
#   var_V <- drop(crossprod(b, cov_Y) %*% b)
#   cov_UV <- drop(crossprod(a, cov_XY) %*% b)
#
#   # return list of relevant quantities of model distribution
#   list(
#     X = list(cat = cat_X, K = K_X, M = M_X, keys = keys_X,
#              mean = mean_X, cov = cov_X),
#     Y = list(cat = cat_Y, K = K_Y, M = M_Y, keys = keys_Y,
#              mean = mean_Y, cov = cov_Y),
#     cov_XY = cov_XY, a = a, b = b,
#     U = list(M = M_U, mean = mean_U, var = var_U),
#     V = list(M = M_V, mean = mean_V, var = var_V),
#     cov_UV = cov_UV,
#     rho = cov_UV / (sqrt(var_U) * sqrt(var_V))
#   )
#
# }


# obtain relevant quantities of a simplified scale in which all items have the
# same marginal distribution and within-scale correlation (an alternative view
# is that we control the weighted average within-scale correlation)
simple_scale <- function(prob = c("centered", "skewed", "polarized", "latent"),
                         K = 5, length = 5, rho_w = 0.5,
                         w = rep.int(1/length, length),
                         ...) {

  # initializations
  if (is.character(prob)) prob <- match.arg(prob)
  if (K < 2) stop("number of categories 'K' must be >= 2")
  if (length < 1) stop("scale must contain at least one item")
  if (length > 1 && (rho_w < 0 || rho_w > 1)) {
    stop("correlation 'rho_w' must be between 0 and 1")
  }
  if (length(w) != length) stop("weight vector 'w' must be of length ", length)

  # obtain marginal distribution
  if (is.character(prob)) {
    force_zero_mean <- prob %in% c("centered", "polarized")
    prob <- get_marginal(prob, K = K, ...)
  } else {
    prob <- as.numeric(prob)
    K <- length(prob)
    force_zero_mean <- FALSE
  }

  # make sure that marginal probabilities sum up to 1
  prob <- prob / sum(prob)

  # we use a centered scale of subsequent integers
  cat <- seq_len(K) - (1 + K) / 2

  # bound of support
  M <- cat[K]

  # keys of items are taken as the signs of the weight vectors
  keys <- sign(w)

  # obtain means
  m <- if (force_zero_mean) 0 else sum(prob * cat)
  mean <- rep.int(m, times = length) * keys

  # obtain covariance structure
  v <- sum(prob * (cat - m)^2)
  cov <- matrix(rho_w * v, nrow = length, ncol = length) * tcrossprod(keys, keys)
  diag(cov) <- v

  # bounds of support of linear combination
  M_w <- sum(abs(w)) * M

  # compute mean and variance of linear combination
  mean_w <- drop(crossprod(w, mean))
  var_w <- drop(crossprod(w, cov) %*% w)

  # Note that there is another way to interpret rho_w that doesn't require all
  # within-scale correlations to be equal (subject to sign). That is, we can
  # interpret it as the weighted average correlation between items. In this
  # case, we can compute the variance of the linear combination as follows:
  # w_pairs <- combn(abs(w), 2)
  # sum_w <- sum(w_pairs[1, ] * w_pairs[2, ])
  # var_w <- v * (sum(w^2) + 2 * rho_w * sum_w)
  # Of course, under the assumption that all within-scale correlations are
  # equal (subject to sign), we get the same result.

  # return list of relevant quantities of scale
  list(
    scale = list(cat = cat, prob = prob, K = K, M = M, keys = keys,
                 mean = mean, cov = cov),
    w = w,
    score = list(M = M_w, mean = mean_w, var = var_w)
  )

}


# obtain relevant quantities for a simplified model distribution
# (as used in the paper)
simple_model <- function(X = simple_scale(...), Y = X, rho = 0, ...) {

  # initializations
  U <- X$score
  a <- X$w
  V <- Y$score
  b <- Y$w
  X <- X$scale
  p <- get_length(X)
  if (p == 0) stop("'X' does not contain any items")
  else if (p == 1) v_X <- drop(X$cov)
  else if (p > 1) {
    var_X <- diag(X$cov)
    v_X <- var_X[1]
    if (!isTRUE(all.equal(rep.int(v_X, p-1), var_X[-1]))) {
      stop("for this simple model, all items in 'X' must have equal variance")
    }
  }
  Y <- Y$scale
  q <- get_length(Y)
  if (q == 0) stop("'Y' does not contain any items")
  else if (q == 1) v_Y <- drop(Y$cov)
  else if (q > 1) {
    var_Y <- diag(Y$cov)
    v_Y <- var_Y[1]
    if (!isTRUE(all.equal(rep.int(v_Y, q-1), var_Y[-1]))) {
      stop("for this simple model, all items in 'Y' must have equal variance")
    }
  }

  # compute between-scale correlation that results in desired correlation
  # between mean scores (note that the standard deviations of U and V are
  # identical, as are those of the individual X's and Y's)
  rho_XY <- rho * sqrt(U$var / v_X) * sqrt(V$var / v_Y)

  # obtain covariance structure between scales
  cov_XY <- matrix(rho_XY * sqrt(v_X) * sqrt(v_Y), nrow = p, ncol = q) *
    tcrossprod(X$keys, Y$keys)

  # compute covariance of linear combinations
  cov_UV <- drop(crossprod(a, cov_XY) %*% b)

  # Note that there is another way to interpret rho_XY that doesn't require all
  # between-scale correlations to be equal (subject to sign). That is, we can
  # interpret it as the weighted average correlation between items. In this
  # case, we can compute the covariance of the linear combinations as follows:
  # cov_UV <- sqrt(v_X) * sqrt(v_Y) * rho_XY * sum(tcrossprod(abs(a), abs(b)))
  # Of course, under the assumption that all between-scale correlations are
  # equal (subject to sign), we get the same result.

  # # check that we indeed obtain the correct target correlation
  # all.equal(rho, cov_UV / (sqrt(U$var) * sqrt(V$var)))

  # return list of relevant quantities of model distribution
  list(X = X, Y = Y, cov_XY = cov_XY, a = a, b = b, U = U, V = V,
       cov_UV = cov_UV, rho = rho)

}


# wrapper function for a simplified model where Y is an identical copy of X
# (to be used for breakdown value calculations)
identical_model <- function(X = simple_scale(...), ...) {
  simple_model(X = X, Y = X, rho = 1)
}


# internal function to get number of variables in a scale
get_length <- function(scale) length(scale$mean)


# obtain relevant quantities for contamination distributions corresponding
# to certain careless responding behavior
careless <- function(model, type = c("straightlining", "random"),
                     target = c("extreme", "all")) {

  # initializations
  type <- match.arg(type)
  target <- match.arg(target)
  K_X <- model$X$K
  K_Y <- model$Y$K
  if (type == "straightlining" && K_X != K_Y) {
    stop("for straightlining, number of categories 'K_X' and 'K_Y' must be equal")
  }
  p <- get_length(model$X)
  q <- get_length(model$Y)

  # we use a centered scale of subsequent integers
  cat_X <- seq_len(K_X) - (1 + K_X) / 2
  cat_Y <- seq_len(K_Y) - (1 + K_Y) / 2

  # obtain marginal probabilities
  prob_X <- get_marginal("uniform", K = K_X, target = target)
  prob_Y <- get_marginal("uniform", K = K_Y, target = target)

  # compute means
  # (For all careless responding distributions considered here, the mean is
  # zero. Actually computing the means may result in small nonzero values.)
  # m_X <- sum(prob_X * cat_X)
  m_X <- 0
  mean_X <- rep.int(m_X, times = p)
  # m_Y <- sum(prob_Y * cat_Y)
  m_Y <- 0
  mean_Y <- rep.int(m_Y, times = q)

  # obtain covariance structure
  v_X <- sum(prob_X * (cat_X - m_X)^2)
  if (type == "straightlining") {
    # for straightlining, we only consider the case where the number of
    # categories in X and Y is equal
    cov_X <- matrix(v_X, nrow = p, ncol = p)
    cov_Y <- matrix(v_X, nrow = q, ncol = q)
    cov_XY <- matrix(v_X, nrow = p, ncol = q)
  } else if (type == "random") {
    v_Y <- sum(prob_Y * (cat_Y - m_Y)^2)
    cov_X <- diag(v_X, nrow = p, ncol = p)
    cov_Y <- diag(v_Y, nrow = q, ncol = q)
    cov_XY <- matrix(0, nrow = p, ncol = q)
  }

  # compute means and variances of linear combinations
  a <- model$a
  b <- model$b
  mean_U <- drop(crossprod(a, mean_X))
  mean_V <- drop(crossprod(b, mean_Y))
  var_U <- drop(crossprod(a, cov_X) %*% a)
  var_V <- drop(crossprod(b, cov_Y) %*% b)
  cov_UV <- drop(crossprod(a, cov_XY) %*% b)

  # return list of relevant quantities of contamination distribution
  list(
    X = list(prob = prob_X, mean = mean_X, cov = cov_X),
    Y = list(prob = prob_Y, mean = mean_Y, cov = cov_Y),
    cov_XY = cov_XY
  )

}


# obtain relevant quantities for adversarial contamination resulting in
# maximum upward or downward bias
adversarial <- function(model, direction = c("upward", "downward")) {

  # initializations
  direction <- match.arg(direction)

  # extract components from object for model distribution
  M_X <- model$X$M
  p <- get_length(model$X)
  M_Y <- model$Y$M
  q <- get_length(model$Y)

  # extract signs of coefficient vectors
  sign_a <- sign(a)
  sign_b <- sign(b)

  # bias is maximized when the means are 0 and the variances are given by the
  # square of the bound, and the covariances are given by the product of bounds
  # with the appropriate sign
  mean_X <- rep.int(0, times = p)
  cov_X <- tcrossprod(sign_a, sign_a) * M_X^2
  mean_Y <- rep.int(0, times = p)
  cov_Y <- tcrossprod(sign_b, sign_b) * M_Y^2
  cov_XY <- switch(direction,
                   upward = tcrossprod(sign_a, sign_b) * M_X * M_Y,
                   downward = -tcrossprod(sign_a, sign_b) * M_X * M_Y)

  # return list of relevant quantities of contamination distribution
  list(
    X = list(mean = mean_X, cov = cov_X),
    Y = list(mean = mean_Y, cov = cov_Y),
    cov_XY = cov_XY
  )

}


# obtain contamination distribution by reversing scale in model distribution
reversed <- function(model, which = c("X", "Y", "both")) {

  # initializations
  which <- match.arg(which)
  reverse_X <- which %in% c("X", "both")
  reverse_Y <- which %in% c("Y", "both")

  # obtain relevant quantities by reversing scale where requested
  mean_X <- if (reverse_X) -model$X$mean else model$X$mean
  mean_Y <- if (reverse_Y) -model$Y$mean else model$Y$mean
  cov_XY <- if (xor(reverse_X, reverse_Y)) -model$cov_XY else model$cov_XY

  # return list of relevant quantities under reversed scale where requested
  list(
    X = list(mean = mean_X, cov = model$X$cov),
    Y = list(mean = mean_Y, cov = model$Y$cov),
    cov_XY = cov_XY
  )

}
