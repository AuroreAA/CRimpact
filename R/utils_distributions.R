# ************************************
# Author: Andreas Alfons
#         Erasmus University Rotterdam
# ************************************


#' Marginal response distribution
#'
#' Generate the probabilities of a marginal response distribution.
#'
#' @param distribution  type of marginal response distribution. The choices
#' `"uniform"` (the default), `"centered"`, `"skewed"`, and `"polarized"` allow
#' to quickly generate marginal response probabilities with the corresponding
#' property. If `"latent"`, the marginal response probabilities are generated
#' based on a latent skew-normal distribution via function
#' \code{\link[latent2likert]{simulate_likert}()}.
#' @param K  number of response categories.
#' @param target  if `distribution = "uniform"`, the target response categories
#' from which to choose uniformly. Possible values are `"all"` (the default) to
#' choose from all response categories, and `"extreme"` to choose only from the
#' extreme response categories.
#' @param mean  if `distribution = "latent"`, the mean of the latent
#' skew-normal distribution (defaults to 0).
#' @param sd  if `distribution = "latent"`, the standard deviation of the
#' latent skew-normal distribution (defaults to 1).
#' @param skew  if `distribution = "latent"`, the skweness of the latent
#' skew-normal distribution (defaults to 0, must be between -0.95 and 0.95).
#'
#' @return A vector of probabilities for the specified marginal response
#' distribution.
#'
#' @author Andreas Alfons
#'
#' @references
#' M. Welz, A. Archimbaud, A. Alfons (2026). Quantifying the impact of careless
#' responding. PsyArXiv. \doi{10.31234/osf.io/8fj6p_v2}.
#'
#' @seealso [simple_scale()], [careless()]
#'
#' @examples
#' # uniform distribution
#' get_marginal("uniform", K = 5, target = "all")
#' get_marginal("uniform", K = 5, target = "extreme")
#'
#' # centered distribution
#' get_marginal("centered", K = 9)
#' # this is actually equivalent to:
#' get_marginal("latent", K = 9, mean = 0, sd = 0.9, skew = 0)
#'
#' # skewed distribution
#' get_marginal("skewed", K = 9,)
#' # this is actually equivalent to:
#' get_marginal("latent", K = 9, mean = 0, sd = 1.1, skew = -0.95)
#'
#' # polarized distribution
#' get_marginal("polarized", K = 9)
#'
#' @importFrom latent2likert simulate_likert
#' @export

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


#' Model distribution based on empirical data
#'
#' Obtain relevant quantities for the model distribution from data on ordinal
#' measurement scales. These quantities include means, within-scale covariance
#' matrices, and between-scale cross-covariance matrices, as well as analogous
#' quantities for specified linear combinations of items.
#'
#' @param X  matrix of observations on an ordinal measurement scale consisting
#' of `p` items.
#' @param Y  matrix of observations on an ordinal measurement scale consisting
#' of `q` items.
#' @param cat_X  vector of response categories in the ordinal measurement scale
#' from `X`.  If `NULL` (the default), the unique observed values in `X` are
#' used.
#' @param cat_Y  vector of response categories in the ordinal measurement scale
#' from `Y`.  If `NULL` (the default), the unique observed values in `Y` are
#' used.
#' @param a  weight vector for computing a linear combination of items in the
#' ordinal measurement scale from `X`.
#' @param b  weight vector for computing a linear combination of items in the
#' ordinal measurement scale from `Y`.
#'
#' @return A list with the following components:
#' \item{X, Y}{each is a list with the following components detailing the
#' distribution of the ordinal measurement scale:
#' \itemize{
#'   \item{`cat`: vector of response categories, encoded as `-M`, `-M+1`,
#'   \dots, `M-1`, `M`.}
#'   \item{`K`: number of response categories.}
#'   \item{`M`: upper bound of the support region of the items, with the
#'   lower bound given by `-M`.}
#'   \item{`keys`: vector indicating whether items are positively keyed (`1`)
#'   or negatively keyed (`-1`). Its elements are simply derived as the signs
#'   of the weight vector of the linear combination.}
#'   \item{`mean`: vector of means of the items.}
#'   \item{`cov`: covariance matrix of the items.}
#' }
#' }
#' \item{cov_XY}{cross-covariance matrix of the measurement scales.}
#' \item{a}{weight vector for the linear combination `U` of items in the
#' ordinal measurement scale from `X`.}
#' \item{b}{weight vector for the linear combination `V` of items in the
#' ordinal measurement scale from `Y`.}
#' \item{U, V}{each is a list with the following components detailing the
#' distribution of the linear combination of the ordinal measurement scale:
#' \itemize{
#'   \item{`M`: upper bound of the support region of the linear combination,
#'   with the lower bound given by `-M`.}
#'   \item{`mean`: mean of the linear combination.}
#'   \item{`var`: variance of the linear combination.}
#' }
#' }
#' \item{cov_UV}{covariance between the linear combinations of the ordinal
#' measurement scales.}
#' \item{rho}{correlation between the linear combinations of the ordinal
#' measurement scales.}
#'
#' @author Andreas Alfons
#'
#' @references
#' M. Welz, A. Archimbaud, A. Alfons (2026). Quantifying the impact of careless
#' responding. PsyArXiv. \doi{10.31234/osf.io/8fj6p_v2}.
#'
#' @seealso
#' [careless()], [reversed()], [adversarial()]
#'
#' [bias()], [max_bias()], [breakdown()]
#'
#'
#' @examples
#' # Neo PI-R measurement of Big Five personality traits
#' if (requireNamespace("qgraph", quietly = TRUE)) {
#' data("big5", package = "qgraph")
#' data("big5groups", package = "qgraph")
#'
#' # keys of items as per Neo PI-R form
#' keys <- c(
#'   -1,  1,  1, -1,  1,  1, -1, -1,  1, -1,
#'   -1,  1,  1, -1,  1,  1, -1, -1,  1, -1,
#'   -1,  1,  1, -1,  1,  1, -1, -1,  1, -1,
#'   1, -1, -1,  1,  1, -1,  1,  1, -1,  1,
#'   1, -1, -1,  1, -1, -1,  1,  1, -1,  1,
#'   1, -1, -1,  1, -1, -1,  1,  1, -1,  1,
#'   -1,  1,  1, -1,  1,  1, -1, -1,  1, -1,
#'   -1,  1,  1, -1,  1,  1, -1, -1,  1, -1,
#'   -1,  1,  1, -1,  1,  1, -1, -1,  1, -1,
#'   1, -1, -1,  1, -1, -1,  1,  1, -1,  1,
#'   1, -1, -1,  1, -1, -1,  1,  1, -1,  1,
#'   1, -1, -1,  1, -1, -1,  1,  1, -1,  1,
#'   -1,  1,  1, -1,  1,  1, -1, -1,  1, -1,
#'   1,  1,  1, -1,  1,  1, -1, -1,  1, -1,
#'   -1,  1,  1, -1,  1,  1, -1, -1,  1, -1,
#'   1,  1, -1,  1, -1, -1,  1,  1, -1,  1,
#'   1, -1, -1,  1,  1, -1,  1,  1, -1,  1,
#'   1,  1, -1,  1, -1, -1,  1,  1,  1,  1,
#'   -1,  1, -1,  1,  1,  1, -1,  1, -1, -1,
#'   1,  1,  1,  1,  1,  1,  1, -1, -1,  1,
#'   1,  1,  1,  1, -1, -1, -1, -1,  1,  1,
#'   1,  1, -1,  1,  1,  1,  1,  1, -1, -1,
#'   1, -1,  1,  1,  1,  1,  1, -1, -1,  1,
#'   -1,  1,  1, -1,  1, -1,  1, -1,  1,  1
#' )
#'
#' # extract scales and weight vectors for
#' # Extraversion and Conscientiousness
#' X <- big5[, big5groups$Extraversion]
#' Y <- big5[, big5groups$Conscientiousness]
#' a <- keys[big5groups$Extraversion] / ncol(X)
#' b <- keys[big5groups$Conscientiousness] / ncol(Y)
#' cat <- min(big5):max(big5)
#'
#' # obtain model distribution from empirical data
#' model_XY <- get_model(X, Y, cat_X = cat, cat_Y = cat,
#'                       a = a, b = b)
#'
#' # contamination fractions
#' eps <- c(0, 0.1, 0.2, 0.3, 0.4, 0.49)
#'
#' # bias under straightlining at extreme response categories
#' bias(model_XY, epsilon = eps, contamination = "careless",
#'      type = "straightlining", target = "extreme")
#' }
#'
#' @importFrom stats cov
#' @export

get_model <- function(X, Y, cat_X = NULL, cat_Y = NULL,
                      a = rep.int(1/p, p), b = rep.int(1/q, q)) {

  # initializations
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  n <- nrow(X)
  if (nrow(Y) != n) {
    stop("'X' and 'Y' must contain the same number of observations")
  }
  if (n < 2) {
    stop("'X' and 'Y' contain too few observations for computing variances ",
         "and covariances")
  }
  p <- ncol(X)
  if (p == 0) stop("'X' does not contain any items")
  q <- ncol(Y)
  if (q == 0) stop("'Y' does not contain any items")
  if (length(a) != p) stop("weight vector 'a' must be of length ", p)
  if (length(b) != q) stop("weight vector 'b' must be of length ", q)

  # number of rating categories (may differ for X and Y)
  if (is.null(cat_X)) observed_cat_X <- sort(unique(as.vector(X)))
  else observed_cat_X <- cat_X
  K_X <- length(observed_cat_X)
  if (is.null(cat_Y)) observed_cat_Y <- sort(unique(as.vector(Y)))
  else observed_cat_Y <- cat_Y
  K_Y <- length(observed_cat_Y)

  # we use a centered scale of subsequent integers
  cat_X <- seq_len(K_X) - (1 + K_X) / 2
  cat_Y <- seq_len(K_Y) - (1 + K_Y) / 2

  # bound of support
  M_X <- cat_X[K_X]
  M_Y <- cat_Y[K_Y]

  # keys of items are taken as the signs of the weight vectors
  keys_X <- sign(a)
  keys_Y <- sign(b)

  # # make sure that X and Y are on a centered scale of subsequent integers
  # # TODO: there should be an argument whether this should be enforced
  # # (for the big5 application, we should not do this due to some non-integer
  # # values in the data, likely from imputation)
  # if (!identical(observed_cat_X, cat_X)) {
  #   names(cat_X) <- observed_cat_X
  #   X <- matrix(cat_X[as.character(X)], nrow = n, ncol = p)
  #   names(cat_X) <- NULL
  # }
  # if (!identical(observed_cat_Y, cat_Y)) {
  #   names(cat_Y) <- observed_cat_Y
  #   Y <- matrix(cat_Y[as.character(Y)], nrow = n, ncol = q)
  #   names(cat_Y) <- NULL
  # }

  # compute means and covariance matrices to be used as true values
  mean_X <- colMeans(X)
  mean_Y <- colMeans(Y)
  cov_X <- cov(X)
  cov_Y <- cov(Y)
  cov_XY <- cov(X, Y)

  # bounds of support of linear combinations
  M_U <- sum(abs(a)) * M_X
  M_V <- sum(abs(b)) * M_Y

  # compute quantities of linear combinations to be used as true values
  mean_U <- drop(crossprod(a, mean_X))
  mean_V <- drop(crossprod(b, mean_Y))
  var_U <- drop(crossprod(a, cov_X) %*% a)
  var_V <- drop(crossprod(b, cov_Y) %*% b)
  cov_UV <- drop(crossprod(a, cov_XY) %*% b)

  # return list of relevant quantities of model distribution
  list(
    X = list(cat = cat_X, K = K_X, M = M_X, keys = keys_X,
             mean = mean_X, cov = cov_X),
    Y = list(cat = cat_Y, K = K_Y, M = M_Y, keys = keys_Y,
             mean = mean_Y, cov = cov_Y),
    cov_XY = cov_XY, a = a, b = b,
    U = list(M = M_U, mean = mean_U, var = var_U),
    V = list(M = M_V, mean = mean_V, var = var_V),
    cov_UV = cov_UV,
    rho = cov_UV / (sqrt(var_U) * sqrt(var_V))
  )

}


#' Stylized ordinal measurement scale
#'
#' Generate relevant quantities of a stylized ordinal measurement scale
#' in which items have equal marginal response distributions and equal
#' correlations.  Analogous quantities are generated for a specified
#' linear combination of items.
#'
#' @param prob  type of marginal response distribution. The choices
#' `"centered"`, `"skewed"`, and `"polarized"` allow to quickly generate
#' marginal response probabilities with the corresponding property. If
#' `"latent"`, the marginal response probabilities are generated based on a
#' latent skew-normal distribution via function
#' \code{\link[latent2likert]{simulate_likert}()}.
#' @param K  number of response categories.
#' @param length  number of items in the ordinal measurement scale.
#' @param rho_w  correlation between the items in the ordinal measurement scale.
#' @param w  weight vector for computing a linear combination of items in the
#' ordinal measurement scale.
#' @param \dots  if `prob = "latent"`, the centered parameters of the latent
#' skew-normal distribution can be specified through the following additional
#' arguments:
#' - `mean`: mean (defaults to 0).
#' - `sd`: standard deviation (defaults to 1)
#' - `skew`: skweness (defaults to 0, must be between -0.95 and 0.95)
#'
#' @return A list with the following components:
#' \item{scale}{a list with the following components detailing the
#' distribution of the ordinal measurement scale:
#' \itemize{
#'   \item{`cat`: vector of response categories, encoded as `-M`, `-M+1`,
#'   \dots, `M-1`, `M`.}
#'   \item{`prob`: vector of response probabilites, shared by all items.}
#'   \item{`K`: number of response categories.}
#'   \item{`M`: upper bound of the support region of the items, with the
#'   lower bound given by `-M`.}
#'   \item{`keys`: vector indicating whether items are positively keyed (`1`)
#'   or negatively keyed (`-1`). Its elements are simply derived as the signs
#'   of the weight vector of the linear combination.}
#'   \item{`mean`: vector of means of the items.}
#'   \item{`cov`: covariance matrix of the items.}
#' }
#' }
#' \item{w}{weight vector for the linear combination of items in the
#' ordinal measurement scale.}
#' \item{score}{a list with the following components detailing the distribution
#' of the linear combination of the ordinal measurement scale:
#' \itemize{
#'   \item{`M`: upper bound of the support region of the linear combination,
#'   with the lower bound given by `-M`.}
#'   \item{`mean`: mean of the linear combination.}
#'   \item{`var`: variance of the linear combination.}
#' }
#' }
#'
#' @author Andreas Alfons
#'
#' @references
#' M. Welz, A. Archimbaud, A. Alfons (2026). Quantifying the impact of careless
#' responding. PsyArXiv. \doi{10.31234/osf.io/8fj6p_v2}.
#'
#' @seealso [simple_model()]
#'
#' @examples
#' # centered distribution
#' scale_X <- simple_scale(prob = "centered", K = 9,
#'                         length = 5, rho_w = 0.4)
#'
#' # this is actually equivalent to:
#' scale_Y <- simple_scale(prob = "latent", mean = 0, sd = 0.9,
#'                         skew = 0, K = 9, length = 5,
#'                         rho_w = 0.4)
#' all.equal(scale_X, scale_Y)
#'
#' # skewed distribution
#' scale_X <- simple_scale(prob = "skewed", K = 9,
#'                         length = 5, rho_w = 0.4)
#'
#' # this is actually equivalent to:
#' scale_Y <- simple_scale(prob = "latent", mean = 0, sd = 1.1,
#'                         skew = -0.95, K = 9, length = 5,
#'                         rho_w = 0.4)
#' all.equal(scale_X, scale_Y)
#'
#' # polarized distribution
#' scale_X <- simple_scale(prob = "polarized", K = 9,
#'                         length = 5, rho_w = 0.4)
#'
#' @export

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


#' Stylized model distribution
#'
#' Generate relevant quantities of a stylized model distribution for ordinal
#' measurement scales in which items have equal marginal response
#' distributions, equal within-scale correlations, and equal between-scale
#' correlations.  Analogous quantities are generated for specified linear
#' combinations of items.
#'
#' @param X  list of relevant quantities for an ordinal measurement scale and
#' a certain linear combination of items, as returned by [simple_scale()].
#' @param Y  list of relevant quantities for an ordinal measurement scale and
#' a certain linear combination of items, as returned by [simple_scale()].
#' @param rho  correlation between the linear combinations of the ordinal
#' measurement scales.
#' @param \dots  additional arguments by default to be passed to
#' [simple_scale()].
#'
#' @return A list with the following components:
#' \item{X, Y}{each is a list with the following components detailing the
#' distribution of the ordinal measurement scale:
#' \itemize{
#'   \item{`cat`: vector of response categories, encoded as `-M`, `-M+1`,
#'   \dots, `M-1`, `M`.}
#'   \item{`prob`: vector of response probabilites, shared by all items.}
#'   \item{`K`: number of response categories.}
#'   \item{`M`: upper bound of the support region of the items, with the
#'   lower bound given by `-M`.}
#'   \item{`keys`: vector indicating whether items are positively keyed (`1`)
#'   or negatively keyed (`-1`). Its elements are simply derived as the signs
#'   of the weight vector of the linear combination.}
#'   \item{`mean`: vector of means of the items.}
#'   \item{`cov`: covariance matrix of the items.}
#' }
#' }
#' \item{cov_XY}{cross-covariance matrix of the measurement scales.}
#' \item{a}{weight vector for the linear combination `U` of items in the
#' ordinal measurement scale from `X`.}
#' \item{b}{weight vector for the linear combination `V` of items in the
#' ordinal measurement scale from `Y`.}
#' \item{U, V}{each is a list with the following components detailing the
#' distribution of the linear combination of the ordinal measurement scale:
#' \itemize{
#'   \item{`M`: upper bound of the support region of the linear combination,
#'   with the lower bound given by `-M`.}
#'   \item{`mean`: mean of the linear combination.}
#'   \item{`var`: variance of the linear combination.}
#' }
#' }
#' \item{cov_UV}{covariance between the linear combinations of the ordinal
#' measurement scales.}
#' \item{rho}{correlation between the linear combinations of the ordinal
#' measurement scales.}
#'
#' @author Andreas Alfons
#'
#' @references
#' M. Welz, A. Archimbaud, A. Alfons (2026). Quantifying the impact of careless
#' responding. PsyArXiv. \doi{10.31234/osf.io/8fj6p_v2}.
#'
#' @seealso
#' [careless()], [reversed()], [adversarial()]
#'
#' [bias()], [max_bias()], [breakdown()]
#'
#' @examples
#' # centered distributions
#' model_XY <- simple_model(prob = "centered", K = 9,
#'                          length = 5, rho_w = 0.4,
#'                          rho = -0.4)
#'
#' # this is actually equivalent to:
#' model_latent <- simple_model(prob = "latent", mean = 0, sd = 0.9,
#'                              skew = 0, K = 9, length = 5,
#'                              rho_w = 0.4, rho = -0.4)
#' all.equal(model_XY, model_latent)
#'
#' # contamination fractions
#' eps <- c(0, 0.1, 0.2, 0.3, 0.4, 0.49)
#'
#' # bias under straightlining at extreme response categories
#' bias(model_XY, epsilon = eps, contamination = "careless",
#'      type = "straightlining", target = "extreme")
#'
#' @export

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


#' Stylized model distribution of identical copies
#'
#' Generate relevant quantities of a stylized model distribution for two
#' identical copies of an ordinal measurement scale and a specified linear
#' combination of items. In this stylized model distribution, items of the
#' measurement scale have equal marginal response distributions and equal
#' within-scale correlations.
#'
#' @inheritParams simple_model
#'
#' @inherit simple_model return
#'
#' @note This is intended for maximum bias and breakdown value calculations
#' relating to a stricter definition of the latter that is commonly found in
#' the statistical literature. It defines the breakdown value as the smallest
#' contamination fraction so that the correlation functional at the
#' contaminated distribution is negative for two random variables with
#' correlation 1 at the model distribution.
#'
#' @author Andreas Alfons
#'
#' @references
#' M. Welz, A. Archimbaud, A. Alfons (2026). Quantifying the impact of careless
#' responding. PsyArXiv. \doi{10.31234/osf.io/8fj6p_v2}.
#'
#' @seealso [max_bias()], [breakdown()]
#'
#' @examples
#' model_XX <- identical_model(prob = "centered", K = 9,
#'                             length = 5, rho_w = 0.4)
#'
#' # compute breakdown value
#' breakdown(model_XX)
#'
#' # verification with maximum bias
#' max_bias(model_XX, epsilon = breakdown(model_XX))
#'
#' @export

identical_model <- function(X = simple_scale(...), ...) {
  # let simple_model() do most of the work
  model_XX <- simple_model(X = X, Y = X, rho = 1)
  # but here the between-scale covariance matrix is identical to the
  # within-scale covariance matrix
  model_XX$cov_XY <- model_XX$X$cov
  # return list of relevant quantities of model distribution
  model_XX
}


# internal function to get number of variables in a scale
get_length <- function(scale) length(scale$mean)


#' Contamination distribution corresponding to careless responding
#'
#' Generate relevant quantities of a contamination distribution corresponding
#' to (stylized) careless responding behavior.
#'
#' @param model  list of relevant quantities detailing the distribution of
#' ordinal measurement scales, as generated by [get_model()] or
#' [simple_model()].
#' @param type  type of careless responding behavior. Possible value are:
#' - `"straightlining"`: randomly select a response category from certain
#'   target categories, then use the selected response for all items.
#' - `"random"`: randomly select a response category from certain target
#'   categories independently for all items.
#' @param target  target response categories for the above types of careless
#' responding behavior. Possible values are:
#' - `"extreme"`: select only from the extreme two response categories.
#' - `"all"`: select from all response categories.
#'
#' @return A list with the following components:
#' \item{X, Y}{each is a list with the following components detailing the
#' distribution of the careless responding behavior:
#' \itemize{
#'   \item{`prob`: vector of response probabilites, shared by all items.}
#'   \item{`mean`: vector of means of the items.}
#'   \item{`cov`: covariance matrix of the items.}
#' }
#' }
#' \item{cov_XY}{cross-covariance matrix of the distribution of the careless
#' responding behavior.}
#'
#' @author Andreas Alfons
#'
#' @references
#' M. Welz, A. Archimbaud, A. Alfons (2026). Quantifying the impact of careless
#' responding. PsyArXiv. \doi{10.31234/osf.io/8fj6p_v2}.
#'
#' @seealso
#' [get_model()], [simple_model()]
#'
#' [bias()]
#'
#' @examples
#' # generate model distribution
#' model_XY <- simple_model(prob = "centered", K = 9,
#'                          length = 5, rho_w = 0.4,
#'                          rho = -0.4)
#'
#' # straightlining from extreme response categories
#' straight_extreme <- careless(model_XY, type = "straightlining",
#'                              target = "extreme")
#'
#' # straightlining from all response categories
#' straight_all <- careless(model_XY, type = "straightlining",
#'                          target = "all")
#'
#' # random responding from extreme response categories
#' random_extreme <- careless(model_XY, type = "random",
#'                            target = "extreme")
#'
#' # random responding from all response categories
#' random_all <- careless(model_XY, type = "random",
#'                        target = "all")
#'
#' @export

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


#' Adversarial contamination distribution
#'
#' Generate relevant quantities of an adversarial contamination distribution
#' resulting in maximum upward or downward bias of Pearson's correlation
#' coefficient.  That is, responses for all items are selected in the most
#' harmful manner for this correlation estimator.
#'
#' @param model  list of relevant quantities detailing the distribution of
#' ordinal measurement scales, as generated by [get_model()],
#' [simple_model()], or [identical_model()].
#' @param direction  direction of the resulting maximum bias. Possible values
#' are:
#' - `"upward"`: adversarial contamination resulting in maximum upward bias.
#' - `"downward"`: adversarial contamination resulting in maximum downward bias.
#'
#' @return A list with the following components:
#' \item{X, Y}{each is a list with the following components detailing the
#' adversarial contamination distribution:
#' \itemize{
#'   \item{`mean`: vector of means of the items.}
#'   \item{`cov`: covariance matrix of the items.}
#' }
#' }
#' \item{cov_XY}{cross-covariance matrix of the distribution of the adversarial
#' contamination distribution.}
#'
#' @author Andreas Alfons
#'
#' @references
#' M. Welz, A. Archimbaud, A. Alfons (2026). Quantifying the impact of careless
#' responding. PsyArXiv. \doi{10.31234/osf.io/8fj6p_v2}.
#'
#' @seealso
#' [get_model()], [simple_model()], [identical_model()]
#'
#' [bias()], [max_bias()], [breakdown()]
#'
#' @examples
#' # generate model distribution
#' model_XY <- simple_model(prob = "centered", K = 9,
#'                          length = 5, rho_w = 0.4,
#'                          rho = -0.4)
#'
#' # adversarial contamination resulting in maximum upward bias
#' adv_upward <- adversarial(model_XY, direction = "upward")
#'
#' # adversarial contamination resulting in maximum downward bias
#' adv_downward <- adversarial(model_XY, direction = "downward")
#'
#' @export

adversarial <- function(model, direction = c("upward", "downward")) {

  # initializations
  direction <- match.arg(direction)

  # extract components from object for model distribution
  M_X <- model$X$M
  p <- get_length(model$X)
  a <- model$a
  M_Y <- model$Y$M
  q <- get_length(model$Y)
  b <- model$b

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


#' Contamination distribution corresponding to scale reversal
#'
#' Generate relevant quantities of a contamination distribution corresponding
#' to respondents reversing one or both scales in a model distribution.
#'
#' @param model  list of relevant quantities detailing the distribution of
#' ordinal measurement scales, as generated by [get_model()] or
#' [simple_model()].
#' @param which  which scale to reverse for the contamination distribution.
#' Possible values are `"X"`, `"Y"` and `"both"`.
#'
#' @return A list with the following components:
#' \item{X, Y}{each is a list with the following components detailing the
#' contamination distribution:
#' \itemize{
#'   \item{`mean`: vector of means of the items.}
#'   \item{`cov`: covariance matrix of the items.}
#' }
#' }
#' \item{cov_XY}{cross-covariance matrix of the distribution of the
#' contamination distribution.}
#'
#' @author Andreas Alfons
#'
#' @references
#' M. Welz, A. Archimbaud, A. Alfons (2026). Quantifying the impact of careless
#' responding. PsyArXiv. \doi{10.31234/osf.io/8fj6p_v2}.
#'
#' @seealso
#' [get_model()], [simple_model()]
#'
#' [bias()]
#'
#' @examples
#' # generate model distribution
#' model_XY <- simple_model(prob = "skewed", K = 9,
#'                          length = 5, rho_w = 0.4,
#'                          rho = 0.4)
#'
#' # contamination with one reversed scale
#' rev_X <- reversed(model_XY, which = "X")
#' rev_Y <- reversed(model_XY, which = "Y")
#'
#' # contamination with both scales reversed
#' rev_both <- reversed(model_XY, which = "both")
#'
#' @export

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

#' Cronbach alpha under our assumptions
#'
#' @param J construct size
#' @param rho Within-construct correlation between items in a construct comprising items that are each identically distributed
#'
#' @return Cronbach alpha (numeric scalar)
#' @author Max Welz
#' @export
cronbach <- function(J, rho)
{
  ## input checks
  .input_check_constructsize(J)
  .input_check_rho(rho)

  ## call main function
  .cronbach(J = J, rho = rho)
}


#' Internal function to calculate cronbach alpha
#'
#'
#' @author Max Welz
#' @noRd
.cronbach <- function(J, rho)
{
  J / (J-1) * (1 - 1 / (1 + (J-1) * rho))
}

#' Void internal function to check a supplied construct size
#'
#' @param x Number of items in a given construct
#'
#' @author Max Welz
#' @noRd
.input_check_constructsize <- function(x)
{
  if(!is.numeric(x) || length(x) > 1L)
  {
    stop("Construct sizes must be a numeric vector of length 1")
  } # IF

  if(!identical(0, x %% 1))
  {
    stop("Construct size must be integer-valued")
  } # IF

  if(x <= 0L)
  {
    stop("Construct size must be at least 1")
  } # IF
} # FUN


#' Void internal function to check a supplied correlation coefficient
#'
#' A correlation coefficient must be a numeric scalar in \eqn{[-1, 1]}.
#'
#' @param x Correlation coefficient
#' @author Max Welz
#' @noRd
.input_check_rho <- function(x)
{
  if(!is.numeric(x) || !is.vector(x) || length(x) > 1L)
  {
    stop("Correlation coefficients must be a numeric vector of length 1")
  } # IF

  if(!(-1.0 <= x && x <= 1.0))
  {
    stop("Correlation coefficients must take values in [-1, 1]")
  } # IF
}

