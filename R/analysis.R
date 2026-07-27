#' Estimate means from split-questionnaire data
#'
#' @param data Data frame containing planned missing values.
#' @param weights Optional survey weights.
#' @return Data frame of means, standard errors and observed sample sizes.
#' @export
estimate_split <- function(data, weights = NULL) {
  data <- as.data.frame(data)
  if (is.null(weights)) weights <- rep(1, nrow(data))
  if (length(weights) != nrow(data) || any(weights < 0))
    stop("weights must match rows and be non-negative")
  out <- lapply(data, function(y) {
    ok <- is.finite(y) & is.finite(weights) & weights > 0
    if (!any(ok)) return(c(estimate = NA, se = NA, n_observed = 0))
    w <- weights[ok]; z <- y[ok]
    mu <- sum(w * z) / sum(w)
    n_eff <- sum(w)^2 / sum(w^2)
    v <- if (length(z) > 1L) sum(w * (z - mu)^2) / sum(w) else 0
    c(estimate = mu, se = sqrt(v / n_eff), n_observed = length(z))
  })
  as.data.frame(do.call(rbind, out))
}

#' Impute planned missing values
#'
#' @param data Numeric data frame.
#' @param method Either "mean" or "hotdeck".
#' @param seed Optional random seed.
#' @return Completed data frame.
#' @export
impute_split <- function(data, method = c("mean", "hotdeck"), seed = NULL) {
  method <- match.arg(method)
  if (!is.null(seed)) set.seed(seed)
  out <- as.data.frame(data)
  for (j in seq_along(out)) {
    y <- out[[j]]; miss <- is.na(y); donors <- y[!miss]
    if (!any(miss)) next
    if (!length(donors)) stop("a variable has no observed donor")
    y[miss] <- if (method == "mean") mean(donors) else
      sample(donors, sum(miss), replace = TRUE)
    out[[j]] <- y
  }
  out
}

#' Generalized regression estimator
#'
#' @param y Study variable.
#' @param x Auxiliary-variable matrix.
#' @param population_x_mean Known population auxiliary means.
#' @param weights Optional sampling weights.
#' @return GREG estimate and fitted coefficients.
#' @export
greg_estimate <- function(y, x, population_x_mean, weights = NULL) {
  x <- as.matrix(x); y <- as.numeric(y)
  if (nrow(x) != length(y)) stop("x and y have incompatible dimensions")
  if (length(population_x_mean) != ncol(x))
    stop("population_x_mean must match columns of x")
  if (is.null(weights)) weights <- rep(1, length(y))
  ok <- is.finite(y) & apply(is.finite(x), 1L, all) & is.finite(weights) & weights > 0
  X <- cbind(1, x[ok, , drop = FALSE]); w <- weights[ok]; yy <- y[ok]
  fit <- stats::lm.wfit(X, yy, w)
  sample_x <- colSums(x[ok, , drop = FALSE] * w) / sum(w)
  y_bar <- sum(w * yy) / sum(w)
  adjustment <- sum(fit$coefficients[-1L] * (population_x_mean - sample_x))
  list(estimate = unname(y_bar + adjustment),
       coefficients = fit$coefficients, n_observed = sum(ok))
}

#' Evaluate a split design against complete data
#'
#' @param truth Named numeric vector of target means.
#' @param estimates Data frame returned by estimate_split().
#' @return Bias, squared error and coverage-ready diagnostics.
#' @export
evaluate_design <- function(truth, estimates) {
  truth <- as.numeric(truth)
  if (length(truth) != nrow(estimates)) stop("truth and estimates differ in length")
  bias <- estimates$estimate - truth
  data.frame(truth = truth, estimate = estimates$estimate, bias = bias,
             squared_error = bias^2, se = estimates$se,
             n_observed = estimates$n_observed)
}
