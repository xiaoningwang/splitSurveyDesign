#' Allocate samples under a survey-cost budget
#'
#' @param cost Positive per-complete costs by form.
#' @param budget Total variable-cost budget.
#' @param importance Optional non-negative importance weights.
#' @param min_n Minimum sample assigned to every form.
#' @return Integer allocation with achieved cost.
#' @export
allocation_cost <- function(cost, budget, importance = NULL, min_n = 0L) {
  cost <- as.numeric(cost)
  if (any(!is.finite(cost) | cost <= 0)) stop("cost must be positive")
  if (!is.finite(budget) || budget < 0) stop("budget must be non-negative")
  min_n <- as.integer(min_n)
  base_cost <- sum(cost * min_n)
  if (base_cost > budget) stop("budget is smaller than the minimum allocation")
  if (is.null(importance)) importance <- rep(1, length(cost))
  importance <- as.numeric(importance)
  if (length(importance) != length(cost) || any(importance < 0))
    stop("importance must match cost and be non-negative")
  score <- sqrt(importance / cost)
  share <- if (sum(score) == 0) rep(1 / length(cost), length(cost)) else score / sum(score)
  allocation <- rep(min_n, length(cost))
  remaining <- budget - base_cost
  target <- remaining * share / cost
  allocation <- allocation + floor(target)
  while (TRUE) {
    feasible <- which(cost <= budget - sum(cost * allocation) + 1e-9)
    if (!length(feasible)) break
    gap <- target[feasible] - floor(target[feasible])
    allocation[feasible[which.max(gap)]] <- allocation[feasible[which.max(gap)]] + 1L
  }
  names(allocation) <- names(cost)
  list(n = allocation, achieved_cost = sum(cost * allocation),
       unused_budget = budget - sum(cost * allocation))
}

#' Neyman-type allocation for several questionnaire forms
#'
#' @param sd Positive standard-deviation proxy by form.
#' @param total_n Total sample size.
#' @param cost Optional positive per-complete costs.
#' @param min_n Minimum allocation per form.
#' @return Integer sample allocation.
#' @export
allocation_variance <- function(sd, total_n, cost = rep(1, length(sd)), min_n = 0L) {
  sd <- as.numeric(sd); cost <- as.numeric(cost)
  total_n <- as.integer(total_n); min_n <- as.integer(min_n)
  if (length(sd) != length(cost) || any(sd < 0) || any(cost <= 0))
    stop("sd and cost must have equal length and valid values")
  if (total_n < length(sd) * min_n) stop("total_n is too small")
  score <- sd / sqrt(cost)
  share <- if (sum(score) == 0) rep(1 / length(sd), length(sd)) else score / sum(score)
  remaining <- total_n - length(sd) * min_n
  raw <- remaining * share
  n <- rep(min_n, length(sd)) + floor(raw)
  if (sum(n) < total_n) {
    order_fraction <- order(raw - floor(raw), decreasing = TRUE)
    n[order_fraction[seq_len(total_n - sum(n))]] <-
      n[order_fraction[seq_len(total_n - sum(n))]] + 1L
  }
  names(n) <- names(sd)
  n
}
