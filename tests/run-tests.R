library(splitSurveyDesign)

p <- pattern_generate(c("core", "a", "b", "c"), core = "core",
                      n_forms = 3, items_per_form = 2, seed = 1)
stopifnot(all(p$pattern[, "core"] == 1), all(rowSums(p$pattern) == 3))

a <- allocation_cost(c(f1 = 2, f2 = 3), budget = 100, min_n = 2)
stopifnot(a$achieved_cost <= 100, all(a$n >= 2))

n <- allocation_variance(c(f1 = 1, f2 = 2), total_n = 101)
stopifnot(sum(n) == 101)

d <- data.frame(a = c(1, 2, NA, 4), b = c(2, NA, 6, 8))
e <- estimate_split(d)
stopifnot(nrow(e) == 2, all(e$n_observed == 3))
stopifnot(!anyNA(impute_split(d, "mean")))
stopifnot(!anyNA(impute_split(d, "hotdeck", seed = 1)))

g <- greg_estimate(1:5, cbind(x = 2:6), population_x_mean = 5)
stopifnot(is.finite(g$estimate))

q <- evaluate_design(c(2, 5), e)
stopifnot(nrow(q) == 2)
