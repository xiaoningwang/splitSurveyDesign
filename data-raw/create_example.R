set.seed(2026)
n <- 300
z <- rnorm(n)
split_example <- data.frame(
  core_age = round(45 + 12 * z + rnorm(n, 0, 5)),
  income = exp(10 + 0.25 * z + rnorm(n, 0, 0.35)),
  health = 60 - 5 * z + rnorm(n, 0, 8),
  trust = 50 + 6 * z + rnorm(n, 0, 10),
  satisfaction = 70 + 4 * z + rnorm(n, 0, 7)
)
save(split_example, file = "data/split_example.rda", compress = "xz")
