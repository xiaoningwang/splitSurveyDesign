# splitSurveyDesign

`splitSurveyDesign` is the companion R package for the manuscript *社会调查中的问卷分割设计与应用*.

```r
R CMD INSTALL splitSurveyDesign
library(splitSurveyDesign)

design <- pattern_generate(
  variables = c("age", "sex", "income", "health", "trust", "satisfaction"),
  core = c("age", "sex"),
  n_forms = 3,
  items_per_form = 2,
  seed = 2026
)

allocation_cost(cost = c(8, 10, 12), budget = 3000, min_n = 30)
```

Version 0.1.0 is a transparent, dependency-free reference implementation.
Production use should add complex-sample variance estimators, calibration,
multiple imputation diagnostics, and constrained optimization backends.
