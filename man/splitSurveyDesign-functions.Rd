\name{splitSurveyDesign-functions}
\alias{pattern_generate}
\alias{allocation_cost}
\alias{allocation_variance}
\alias{estimate_split}
\alias{impute_split}
\alias{greg_estimate}
\alias{evaluate_design}
\title{Core split-questionnaire design and analysis functions}
\description{
Functions for questionnaire-pattern construction, sample allocation, estimation,
imputation, generalized regression estimation, and design evaluation.
}
\usage{
pattern_generate(variables, core = character(), n_forms = 3L,
                 items_per_form = NULL, seed = NULL)
allocation_cost(cost, budget, importance = NULL, min_n = 0L)
allocation_variance(sd, total_n, cost = rep(1, length(sd)), min_n = 0L)
estimate_split(data, weights = NULL)
impute_split(data, method = c("mean", "hotdeck"), seed = NULL)
greg_estimate(y, x, population_x_mean, weights = NULL)
evaluate_design(truth, estimates)
}
\arguments{
\item{variables, core}{Variable names and the subset included in all forms.}
\item{n_forms, items_per_form}{Number of forms and optional items per form.}
\item{seed}{Optional random seed.}
\item{cost, budget, importance, min_n}{Cost-allocation inputs.}
\item{sd, total_n}{Variance proxy and total sample size.}
\item{data, weights}{Survey data and optional non-negative weights.}
\item{method}{Imputation method.}
\item{y, x, population_x_mean}{Study variable, auxiliaries, and known population auxiliary means.}
\item{truth, estimates}{Target values and output from \code{estimate_split}.}
}
\value{
Function-specific lists, vectors, or data frames containing patterns,
allocations, estimates, diagnostics, or completed data.
}
\examples{
p <- pattern_generate(c("core", "a", "b"), core = "core",
                      n_forms = 2, items_per_form = 1, seed = 1)
allocation_cost(c(2, 3), budget = 100, min_n = 2)
estimate_split(data.frame(a = c(1, 2, NA), b = c(2, NA, 4)))
}
