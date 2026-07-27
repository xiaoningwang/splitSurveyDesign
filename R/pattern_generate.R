#' Generate balanced split-questionnaire patterns
#'
#' @param variables Character vector of all variable names.
#' @param core Character vector answered in every form.
#' @param n_forms Number of questionnaire forms.
#' @param items_per_form Number of non-core items per form.
#' @param seed Optional random seed.
#' @return A list containing a binary pattern matrix and diagnostics.
#' @export
pattern_generate <- function(variables, core = character(), n_forms = 3L,
                             items_per_form = NULL, seed = NULL) {
  variables <- unique(as.character(variables))
  core <- unique(as.character(core))
  if (!length(variables) || !all(core %in% variables))
    stop("variables must be non-empty and contain every core item")
  n_forms <- as.integer(n_forms)
  if (n_forms < 2L) stop("n_forms must be at least 2")
  optional <- setdiff(variables, core)
  if (!length(optional)) stop("at least one non-core item is required")
  if (is.null(items_per_form))
    items_per_form <- ceiling(length(optional) / n_forms)
  items_per_form <- as.integer(items_per_form)
  if (items_per_form < 1L || items_per_form > length(optional))
    stop("items_per_form is outside the allowable range")
  if (!is.null(seed)) set.seed(seed)

  pattern <- matrix(0L, nrow = n_forms, ncol = length(variables),
                    dimnames = list(paste0("form", seq_len(n_forms)), variables))
  pattern[, core] <- 1L
  load <- stats::setNames(integer(length(optional)), optional)
  for (f in seq_len(n_forms)) {
    jitter <- stats::runif(length(optional), 0, 1e-6)
    chosen <- names(sort(load + jitter))[seq_len(items_per_form)]
    pattern[f, chosen] <- 1L
    load[chosen] <- load[chosen] + 1L
  }
  list(
    pattern = pattern,
    optional_frequency = colSums(pattern[, optional, drop = FALSE]),
    core = core,
    form_length = rowSums(pattern)
  )
}
