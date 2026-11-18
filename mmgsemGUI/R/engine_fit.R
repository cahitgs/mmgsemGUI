# Structural model and clustering (Step 2) -----------------------------------

#' Estimate MMG-SEM for one number of clusters
#'
#' A thin wrapper around [mmgsem::mmgsem()] that always reuses the measurement
#' model estimated in Step 1, as in Section 4.3.1 of the tutorial.
#' @param data Data frame prepared by [prepare_data()].
#' @param S1,S2 Measurement- and structural-model syntax. With measurement
#'   blocks, `S1` is the list returned by [split_blocks()].
#' @param group Name of the grouping variable.
#' @param k Number of clusters.
#' @param s1_fit Fitted measurement model from [fit_measurement()] (a list of
#'   fits with measurement blocks).
#' @param seed Random seed.
#' @param nstarts Number of random starts.
#' @param max_it Maximum number of EM iterations.
#' @param endogenous_cov,endo_group_specific See [mmgsem::mmgsem()].
#' @param missing `"fiml"` when the measurement model was estimated with
#'   full-information maximum likelihood; mmgsem then takes the covariance
#'   matrices it needs from the incomplete data in the same way.
#' @param ordinal The items were treated as ordinal (see [fit_measurement()]).
#'   mmgsem is then called with `ordered = TRUE` and the equality constraints
#'   of Step 1. Only then does it take polychoric correlations as the sample
#'   statistics and rescale the factors to the marker-variable scale before
#'   Step 2; the result equals letting mmgsem estimate Step 1 itself.
#' @param noninv Released loadings; mmgsem needs them again for ordinal items.
#' @return The `mmgsem` fit with the elapsed time in `attr(, "seconds")`.
#' @export
fit_clusters <- function(data, S1, S2, group, k, s1_fit, seed = 1L,
                         nstarts = 20L, max_it = 10000L,
                         endogenous_cov = TRUE, endo_group_specific = TRUE,
                         missing = c("listwise", "fiml"), ordinal = FALSE,
                         noninv = character()) {
  missing <- match.arg(missing)
  args <- list(
    dat = data, S1 = S1, S2 = S2, group = group, nclus = as.integer(k),
    seed = seed, s1_type = "lavaan", s1_fit = s1_fit,
    nstarts = as.integer(nstarts), max_it = as.integer(max_it),
    endogenous_cov = endogenous_cov,
    endo_group_specific = endo_group_specific
  )
  args <- c(args, cluster_extra_args(missing, ordinal, noninv))
  t0 <- proc.time()[["elapsed"]]
  fit <- suppressMessages(do.call(mmgsem::mmgsem, args))
  attr(fit, "seconds") <- proc.time()[["elapsed"]] - t0
  fit
}

# Arguments that mmgsem passes on to lavaan for the options of Step 1.
cluster_extra_args <- function(missing = "listwise", ordinal = FALSE, noninv = character()) {
  extra <- list()
  if (identical(missing, "fiml")) extra$missing <- "fiml"
  if (isTRUE(ordinal)) {
    extra$ordered <- TRUE
    extra$group.equal <- c("thresholds", "loadings")
    if (length(noninv)) extra$group.partial <- noninv
  }
  extra
}

#' Combine separately estimated models into a model-selection object
#'
#' Produces the same object as [mmgsem::ModelSelection()] (an `Overview` table
#' and the list of `Models`), so that `summary()`, `plot()` and
#' [mmgsem::extract()] work on it. Estimating the models separately is what
#' lets the interface run them in parallel and report progress.
#' @param fits List of `mmgsem` fits, one per number of clusters.
#' @return An object of class `mmgsem` with `Overview` and `Models`.
#' @export
assemble_selection <- function(fits) {
  ks <- vapply(fits, n_clusters, integer(1))
  fits <- fits[order(ks)]
  ks <- sort(ks)
  if (length(ks) > 1 && any(diff(ks) != 1)) {
    stop("The numbers of clusters must be consecutive.", call. = FALSE)
  }
  pick <- function(f) vapply(fits, f, numeric(1))
  LL <- pick(function(x) x$logLik$obs_loglik)
  LL_fac <- pick(function(x) x$logLik$loglik)
  nrpar <- pick(function(x) x$NrPar$Obs.nrpar)
  nrpar_fac <- pick(function(x) x$NrPar$Fac.nrpar)
  chull_fun <- utils::getFromNamespace("CHull", "mmgsem")
  ch <- chull_fun(loglik = LL, nrpar = nrpar, nsclust = range(ks))
  ch_fac <- chull_fun(loglik = LL_fac, nrpar = nrpar_fac, nsclust = range(ks))
  overview <- cbind(
    pick(function(x) x$model_sel$R2_entropy),
    ch,
    pick(function(x) x$model_sel$BIC$observed$BIC_G),
    pick(function(x) x$model_sel$BIC$observed$BIC_N),
    pick(function(x) x$model_sel$AIC$observed),
    pick(function(x) x$model_sel$AIC3$observed),
    pick(function(x) x$model_sel$ICL$observed),
    ch_fac[, 2:4, drop = FALSE],
    pick(function(x) x$model_sel$BIC$Factors$BIC_G),
    pick(function(x) x$model_sel$BIC$Factors$BIC_N),
    pick(function(x) x$model_sel$AIC$Factors),
    pick(function(x) x$model_sel$AIC3$Factors),
    pick(function(x) x$model_sel$ICL$Factors)
  )
  colnames(overview) <- c(
    "R2entropy", "Clusters", "LL", "nrpar", "Chull", "BIC_G", "BIC_N", "AIC",
    "AIC3", "ICL", "LL_fac", "nrpar_fac", "Chull_fac", "BIC_G_fac",
    "BIC_N_fac", "AIC_fac", "AIC3_fac", "ICL_fac"
  )
  overview <- as.data.frame(overview)
  rownames(overview) <- NULL
  names(fits) <- paste0(ks, "-cluster model")
  out <- list(Overview = overview, Models = fits)
  class(out) <- "mmgsem"
  out
}

#' Number of clusters of a fitted model
#' @keywords internal
n_clusters <- function(fit) as.integer(ncol(fit$posteriors) - 1L)

#' Things to check before a clustering solution is used
#'
#' mmgsem stops the EM algorithm when the log-likelihood changes by less than
#' 1e-6, when the iteration limit is reached, or when the log-likelihood
#' decreases, and it returns only the number of iterations of the best start.
#' The first two cases can be told apart; the third cannot.
#'
#' @param fit An `mmgsem` fit.
#' @param max_it The iteration limit that was used.
#' @return List with `hit_limit` (the best start stopped at the limit, so it
#'   did not converge) and `empty` (clusters to which no group is assigned).
#' @export
cluster_checks <- function(fit, max_it = 10000L) {
  k <- n_clusters(fit)
  list(
    hit_limit = isTRUE(as.numeric(fit$iterations) >= as.numeric(max_it)),
    empty = setdiff(seq_len(k), unique(tidy_clusters(fit)$cluster))
  )
}

#' How many random starts reached the best solution
#'
#' Following Perez Alonso et al. (2024), a start counts as a local maximum when
#' its log-likelihood is more than 0.001 below the best one.
#' @param fit An `mmgsem` fit.
#' @param tol Tolerance on the log-likelihood.
#' @return List with `loglik` (per start), `best`, `n_starts`, `n_best`.
#' @export
start_diagnostics <- function(fit, tol = 1e-3) {
  ll <- as.numeric(unlist(fit$logLik$runs_loglik))
  ll <- ll[is.finite(ll)]
  if (!length(ll)) ll <- as.numeric(fit$logLik$loglik)
  best <- max(ll)
  list(loglik = ll, best = best, n_starts = length(ll),
       n_best = sum(ll >= best - tol))
}

#' Standard errors of the structural parameters
#' @param fit An `mmgsem` fit.
#' @param naive If TRUE, skip the correction for stepwise estimation (fast).
#' @keywords internal
standard_errors <- function(fit, naive = TRUE) {
  t0 <- proc.time()[["elapsed"]]
  se <- mmgsem::compute_se(object = fit, naive = naive)
  attr(se, "seconds") <- proc.time()[["elapsed"]] - t0
  attr(se, "naive") <- naive
  se
}

#' Corrected standard errors of the tutorial example, computed in advance
#'
#' Computed with [mmgsem::compute_se()] for the four-cluster model of the
#' tutorial example (seed 1, 20 random starts) by `data-raw/tutorial_se.R`.
#' `compute_se()` was revised in October 2026. Since 17 November 2026 mmgsem
#' stores the values of the revised function as `standard_errors`, which are
#' identical to these, and keeps the values behind the output boxes of the
#' tutorial as `standard_errors_deprecated`.
#' @return A list with `se`, `mmgsem` (version and commit used) and `seconds`,
#'   or NULL when the file is not installed.
#' @keywords internal
precomputed_se <- function() {
  f <- system.file("extdata", "tutorial_se.rds", package = "mmgsemGUI")
  if (!nzchar(f)) return(NULL)
  tryCatch(readRDS(f), error = function(e) NULL)
}

#' Do stored standard errors belong to this fitted model?
#'
#' Cluster labels can switch between runs, so precomputed standard errors are
#' only attached when their estimates equal the estimates of the model.
#' @keywords internal
se_matches_fit <- function(se, fit, tol = 1e-6) {
  est <- tryCatch(beta_vector(fit), error = function(e) NULL)
  ref <- se$estimates_vector$betas_est
  if (is.null(est) || is.null(ref) || length(est) != length(ref)) return(FALSE)
  isTRUE(all(abs(unname(est) - unname(ref)) < tol))
}
