# Tidy tables from fitted models ---------------------------------------------
# The mmgsem package prints its results; the interface needs them as data
# frames. The computations below follow summary.mmgsem() and test.mmgsem()
# line by line and are checked against the printed tutorial output in the
# tests.

beta_list <- function(fit) {
  b <- fit$param$beta_ks
  if (!is.list(b)) b <- list(b)
  b
}

# Positions of the estimated regressions (response = row, predictor = column),
# in the column-major order used by mmgsem.
beta_index <- function(fit) {
  b <- beta_list(fit)
  free <- Reduce(`|`, lapply(b, function(m) m != 0))
  idx <- which(free, arr.ind = TRUE)
  idx <- idx[order(idx[, 2], idx[, 1]), , drop = FALSE]
  data.frame(
    response = rownames(b[[1]])[idx[, 1]],
    predictor = colnames(b[[1]])[idx[, 2]],
    row = idx[, 1], col = idx[, 2],
    stringsAsFactors = FALSE
  )
}

# Named vector of estimates, named as in compute_se(): "F3~F1.k1"
beta_vector <- function(fit) {
  b <- beta_list(fit)
  idx <- beta_index(fit)
  out <- unlist(lapply(seq_along(b), function(k) {
    v <- b[[k]][cbind(idx$row, idx$col)]
    stats::setNames(v, paste0(idx$response, "~", idx$predictor, ".k", k))
  }))
  out
}

se_vector <- function(se) {
  corrected <- se$SE_vector$betas_se_corrected
  if (!is.null(corrected) && !all(is.na(corrected))) {
    list(se = corrected, type = "corrected")
  } else {
    list(se = se$SE_vector$betas_se, type = "naive")
  }
}

#' Regression coefficients per cluster
#'
#' @param fit An `mmgsem` fit.
#' @param se Optional result of [mmgsem::compute_se()]; adds standard errors,
#'   z statistics, two-sided p values and 95% confidence intervals.
#' @return Data frame with one row per cluster and regression.
#' @export
tidy_betas <- function(fit, se = NULL) {
  idx <- beta_index(fit)
  est <- beta_vector(fit)
  k <- length(beta_list(fit))
  out <- data.frame(
    cluster = rep(seq_len(k), each = nrow(idx)),
    parameter = rep(paste(idx$response, "~", idx$predictor), times = k),
    response = rep(idx$response, times = k),
    predictor = rep(idx$predictor, times = k),
    estimate = unname(est),
    stringsAsFactors = FALSE
  )
  if (!is.null(se)) {
    s <- se_vector(se)
    sev <- unname(s$se[names(est)])
    out$se <- sev
    out$z <- out$estimate / sev
    out$p <- 2 * stats::pnorm(abs(out$z), lower.tail = FALSE)
    out$ci_lower <- out$estimate - 1.96 * sev
    out$ci_upper <- out$estimate + 1.96 * sev
    attr(out, "se_type") <- s$type
  }
  out
}

#' Cluster membership of every group
#'
#' @param fit An `mmgsem` fit.
#' @return Data frame: group, modal cluster, its posterior probability, and
#'   the posterior probabilities of all clusters (`p1`, `p2`, ...).
#' @export
tidy_clusters <- function(fit) {
  post <- fit$posteriors
  p <- as.matrix(post[, -1, drop = FALSE])
  modal <- as.matrix(fit$modal_post[, -1, drop = FALSE])
  cl <- apply(modal, 1, which.max)
  out <- data.frame(
    group = as.character(post$Group),
    cluster = as.integer(cl),
    probability = p[cbind(seq_len(nrow(p)), cl)],
    stringsAsFactors = FALSE
  )
  colnames(p) <- paste0("p", seq_len(ncol(p)))
  cbind(out, as.data.frame(p))
}

#' Model-selection table and the choice of each criterion
#'
#' @param ms Result of [assemble_selection()] or [mmgsem::ModelSelection()].
#' @return List with `table` (observed-data criteria per number of clusters)
#'   and `picks` (named integer vector: criterion -> selected number of clusters).
#' @export
tidy_selection <- function(ms) {
  ov <- ms$Overview
  tab <- ov[, c("Clusters", "LL", "nrpar", "Chull", "BIC_G", "BIC_N", "AIC",
                "AIC3", "ICL", "R2entropy")]
  pick <- function(x, maximize = FALSE) {
    i <- if (maximize) which.max(x) else which.min(x)
    if (!length(i) || is.na(x[i])) NA_integer_ else as.integer(ov$Clusters[i])
  }
  picks <- c(
    Chull = pick(ov$Chull, TRUE), BIC_G = pick(ov$BIC_G), BIC_N = pick(ov$BIC_N),
    AIC = pick(ov$AIC), AIC3 = pick(ov$AIC3), ICL = pick(ov$ICL)
  )
  list(table = tab, picks = picks)
}

#' Number of clusters supported by most criteria
#'
#' Ties are broken in favour of AIC3 and then the convex hull, the two
#' measures the tutorial recommends inspecting.
#' @keywords internal
suggest_k <- function(picks) {
  p <- picks[!is.na(picks)]
  if (!length(p)) return(NA_integer_)
  tab <- table(p)
  top <- as.integer(names(tab)[tab == max(tab)])
  if (length(top) == 1) return(top)
  for (crit in c("AIC3", "Chull")) {
    if (!is.na(picks[crit]) && picks[crit] %in% top) return(as.integer(picks[crit]))
  }
  min(top)
}

#' Wald test per regression: is it equal across all clusters?
#'
#' @param fit An `mmgsem` fit with at least two clusters.
#' @param se Result of [mmgsem::compute_se()].
#' @return Data frame: parameter, Wald statistic, degrees of freedom, p value
#'   (NA for a regression with a missing standard error).
#' @export
tidy_wald <- function(fit, se) {
  idx <- beta_index(fit)
  est <- se$estimates_vector$betas_est
  V <- se$vcov_betas
  k <- length(beta_list(fit))
  if (k < 2) stop("Comparing clusters needs at least two clusters.", call. = FALSE)
  R <- matrix(0, nrow = k - 1, ncol = k)
  for (i in seq_len(k - 1)) {
    R[i, i] <- 1
    R[i, i + 1] <- -1
  }
  rows <- lapply(seq_len(nrow(idx)), function(b) {
    nm <- paste0(idx$response[b], "~", idx$predictor[b], ".k", seq_len(k))
    d <- R %*% matrix(est[nm])
    M <- R %*% V[nm, nm] %*% t(R)
    # compute_se() returns NA where it could not estimate a variance; the test
    # of that regression is then not available.
    W <- if (anyNA(d) || anyNA(M)) NA_real_ else as.numeric(t(d) %*% MASS_ginv(M) %*% d)
    data.frame(parameter = paste(idx$response[b], "~", idx$predictor[b]),
               wald = W, df = k - 1L,
               p = stats::pchisq(W, k - 1L, lower.tail = FALSE),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Moore-Penrose inverse, as MASS::ginv (kept local to avoid a dependency).
MASS_ginv <- function(X, tol = sqrt(.Machine$double.eps)) {
  s <- svd(X)
  pos <- s$d > max(tol * s$d[1L], 0)
  if (all(pos)) {
    s$v %*% (1 / s$d * t(s$u))
  } else if (!any(pos)) {
    array(0, dim(X)[2L:1L])
  } else {
    s$v[, pos, drop = FALSE] %*% ((1 / s$d[pos]) * t(s$u[, pos, drop = FALSE]))
  }
}

#' Pairwise comparison of each regression between clusters
#'
#' z tests on the difference between two clusters, with the Bonferroni-
#' corrected alpha level reported by `test.mmgsem(multiple_comparison = TRUE)`.
#' @param fit An `mmgsem` fit with at least two clusters.
#' @param se Result of [mmgsem::compute_se()].
#' @param alpha Family-wise alpha level before correction.
#' @return Data frame with one row per regression and pair of clusters; the
#'   corrected alpha level is in `attr(, "alpha")`.
#' @export
tidy_pairwise <- function(fit, se, alpha = 0.05) {
  idx <- beta_index(fit)
  est <- se$estimates_vector$betas_est
  sev <- se_vector(se)$se
  k <- length(beta_list(fit))
  if (k < 2) stop("Comparing clusters needs at least two clusters.", call. = FALSE)
  pairs <- t(utils::combn(k, 2))
  n_tests <- nrow(pairs) * nrow(idx)
  new_alpha <- alpha / n_tests
  rows <- lapply(seq_len(nrow(idx)), function(b) {
    base <- paste0(idx$response[b], "~", idx$predictor[b], ".k")
    do.call(rbind, lapply(seq_len(nrow(pairs)), function(i) {
      a <- paste0(base, pairs[i, 1])
      c2 <- paste0(base, pairs[i, 2])
      d <- unname(est[a] - est[c2])
      z <- d / sqrt(unname(sev[a])^2 + unname(sev[c2])^2)
      p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
      data.frame(parameter = paste(idx$response[b], "~", idx$predictor[b]),
                 cluster_a = pairs[i, 1], cluster_b = pairs[i, 2],
                 difference = d, z = z, p = p, significant = !is.na(p) & p < new_alpha,
                 stringsAsFactors = FALSE)
    }))
  })
  out <- do.call(rbind, rows)
  attr(out, "alpha") <- new_alpha
  attr(out, "n_tests") <- n_tests
  out
}

#' Headline numbers of a fitted model
#' @keywords internal
fit_headline <- function(fit) {
  sd <- start_diagnostics(fit)
  list(
    k = n_clusters(fit),
    nstarts = fit$nstarts,
    iterations = fit$iterations,
    entropy = fit$model_sel$R2_entropy,
    loglik_step2 = fit$logLik$loglik,
    loglik_observed = fit$logLik$obs_loglik,
    nrpar = fit$NrPar$Obs.nrpar,
    n_best = sd$n_best,
    n_starts = sd$n_starts
  )
}

#' Explained variance of each endogenous factor per group
#' @keywords internal
tidy_r2 <- function(fit) {
  r2 <- fit$R2
  cl <- tidy_clusters(fit)
  out <- data.frame(group = colnames(r2), cluster = cl$cluster[match(colnames(r2), cl$group)],
                    stringsAsFactors = FALSE)
  for (f in rownames(r2)) out[[f]] <- as.numeric(r2[f, ])
  out
}

#' The package's own printed output, verbatim
#' @keywords internal
verbatim <- function(expr) {
  out <- tryCatch(utils::capture.output(force(expr)), error = function(e) {
    paste("Error:", conditionMessage(e))
  })
  paste(out, collapse = "\n")
}
