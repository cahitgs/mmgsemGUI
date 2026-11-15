# Model specification and the measurement model (Step 1) ----------------------

#' Write lavaan syntax for the measurement model
#' @param factors Named list: factor name -> character vector of items.
#' @return A lavaan syntax string, one line per factor.
#' @export
build_s1 <- function(factors) {
  factors <- factors[lengths(factors) > 0]
  if (!length(factors)) return("")
  paste(vapply(names(factors), function(f) {
    paste0(f, " =~ ", paste(factors[[f]], collapse = " + "))
  }, character(1)), collapse = "\n")
}

#' Write lavaan syntax for the structural model
#' @param regs Named list: outcome factor -> character vector of predictors.
#' @export
build_s2 <- function(regs) {
  regs <- regs[lengths(regs) > 0]
  if (!length(regs)) return("")
  paste(vapply(names(regs), function(y) {
    paste0(y, " ~ ", paste(regs[[y]], collapse = " + "))
  }, character(1)), collapse = "\n")
}

lavaanify_quiet <- function(syntax) {
  suppressWarnings(lavaan::lavaanify(syntax, auto = FALSE))
}

#' Read the factors and their items from measurement-model syntax
#' @param syntax lavaan syntax containing `=~` lines.
#' @return Named list factor -> items (in the order written), or an error.
#' @export
parse_s1 <- function(syntax) {
  if (!nzchar(trimws(syntax))) stop("The measurement model is empty.", call. = FALSE)
  pt <- lavaanify_quiet(syntax)
  pt <- pt[pt$op == "=~", , drop = FALSE]
  if (!nrow(pt)) stop("No factor definitions (=~) found.", call. = FALSE)
  lvs <- unique(pt$lhs)
  stats::setNames(lapply(lvs, function(f) pt$rhs[pt$lhs == f]), lvs)
}

#' Read the regressions from structural-model syntax
#' @param syntax lavaan syntax containing `~` lines.
#' @return Named list outcome -> predictors.
#' @export
parse_s2 <- function(syntax) {
  if (!nzchar(trimws(syntax))) stop("The structural model is empty.", call. = FALSE)
  pt <- lavaanify_quiet(syntax)
  pt <- pt[pt$op == "~", , drop = FALSE]
  if (!nrow(pt)) stop("No regressions (~) found.", call. = FALSE)
  ys <- unique(pt$lhs)
  stats::setNames(lapply(ys, function(y) pt$rhs[pt$lhs == y]), ys)
}

#' Loadings that may be released across groups
#'
#' The first item of each factor is the marker (its loading is fixed to 1), so
#' it cannot be specified as non-invariant.
#' @keywords internal
free_loadings <- function(factors) {
  unlist(lapply(names(factors), function(f) {
    it <- factors[[f]]
    if (length(it) < 2) return(character())
    paste0(f, " =~ ", it[-1])
  }), use.names = FALSE)
}

#' Check a structural model against the factors of the measurement model
#' @return Character vector of problem codes (empty when the model is usable).
#' @keywords internal
check_s2 <- function(regs, lvs) {
  problems <- character()
  regs <- regs[lengths(regs) > 0]
  if (!length(regs)) return("empty")
  used <- unique(c(names(regs), unlist(regs)))
  if (length(setdiff(used, lvs))) problems <- c(problems, "unknown_factor")
  if (any(vapply(names(regs), function(y) y %in% regs[[y]], logical(1)))) {
    problems <- c(problems, "self")
  }
  if (is.null(node_depth(regs, lvs))) problems <- c(problems, "cycle")
  problems
}

#' Depth of each factor in the structural model (0 = exogenous)
#' @return Named integer vector, or NULL when the model contains a cycle.
#' @keywords internal
node_depth <- function(regs, lvs) {
  regs <- regs[lengths(regs) > 0]
  nodes <- unique(c(lvs, names(regs), unlist(regs)))
  depth <- stats::setNames(rep(NA_integer_, length(nodes)), nodes)
  depth[setdiff(nodes, names(regs))] <- 0L
  for (i in seq_along(nodes)) {
    changed <- FALSE
    for (y in names(regs)) {
      if (is.na(depth[y]) && !anyNA(depth[regs[[y]]])) {
        depth[y] <- max(depth[regs[[y]]]) + 1L
        changed <- TRUE
      }
    }
    if (!changed) break
  }
  if (anyNA(depth)) return(NULL)
  depth
}

#' Split a measurement model into one block per factor
#'
#' Measurement blocks (Rosseel & Loh, 2024; Appendix A of the tutorial)
#' estimate each factor in its own multigroup CFA, so that a misspecification
#' in one factor cannot bias the others. That is only possible when the model
#' consists of plain factor definitions without cross-loadings.
#' @param S1 Measurement-model syntax.
#' @return List of syntax strings, one per factor; an error when the model
#'   cannot be split.
#' @keywords internal
split_blocks <- function(S1) {
  pt <- lavaanify_quiet(S1)
  pt <- pt[pt$user == 1L, , drop = FALSE]      # what the user wrote
  plain <- nrow(pt) > 0 && all(pt$op == "=~") && all(is.na(pt$ustart)) && all(pt$label == "") &&
    !anyDuplicated(pt$rhs)
  if (!plain) {
    stop("Measurement blocks need a model of plain factor definitions without cross-loadings.",
         call. = FALSE)
  }
  f <- parse_s1(S1)
  lapply(names(f), function(n) build_s1(f[n]))
}

#' Estimate the measurement model with multigroup CFA
#'
#' Mirrors the tutorial: loadings are constrained to be equal across groups
#' (metric invariance) and the listed loadings are released (partial metric
#' invariance; Section 4.2.1). Optionally incomplete rows are used with
#' full-information maximum likelihood (Section 4.2.1), each factor is
#' estimated in its own block (Appendix A), or the items are treated as
#' ordinal (Appendix B).
#' @param data Data frame prepared by [prepare_data()].
#' @param S1 Measurement-model syntax.
#' @param group Name of the grouping variable.
#' @param noninv Loadings allowed to differ across groups, e.g. `"F1 =~ V2"`.
#' @param level `"metric"` (default) or `"configural"` (no equality constraints
#'   on the loadings).
#' @param se Passed to [lavaan::cfa()]. MMG-SEM only uses the estimates of the
#'   measurement model, so its standard errors are skipped by default; with 24
#'   groups this makes the step about four times faster and leaves the
#'   estimates and fit indices unchanged. Not used for ordinal items, whose
#'   scaled test statistic needs them.
#' @param missing `"listwise"` or `"fiml"`.
#' @param ordinal Treat the items as ordinal: multigroup categorical CFA with
#'   the Wu and Estabrook (2016) identification (delta parameterization),
#'   thresholds and loadings equal across groups.
#' @param blocks Estimate one block per factor (see [split_blocks()]).
#' @return A fitted lavaan object, or a list of them when `blocks = TRUE`.
#' @export
fit_measurement <- function(data, S1, group, noninv = character(),
                            level = c("metric", "configural"), se = "none",
                            missing = c("listwise", "fiml"), ordinal = FALSE,
                            blocks = FALSE) {
  level <- match.arg(level)
  missing <- match.arg(missing)
  if (ordinal && (blocks || missing == "fiml")) {
    stop("Ordinal items cannot be combined with measurement blocks or FIML.", call. = FALSE)
  }
  if (blocks) {
    return(lapply(split_blocks(S1), function(block) {
      f <- names(parse_s1(block))
      fit_measurement(data, block, group, noninv[startsWith(noninv, paste0(f, " =~ "))],
                      level = level, se = se, missing = missing)
    }))
  }
  args <- list(model = S1, data = data, group = group)
  if (ordinal) {
    if (level != "metric") stop("Only the metric model is available for ordinal items.", call. = FALSE)
    equal <- c("thresholds", "loadings")
    args$model <- ordinal_syntax(S1, data, group, equal, noninv)
    args$group.equal <- equal
    if (length(noninv)) args$group.partial <- noninv
    return(do.call(lavaan::cfa, args))
  }
  args$se <- se
  if (missing == "fiml") args$missing <- "fiml"
  if (level == "metric") {
    args$group.equal <- "loadings"
    if (length(noninv)) args$group.partial <- noninv
  }
  do.call(lavaan::cfa, args)
}

#' Measurement-model syntax for ordinal items (Appendix B of the tutorial)
#' @keywords internal
ordinal_syntax <- function(S1, data, group, equal = c("thresholds", "loadings"),
                           partial = character()) {
  args <- list(configural.model = S1, dat = data, parameterization = "delta", ordered = TRUE,
               ID.fac = "std.lv", ID.cat = "Wu", group = group, group.equal = equal)
  # An empty vector here makes measEq.syntax() return an empty model.
  if (length(partial)) args$group.partial <- partial
  as.character(do.call(semTools::measEq.syntax, args))
}

is_block_list <- function(fit) is.list(fit) && !methods::is(fit, "lavaan")

#' Fit indices and convergence information of a measurement model
#'
#' For ordinal items the scaled test statistic and the fit indices based on
#' it are reported. For measurement blocks the result has one entry per block.
#' @keywords internal
measurement_summary <- function(fit) {
  if (is_block_list(fit)) {
    parts <- lapply(fit, measurement_summary)
    return(list(
      converged = all(vapply(parts, function(p) isTRUE(p$converged), logical(1))),
      post_check = all(vapply(parts, function(p) !identical(p$post_check, FALSE), logical(1))),
      blocks = parts,
      fit = NULL,
      scaled = FALSE,
      ngroups = parts[[1]]$ngroups,
      nobs = parts[[1]]$nobs
    ))
  }
  conv <- isTRUE(lavaan::lavInspect(fit, "converged"))
  idx <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "npar")
  scaled <- FALSE
  fm <- NULL
  if (conv) {
    # Only the indices that are shown are requested: computing every fit measure
    # lavaan knows takes most of a minute for a model with many groups.
    test <- tryCatch(lavaan::lavInspect(fit, "options")$test, error = function(e) "standard")
    robust <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled",
                "rmsea.scaled", "srmr", "npar")
    get_fm <- function(names) tryCatch(lavaan::fitMeasures(fit, names), error = function(e) NULL)
    if (any(test != "standard")) {
      got <- get_fm(robust)
      if (!is.null(got) && all(robust %in% names(got)) && !is.na(got[["chisq.scaled"]])) {
        scaled <- TRUE
        fm <- stats::setNames(unname(got[robust]), idx)
      }
    }
    if (is.null(fm)) {
      got <- get_fm(idx)
      if (!is.null(got) && all(idx %in% names(got))) fm <- stats::setNames(unname(got[idx]), idx)
    }
  }
  if (is.null(fm)) fm <- stats::setNames(rep(NA_real_, length(idx)), idx)
  list(
    converged = conv,
    fit = as.list(fm),
    scaled = scaled,
    factors = lavaan::lavNames(fit, "lv"),
    ngroups = lavaan::lavInspect(fit, "ngroups"),
    nobs = sum(lavaan::lavInspect(fit, "nobs")),
    post_check = tryCatch(suppressWarnings(lavaan::lavInspect(fit, "post.check")),
                          error = function(e) NA)
  )
}

#' Rank loadings by how strongly the data ask for releasing them
#'
#' Uses lavaan's score tests on the equality constraints of a metric model and
#' adds them up per loading. This is a screening aid, not a formal invariance
#' test: the user decides which loadings to release.
#' @param fit A metric (or partial metric) multigroup CFA.
#' @return Data frame: loading, total score statistic, number of constraints.
#' @keywords internal
loading_screen <- function(fit, fast = TRUE) {
  if (is_block_list(fit)) {
    res <- do.call(rbind, lapply(fit, loading_screen, fast = fast))
    res <- res[order(-res$score), , drop = FALSE]
    rownames(res) <- NULL
    return(res)
  }
  uni <- if (fast) tryCatch(score_tests_fast(fit), error = function(e) NULL)
  if (is.null(uni)) uni <- lavaan::lavTestScore(fit, cumulative = FALSE)$uni
  pt <- lavaan::parTable(fit)
  load <- pt[pt$op == "=~" & pt$free > 0, , drop = FALSE]
  key <- stats::setNames(paste(load$lhs, "=~", load$rhs), load$plabel)
  uni$loading <- key[uni$lhs]
  uni <- uni[!is.na(uni$loading), , drop = FALSE]
  if (!nrow(uni)) return(data.frame(loading = character(), score = numeric(), constraints = integer()))
  res <- stats::aggregate(uni$X2, by = list(loading = uni$loading), FUN = sum)
  names(res)[2] <- "score"
  res$constraints <- as.integer(table(uni$loading)[res$loading])
  res <- res[order(-res$score), , drop = FALSE]
  rownames(res) <- NULL
  res
}

#' Univariate score tests of all equality constraints, computed at once
#'
#' The same statistics as the univariate tests of [lavaan::lavTestScore()]:
#' for constraint r, n s' J_r s, where s is the score, and J_r the inverse of
#' the information matrix restricted to the parameter space that satisfies all
#' constraints except r. lavaan builds J_r from scratch for every constraint,
#' which takes about an hour for the tutorial example (24 groups, 368
#' constraints). Here the restricted inverse for all constraints is factorised
#' once and each J_r follows from adding the one direction that constraint r
#' excludes (a bordered-matrix update), which takes seconds. The result is
#' checked against lavaan in the tests.
#'
#' @param fit A multigroup lavaan fit with explicit equality constraints.
#' @return Data frame with `lhs`, `op`, `rhs` (parameter labels), `X2`, `df`
#'   and `p.value`, in the order of the constraints.
#' @keywords internal
score_tests_fast <- function(fit) {
  R <- fit@Model@ceq.JAC
  m <- nrow(R)
  p <- ncol(R)
  pt <- lavaan::parTable(fit)
  eq <- which(pt$op == "==")
  if (m == 0 || length(eq) != m) stop("no explicit equality constraints")
  if (fit@Data@nlevels != 1L || length(fit@Data@sampling.weights) > 0L ||
      !identical(fit@Model@estimator, "ML")) {
    stop("unsupported model")
  }
  n <- fit@SampleStats@ntotal
  s <- as.numeric(lavaan::lavTech(fit, "gradient.logl"))
  info <- lavaan::lavTech(fit, "information.expected")
  qr_r <- qr(t(R))
  if (qr_r$rank < m) stop("redundant constraints")
  Z <- qr.Q(qr_r, complete = TRUE)[, seq.int(m + 1L, p), drop = FALSE]   # satisfies all constraints
  Q <- t(R) %*% solve(tcrossprod(R))        # column r: the direction constraint r excludes
  H <- crossprod(Z, info %*% Z)
  IQ <- info %*% Q
  C <- crossprod(Z, IQ)
  HC <- solve(H, C)
  kappa <- colSums(Q * IQ) - colSums(C * HC)
  a <- crossprod(Z, s)
  Ha <- solve(H, a)
  b <- crossprod(Q, s)
  x2 <- as.numeric(n * (sum(a * Ha) + (b - crossprod(C, Ha))^2 / kappa))
  data.frame(lhs = pt$lhs[eq], op = "==", rhs = pt$rhs[eq], X2 = x2, df = 1,
             p.value = stats::pchisq(x2, 1, lower.tail = FALSE), stringsAsFactors = FALSE)
}

#' Loadings per group (to show how much the released loadings vary)
#' @keywords internal
loading_ranges <- function(fit) {
  if (is_block_list(fit)) return(do.call(rbind, lapply(fit, loading_ranges)))
  est <- lavaan::lavInspect(fit, "est")
  lam <- lapply(est, function(e) e$lambda)
  first <- lam[[1]]
  idx <- which(first != 0, arr.ind = TRUE)
  out <- data.frame(
    loading = paste(colnames(first)[idx[, 2]], "=~", rownames(first)[idx[, 1]]),
    min = NA_real_, max = NA_real_, stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(idx))) {
    v <- vapply(lam, function(m) m[idx[i, 1], idx[i, 2]], numeric(1))
    out$min[i] <- min(v)
    out$max[i] <- max(v)
  }
  out$varies <- (out$max - out$min) > 1e-8
  out
}
