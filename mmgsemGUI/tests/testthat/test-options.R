# The three ways of estimating the measurement model that the tutorial describes
# besides the default: full-information maximum likelihood for missing answers
# (Section 4.2.1), measurement blocks (Appendix A) and ordinal items (Appendix B).
# Each is compared with mmgsem and lavaan called directly, and the exported
# script is run to check that it reproduces the result.

small <- function() {
  d <- example_data()
  d[d$group %in% 1:8, c(paste0("V", 1:10), "group")]
}
S1 <- "F1 =~ V1 + V2 + V3 + V4 + V5\nF2 =~ V6 + V7 + V8 + V9 + V10"
S2 <- "F2 ~ F1"
noninv <- c("F1 =~ V2", "F2 =~ V7")
items <- paste0("V", 1:10)

run_script <- function(spec, data) {
  td <- tempfile("script_")
  dir.create(td)
  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)
  utils::write.csv(data, "data.csv", row.names = FALSE)
  app <- mmgsemGUI:::read_data_file("data.csv", "data.csv")
  spec$data_source <- "data.csv"
  spec$data_reader <- attr(app, "reader")
  script <- make_script(spec)
  env <- new.env()
  invisible(utils::capture.output(suppressMessages(eval(parse(text = script), env))))
  env
}

test_that("measurement blocks split a plain model and refuse anything else", {
  expect_identical(mmgsemGUI:::split_blocks(S1),
                   list("F1 =~ V1 + V2 + V3 + V4 + V5", "F2 =~ V6 + V7 + V8 + V9 + V10"))
  expect_error(mmgsemGUI:::split_blocks("F1 =~ V1 + V2 + V3\nF2 =~ V3 + V4 + V5"), "cross-loadings")
  expect_error(mmgsemGUI:::split_blocks("F1 =~ V1 + a*V2 + V3"), "plain")
  expect_error(mmgsemGUI:::split_blocks("F1 =~ V1 + V2 + V3\nV1 ~~ V2"), "plain")
})

test_that("incomplete rows are kept for FIML", {
  df <- data.frame(g = c("a", "a", NA, "b", "b"), x = c(1, NA, 3, NA, 5), y = c(1, 2, 3, NA, 5))
  pd <- prepare_data(df, "g", c("x", "y"), missing = "fiml")
  expect_equal(nrow(pd$data), 3)        # no group, and no answers at all, are left out
  expect_equal(pd$n_removed, 2)
  expect_equal(pd$n_incomplete, 1)
  expect_equal(prepare_data(df, "g", c("x", "y"))$n_incomplete, 0)
})

test_that("the loadings are held equal, which mmgsem estimating Step 1 itself does not do (Appendix D)", {
  skip_on_cran()
  d <- small()
  mm <- fit_measurement(d, S1, "group")
  app <- fit_clusters(d, S1, S2, "group", k = 2, s1_fit = mm, nstarts = 5)
  alone <- function(...) {
    suppressMessages(mmgsem::mmgsem(dat = d, S1 = S1, S2 = S2, group = "group", nclus = 2,
                                    seed = 1, nstarts = 5, s1_type = "lavaan", ...))
  }
  equal <- alone(group.equal = "loadings")
  expect_equal(app$logLik$obs_loglik, equal$logLik$obs_loglik)
  expect_equal(app$NrPar$Obs.nrpar, equal$NrPar$Obs.nrpar)
  # As printed in Appendix D, without group.equal, the model is configural:
  # eight loadings are free in each of the seven other groups.
  as_printed <- alone()
  expect_equal(as_printed$NrPar$Obs.nrpar - app$NrPar$Obs.nrpar, 8 * 7)
  expect_gt(diff(range(vapply(as_printed$param$lambda, function(m) m["V3", "F1"], numeric(1)))), 0.1)
  expect_lt(diff(range(vapply(app$param$lambda, function(m) m["V3", "F1"], numeric(1)))), 1e-8)
})

test_that("measurement blocks agree with mmgsem called directly, and the script reproduces them", {
  skip_on_cran()
  d <- small()
  mm <- fit_measurement(d, S1, "group", noninv, blocks = TRUE)
  expect_length(mm, 2)
  s <- mmgsemGUI:::measurement_summary(mm)
  expect_true(s$converged)
  expect_length(s$blocks, 2)
  expect_identical(s$blocks[[2]]$factors, "F2")
  parts <- mmgsemGUI:::split_blocks(S1)
  direct_mm <- list(
    lavaan::cfa(parts[[1]], data = d, group = "group", group.equal = "loadings", group.partial = "F1 =~ V2"),
    lavaan::cfa(parts[[2]], data = d, group = "group", group.equal = "loadings", group.partial = "F2 =~ V7")
  )
  expect_equal(lavaan::coef(mm[[1]]), lavaan::coef(direct_mm[[1]]), tolerance = 1e-8)
  fit <- fit_clusters(d, parts, S2, "group", k = 2, s1_fit = mm, nstarts = 5)
  direct <- suppressMessages(mmgsem::mmgsem(dat = d, S1 = parts, S2 = S2, group = "group", nclus = 2,
                                            seed = 1, nstarts = 5, s1_fit = direct_mm))
  expect_equal(tidy_betas(fit)$estimate, tidy_betas(direct)$estimate, tolerance = 1e-6)
  expect_identical(tidy_clusters(fit)$cluster, tidy_clusters(direct)$cluster)
  expect_equal(nrow(mmgsemGUI:::loading_ranges(mm)), 10)
  expect_false(any(mmgsemGUI:::loading_screen(mm)$loading %in% noninv))
  env <- run_script(list(group = "group", items = items, S1 = S1, noninv = noninv, S2 = S2,
                         k_min = 2, k_max = 2, seed = 1, nstarts = 5, blocks = TRUE), d)
  expect_equal(tidy_betas(env$selected_fit)$estimate, tidy_betas(fit)$estimate, tolerance = 1e-6)
})

test_that("ordinal items follow the ordinal route of mmgsem, and the script reproduces them", {
  skip_on_cran()
  d <- small()
  for (v in items) d[[v]] <- as.integer(cut(d[[v]], breaks = c(-Inf, -0.8, 0, 0.8, Inf)))
  mm <- fit_measurement(d, S1, "group", noninv, ordinal = TRUE)
  s <- mmgsemGUI:::measurement_summary(mm)
  expect_true(s$converged)
  expect_true(s$scaled)                 # fit indices from the scaled test statistic
  expect_identical(lavaan::lavInspect(mm, "options")$estimator, "DWLS")
  fit <- suppressWarnings(fit_clusters(d, S1, S2, "group", k = 2, s1_fit = mm, nstarts = 5,
                                       ordinal = TRUE, noninv = noninv))
  # the same result as when mmgsem estimates the ordinal measurement model itself
  own <- suppressWarnings(suppressMessages(mmgsem::mmgsem(
    dat = d, S1 = S1, S2 = S2, group = "group", nclus = 2, seed = 1, nstarts = 5,
    ordered = TRUE, group.equal = c("thresholds", "loadings"), group.partial = noninv
  )))
  expect_equal(tidy_betas(fit)$estimate, tidy_betas(own)$estimate, tolerance = 1e-5)
  expect_equal(fit$logLik$obs_loglik, own$logLik$obs_loglik, tolerance = 1e-6)
  expect_identical(tidy_clusters(fit)$cluster, tidy_clusters(own)$cluster)
  env <- suppressWarnings(run_script(list(group = "group", items = items, S1 = S1, noninv = noninv,
                                          S2 = S2, k_min = 2, k_max = 2, seed = 1, nstarts = 5,
                                          ordinal = TRUE), d))
  expect_equal(tidy_betas(env$selected_fit)$estimate, tidy_betas(fit)$estimate, tolerance = 1e-5)
  expect_error(fit_measurement(d, S1, "group", level = "configural", ordinal = TRUE), "metric")
})

test_that("the options work when no loading, or not every factor, has a released loading", {
  skip_on_cran()
  d <- small()
  # blocks in which only the first factor has a released loading
  mm <- fit_measurement(d, S1, "group", "F1 =~ V2", blocks = TRUE)
  fit <- fit_clusters(d, mmgsemGUI:::split_blocks(S1), S2, "group", k = 2, s1_fit = mm, nstarts = 3)
  env <- run_script(list(group = "group", items = items, S1 = S1, noninv = "F1 =~ V2", S2 = S2,
                         k_min = 2, k_max = 2, seed = 1, nstarts = 3, blocks = TRUE), d)
  expect_equal(tidy_betas(env$selected_fit)$estimate, tidy_betas(fit)$estimate, tolerance = 1e-6)
  # ordinal items without any released loading
  for (v in items) d[[v]] <- as.integer(cut(d[[v]], breaks = c(-Inf, -0.8, 0, 0.8, Inf)))
  mmo <- fit_measurement(d, S1, "group", ordinal = TRUE)
  expect_true(mmgsemGUI:::measurement_summary(mmo)$converged)
  fo <- suppressWarnings(fit_clusters(d, S1, S2, "group", k = 2, s1_fit = mmo, nstarts = 3, ordinal = TRUE))
  env <- suppressWarnings(run_script(list(group = "group", items = items, S1 = S1, noninv = character(),
                                          S2 = S2, k_min = 2, k_max = 2, seed = 1, nstarts = 3,
                                          ordinal = TRUE), d))
  expect_equal(tidy_betas(env$selected_fit)$estimate, tidy_betas(fo)$estimate, tolerance = 1e-5)
})

test_that("FIML agrees with mmgsem called directly, and the script reproduces it", {
  skip_on_cran()
  d <- small()
  set.seed(8)
  for (v in items) d[sample(nrow(d), 40), v] <- NA
  pd <- prepare_data(d, "group", items, missing = "fiml")
  expect_gt(pd$n_incomplete, 0)
  expect_equal(pd$n_removed, 0)
  mm <- fit_measurement(pd$data, S1, "group", noninv, missing = "fiml")
  expect_equal(sum(lavaan::lavInspect(mm, "nobs")), nrow(pd$data))
  direct_mm <- lavaan::cfa(S1, data = pd$data, group = "group", group.equal = "loadings",
                           group.partial = noninv, missing = "fiml")
  expect_equal(lavaan::coef(mm), lavaan::coef(direct_mm), tolerance = 1e-6)
  fit <- fit_clusters(pd$data, S1, S2, "group", k = 2, s1_fit = mm, nstarts = 5, missing = "fiml")
  direct <- suppressMessages(mmgsem::mmgsem(dat = pd$data, S1 = S1, S2 = S2, group = "group", nclus = 2,
                                            seed = 1, nstarts = 5, s1_fit = direct_mm, missing = "fiml"))
  expect_equal(tidy_betas(fit)$estimate, tidy_betas(direct)$estimate, tolerance = 1e-5)
  expect_equal(fit$logLik$obs_loglik, direct$logLik$obs_loglik, tolerance = 1e-6)
  env <- run_script(list(group = "group", items = items, S1 = S1, noninv = noninv, S2 = S2,
                         k_min = 2, k_max = 2, seed = 1, nstarts = 5, missing = "fiml"), d)
  expect_equal(nrow(env$data), nrow(pd$data))
  expect_equal(tidy_betas(env$selected_fit)$estimate, tidy_betas(fit)$estimate, tolerance = 1e-5)
  expect_equal(env$selected_fit$logLik$obs_loglik, fit$logLik$obs_loglik, tolerance = 1e-6)
})
