# The interface must show exactly what the mmgsem package computes. These tests
# compare the tables of the interface with the output printed in the tutorial
# (Perez Alonso et al., 2026, Output Boxes 1, 2 and 5).
#
# The fixture holds the six models of the tutorial example as estimated by
# mmgsem (seed 1, 20 random starts), without the large measurement-model object.

fx <- readRDS(test_path("fixtures", "tutorial_models.rds"))
ms <- assemble_selection(unname(fx$models))
fit4 <- mmgsem::extract(ms, 4)

test_that("the model-selection table equals ModelSelection() and Output Box 1", {
  expect_equal(ms$Overview, fx$overview, ignore_attr = TRUE)
  ov <- ms$Overview
  expect_equal(round(ov$LL, 1), c(-108619.5, -108582.1, -108556.3, -108536.5, -108532.9, -108530.1))
  expect_equal(ov$nrpar, c(712, 717, 722, 727, 732, 737))
  expect_equal(round(ov$Chull, 3), c(NA, 1.455, 1.294, 5.605, 1.246, NA))
  expect_equal(round(ov$BIC_G, 1), c(219501.8, 219442.8, 219407.3, 219383.4, 219392.2, 219402.4))
  expect_equal(round(ov$BIC_N, 1), c(223274.2, 223241.7, 223232.6, 223235.3, 223270.5, 223307.2))
  expect_equal(round(ov$AIC, 1), c(218663.0, 218598.1, 218556.7, 218526.9, 218529.8, 218534.1))
  expect_equal(round(ov$AIC3, 1), c(219375.0, 219315.1, 219278.7, 219253.9, 219261.8, 219271.1))
  expect_equal(round(ov$ICL, 1), c(219501.8, 219443.8, 219407.4, 219383.8, 219393.1, 219403.4))
})

test_that("each criterion selects the model reported in the tutorial", {
  picks <- tidy_selection(ms)$picks
  expect_equal(picks, c(Chull = 4L, BIC_G = 4L, BIC_N = 3L, AIC = 4L, AIC3 = 4L, ICL = 4L))
  expect_equal(mmgsemGUI:::suggest_k(picks), 4L)
})

test_that("coefficients and clusters equal Output Box 2", {
  b <- tidy_betas(fit4)
  expect_equal(b$parameter[1:4], c("F3 ~ F1", "F4 ~ F1", "F3 ~ F2", "F4 ~ F3"))
  expect_equal(round(b$estimate, 3), c(
    0.025, 0.330, 0.305, 0.308,
    0.318, -0.033, 0.276, 0.295,
    0.315, 0.316, 0.278, 0.019,
    0.278, 0.311, 0.004, 0.357
  ))
  cl <- tidy_clusters(fit4)
  expect_equal(cl$group, as.character(1:24))
  expect_equal(cl$cluster, rep(c(2L, 1L, 4L, 3L), each = 6))
  h <- mmgsemGUI:::fit_headline(fit4)
  expect_equal(h$iterations, 12)
  expect_equal(round(h$entropy, 7), 0.9931549)
  expect_equal(round(h$loglik_step2, 1), -22777.6)
  expect_equal(round(h$loglik_observed, 1), -108536.5)
})

test_that("random-start diagnostics count the starts at the best solution", {
  d <- start_diagnostics(fit4)
  expect_equal(d$n_starts, 20)
  expect_equal(d$n_best, 20)
  d3 <- start_diagnostics(mmgsem::extract(ms, 3))
  expect_lt(d3$n_best, d3$n_starts)
})

test_that("tests computed from the package's standard errors equal its own output", {
  se <- mmgsem::standard_errors
  expect_true(mmgsemGUI:::se_matches_fit(se, fit4))
  b <- tidy_betas(fit4, se)
  expect_identical(attr(b, "se_type"), "corrected")
  expect_equal(b$se, unname(se$SE_vector$betas_se_corrected), tolerance = 1e-12)
  expect_equal(b$z, b$estimate / b$se)
  # Output Box 5 (Wald test per regression)
  w <- tidy_wald(fit4, se)
  expect_equal(round(w$wald, 3), c(64.732, 95.995, 48.835, 75.843))
  expect_equal(w$df, rep(3L, 4))
  # test.mmgsem(multiple_comparison = TRUE)
  pw <- tidy_pairwise(fit4, se)
  expect_equal(attr(pw, "n_tests"), 24)
  expect_equal(attr(pw, "alpha"), 0.05 / 24)
  expect_equal(round(pw$difference[1:3], 3), c(-0.294, -0.290, -0.253))
  expect_equal(round(pw$z[1:3], 3), c(-6.721, -7.055, -6.058))
  printed <- utils::capture.output(mmgsem::test.mmgsem(model = fit4, se = se, multiple_comparison = TRUE))
  expect_equal(sum(grepl("\\*\\s*$", printed)), sum(pw$significant))
})

test_that("standard errors of another solution are not attached to this one", {
  expect_false(mmgsemGUI:::se_matches_fit(mmgsem::standard_errors, mmgsem::extract(ms, 3)))
})

test_that("the figures can be drawn", {
  regs <- list(F3 = c("F1", "F2"), F4 = c("F1", "F3"))
  b <- tidy_betas(fit4, mmgsem::standard_errors)
  plots <- list(
    plot_paths(regs, paste0("F", 1:4)),
    plot_paths(regs, paste0("F", 1:4), betas = b, sizes = table(tidy_clusters(fit4)$cluster)),
    plot_selection(ms, "Chull", selected = 4),
    plot_selection(ms, "AIC3", selected = 4),
    plot_posteriors(fit4),
    plot_betas(b),
    plot_starts(fit4),
    plot_covariate(tidy_clusters(fit4), values = seq_len(24), label = "x")
  )
  for (p in plots) {
    expect_s3_class(p, "ggplot")
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("the standard errors of the current mmgsem agree with lavaan, those of Output Box 4 do not", {
  # With the groups assigned to their clusters, Step 2 is a multigroup path
  # model on the factor covariance matrices of Step 1 in which the regressions
  # are equal within a cluster. lavaan's analytic standard errors for that model
  # use no code of mmgsem, so they show which standard errors are right.
  cl <- tidy_clusters(fit4)$cluster
  lab <- function(name) paste0("c(", paste0(name, "_k", cl, collapse = ", "), ") * ")
  model <- paste0("F4 ~ ", lab("b41"), "F1 + ", lab("b43"), "F3\n",
                  "F3 ~ ", lab("b31"), "F1 + ", lab("b32"), "F2\n")
  covs <- lapply(fit4$param$cov_eta, function(m) {
    m <- as.matrix(m)
    (m + t(m)) / 2
  })
  lav <- lavaan::sem(model, sample.cov = covs, sample.nobs = as.numeric(fit4$N_gs),
                     likelihood = "normal", information = "expected")
  pe <- lavaan::parameterEstimates(lav)
  pe <- pe[pe$op == "~" & pe$group %in% match(1:4, cl), ]
  ref <- stats::setNames(pe$se, paste0(pe$lhs, "~", pe$rhs, ".k", cl[pe$group]))
  now <- mmgsemGUI:::precomputed_se()$se$SE_vector$betas_se     # before the correction
  ref <- ref[names(now)]
  expect_false(anyNA(ref))
  expect_lt(max(abs(now - ref)), 0.002)
  box4 <- c(0.031, 0.027, 0.028, 0.025, 0.031, 0.032, 0.093, 0.029,
            0.027, 0.032, 0.039, 0.032, 0.028, 0.026, 0.035, 0.025)
  expect_gt(max(abs(box4 - ref)), 0.05)
  # the correction for stepwise estimation can only add uncertainty
  corrected <- mmgsemGUI:::precomputed_se()$se$SE_vector$betas_se_corrected
  expect_true(all(corrected >= now))
  expect_lt(max(corrected / now), 1.03)
})

test_that("the pairwise tests find the differences the example was generated with", {
  # Section 3 of the tutorial: every regression is zero in one cluster and 0.3
  # in the other three, so three of the six pairs of clusters differ.
  b <- tidy_betas(fit4)
  zero <- tapply(abs(b$estimate), b$parameter, which.min)
  truly <- function(pw) {
    z <- as.vector(zero[pw$parameter])
    z == pw$cluster_a | z == pw$cluster_b
  }
  pw <- tidy_pairwise(fit4, mmgsemGUI:::precomputed_se()$se)
  expect_identical(pw$significant, truly(pw))
  # with the object stored in mmgsem (Output Box 6) one of the twelve is missed
  old <- tidy_pairwise(fit4, mmgsem::standard_errors)
  expect_equal(sum(old$significant), 11)
  expect_equal(sum(truly(old) & !old$significant), 1)
})

test_that("the five-cluster solution of Appendix F is reproduced", {
  fit5 <- mmgsem::extract(ms, 5)
  h <- mmgsemGUI:::fit_headline(fit5)
  expect_equal(h$iterations, 17)
  expect_equal(round(h$entropy, 7), 0.9870289)
  expect_equal(round(h$loglik_observed, 1), -108532.9)
  cl <- tidy_clusters(fit5)
  expect_identical(cl$group[cl$cluster == 4], "21")              # Output Box 7
  se5 <- mmgsem::five_K_se
  expect_true(mmgsemGUI:::se_matches_fit(se5, fit5))
  pw <- tidy_pairwise(fit5, se5)                                  # Output Box 8
  expect_equal(attr(pw, "n_tests"), 40)
  expect_equal(attr(pw, "alpha"), 0.00125)
  last <- pw[pw$cluster_a == 4 & pw$cluster_b == 5, ]
  expect_equal(round(last$difference, 3), c(0.200, 0.233, -0.052, -0.200))
  expect_equal(round(last$z, 3), c(2.666, 2.805, -0.718, -2.603))
  expect_false(any(last$significant))
})

test_that("the precomputed standard errors that ship with the interface belong to the tutorial model", {
  pre <- mmgsemGUI:::precomputed_se()
  expect_false(is.null(pre))
  expect_true(mmgsemGUI:::se_matches_fit(pre$se, fit4))
  b <- tidy_betas(fit4, pre$se)
  expect_identical(attr(b, "se_type"), "corrected")
  expect_true(all(b$se > 0.02 & b$se < 0.04))
  # computed with the current compute_se(): they differ from the object stored
  # in mmgsem for the regressions of F3 on F2, and agree elsewhere
  old <- unname(mmgsem::standard_errors$SE_vector$betas_se_corrected)
  f3f2 <- b$parameter == "F3 ~ F2" & b$cluster > 1
  expect_true(all(abs(b$se[!f3f2] - old[!f3f2]) < 0.003))
  expect_true(all(old[f3f2] - b$se[f3f2] > 0.005))
  w <- tidy_wald(fit4, pre$se)
  expect_equal(round(w$wald, 1), c(69.5, 104.6, 79.6, 81.2))
})
