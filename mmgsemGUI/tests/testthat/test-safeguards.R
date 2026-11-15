# Safeguards: a result is only offered for use when it can be trusted, and the
# exported script reads the data exactly as the interface did.

fx <- readRDS(test_path("fixtures", "tutorial_models.rds"))
fit4 <- fx$models[[4]]

test_that("a solution that stopped at the iteration limit is recognised", {
  expect_false(cluster_checks(fit4, max_it = 10000)$hit_limit)
  # the best start of this model needed 12 iterations
  expect_true(cluster_checks(fit4, max_it = 12)$hit_limit)
  expect_length(cluster_checks(fit4)$empty, 0)
})

test_that("a cluster without groups is recognised", {
  hollow <- fit4
  moved <- hollow$modal_post[[4]] == 1          # column 4 is cluster 3
  hollow$modal_post[[4]][moved] <- 0
  hollow$modal_post[[2]][moved] <- 1
  expect_identical(cluster_checks(hollow)$empty, 3L)
})

test_that("an estimation cut short reports that it did not converge", {
  skip_on_cran()
  d <- example_data()
  d <- d[d$group %in% 1:8, c(paste0("V", 1:10), "group")]
  S1 <- "F1 =~ V1 + V2 + V3 + V4 + V5\nF2 =~ V6 + V7 + V8 + V9 + V10"
  mm <- fit_measurement(d, S1, "group")
  cut <- fit_clusters(d, S1, "F2 ~ F1", "group", k = 2, s1_fit = mm, nstarts = 3, max_it = 2)
  full <- fit_clusters(d, S1, "F2 ~ F1", "group", k = 2, s1_fit = mm, nstarts = 3)
  expect_true(cluster_checks(cut, max_it = 2)$hit_limit)
  expect_false(cluster_checks(full, max_it = 10000)$hit_limit)
})

test_that("a measurement model that did not converge keeps the next steps closed", {
  shiny::testServer(mmgsemGUI:::app_server, {
    rv$raw <- example_data()
    rv$data_id <- 1L
    session$setInputs(group_var = "group", mm_mode = "syntax",
                      s1_text = "F1 =~ V1 + V2 + V3\nF2 =~ V6 + V7 + V8",
                      mm_missing = "listwise", mm_ordinal = FALSE, mm_blocks = FALSE)
    rv$mm <- list(spec = mm_spec(), summary = list(converged = FALSE, post_check = TRUE))
    expect_true(mm_current())
    expect_false(mm_ok())
    expect_false(step_state()$structure$done && step_state()$clusters$enabled)
    expect_false(step_state()$clusters$enabled)
    rv$mm$summary$converged <- TRUE
    expect_true(mm_ok())
    # changing how the model is estimated makes the estimated model out of date
    session$setInputs(mm_blocks = TRUE)
    expect_false(mm_current())
    expect_false(mm_ok())
  })
})

test_that("drawing the structural-model builder does not change an unchanged model", {
  shiny::testServer(mmgsemGUI:::app_server, {
    rv$raw <- example_data()
    rv$data_id <- 1L
    session$setInputs(group_var = "group", mm_mode = "syntax", s2_mode = "builder",
                      s1_text = paste0("F", 1:4, " =~ V", c(1, 6, 11, 16), " + V", c(2, 7, 12, 17),
                                       " + V", c(3, 8, 13, 18), collapse = "\n"))
    # as when the tutorial example is loaded or a project is opened
    rerender_s2(paste0("F", 1:4), list(F4 = c("F1", "F3"), F3 = c("F1", "F2")))
    before <- S2()
    expect_identical(before, "F4 ~ F1 + F3\nF3 ~ F1 + F2")
    # the builder is drawn and reports its inputs, some of them in another order
    gen <- rv$s2_gen
    values <- list("1", NULL, NULL, c("F2", "F1"), c("F3", "F1"))
    names(values) <- c(sprintf("s2_mark_%d", gen), sprintf("pred_%d_%d", gen, 1:4))
    do.call(session$setInputs, values)
    expect_identical(S2(), before)
    # a real change is still seen
    do.call(session$setInputs, stats::setNames(list("F1"), sprintf("pred_%d_%d", gen, 4)))
    expect_identical(S2(), "F4 ~ F1\nF3 ~ F1 + F2")
  })
})

test_that("options that cannot be combined are refused", {
  shiny::testServer(mmgsemGUI:::app_server, {
    rv$raw <- example_data()
    rv$data_id <- 1L
    session$setInputs(group_var = "group", mm_mode = "syntax",
                      s1_text = "F1 =~ V1 + V2 + V3\nF2 =~ V6 + V7 + V8",
                      mm_missing = "fiml", mm_ordinal = TRUE, mm_blocks = FALSE)
    expect_false(is.null(mm_option_problem()))
    session$setInputs(mm_missing = "listwise")
    expect_null(mm_option_problem())
    # blocks need a model without cross-loadings
    session$setInputs(mm_ordinal = FALSE, mm_blocks = TRUE,
                      s1_text = "F1 =~ V1 + V2 + V3\nF2 =~ V3 + V7 + V8")
    expect_false(is.null(mm_option_problem()))
  })
  expect_error(fit_measurement(example_data(), "F1 =~ V1 + V2 + V3", "group",
                               ordinal = TRUE, missing = "fiml"), "cannot be combined")
})

test_that("the exported script gives the data the column names the interface uses", {
  td <- tempfile("names_")
  dir.create(td)
  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)
  set.seed(4)
  d <- data.frame(`Madde 1` = rnorm(8), `Madde 2` = rnorm(8), `1.grup` = rep(c("a", "b"), 4),
                  check.names = FALSE)
  d$`Madde 2`[3] <- NA
  saveRDS(d, "veri.rds")
  utils::write.csv(d, "veri.csv", row.names = FALSE)
  utils::write.csv2(d, "veri2.csv", row.names = FALSE)
  utils::write.table(d, "veri.tsv", sep = "\t", row.names = FALSE)
  utils::write.table(d, "veri.txt", sep = " ", row.names = FALSE)
  save(d, file = "veri.RData")
  for (f in c("veri.rds", "veri.csv", "veri2.csv", "veri.tsv", "veri.txt", "veri.RData")) {
    app <- mmgsemGUI:::read_data_file(f, f)
    expect_named(app, c("Madde.1", "Madde.2", "X1.grup"), info = f)
    pd <- prepare_data(app, "X1.grup", c("Madde.1", "Madde.2"))
    script <- make_script(list(data_source = f, data_reader = attr(app, "reader"),
                               group = "X1.grup", items = c("Madde.1", "Madde.2")))
    env <- new.env()
    eval(parse(text = script), env)
    expect_equal(env$data, pd$data, ignore_attr = TRUE, info = f)
  }
})

test_that("a standard error that could not be computed does not break the tests", {
  # Since October 2026 compute_se() returns NA, with a warning, for a variance
  # it could not estimate.
  se <- mmgsemGUI:::precomputed_se()$se
  se$SE_vector$betas_se_corrected["F3~F2.k2"] <- NA_real_
  se$vcov_betas["F3~F2.k2", ] <- NA_real_
  se$vcov_betas[, "F3~F2.k2"] <- NA_real_
  b <- tidy_betas(fit4, se)
  lost <- b$parameter == "F3 ~ F2" & b$cluster == 2
  expect_true(all(is.na(b$z[lost]), is.na(b$p[lost])))
  expect_false(anyNA(b$p[!lost]))
  w <- tidy_wald(fit4, se)
  expect_identical(is.na(w$wald), w$parameter == "F3 ~ F2")
  expect_identical(is.na(w$p), w$parameter == "F3 ~ F2")
  pw <- tidy_pairwise(fit4, se)
  expect_false(anyNA(pw$significant))
  affected <- pw$parameter == "F3 ~ F2" & (pw$cluster_a == 2 | pw$cluster_b == 2)
  expect_true(all(is.na(pw$p[affected])))
  expect_false(any(pw$significant[affected]))
  expect_false(anyNA(pw$p[!affected]))
})

test_that("ordinal items are checked before estimation", {
  d <- data.frame(a = c(1, 2, 3, 4), b = c(1.5, 2, 3, 4), c = 1:4 * 10, d = rep(1, 4))
  expect_identical(mmgsemGUI:::ordinal_problems(d, c("a", "b", "c", "d")), c("b", "d"))
  wide <- data.frame(x = 1:30)
  expect_identical(mmgsemGUI:::ordinal_problems(wide, "x"), "x")
})
