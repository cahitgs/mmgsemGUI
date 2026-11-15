test_that("model syntax is written and read back unchanged", {
  factors <- list(F1 = c("V1", "V2", "V3"), F2 = c("V4", "V5", "V6"))
  s1 <- build_s1(factors)
  expect_identical(s1, "F1 =~ V1 + V2 + V3\nF2 =~ V4 + V5 + V6")
  expect_identical(parse_s1(s1), factors)
  regs <- list(F2 = "F1")
  expect_identical(parse_s2(build_s2(regs)), regs)
  expect_error(parse_s1(""), "empty")
  expect_error(parse_s2("F1 =~ V1 + V2"), "No regressions")
})

test_that("only non-marker loadings can be released", {
  factors <- list(F1 = c("V1", "V2", "V3"), F2 = c("V4", "V5"))
  expect_identical(mmgsemGUI:::free_loadings(factors), c("F1 =~ V2", "F1 =~ V3", "F2 =~ V5"))
})

test_that("structural models are checked", {
  lvs <- c("A", "B", "C")
  expect_length(mmgsemGUI:::check_s2(list(C = c("A", "B")), lvs), 0)
  expect_identical(mmgsemGUI:::check_s2(list(), lvs), "empty")
  expect_true("cycle" %in% mmgsemGUI:::check_s2(list(A = "B", B = "A"), lvs))
  expect_true("unknown_factor" %in% mmgsemGUI:::check_s2(list(C = "Z"), lvs))
  expect_identical(unname(mmgsemGUI:::node_depth(list(C = "B", B = "A"), lvs)), c(0L, 1L, 2L))
})

test_that("incomplete rows are removed and reported", {
  df <- data.frame(g = rep(c("a", "b"), each = 5), x = c(NA, 1:9), y = 1:10, z = letters[1:10])
  pd <- prepare_data(df, "g", c("x", "y"))
  expect_equal(pd$n_total, 10)
  expect_equal(pd$n_removed, 1)
  expect_named(pd$data, c("x", "y", "g"))
  expect_equal(as.integer(pd$group_sizes), c(4L, 5L))
  expect_error(prepare_data(df, "g", c("x", "z")), "not numeric")
  expect_error(prepare_data(df, "nope", "x"), "not found")
})

test_that("delimited files are read with the right separator and decimal mark", {
  df <- data.frame(a = c(1.5, 2.25, NA), b = c("x", "y", "z"), grp = c(1, 1, 2))
  f1 <- tempfile(fileext = ".csv")
  f2 <- tempfile(fileext = ".csv")
  utils::write.csv(df, f1, row.names = FALSE)
  utils::write.csv2(df, f2, row.names = FALSE)
  r1 <- mmgsemGUI:::read_data_file(f1)
  r2 <- mmgsemGUI:::read_data_file(f2)
  expect_equal(r1$a, df$a)
  expect_equal(r2$a, df$a)
  expect_match(attr(r1, "reader"), "^read\\.csv\\(")
  expect_match(attr(r2, "reader"), "^read\\.csv2\\(")
  # the exported script reads the file the same way
  code <- sprintf(attr(r2, "reader"), gsub("\\\\", "/", f2))
  expect_equal(eval(parse(text = code))$a, df$a)
})

test_that("the exported script is valid R and follows the analysis", {
  spec <- list(
    data_source = "example", group = "group", items = paste0("V", 1:20),
    S1 = "F1 =~ V1 + V2 + V3\nF2 =~ V6 + V7 + V8", noninv = "F1 =~ V2",
    S2 = "F2 ~ F1", k_min = 1, k_max = 6, seed = 1, nstarts = 20,
    selected_k = 4, se = "naive"
  )
  script <- make_script(spec)
  expect_silent(parse(text = script))
  expect_match(script, "ModelSelection(", fixed = TRUE)
  expect_match(script, "nclus   = c(1, 6)", fixed = TRUE)
  expect_match(script, "extract(object = modelSel_fit, nclus = 4)", fixed = TRUE)
  expect_match(script, "compute_se(object = selected_fit, naive = TRUE)", fixed = TRUE)
  expect_match(script, 'group.partial = c("F1 =~ V2")', fixed = TRUE)
  single <- make_script(utils::modifyList(spec, list(k_min = 3, k_max = 3, selected_k = 3, se = "none")))
  expect_silent(parse(text = single))
  expect_match(single, "selected_fit <- mmgsem(", fixed = TRUE)
  expect_false(grepl("compute_se", single, fixed = TRUE))
  early <- make_script(list(data_source = "my data.csv", data_reader = 'read.csv2("%s")', group = "g"))
  expect_match(early, 'read.csv2("my data.csv")', fixed = TRUE)
})

test_that("every interface text is one string, and placeholders are filled in", {
  d <- mmgsemGUI:::dict
  expect_true(all(vapply(d, function(x) is.character(x) && length(x) == 1L && nzchar(x), logical(1))))
  expect_identical(names(d)[duplicated(names(d))], character())
  L <- mmgsemGUI:::translator()
  expect_identical(L("n_clusters", 4L), "4 clusters")
  expect_identical(L("no_such_key"), "no_such_key")
})

test_that("a small analysis runs end to end and agrees with mmgsem called directly", {
  skip_on_cran()
  d <- example_data()
  d <- d[d$group %in% 1:8, c(paste0("V", 1:10), "group")]
  factors <- list(F1 = paste0("V", 1:5), F2 = paste0("V", 6:10))
  S1 <- build_s1(factors)
  S2 <- build_s2(list(F2 = "F1"))
  noninv <- c("F1 =~ V2", "F2 =~ V7")
  pd <- prepare_data(d, "group", unlist(factors))
  mm <- fit_measurement(pd$data, S1, "group", noninv)
  s <- mmgsemGUI:::measurement_summary(mm)
  expect_true(s$converged)
  expect_equal(s$ngroups, 8)
  # skipping the standard errors of the measurement model does not change it
  mm_se <- fit_measurement(pd$data, S1, "group", noninv, se = "standard")
  expect_equal(lavaan::coef(mm), lavaan::coef(mm_se), tolerance = 1e-10)

  fits <- lapply(1:2, function(k) fit_clusters(pd$data, S1, S2, "group", k, mm, nstarts = 5))
  ms <- assemble_selection(fits)
  ref <- suppressMessages(utils::capture.output(
    direct <- mmgsem::ModelSelection(dat = pd$data, S1 = S1, S2 = S2, group = "group",
                                     nclus = c(1, 2), seed = 1, nstarts = 5,
                                     s1_type = "lavaan", s1_fit = mm)
  ))
  expect_equal(ms$Overview, direct$Overview, ignore_attr = TRUE)
  expect_equal(tidy_betas(mmgsem::extract(ms, 2))$estimate,
               tidy_betas(mmgsem::extract(direct, 2))$estimate)
  scr <- mmgsemGUI:::loading_screen(mm)
  expect_true(all(c("loading", "score", "constraints") %in% names(scr)))
  expect_false(any(scr$loading %in% noninv))
  # the fast score tests are the univariate tests of lavaan::lavTestScore()
  ref <- lavaan::lavTestScore(mm, cumulative = FALSE)$uni
  fast <- mmgsemGUI:::score_tests_fast(mm)
  expect_identical(fast$lhs, ref$lhs)
  expect_identical(fast$rhs, ref$rhs)
  expect_equal(fast$X2, ref$X2, tolerance = 1e-8)
  expect_equal(scr, mmgsemGUI:::loading_screen(mm, fast = FALSE), tolerance = 1e-8)
})
