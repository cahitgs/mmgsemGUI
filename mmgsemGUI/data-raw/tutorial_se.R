# Corrected standard errors of the tutorial example, shipped with the interface
# as inst/extdata/tutorial_se.rds so that the hypothesis tests of the example
# can be shown without the long computation.
#
# Run from the package folder:  Rscript data-raw/tutorial_se.R
# It follows the tutorial script of Perez Alonso et al. (2026) and takes well
# over an hour.
#
# compute_se() was revised in October 2026 ("Fixed issues related to indexing
# of the parameter vector"), so the object that mmgsem stored for the tutorial
# since August 2025 no longer agreed with the function. Since 17 November 2026
# mmgsem stores the values of the revised function as `standard_errors`; they
# are identical to the ones computed here. The earlier values are kept in
# mmgsem as `standard_errors_deprecated`.

library(mmgsem)
library(lavaan)

data <- mmgsem::data_example
S1_model <- '
    F1 =~ V1 + V2 + V3 + V4 + V5
    F2 =~ V6 + V7 + V8 + V9 + V10
    F3 =~ V11 + V12 + V13 + V14 + V15
    F4 =~ V16 + V17 + V18 + V19 + V20
'
S2_model <- '
    F4 ~ F1 + F3
    F3 ~ F1 + F2
'
MM_fit <- lavaan::cfa(model = S1_model, data = data, group = "group",
                      group.equal = "loadings",
                      group.partial = c("F1 =~ V2", "F2 =~ V7", "F3 =~ V12", "F4 =~ V17"))
fit <- mmgsem(dat = data, S1 = S1_model, S2 = S2_model, group = "group", nclus = 4,
              seed = 1, s1_type = "lavaan", s1_fit = MM_fit)

timing <- system.time(se <- compute_se(object = fit))
se$HESS <- NULL   # large and not needed for the tests

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(
  se = se,
  mmgsem = paste0(as.character(packageVersion("mmgsem")), " (commit 758e845)"),
  lavaan = as.character(packageVersion("lavaan")),
  R = paste(R.version$major, R.version$minor, sep = "."),
  seconds = timing[["elapsed"]],
  date = as.character(Sys.Date())
), "inst/extdata/tutorial_se.rds", compress = "xz")
