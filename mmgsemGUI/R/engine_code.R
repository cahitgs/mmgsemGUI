# Reproducible R script --------------------------------------------------------

quote_vec <- function(x, indent = 0) {
  if (!length(x)) return("character()")
  pad <- strrep(" ", indent)
  paste0("c(", paste0('"', x, '"', collapse = paste0(",\n", pad, "  ")), ")")
}

# A long character vector, wrapped over several lines.
wrap_vec <- function(x, indent = 0, width = 78) {
  txt <- paste0('"', x, '"', collapse = ", ")
  lines <- strwrap(txt, width = width - indent - 2)
  paste0("c(", paste(lines, collapse = paste0("\n", strrep(" ", indent + 2))), ")")
}

indent_syntax <- function(s) {
  lines <- trimws(strsplit(s, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  paste0("    ", lines, collapse = "\n")
}

#' Write the R script that reproduces an analysis
#'
#' The script uses only `mmgsem` and `lavaan`, following the structure of the
#' tutorial script by Perez Alonso et al. (2026).
#'
#' @param spec List describing the analysis: `data_source` (`"example"` or a
#'   file name), `data_reader` (R code that reads the file, with `%s` for the
#'   file name), `group`, `items`, `S1`, `noninv`, `S2`, `k_min`, `k_max`,
#'   `seed`, `nstarts`, `max_it`, `endogenous_cov`, `endo_group_specific`,
#'   `selected_k` and `se` (`"none"`, `"naive"` or `"corrected"`). Elements for
#'   steps that have not been reached may be NULL.
#' @return The script as a single string.
#' @export
make_script <- function(spec) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  out <- character()
  add <- function(...) out <<- c(out, paste0(...))
  group <- spec$group %||% "group"

  add("# Mixture Multigroup SEM (MMG-SEM) analysis")
  add("# Written by mmgsemGUI ", as.character(utils::packageVersion("mmgsemGUI")),
      " on ", format(Sys.Date(), "%Y-%m-%d"), ".")
  add("# Method and package: Perez Alonso, Vermunt, Rosseel, Zhao & De Roover (2026),")
  add("# https://doi.org/10.1080/10705511.2026.2617170")
  add("")
  add("# install.packages(\"pak\"); pak::pak(\"AndresFPA/mmgsem\")")
  add("library(mmgsem)")
  add("library(lavaan)")
  add("")
  add("#### Data ####")
  src <- spec$data_source %||% "example"
  if (identical(src, "example")) {
    add("data <- as.data.frame(mmgsem::data_example)")
  } else {
    # `data_reader` is the R code that reads the file the way the interface did
    # (separator, decimal mark, file type), with %s for the file name.
    reader <- sprintf(spec$data_reader %||% 'read.csv("%s")', src)
    add("# Adjust the path so that it points to your data file.")
    add("data <- ", reader)
    add("# The interface turns column names into valid R names; so does this script.")
    add("names(data) <- make.unique(make.names(names(data)))")
  }
  missing <- spec$missing %||% "listwise"
  ordinal <- isTRUE(spec$ordinal)
  blocks <- isTRUE(spec$blocks)
  fiml <- identical(missing, "fiml")
  if (length(spec$items)) {
    add("items <- ", wrap_vec(spec$items, indent = 9))
    if (fiml) {
      add("# Incomplete rows are kept for full-information maximum likelihood; only rows")
      add("# without a group or without any answer are left out.")
      add("data <- data[!is.na(data[[\"", group, "\"]]) & rowSums(!is.na(data[, items])) > 0, c(items, \"", group, "\")]")
    } else {
      add("# mmgsem expects complete data: keep the complete rows of the analysis variables")
      add("data <- data[stats::complete.cases(data[, c(items, \"", group, "\")]), c(items, \"", group, "\")]")
    }
  }
  if (!nzchar(spec$S1 %||% "")) return(paste(out, collapse = "\n"))

  add("")
  if (blocks) {
    parts <- split_blocks(spec$S1)
    add("#### Step 1: measurement model, one block per factor (Rosseel & Loh, 2024) ####")
    add("S1_model <- list(")
    add(paste0("    '", unlist(parts), "'", collapse = ",\n"))
    add(")")
    add("noninvariant <- ", if (length(spec$noninv)) quote_vec(spec$noninv, indent = 16) else "character()")
    add("MM_fit <- lapply(S1_model, function(block) {")
    add("  factor <- trimws(sub(\"=~.*\", \"\", block))")
    add("  released <- noninvariant[startsWith(noninvariant, paste0(factor, \" =~ \"))]")
    add("  lavaan::cfa(model = block,")
    add("              data  = data,")
    add("              group = \"", group, "\",")
    add("              group.equal = \"loadings\",")
    add("              group.partial = if (length(released)) released else \"\"",
        if (fiml) ",\n              missing = \"fiml\")" else ")")
    add("})")
    add("lapply(MM_fit, lavaan::fitMeasures, c(\"chisq\", \"df\", \"cfi\", \"tli\", \"rmsea\", \"srmr\"))")
  } else if (ordinal) {
    add("#### Step 1: measurement model for ordinal items (categorical CFA; Wu & Estabrook, 2016) ####")
    add("S1_model <- '")
    add(indent_syntax(spec$S1))
    add("'")
    has_partial <- length(spec$noninv) > 0
    if (has_partial) add("noninvariant <- ", quote_vec(spec$noninv, indent = 16))
    add("S1_model_Wu <- as.character(")
    add("  semTools::measEq.syntax(configural.model = S1_model,")
    add("                          dat              = data,")
    add("                          parameterization = \"delta\",")
    add("                          ordered          = TRUE,")
    add("                          ID.fac           = \"std.lv\",")
    add("                          ID.cat           = \"Wu\",")
    add("                          group            = \"", group, "\",")
    add("                          group.equal      = c(\"thresholds\", \"loadings\")",
        if (has_partial) ",\n                          group.partial    = noninvariant)" else ")")
    add(")")
    add("MM_fit <- lavaan::cfa(model = S1_model_Wu,")
    add("                      data  = data,")
    add("                      group = \"", group, "\",")
    add("                      group.equal = c(\"thresholds\", \"loadings\")",
        if (has_partial) ",\n                      group.partial = noninvariant)" else ")")
    add("lavaan::fitMeasures(MM_fit, c(\"chisq.scaled\", \"df.scaled\", \"cfi.scaled\", \"tli.scaled\", \"rmsea.scaled\", \"srmr\"))")
  } else {
    add("#### Step 1: measurement model (multigroup CFA, metric invariance) ####")
    add("S1_model <- '")
    add(indent_syntax(spec$S1))
    add("'")
    add("MM_fit <- lavaan::cfa(model = S1_model,")
    add("                      data  = data,")
    add("                      group = \"", group, "\",")
    tail <- if (fiml) ",\n                      missing = \"fiml\")" else ")"
    if (length(spec$noninv)) {
      add("                      group.equal = \"loadings\",")
      add("                      group.partial = ", quote_vec(spec$noninv, indent = 38), tail)
    } else {
      add("                      group.equal = \"loadings\"", tail)
    }
    add("lavaan::fitMeasures(MM_fit, c(\"chisq\", \"df\", \"cfi\", \"tli\", \"rmsea\", \"srmr\"))")
  }
  if (!nzchar(spec$S2 %||% "")) return(paste(out, collapse = "\n"))

  add("")
  add("#### Step 2: structural model and clustering of the groups ####")
  add("S2_model <- '")
  add(indent_syntax(spec$S2))
  add("'")
  kmin <- spec$k_min %||% 1L
  kmax <- spec$k_max %||% kmin
  extra <- character()
  if (!isTRUE(spec$endogenous_cov %||% TRUE)) extra <- c(extra, "endogenous_cov = FALSE")
  if (!isTRUE(spec$endo_group_specific %||% TRUE)) extra <- c(extra, "endo_group_specific = FALSE")
  if ((spec$max_it %||% 10000L) != 10000L) extra <- c(extra, paste0("max_it = ", spec$max_it))
  # Options of Step 1 that mmgsem needs again (it passes them on to lavaan).
  if (fiml) extra <- c(extra, "missing = \"fiml\"")
  if (ordinal) {
    extra <- c(extra, "ordered = TRUE", "group.equal = c(\"thresholds\", \"loadings\")",
               if (length(spec$noninv)) "group.partial = noninvariant")
  }
  common <- function(fun, nclus, pad) {
    args <- c(
      "dat     = data", "S1      = S1_model", "S2      = S2_model",
      paste0("group   = \"", group, "\""), paste0("nclus   = ", nclus),
      paste0("seed    = ", spec$seed %||% 1L),
      paste0("nstarts = ", spec$nstarts %||% 20L),
      "s1_type = \"lavaan\"", "s1_fit  = MM_fit", extra
    )
    paste0(fun, "(", paste(args, collapse = paste0(",\n", strrep(" ", pad))), ")")
  }
  sel <- spec$selected_k
  if (kmin == kmax) {
    add("selected_fit <- ", common("mmgsem", kmin, 23))
  } else {
    add("modelSel_fit <- ", common("ModelSelection", paste0("c(", kmin, ", ", kmax, ")"), 31))
    add("summary(modelSel_fit, model_selection = TRUE)")
    add("plot(modelSel_fit, criteria = \"Chull\")")
    add("plot(modelSel_fit, criteria = \"AIC3\")")
    if (is.null(sel)) return(paste(out, collapse = "\n"))
    add("")
    add("# Model chosen in the interface")
    add("selected_fit <- extract(object = modelSel_fit, nclus = ", sel, ")")
  }
  add("summary(selected_fit)")
  add("print(selected_fit$posteriors)")

  se <- spec$se %||% "none"
  if (!identical(se, "none") && (sel %||% kmin) >= 1) {
    add("")
    add("#### Standard errors and hypothesis tests ####")
    if (identical(se, "naive")) {
      add("# naive = TRUE skips the correction for stepwise estimation (fast).")
      add("standard_errors <- compute_se(object = selected_fit, naive = TRUE)")
    } else {
      add("# Corrected standard errors; this can take a long time.")
      add("standard_errors <- compute_se(object = selected_fit)")
    }
    add("summary(selected_fit, se = standard_errors)")
    if ((sel %||% kmin) >= 2) {
      add("test.mmgsem(model = selected_fit, se = standard_errors)")
      add("test.mmgsem(model = selected_fit, se = standard_errors, multiple_comparison = TRUE)")
    }
  }
  paste(out, collapse = "\n")
}
