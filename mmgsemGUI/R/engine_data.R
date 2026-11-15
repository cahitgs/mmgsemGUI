# Data handling -------------------------------------------------------------
# Everything in the engine_* files is plain R with no Shiny dependency, so it
# can be tested against the published tutorial output.

#' Tutorial data set of the mmgsem package
#'
#' The simulated example used in Perez Alonso et al. (2026): 24 groups of 200
#' observations, four factors measured by five items each.
#' @return A data frame with the items V1..V20 and the column `group`.
#' @export
example_data <- function() {
  as.data.frame(mmgsem::data_example)
}

#' Read a data file into a plain data frame
#'
#' Supports csv, tsv/txt, xlsx/xls, sav, rds and rda/RData.
#' @param path File path.
#' @param name Original file name (used to detect the type); defaults to `path`.
#' @return A data frame.
#' @keywords internal
read_data_file <- function(path, name = path) {
  ext <- tolower(sub(".*\\.", "", name))
  df <- switch(
    ext,
    csv = read_delimited(path),
    txt = read_delimited(path),
    tsv = read_delimited(path),
    dat = read_delimited(path),
    xlsx = ,
    xls = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Reading Excel files needs the 'readxl' package.", call. = FALSE)
      }
      readxl::read_excel(path)
    },
    sav = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        stop("Reading SPSS files needs the 'haven' package.", call. = FALSE)
      }
      haven::zap_labels(haven::read_sav(path))
    },
    rds = readRDS(path),
    rda = ,
    rdata = {
      e <- new.env()
      nm <- load(path, envir = e)
      get(nm[1], envir = e)
    },
    stop("Unsupported file type: .", ext, call. = FALSE)
  )
  reader <- attr(df, "reader") %||% switch(
    ext,
    xlsx = , xls = 'as.data.frame(readxl::read_excel("%s"))',
    sav = 'as.data.frame(haven::zap_labels(haven::read_sav("%s")))',
    rds = 'as.data.frame(readRDS("%s"))',
    'as.data.frame(get(load("%s")))'
  )
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!ncol(df) || !nrow(df)) stop("The file contains no data.", call. = FALSE)
  names(df) <- make.unique(make.names(names(df)))
  # R code that reads the same file the same way; used in the exported script.
  attr(df, "reader") <- reader
  df
}

read_delimited <- function(path) {
  first <- readLines(path, n = 5L, warn = FALSE)
  first <- first[nzchar(first)]
  if (!length(first)) stop("The file is empty.", call. = FALSE)
  count <- function(ch) min(lengths(regmatches(first, gregexpr(ch, first, fixed = TRUE))))
  n <- c("," = count(","), ";" = count(";"), "\t" = count("\t"))
  sep <- names(n)[which.max(n)]
  if (max(n) == 0) sep <- ""
  dec <- if (sep == ";" && any(grepl("[0-9],[0-9]", first))) "," else "."
  df <- utils::read.table(path, header = TRUE, sep = sep, dec = dec, quote = "\"",
                          stringsAsFactors = FALSE, check.names = FALSE,
                          comment.char = "", fill = TRUE, na.strings = c("NA", "", "."))
  # The same call in the form a user would write it. read.csv(), read.csv2() and
  # read.delim() have the quote, fill and comment settings used above.
  opts <- ', check.names = FALSE, na.strings = c("NA", "", "."))'
  attr(df, "reader") <- if (sep == "," && dec == ".") {
    paste0('read.csv("%s"', opts)
  } else if (sep == ";" && dec == ",") {
    paste0('read.csv2("%s"', opts)
  } else if (sep == "\t" && dec == ".") {
    paste0('read.delim("%s"', opts)
  } else {
    paste0('read.table("%s", header = TRUE, sep = "', if (sep == "\t") "\\t" else sep,
           '", dec = "', dec, '", quote = "\\"", comment.char = "", fill = TRUE', opts)
  }
  df
}

#' Columns that can serve as questionnaire items
#' @keywords internal
numeric_columns <- function(df, exclude = character()) {
  ok <- vapply(df, function(x) is.numeric(x) || is.integer(x), logical(1))
  setdiff(names(df)[ok], exclude)
}

#' Keep the analysis variables and apply listwise deletion
#'
#' mmgsem expects complete data. The interface therefore removes incomplete
#' rows up front and reports how many were removed, instead of leaving it to
#' the estimation functions.
#' @param df Data frame.
#' @param group Name of the grouping variable.
#' @param items Names of the observed items used in the measurement model.
#' @param missing `"listwise"` (default) removes every row with a missing
#'   value. `"fiml"` keeps incomplete rows for full-information maximum
#'   likelihood and removes only rows without a group or without any answer.
#' @return A list with `data` (items + group), `n_total`, `n_removed`,
#'   `n_incomplete` (rows kept with missing values), `group_sizes` and
#'   `problems` (character vector).
#' @export
prepare_data <- function(df, group, items, missing = c("listwise", "fiml")) {
  missing <- match.arg(missing)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  problems <- character()
  if (!group %in% names(df)) stop("Grouping variable not found: ", group, call. = FALSE)
  missing_items <- setdiff(items, names(df))
  if (length(missing_items)) {
    stop("Items not found in the data: ", paste(missing_items, collapse = ", "), call. = FALSE)
  }
  if (group %in% items) stop("The grouping variable cannot also be an item.", call. = FALSE)
  out <- df[, c(items, group), drop = FALSE]
  not_num <- items[!vapply(out[items], is.numeric, logical(1))]
  if (length(not_num)) {
    stop("These items are not numeric: ", paste(not_num, collapse = ", "), call. = FALSE)
  }
  n_total <- nrow(out)
  complete <- stats::complete.cases(out)
  keep <- if (missing == "fiml") {
    !is.na(out[[group]]) & rowSums(!is.na(out[items])) > 0
  } else {
    complete
  }
  n_incomplete <- sum(keep & !complete)
  out <- out[keep, , drop = FALSE]
  rownames(out) <- NULL
  sizes <- table(out[[group]])
  sizes <- sizes[as.character(unique(out[[group]]))]
  if (length(sizes) < 2) problems <- c(problems, "one_group")
  if (any(sizes <= length(items))) problems <- c(problems, "tiny_groups")
  list(
    data = out,
    n_total = n_total,
    n_removed = n_total - nrow(out),
    n_incomplete = n_incomplete,
    group_sizes = sizes,
    problems = problems
  )
}

#' Read group-level covariates (one row per group)
#' @keywords internal
match_covariates <- function(cov_df, group_col, groups) {
  cov_df <- as.data.frame(cov_df, stringsAsFactors = FALSE)
  key <- as.character(cov_df[[group_col]])
  idx <- match(as.character(groups), key)
  list(data = cov_df[idx, , drop = FALSE], n_matched = sum(!is.na(idx)))
}

#' Items that cannot be treated as ordinal
#'
#' Ordinal items must be whole numbers with between two and `max_cat`
#' categories.
#' @return Names of the items that do not qualify.
#' @keywords internal
ordinal_problems <- function(data, items, max_cat = 10) {
  bad <- vapply(items, function(v) {
    x <- data[[v]]
    x <- x[!is.na(x)]
    n <- length(unique(x))
    any(x != round(x)) || n > max_cat || n < 2
  }, logical(1))
  items[bad]
}
