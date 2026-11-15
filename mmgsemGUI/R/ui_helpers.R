# Small building blocks for the interface ---------------------------------------

`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

norm_syntax <- function(s) {
  lines <- trimws(strsplit(s %||% "", "\n", fixed = TRUE)[[1]])
  paste(lines[nzchar(lines)], collapse = "\n")
}

clean_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_]", "_", trimws(x))
  if (nzchar(x) && grepl("^[0-9_]", x)) x <- paste0("F", x)
  x
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "–", formatC(x, format = "f", digits = digits, big.mark = ","))
}

fmt_int <- function(x) {
  ifelse(is.na(x), "–", formatC(x, format = "d", big.mark = ","))
}

fmt_p <- function(p) {
  ifelse(is.na(p), "–",
         ifelse(p < 0.001, "< .001", sub("^0", "", formatC(p, format = "f", digits = 3))))
}

fmt_fit <- function(x) {
  ifelse(is.na(x), "–", sub("^0", "", formatC(x, format = "f", digits = 3)))
}

#' An HTML table with full control over the cells
#' @param df Data frame of already formatted values (character) or tags.
#' @param header Column headers.
#' @param align Character vector of "l"/"r" per column.
#' @param row_class Optional character vector of classes per row.
#' @keywords internal
html_table <- function(df, header = names(df), align = NULL, row_class = NULL,
                       class = "tbl") {
  df <- as.list(df)
  n <- length(df)
  nr <- length(df[[1]])
  align <- align %||% rep("l", n)
  cls <- ifelse(align == "r", "num", "")
  head <- tags$thead(tags$tr(lapply(seq_len(n), function(j) {
    tags$th(class = cls[j], header[[j]])
  })))
  body <- tags$tbody(lapply(seq_len(nr), function(i) {
    tags$tr(class = if (!is.null(row_class)) row_class[i], lapply(seq_len(n), function(j) {
      cell <- df[[j]][[i]]
      tags$td(class = cls[j], cell)
    }))
  }))
  tags$div(class = "tbl-wrap", tags$table(class = class, head, body))
}

tile <- function(label, value, note = NULL) {
  tags$div(class = "tile",
           tags$div(class = "tile-value", value),
           tags$div(class = "tile-label", label),
           if (!is.null(note)) tags$div(class = "tile-note", note))
}

notice <- function(..., type = "info") {
  tags$div(class = paste("notice", paste0("notice-", type)), role = if (type == "error") "alert", ...)
}

sheet <- function(..., title = NULL, class = NULL) {
  tags$section(class = paste("sheet", class), if (!is.null(title)) tags$h2(title), ...)
}

hint <- function(...) tags$p(class = "hint", ...)

cluster_dot <- function(k, n) {
  tags$span(class = "cluster-dot", style = paste0("background:", cluster_colours(n)[k]))
}

progress_bar <- function(pct) {
  if (is.na(pct)) {
    tags$div(class = "bar bar-busy", tags$div(class = "bar-fill"))
  } else {
    tags$div(class = "bar", role = "progressbar", `aria-valuenow` = pct,
             `aria-valuemin` = 0, `aria-valuemax` = 100,
             tags$div(class = "bar-fill", style = paste0("transform:scaleX(", pct / 100, ")")))
  }
}

# A code block filled by a text output, without stray blank lines.
code_out <- function(id, tall = FALSE) {
  textOutput(id, container = function(...) {
    tags$pre(..., class = if (tall) "code code-tall" else "code")
  })
}
