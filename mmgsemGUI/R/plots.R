# Graphics ---------------------------------------------------------------------
# In the interface colour is reserved for clusters; everything else is ink on
# paper. The cluster palette is Paul Tol's "muted" scheme (colour-blind safe).

ink <- "#17232B"
ink_soft <- "#56646D"
rule_col <- "#D9DED9"
mark_col <- "#FFE45C"

cluster_palette <- c("#332288", "#CC6677", "#117733", "#999933", "#882255",
                     "#44AA99", "#AA4499", "#6699CC", "#DDCC77", "#88CCEE")

#' Colours for k clusters
#' @keywords internal
cluster_colours <- function(k) {
  cols <- rep_len(cluster_palette, max(k, 1))
  stats::setNames(cols[seq_len(k)], as.character(seq_len(k)))
}

theme_mmg <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = ink),
      axis.text = ggplot2::element_text(colour = ink_soft),
      axis.title = ggplot2::element_text(colour = ink_soft, size = ggplot2::rel(0.92)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E9ECE8", linewidth = 0.4),
      strip.text = ggplot2::element_text(colour = ink, face = "bold", hjust = 0,
                                         size = ggplot2::rel(1)),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05)),
      plot.title.position = "plot",
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(8, 12, 8, 8)
    )
}

label_layer <- function(mapping, data, size = 3.6, ...) {
  args <- list(mapping = mapping, data = data, size = size, colour = ink,
               fill = "white", label.padding = grid::unit(0.14, "lines"),
               label.r = grid::unit(0.1, "lines"), ...)
  if (utils::packageVersion("ggplot2") >= "4.0.0") {
    args$linewidth <- 0
  } else {
    args$label.size <- 0
  }
  do.call(ggplot2::geom_label, args)
}

fmt_coef <- function(x, digits = 2) {
  out <- formatC(x, format = "f", digits = digits)
  sub("^-0\\.0+$", sub("-", "", formatC(0, format = "f", digits = digits)), out)
}

# ---- Path diagrams -------------------------------------------------------------

path_layout <- function(regs, lvs) {
  depth <- node_depth(regs, lvs)
  if (is.null(depth)) return(NULL)
  nodes <- data.frame(name = names(depth), depth = as.integer(depth),
                      stringsAsFactors = FALSE)
  nodes$x <- nodes$depth * 2.1
  nodes$y <- 0
  for (d in unique(nodes$depth)) {
    i <- which(nodes$depth == d)
    nodes$y[i] <- ((length(i) - 1) / 2 - (seq_along(i) - 1)) * 1.15
  }
  nodes
}

node_rx <- 0.52
node_ry <- 0.30

ellipse_points <- function(nodes, n = 60) {
  t <- seq(0, 2 * pi, length.out = n)
  do.call(rbind, lapply(seq_len(nrow(nodes)), function(i) {
    data.frame(name = nodes$name[i], x = nodes$x[i] + node_rx * cos(t),
               y = nodes$y[i] + node_ry * sin(t))
  }))
}

inside_node <- function(x, y, cx, cy, pad = 1.06) {
  ((x - cx) / (node_rx * pad))^2 + ((y - cy) / (node_ry * pad))^2 < 1
}

# One edge as a sequence of points from the border of `from` to the border of
# `to`. Edges that jump over a layer are bent away from the nodes in between.
edge_points <- function(from, to, nodes, n = 40) {
  a <- nodes[nodes$name == from, ]
  b <- nodes[nodes$name == to, ]
  t <- seq(0, 1, length.out = n)
  bend <- 0
  between <- nodes[nodes$depth > min(a$depth, b$depth) &
                     nodes$depth < max(a$depth, b$depth), , drop = FALSE]
  if (nrow(between)) {
    y_line <- a$y + (b$y - a$y) * (between$x - a$x) / (b$x - a$x)
    gap <- y_line - between$y
    if (any(abs(gap) < node_ry * 2.2)) {
      side <- if (mean(gap) >= 0) 1 else -1
      bend <- side * 0.75
    }
  }
  dx <- b$x - a$x
  dy <- b$y - a$y
  len <- sqrt(dx^2 + dy^2)
  cx <- (a$x + b$x) / 2 - dy / len * bend
  cy <- (a$y + b$y) / 2 + dx / len * bend
  x <- (1 - t)^2 * a$x + 2 * (1 - t) * t * cx + t^2 * b$x
  y <- (1 - t)^2 * a$y + 2 * (1 - t) * t * cy + t^2 * b$y
  keep <- !inside_node(x, y, a$x, a$y) & !inside_node(x, y, b$x, b$y, pad = 1.16)
  x <- x[keep]
  y <- y[keep]
  mid <- ceiling(length(x) * 0.5)
  list(path = data.frame(x = x, y = y), label = data.frame(x = x[mid], y = y[mid]))
}

# The covariance between two exogenous factors: an arc on the outer side of
# the first layer, from the border of one node to the border of the other.
cov_points <- function(from, to, nodes, n = 60) {
  a <- nodes[nodes$name == from, ]
  b <- nodes[nodes$name == to, ]
  t <- seq(0, 1, length.out = n)
  cx <- min(a$x, b$x) - (node_rx + 1 + 0.45 * abs(a$y - b$y))
  cy <- (a$y + b$y) / 2
  x <- (1 - t)^2 * a$x + 2 * (1 - t) * t * cx + t^2 * b$x
  y <- (1 - t)^2 * a$y + 2 * (1 - t) * t * cy + t^2 * b$y
  keep <- !inside_node(x, y, a$x, a$y, pad = 1.16) & !inside_node(x, y, b$x, b$y, pad = 1.16)
  data.frame(x = x[keep], y = y[keep])
}

#' Path diagram of the structural model, one panel per cluster
#'
#' Line width shows the size of a regression coefficient. When standard
#' errors are supplied, coefficients that do not differ from zero at the 5%
#' level are drawn dashed. Thin grey arcs are the covariances between the
#' exogenous factors, which MMG-SEM estimates in every group (Perez Alonso et
#' al., 2024); they carry no number because they are not cluster-specific.
#'
#' @param regs Named list outcome -> predictors (see [parse_s2()]).
#' @param lvs Names of all factors.
#' @param betas Optional result of [tidy_betas()]. Without it only the
#'   structure of the model is drawn.
#' @param sizes Optional named vector: number of groups per cluster.
#' @param cluster_word Word used in the panel titles.
#' @return A ggplot object.
#' @export
plot_paths <- function(regs, lvs, betas = NULL, sizes = NULL, cluster_word = "Cluster",
                       groups_word = "groups") {
  regs <- regs[lengths(regs) > 0]
  nodes <- path_layout(regs, lvs)
  if (is.null(nodes)) return(NULL)
  used <- unique(c(names(regs), unlist(regs)))
  nodes <- nodes[nodes$name %in% used | !length(used), , drop = FALSE]
  edges <- do.call(rbind, lapply(names(regs), function(y) {
    data.frame(from = regs[[y]], to = y, stringsAsFactors = FALSE)
  }))
  if (is.null(edges)) edges <- data.frame(from = character(), to = character())
  geo <- lapply(seq_len(nrow(edges)), function(i) edge_points(edges$from[i], edges$to[i], nodes))
  exo <- nodes$name[nodes$depth == 0L]
  arcs <- if (length(exo) > 1 && nrow(edges)) {
    pairs <- utils::combn(exo, 2)
    lapply(seq_len(ncol(pairs)), function(i) cov_points(pairs[1, i], pairs[2, i], nodes))
  }

  panels <- if (is.null(betas)) NA else sort(unique(betas$cluster))
  cols <- if (is.null(betas)) c("NA" = ink) else cluster_colours(max(panels))
  panel_label <- function(k) {
    if (is.na(k)) return("")
    lab <- paste(cluster_word, k)
    if (!is.null(sizes) && !is.na(sizes[as.character(k)])) {
      lab <- paste0(lab, " (", sizes[as.character(k)], " ", groups_word, ")")
    }
    lab
  }
  plabs <- vapply(panels, panel_label, character(1))
  path_df <- list()
  lab_df <- list()
  for (k in panels) {
    for (i in seq_len(nrow(edges))) {
      p <- geo[[i]]$path
      p$edge <- paste(k, i)
      p$panel <- panel_label(k)
      p$cluster <- as.character(k)
      est <- NA_real_
      dashed <- FALSE
      if (!is.null(betas)) {
        r <- betas[betas$cluster == k & betas$response == edges$to[i] &
                     betas$predictor == edges$from[i], , drop = FALSE]
        if (nrow(r)) {
          est <- r$estimate[1]
          if (!is.null(r$p)) dashed <- isTRUE(r$p[1] >= 0.05)
        }
      }
      p$est <- est
      p$dashed <- dashed
      path_df[[length(path_df) + 1]] <- p
      l <- geo[[i]]$label
      l$panel <- panel_label(k)
      l$text <- if (is.na(est)) "" else fmt_coef(est)
      lab_df[[length(lab_df) + 1]] <- l
    }
  }
  path_df <- do.call(rbind, path_df)
  lab_df <- do.call(rbind, lab_df)
  node_df <- do.call(rbind, lapply(panels, function(k) {
    e <- ellipse_points(nodes)
    e$panel <- panel_label(k)
    e$id <- paste(k, e$name)
    e
  }))
  text_df <- do.call(rbind, lapply(panels, function(k) {
    n <- nodes
    n$panel <- panel_label(k)
    n
  }))
  arc_df <- if (length(arcs)) {
    do.call(rbind, lapply(panels, function(k) {
      do.call(rbind, lapply(seq_along(arcs), function(i) {
        a <- arcs[[i]]
        a$arc <- paste(k, i)
        a$panel <- panel_label(k)
        a
      }))
    }))
  }
  for (d in c("path_df", "lab_df", "node_df", "text_df", "arc_df")) {
    x <- get(d)
    if (!is.null(x)) {
      x$panel <- factor(x$panel, levels = plabs)
      assign(d, x)
    }
  }
  g <- ggplot2::ggplot()
  if (!is.null(arc_df)) {
    g <- g + ggplot2::geom_path(
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$arc), data = arc_df,
      colour = "#9AA5AB", linewidth = 0.45, lineend = "round",
      arrow = grid::arrow(length = grid::unit(0.065, "inches"), ends = "both", type = "open", angle = 26)
    )
  }
  if (!is.null(path_df)) {
    max_abs <- suppressWarnings(max(abs(path_df$est), na.rm = TRUE))
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
    path_df$w <- ifelse(is.na(path_df$est), 0.7, 0.35 + 2.4 * abs(path_df$est) / max_abs)
    g <- g + ggplot2::geom_path(
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$edge, colour = .data$cluster,
                   linewidth = .data$w, linetype = .data$dashed),
      data = path_df, lineend = "butt", linejoin = "round",
      arrow = grid::arrow(length = grid::unit(0.09, "inches"), type = "closed", angle = 22)
    )
  }
  g <- g +
    ggplot2::geom_polygon(ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
                          data = node_df, fill = "white", colour = ink, linewidth = 0.55) +
    ggplot2::geom_text(ggplot2::aes(x = .data$x, y = .data$y, label = .data$name),
                       data = text_df, colour = ink, size = 4.1, fontface = "bold")
  if (!is.null(lab_df) && any(nzchar(lab_df$text))) {
    g <- g + label_layer(ggplot2::aes(x = .data$x, y = .data$y, label = .data$text),
                         data = lab_df[nzchar(lab_df$text), , drop = FALSE])
  }
  g <- g +
    ggplot2::scale_colour_manual(values = cols, na.value = ink) +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22")) +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.25)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.35)) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(colour = ink, face = "bold", hjust = 0, size = 12.5,
                                         margin = ggplot2::margin(4, 0, 8, 2)),
      panel.spacing = grid::unit(1.6, "lines"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(6, 10, 6, 10)
    )
  if (!is.null(betas)) {
    g <- g + ggplot2::facet_wrap(~panel, ncol = min(length(panels), 2))
  }
  g
}

# ---- Model selection ------------------------------------------------------------

#' Scree plot of a model-selection criterion
#'
#' @param ms Result of [assemble_selection()] or [mmgsem::ModelSelection()].
#' @param criterion One of `"Chull"` (log-likelihood, used for the convex
#'   hull), `"AIC3"`, `"AIC"`, `"BIC_G"`, `"BIC_N"`, `"ICL"`.
#' @param selected Number of clusters to highlight.
#' @param xlab Label of the horizontal axis.
#' @return A ggplot object.
#' @export
plot_selection <- function(ms, criterion = "AIC3", selected = NULL, xlab = "Clusters") {
  ov <- ms$Overview
  col <- if (identical(criterion, "Chull")) "LL" else criterion
  d <- data.frame(k = ov$Clusters, value = ov[[col]])
  ylab <- if (identical(criterion, "Chull")) "Log-likelihood" else criterion
  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$k, y = .data$value)) +
    ggplot2::geom_line(colour = ink, linewidth = 0.6) +
    ggplot2::geom_point(colour = ink, size = 2.4)
  if (!is.null(selected) && selected %in% d$k) {
    g <- g + ggplot2::geom_point(data = d[d$k == selected, , drop = FALSE], shape = 21,
                                 fill = mark_col, colour = ink, size = 5.2, stroke = 1)
  }
  g + ggplot2::scale_x_continuous(breaks = d$k) +
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    ggplot2::labs(x = xlab, y = ylab) +
    theme_mmg() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# ---- Posterior probabilities ----------------------------------------------------

#' Heat map of the posterior cluster-membership probabilities
#'
#' @param fit An `mmgsem` fit.
#' @param cluster_word Word used on the horizontal axis.
#' @return A ggplot object.
#' @export
plot_posteriors <- function(fit, cluster_word = "Cluster") {
  cl <- tidy_clusters(fit)
  k <- n_clusters(fit)
  ord <- order(cl$cluster, seq_len(nrow(cl)))
  groups <- cl$group[ord]
  long <- do.call(rbind, lapply(seq_len(k), function(j) {
    data.frame(group = cl$group, cluster = j, p = cl[[paste0("p", j)]],
               stringsAsFactors = FALSE)
  }))
  long$group <- factor(long$group, levels = rev(groups))
  long$clusterf <- factor(long$cluster, levels = seq_len(k),
                          labels = paste(cluster_word, seq_len(k)))
  long$label <- ifelse(long$p > 0.005 & long$p < 0.995,
                       sub("^0", "", formatC(long$p, format = "f", digits = 2)), "")
  ggplot2::ggplot(long, ggplot2::aes(x = .data$clusterf, y = .data$group)) +
    ggplot2::geom_tile(ggplot2::aes(fill = as.character(.data$cluster), alpha = .data$p),
                       colour = "white", linewidth = 1.1) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), colour = ink, size = 3.3) +
    ggplot2::scale_fill_manual(values = cluster_colours(k)) +
    ggplot2::scale_alpha_continuous(range = c(0.04, 1), limits = c(0, 1)) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_mmg() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(colour = ink, face = "bold"),
                   axis.text.y = ggplot2::element_text(colour = ink))
}

# ---- Coefficients ----------------------------------------------------------------

#' Regression coefficients per cluster, with confidence intervals if available
#'
#' @param betas Result of [tidy_betas()].
#' @param cluster_word Word used on the vertical axis.
#' @return A ggplot object.
#' @export
plot_betas <- function(betas, cluster_word = "Cluster") {
  k <- max(betas$cluster)
  betas$clusterf <- factor(betas$cluster, levels = rev(seq_len(k)),
                           labels = rev(paste(cluster_word, seq_len(k))))
  betas$parameter <- factor(betas$parameter, levels = unique(betas$parameter))
  g <- ggplot2::ggplot(betas, ggplot2::aes(x = .data$estimate, y = .data$clusterf,
                                           colour = as.character(.data$cluster))) +
    ggplot2::geom_vline(xintercept = 0, colour = ink_soft, linewidth = 0.4)
  if (!is.null(betas$ci_lower)) {
    g <- g + ggplot2::geom_segment(
      ggplot2::aes(x = .data$ci_lower, xend = .data$ci_upper, yend = .data$clusterf),
      linewidth = 1.1, lineend = "round"
    )
  }
  g + ggplot2::geom_point(size = 3) +
    ggplot2::scale_colour_manual(values = cluster_colours(k)) +
    ggplot2::facet_wrap(~parameter, nrow = 1) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_mmg() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = ink),
                   panel.spacing = grid::unit(1.2, "lines"))
}

# ---- Random starts ----------------------------------------------------------------

#' Log-likelihood reached by each random start
#'
#' @param fit An `mmgsem` fit.
#' @param xlab,ylab Axis labels.
#' @return A ggplot object.
#' @export
plot_starts <- function(fit, xlab = "Random starts, best first", ylab = "Log-likelihood (Step 2)") {
  sd <- start_diagnostics(fit)
  d <- data.frame(ll = sort(sd$loglik, decreasing = TRUE))
  d$i <- seq_len(nrow(d))
  d$best <- d$ll >= sd$best - 1e-3
  ggplot2::ggplot(d, ggplot2::aes(x = .data$i, y = .data$ll)) +
    ggplot2::geom_hline(yintercept = sd$best, colour = rule_col, linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(fill = .data$best), shape = 21, colour = ink,
                        size = 3, stroke = 0.6) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = ink, `FALSE` = "white")) +
    ggplot2::scale_x_continuous(breaks = function(l) unique(round(pretty(l)))) +
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
    ggplot2::labs(x = xlab, y = ylab) +
    theme_mmg() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# ---- Group-level covariates --------------------------------------------------------

#' A group-level variable compared across clusters
#'
#' Descriptive only: the clusters are estimated without this variable.
#'
#' @param clusters Result of [tidy_clusters()].
#' @param values Numeric vector with one value per group, in the same order.
#' @param label Axis label for the variable.
#' @param cluster_word Word used on the horizontal axis.
#' @return A ggplot object.
#' @export
plot_covariate <- function(clusters, values, label = "", cluster_word = "Cluster") {
  k <- max(clusters$cluster)
  d <- data.frame(cluster = clusters$cluster, value = as.numeric(values),
                  group = clusters$group)
  d <- d[!is.na(d$value), , drop = FALSE]
  d$clusterf <- factor(d$cluster, levels = seq_len(k), labels = paste(cluster_word, seq_len(k)))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$clusterf, y = .data$value,
                                  colour = as.character(.data$cluster))) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = as.character(.data$cluster)), alpha = 0.12,
                          width = 0.5, outlier.shape = NA, linewidth = 0.5) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.09, height = 0, seed = 1),
                        size = 2.6, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = cluster_colours(k)) +
    ggplot2::scale_fill_manual(values = cluster_colours(k)) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(x = NULL, y = label) +
    theme_mmg() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(colour = ink, face = "bold"))
}
