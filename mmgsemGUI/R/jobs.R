# Background jobs ----------------------------------------------------------------
# Estimation takes from seconds to hours, so it never runs in the process that
# serves the interface. Each job is a separate R process started with callr;
# the interface polls it, shows progress and can cancel it.

with_warnings <- function(expr) {
  w <- character()
  value <- withCallingHandlers(expr, warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = unique(w))
}

# ---- What the jobs do (run inside the child process) ---------------------------

job_measurement <- function(data, S1, group, noninv, level = "metric",
                            missing = "listwise", ordinal = FALSE, blocks = FALSE) {
  t0 <- proc.time()[["elapsed"]]
  r <- with_warnings(fit_measurement(data, S1, group, noninv, level, missing = missing,
                                     ordinal = ordinal, blocks = blocks))
  fit <- r$value
  list(
    fit = if (identical(level, "metric")) fit else NULL,
    summary = measurement_summary(fit),
    loadings = tryCatch(loading_ranges(fit), error = function(e) NULL),
    warnings = r$warnings,
    seconds = proc.time()[["elapsed"]] - t0
  )
}

job_screen <- function(fit) {
  loading_screen(fit)
}

job_clusters <- function(data, S1, S2, group, k, s1_fit, seed, nstarts, max_it,
                         endogenous_cov, endo_group_specific, missing = "listwise",
                         ordinal = FALSE, noninv = character()) {
  r <- with_warnings(fit_clusters(
    data = data, S1 = S1, S2 = S2, group = group, k = k, s1_fit = s1_fit,
    seed = seed, nstarts = nstarts, max_it = max_it,
    endogenous_cov = endogenous_cov, endo_group_specific = endo_group_specific,
    missing = missing, ordinal = ordinal, noninv = noninv
  ))
  fit <- r$value
  seconds <- attr(fit, "seconds")
  # The measurement model is large and identical for every number of clusters;
  # it is attached again in the interface process.
  fit$MM <- NULL
  fit$final_fit <- NULL
  list(fit = fit, warnings = r$warnings, seconds = seconds)
}

job_se <- function(fit, naive) {
  r <- with_warnings(standard_errors(fit, naive = naive))
  list(se = r$value, warnings = r$warnings, seconds = attr(r$value, "seconds"))
}

# ---- Starting, polling and stopping jobs ----------------------------------------

job_start <- function(kind, args) {
  fun <- switch(
    kind,
    measurement = function(...) mmgsemGUI:::job_measurement(...),
    screen = function(...) mmgsemGUI:::job_screen(...),
    clusters = function(...) mmgsemGUI:::job_clusters(...),
    se = function(...) mmgsemGUI:::job_se(...),
    stop("Unknown job: ", kind, call. = FALSE)
  )
  log <- tempfile("mmgsemgui_", fileext = ".log")
  proc <- callr::r_bg(fun, args = args, stdout = log, stderr = "2>&1",
                      supervise = TRUE)
  list(kind = kind, proc = proc, log = log, started = Sys.time())
}

job_running <- function(job) {
  isTRUE(tryCatch(job$proc$is_alive(), error = function(e) FALSE))
}

# Result of a finished job: list(ok = TRUE, value = ...) or
# list(ok = FALSE, message = "...").
job_result <- function(job) {
  tryCatch(
    list(ok = TRUE, value = job$proc$get_result()),
    error = function(e) {
      msg <- if (!is.null(e$parent)) conditionMessage(e$parent) else conditionMessage(e)
      list(ok = FALSE, message = msg)
    }
  )
}

job_stop <- function(job) {
  try(job$proc$kill(), silent = TRUE)
  invisible(NULL)
}

job_seconds <- function(job) {
  as.numeric(difftime(Sys.time(), job$started, units = "secs"))
}

# Last percentage written by a text progress bar in the job's output, or NA.
job_progress <- function(job) {
  if (!file.exists(job$log)) return(NA_real_)
  size <- file.size(job$log)
  if (is.na(size) || size == 0) return(NA_real_)
  con <- file(job$log, "rb")
  on.exit(close(con))
  if (size > 400) seek(con, size - 400)
  txt <- tryCatch(rawToChar(readBin(con, "raw", 400)), error = function(e) "")
  m <- regmatches(txt, gregexpr("[0-9]+%", txt))[[1]]
  if (!length(m)) return(NA_real_)
  as.numeric(sub("%", "", m[length(m)]))
}

# Number of models estimated at the same time.
job_workers <- function() {
  n <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA)
  if (is.na(n)) n <- 2L
  max(1L, min(as.integer(n) - 1L, 6L))
}

fmt_duration <- function(sec) {
  sec <- round(sec)
  if (is.na(sec)) return("")
  if (sec < 60) return(paste0(sec, " s"))
  if (sec < 3600) return(paste0(sec %/% 60, " min ", sprintf("%02d", sec %% 60), " s"))
  paste0(sec %/% 3600, " h ", sprintf("%02d", (sec %% 3600) %/% 60), " min")
}
