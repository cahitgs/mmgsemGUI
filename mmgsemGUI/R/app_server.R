# Interface logic -------------------------------------------------------------------

tutorial_rows <- function() {
  list(
    list(name = "F1", items = paste0("V", 1:5)),
    list(name = "F2", items = paste0("V", 6:10)),
    list(name = "F3", items = paste0("V", 11:15)),
    list(name = "F4", items = paste0("V", 16:20))
  )
}

guess_group <- function(df) {
  nm <- names(df)
  hit <- grep("^(group|grp|country|cntry|nation|school|class|cluster|region|site)$", nm,
              ignore.case = TRUE)
  if (length(hit)) return(nm[hit[1]])
  nu <- vapply(df, function(x) length(unique(x)), integer(1))
  ok <- which(nu >= 2 & nu <= max(2, nrow(df) / 5))
  if (length(ok)) return(nm[ok[which.min(nu[ok])]])
  nm[1]
}

app_server <- function(input, output, session) {
  lang <- pick_lang(parseQueryString(isolate(session$clientData$url_search) %||% "")$lang)
  L <- translator(lang)
  cw <- L("cluster")
  track_session(session)

  rv <- reactiveValues(
    raw = NULL, raw_name = NULL, raw_reader = NULL, is_example = FALSE, data_id = 0L,
    mm_gen = 0L, s2_gen = 0L, noninv_pending = NULL,
    mm = NULL, mm_error = NULL, mm_config = NULL, screen = NULL, mm_extra_error = NULL,
    ms = NULL, ms_spec = NULL, ms_errors = NULL, ms_notes = NULL, k_preset = NULL,
    se = list(), se_error = NULL, cov = NULL
  )
  # Non-reactive bookkeeping: what the builders currently show, and the jobs.
  B <- new.env()
  B$rows <- list(list(name = "F1", items = character()), list(name = "F2", items = character()))
  B$group <- NULL
  B$lvs <- character()
  B$regs <- list()
  J <- new.env()
  J$jobs <- list()
  J$queue <- integer()
  J$kfits <- list()
  J$kwarnings <- list()
  J$kstatus <- NULL
  J$ms_args <- NULL
  J$ms_snapshot <- NULL
  tick <- reactiveVal(0L)
  active <- reactiveVal(FALSE)
  running <- reactiveVal(character())

  # ---- Jobs ---------------------------------------------------------------------
  refresh_running <- function() {
    now <- sort(names(J$jobs))
    if (!identical(now, isolate(running()))) running(now)
  }
  start_job <- function(id, kind, args, on_done) {
    job <- job_start(kind, args)
    job$on_done <- on_done
    J$jobs[[id]] <- job
    active(TRUE)
    refresh_running()
  }
  stop_jobs <- function(pattern) {
    for (id in grep(pattern, names(J$jobs), value = TRUE)) {
      job_stop(J$jobs[[id]])
      J$jobs[[id]] <- NULL
    }
    refresh_running()
  }
  is_running <- function(pattern) any(grepl(pattern, running()))

  observe({
    req(active())
    invalidateLater(700, session)
    isolate({
      for (id in names(J$jobs)) {
        job <- J$jobs[[id]]
        if (!job_running(job)) {
          J$jobs[[id]] <- NULL
          res <- job_result(job)
          res$wall <- job_seconds(job)
          tryCatch(job$on_done(res), error = function(e) {
            showNotification(conditionMessage(e), type = "error", duration = NULL)
          })
        }
      }
      launch_queued()
      refresh_running()
      if (!length(J$jobs) && !length(J$queue)) active(FALSE)
      tick(tick() + 1L)
    })
  })
  session$onSessionEnded(function() {
    for (job in J$jobs) job_stop(job)
  })

  output$job_badge <- renderUI({
    n <- length(running())
    if (!n) return(NULL)
    tags$span(class = "job-badge", tags$span(class = "pulse"), L("jobs_running", n))
  })

  # ---- Navigation -----------------------------------------------------------------
  go <- function(id) updateTabsetPanel(session, "step", selected = id)
  lapply(step_ids, function(id) {
    observeEvent(input[[paste0("go_", id)]], go(id))
  })
  observeEvent(input$next_data, go("measure"))
  observeEvent(input$next_mm, go("structure"))
  observeEvent(input$next_s2, go("clusters"))
  observeEvent(input$next_ms, go("results"))
  observeEvent(input$next_res, go("tests"))

  # ---- Data -----------------------------------------------------------------------
  has_data <- reactive(!is.null(rv$raw))
  group_now <- reactive({
    g <- input$group_var
    if (has_data() && length(g) == 1 && g %in% names(rv$raw)) g else NULL
  })
  clear_results <- function(from = "mm") {
    stop_jobs(if (from == "mm") "." else "^(k|se)")
    J$queue <- integer()
    J$kfits <- list()
    J$kwarnings <- list()
    J$kstatus <- NULL
    if (from == "mm") {
      rv$mm <- NULL
      rv$mm_error <- NULL
      rv$mm_config <- NULL
      rv$screen <- NULL
      rv$mm_extra_error <- NULL
    }
    rv$ms <- NULL
    rv$ms_spec <- NULL
    rv$ms_errors <- NULL
    rv$ms_notes <- NULL
    rv$se <- list()
    rv$se_error <- NULL
  }
  set_data <- function(df, name, example = FALSE, rows = NULL, group = NULL) {
    rv$raw_reader <- attr(df, "reader") %||% rv$raw_reader
    rv$raw <- df
    rv$raw_name <- name
    rv$is_example <- example
    rv$data_id <- rv$data_id + 1L
    rv$cov <- NULL
    clear_results("mm")
    g <- group %||% guess_group(df)
    B$group <- g
    updateSelectInput(session, "group_var", choices = names(df), selected = g)
    rerender_builder(rows %||% list(list(name = "F1", items = character()),
                                    list(name = "F2", items = character())))
  }
  observeEvent(input$load_example, {
    set_data(example_data(), "example", example = TRUE, rows = tutorial_rows(), group = "group")
    set_noninv(c("F1 =~ V2", "F2 =~ V7", "F3 =~ V12", "F4 =~ V17"),
               free_loadings(rows_to_factors(tutorial_rows())))
    rerender_s2(c("F1", "F2", "F3", "F4"), list(F4 = c("F1", "F3"), F3 = c("F1", "F2")))
    updateRadioButtons(session, "mm_mode", selected = "builder")
    updateRadioButtons(session, "s2_mode", selected = "builder")
    updateRadioButtons(session, "mm_missing", selected = "listwise")
    updateCheckboxInput(session, "mm_ordinal", value = FALSE)
    updateCheckboxInput(session, "mm_blocks", value = FALSE)
    updateNumericInput(session, "k_min", value = 1)
    updateNumericInput(session, "k_max", value = 6)
    updateNumericInput(session, "nstarts", value = 20)
    updateNumericInput(session, "seed", value = 1)
  })
  observeEvent(input$datafile, {
    f <- input$datafile
    df <- tryCatch(read_data_file(f$datapath, f$name), error = function(e) {
      showNotification(paste(L("data_read_error"), conditionMessage(e)), type = "error",
                       duration = 10)
      NULL
    })
    if (is.null(df)) return()
    set_data(df, f$name)
    set_noninv(character(), character())
    rerender_s2(character(), list())
  })
  observeEvent(input$group_var, {
    g <- group_now()
    if (is.null(g) || identical(g, B$group)) return()
    B$group <- g
    rows <- lapply(isolate(mm_rows()), function(r) {
      r$items <- setdiff(r$items, g)
      r
    })
    rerender_builder(rows)
  }, ignoreInit = TRUE)

  output$data_stage <- renderUI({
    if (!has_data()) {
      # Table 1 of the tutorial; its last row is numbered 2400 there, but the
      # example has 24 groups of 200 respondents.
      ex <- list(c("1", "2", "…", "4800"), c("1", "1", "…", "24"),
                 c("3", "5", "…", "3"), c("2", "4", "…", "1"),
                 c("…", "…", "…", "…"), c("3", "3", "…", "2"))
      return(sheet(
        title = L("data_empty_title"), class = "empty",
        tags$p(L("data_empty_text")),
        html_table(ex, header = c("ID", L("group"), "V1", "V2", "…", "V20"),
                   align = rep("r", 6)),
        hint(L("data_empty_formats"))
      ))
    }
    df <- rv$raw
    g <- group_now()
    tiles <- list(tile(L("rows"), fmt_int(nrow(df))), tile(L("columns"), fmt_int(ncol(df))))
    warn <- NULL
    if (!is.null(g)) {
      sizes <- table(df[[g]])
      tiles <- c(tiles, list(
        tile(L("groups"), fmt_int(length(sizes))),
        tile(L("group_size"), if (min(sizes) == max(sizes)) fmt_int(min(sizes)) else
          paste0(fmt_int(min(sizes)), "–", fmt_int(max(sizes))))
      ))
      if (length(sizes) > nrow(df) / 2) warn <- notice(L("data_group_warn"), type = "warn")
      if (length(sizes) < 2) warn <- notice(L("data_one_group"), type = "warn")
    }
    show <- utils::head(df, 8)[, seq_len(min(ncol(df), 10)), drop = FALSE]
    cells <- lapply(show, function(x) if (is.numeric(x)) formatC(x, digits = 3, format = "g") else as.character(x))
    tagList(
      tags$div(class = "tiles", tiles),
      warn,
      sheet(
        title = L("data_preview", if (isTRUE(rv$is_example)) L("data_example_name") else rv$raw_name),
        html_table(cells, header = names(show),
                   align = ifelse(vapply(show, is.numeric, logical(1)), "r", "l")),
        if (ncol(df) > 10) hint(L("data_more_cols", ncol(df) - 10))
      )
    )
  })
  output$data_footer <- renderUI({
    if (is.null(group_now())) return(NULL)
    tags$div(class = "step-foot", actionButton("next_data", L("continue_measure"), class = "btn-ink"))
  })

  # ---- Measurement model: builder ----------------------------------------------------
  rows_to_factors <- function(rows) {
    out <- list()
    for (r in rows) {
      nm <- clean_name(r$name %||% "")
      if (nzchar(nm) && length(r$items)) out[[nm]] <- r$items
    }
    out
  }
  rerender_builder <- function(rows) {
    B$rows <- rows
    rv$mm_gen <- isolate(rv$mm_gen) + 1L
  }
  mm_rows <- reactive({
    gen <- rv$mm_gen
    lapply(seq_along(B$rows), function(i) {
      nm <- input[[sprintf("f_name_%d_%d", gen, i)]]
      if (is.null(nm)) return(B$rows[[i]])
      list(name = nm, items = input[[sprintf("f_items_%d_%d", gen, i)]] %||% character())
    })
  })
  output$mm_builder <- renderUI({
    gen <- rv$mm_gen
    df <- isolate(rv$raw)
    choices <- if (is.null(df)) character() else numeric_columns(df, exclude = B$group %||% character())
    tagList(
      if (is.null(df)) notice(L("mm_need_data"), type = "info"),
      lapply(seq_along(B$rows), function(i) {
        r <- B$rows[[i]]
        tags$div(
          class = "factor-row",
          textInput(sprintf("f_name_%d_%d", gen, i), if (i == 1) L("factor_name"),
                    value = r$name, width = "100%"),
          selectizeInput(sprintf("f_items_%d_%d", gen, i), if (i == 1) L("factor_items"),
                         choices = choices, selected = intersect(r$items, choices),
                         multiple = TRUE, width = "100%",
                         options = list(plugins = list("remove_button"),
                                        placeholder = L("items_placeholder")))
        )
      })
    )
  })
  observeEvent(input$add_factor, {
    rows <- mm_rows()
    rerender_builder(c(rows, list(list(name = paste0("F", length(rows) + 1), items = character()))))
  })
  observeEvent(input$remove_factor, {
    rows <- mm_rows()
    if (length(rows) > 1) rerender_builder(rows[-length(rows)])
  })
  observeEvent(input$mm_mode, {
    if (identical(input$mm_mode, "syntax") && !nzchar(norm_syntax(input$s1_text))) {
      updateTextAreaInput(session, "s1_text", value = build_s1(rows_to_factors(mm_rows())))
    }
  }, ignoreInit = TRUE)

  S1 <- reactive({
    if (identical(input$mm_mode, "syntax")) {
      norm_syntax(input$s1_text)
    } else {
      build_s1(rows_to_factors(mm_rows()))
    }
  })
  mm_factors <- reactive({
    s <- S1()
    if (!nzchar(s)) return(list())
    tryCatch(parse_s1(s), error = function(e) structure(list(), error = conditionMessage(e)))
  })
  lv_now <- reactiveVal(character())
  observe({
    n <- names(mm_factors()) %||% character()
    if (!identical(n, isolate(lv_now()))) lv_now(n)
  })
  output$s1_preview <- renderText({
    s <- S1()
    if (nzchar(s)) s else L("syntax_empty")
  })

  # Loadings released across groups
  set_noninv <- function(selected, choices) {
    rv$noninv_pending <- selected
    updateSelectizeInput(session, "noninv", choices = choices, selected = selected)
  }
  observe({
    f <- mm_factors()
    choices <- if (length(f)) free_loadings(f) else character()
    keep <- isolate(rv$noninv_pending %||% input$noninv %||% character())
    updateSelectizeInput(session, "noninv", choices = choices, selected = intersect(keep, choices))
  })
  observeEvent(input$noninv, {
    p <- rv$noninv_pending
    if (!is.null(p) && setequal(input$noninv %||% character(), p)) rv$noninv_pending <- NULL
  }, ignoreNULL = FALSE)
  noninv_now <- reactive({
    f <- mm_factors()
    choices <- if (length(f)) free_loadings(f) else character()
    sort(intersect(rv$noninv_pending %||% input$noninv %||% character(), choices))
  })

  mm_problems <- reactive({
    f <- mm_factors()
    p <- character()
    if (!is.null(attr(f, "error"))) return(paste(L("syntax_error"), attr(f, "error")))
    if (!length(f)) return(character())
    if (any(lengths(f) < 2)) p <- c(p, L("mm_few_items"))
    items <- unlist(f)
    if (anyDuplicated(items)) p <- c(p, L("mm_dup_items", paste(unique(items[duplicated(items)]), collapse = ", ")))
    if (has_data()) {
      miss <- setdiff(items, names(rv$raw))
      if (length(miss)) p <- c(p, L("mm_missing_items", paste(miss, collapse = ", ")))
    }
    p
  })
  # How the measurement model is estimated (Section 4.2.1, Appendices A and B)
  mm_opts <- reactive({
    list(missing = if (identical(input$mm_missing, "fiml")) "fiml" else "listwise",
         ordinal = isTRUE(input$mm_ordinal), blocks = isTRUE(input$mm_blocks))
  })
  mm_option_problem <- reactive({
    o <- mm_opts()
    if (o$ordinal && (o$blocks || o$missing == "fiml")) return(L("opt_ordinal_alone"))
    s <- S1()
    if (o$blocks && nzchar(s) && is.null(attr(mm_factors(), "error"))) {
      ok <- !inherits(try(split_blocks(s), silent = TRUE), "try-error")
      if (!ok) return(L("opt_blocks_plain"))
    }
    NULL
  })
  mm_spec <- reactive({
    c(list(data_id = rv$data_id, group = group_now(), S1 = S1(), noninv = noninv_now()),
      mm_opts())
  })
  # The estimated model belongs to the current settings ...
  mm_current <- reactive(!is.null(rv$mm) && identical(rv$mm$spec, mm_spec()))
  # ... and can be used: a model that did not converge has no valid estimates.
  mm_ok <- reactive(mm_current() && isTRUE(rv$mm$summary$converged))

  observeEvent(input$fit_mm, {
    if (is_running("^mm$")) return()
    f <- mm_factors()
    g <- group_now()
    if (is.null(g) || !length(f) || !is.null(attr(f, "error")) || any(lengths(f) < 2)) {
      showNotification(L("mm_not_ready"), type = "warning")
      return()
    }
    if (!is.null(mm_option_problem())) {
      showNotification(mm_option_problem(), type = "error", duration = 10)
      return()
    }
    spec <- mm_spec()
    items <- unique(unlist(f))
    pd <- tryCatch(prepare_data(rv$raw, g, items, missing = spec$missing), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 10)
      NULL
    })
    if (is.null(pd)) return()
    if ("one_group" %in% pd$problems) {
      showNotification(L("data_one_group"), type = "error", duration = 10)
      return()
    }
    if (spec$ordinal) {
      bad <- ordinal_problems(pd$data, items)
      if (length(bad)) {
        showNotification(L("opt_ordinal_bad", paste(bad, collapse = ", ")), type = "error",
                         duration = 12)
        return()
      }
    }
    clear_results("mm")
    start_job("mm", "measurement",
              list(data = pd$data, S1 = spec$S1, group = g, noninv = spec$noninv,
                   missing = spec$missing, ordinal = spec$ordinal, blocks = spec$blocks),
              function(res) {
                if (!res$ok) {
                  rv$mm_error <- res$message
                  return()
                }
                v <- res$value
                rv$mm_error <- NULL
                rv$mm <- list(spec = spec, fit = v$fit, summary = v$summary,
                              loadings = v$loadings, warnings = v$warnings,
                              seconds = v$seconds, data = pd$data, items = items,
                              n_removed = pd$n_removed, n_total = pd$n_total,
                              n_incomplete = pd$n_incomplete,
                              tiny = "tiny_groups" %in% pd$problems, factors = f)
              })
  })
  observeEvent(input$mm_stop, {
    stop_jobs("^mm")
  })
  output$mm_cancel <- renderUI({
    if (is_running("^mm")) actionButton("mm_stop", L("cancel"), class = "btn-quiet")
  })
  output$mm_status <- renderUI({
    out <- list()
    if (is_running("^mm$")) {
      tick()
      job <- J$jobs[["mm"]]
      out <- c(out, list(sheet(
        class = "busy",
        tags$p(class = "busy-title", L("mm_running")),
        progress_bar(NA),
        hint(L("elapsed", fmt_duration(if (is.null(job)) 0 else job_seconds(job))))
      )))
    }
    if (!is.null(rv$mm_error)) {
      out <- c(out, list(notice(tags$strong(L("mm_failed")), tags$br(), rv$mm_error, type = "error")))
    }
    for (p in mm_problems()) out <- c(out, list(notice(p, type = "warn")))
    if (!is.null(mm_option_problem())) {
      out <- c(out, list(notice(mm_option_problem(), type = "warn")))
    }
    if (!is.null(rv$mm) && !mm_current() && !is_running("^mm$")) {
      out <- c(out, list(notice(L("mm_stale"), type = "warn")))
    }
    tagList(out)
  })
  output$mm_result <- renderUI({
    m <- rv$mm
    if (is.null(m)) return(NULL)
    s <- m$summary
    fm <- s$fit
    ordinal <- isTRUE(m$spec$ordinal)
    extras <- list()
    if (!isTRUE(s$converged)) {
      extras <- c(extras, list(notice(L("mm_not_converged"), type = "error")))
    } else if (identical(s$post_check, FALSE)) {
      extras <- c(extras, list(notice(L("mm_inadmissible"), type = "warn")))
    }
    if (m$n_removed > 0) {
      extras <- c(extras, list(notice(L("mm_removed", fmt_int(m$n_removed), fmt_int(m$n_total)), type = "info")))
    }
    if (isTRUE((m$n_incomplete %||% 0) > 0)) {
      extras <- c(extras, list(notice(L("mm_fiml_rows", fmt_int(m$n_incomplete), fmt_int(nrow(m$data))), type = "info")))
    }
    if (isTRUE(m$tiny)) extras <- c(extras, list(notice(L("mm_tiny_groups"), type = "warn")))
    for (w in m$warnings) extras <- c(extras, list(notice(tags$strong(L("lavaan_says")), " ", w, type = "warn")))
    lo <- m$loadings
    load_tbl <- NULL
    if (!is.null(lo)) {
      load_tbl <- tags$details(
        class = "more",
        tags$summary(L("mm_loadings")),
        html_table(
          list(lo$loading,
               ifelse(lo$varies, paste0(fmt_num(lo$min, 2), " – ", fmt_num(lo$max, 2)), fmt_num(lo$min, 2)),
               ifelse(lo$varies, L("mm_released"), L("mm_equal"))),
          header = c(L("loading"), L("estimate_range"), L("across_groups")),
          align = c("l", "r", "l")
        )
      )
    }
    fit_block <- if (!is.null(s$blocks)) {
      b <- s$blocks
      col <- function(f) vapply(b, f, character(1))
      html_table(
        list(col(function(p) paste(p$factors, collapse = ", ")),
             col(function(p) fmt_num(p$fit$chisq, 1)),
             col(function(p) fmt_int(as.integer(p$fit$df))),
             col(function(p) fmt_fit(p$fit$cfi)), col(function(p) fmt_fit(p$fit$tli)),
             col(function(p) fmt_fit(p$fit$rmsea)), col(function(p) fmt_fit(p$fit$srmr)),
             col(function(p) if (isTRUE(p$converged)) "" else L("mm_block_failed"))),
        header = c(L("factor_name"), "χ²", "df", "CFI", "TLI", "RMSEA", "SRMR", ""),
        align = c("l", rep("r", 6), "l")
      )
    } else {
      tags$div(class = "tiles",
               tile("χ²", fmt_num(fm$chisq, 1), paste0("df = ", fmt_int(as.integer(fm$df)))),
               tile("CFI", fmt_fit(fm$cfi)), tile("TLI", fmt_fit(fm$tli)),
               tile("RMSEA", fmt_fit(fm$rmsea)), tile("SRMR", fmt_fit(fm$srmr)))
    }
    how <- if (ordinal) L("mm_how_ordinal") else if (!is.null(s$blocks)) L("mm_how_blocks", length(s$blocks)) else L("mm_how_default")
    tagList(
      sheet(
        title = L("mm_fit_title"),
        fit_block,
        hint(L("mm_fit_hint", fmt_int(s$ngroups), fmt_int(s$nobs), fmt_duration(m$seconds)), " ", how),
        extras,
        load_tbl
      ),
      sheet(
        title = L("mi_title"),
        if (ordinal) {
          tags$p(L("mi_not_ordinal"))
        } else {
          tagList(
            tags$p(L("mi_intro")),
            tags$div(class = "row-actions",
                     actionButton("mm_config_btn", L("mi_config_btn"), class = "btn-quiet"),
                     actionButton("mm_screen_btn", L("mi_screen_btn"), class = "btn-quiet")),
            uiOutput("mm_extra")
          )
        }
      )
    )
  })
  observeEvent(input$mm_config_btn, {
    m <- rv$mm
    if (is.null(m) || is_running("^mm_config$")) return()
    rv$mm_extra_error <- NULL
    start_job("mm_config", "measurement",
              list(data = m$data, S1 = m$spec$S1, group = m$spec$group,
                   noninv = character(), level = "configural",
                   missing = m$spec$missing %||% "listwise", blocks = isTRUE(m$spec$blocks)),
              function(res) {
                if (res$ok) rv$mm_config <- res$value$summary else rv$mm_extra_error <- res$message
              })
  })
  observeEvent(input$mm_screen_btn, {
    m <- rv$mm
    if (is.null(m) || is_running("^mm_screen$")) return()
    rv$mm_extra_error <- NULL
    start_job("mm_screen", "screen", list(fit = m$fit), function(res) {
      if (res$ok) rv$screen <- res$value else rv$mm_extra_error <- res$message
    })
  })
  output$mm_extra <- renderUI({
    m <- rv$mm
    if (is.null(m)) return(NULL)
    out <- list()
    if (is_running("^mm_(config|screen)$")) {
      tick()
      out <- c(out, list(tags$div(class = "inline-busy", progress_bar(NA), hint(L("working")))))
    }
    if (!is.null(rv$mm_extra_error)) out <- c(out, list(notice(rv$mm_extra_error, type = "error")))
    cf <- rv$mm_config
    if (!is.null(cf)) {
      tab <- if (!is.null(cf$blocks) && !is.null(m$summary$blocks)) {
        n <- min(length(cf$blocks), length(m$summary$blocks))
        a <- lapply(cf$blocks[seq_len(n)], `[[`, "fit")
        b <- lapply(m$summary$blocks[seq_len(n)], `[[`, "fit")
        num <- function(f) vapply(seq_len(n), f, character(1))
        html_table(
          list(num(function(i) paste(m$summary$blocks[[i]]$factors, collapse = ", ")),
               num(function(i) fmt_num(b[[i]]$chisq - a[[i]]$chisq, 1)),
               num(function(i) fmt_int(as.integer(b[[i]]$df - a[[i]]$df))),
               num(function(i) fmt_fit(a[[i]]$cfi)), num(function(i) fmt_fit(b[[i]]$cfi)),
               num(function(i) fmt_num(b[[i]]$cfi - a[[i]]$cfi, 3)),
               num(function(i) fmt_num(b[[i]]$rmsea - a[[i]]$rmsea, 3))),
          header = c(L("factor_name"), "Δχ²", "Δdf", L("mi_cfi_configural"),
                     L("mi_cfi_current"), "ΔCFI", "ΔRMSEA"),
          align = c("l", rep("r", 6))
        )
      } else {
        a <- cf$fit
        b <- m$summary$fit
        html_table(
          list(c(L("mi_configural"), L("mi_current"), L("mi_difference")),
               c(fmt_num(a$chisq, 1), fmt_num(b$chisq, 1), fmt_num(b$chisq - a$chisq, 1)),
               c(fmt_int(as.integer(a$df)), fmt_int(as.integer(b$df)), fmt_int(as.integer(b$df - a$df))),
               c(fmt_fit(a$cfi), fmt_fit(b$cfi), fmt_num(b$cfi - a$cfi, 3)),
               c(fmt_fit(a$rmsea), fmt_fit(b$rmsea), fmt_num(b$rmsea - a$rmsea, 3)),
               c(fmt_fit(a$srmr), fmt_fit(b$srmr), fmt_num(b$srmr - a$srmr, 3))),
          header = c(L("model"), "χ²", "df", "CFI", "RMSEA", "SRMR"),
          align = c("l", rep("r", 5))
        )
      }
      out <- c(out, list(tags$h3(L("mi_compare_title")), tab, hint(L("mi_compare_hint"))))
    }
    sc <- rv$screen
    if (!is.null(sc)) {
      top <- utils::head(sc, 8)
      out <- c(out, list(
        tags$h3(L("mi_screen_title")),
        if (!nrow(top)) tags$p(L("mi_screen_none")) else html_table(
          list(top$loading, fmt_num(top$score, 1), fmt_int(top$constraints)),
          header = c(L("loading"), L("mi_score"), L("mi_constraints")),
          align = c("l", "r", "r")
        ),
        hint(L("mi_screen_hint"))
      ))
    }
    tagList(out)
  })
  output$mm_footer <- renderUI({
    if (mm_ok()) {
      return(tags$div(class = "step-foot",
                      actionButton("next_mm", L("continue_structure"), class = "btn-ink")))
    }
    if (mm_current() && !is_running("^mm$")) {
      return(tags$div(class = "step-foot", hint(L("mm_blocked"))))
    }
    NULL
  })

  # ---- Structural model ------------------------------------------------------------------
  rerender_s2 <- function(lvs, regs) {
    B$lvs <- lvs
    B$regs <- regs
    rv$s2_gen <- isolate(rv$s2_gen) + 1L
  }
  # The regressions in a stable order: outcomes as they were given (the example
  # or the opened project) and then in the order of the factors, predictors in
  # the order of the factors. Otherwise drawing the builder would write an
  # unchanged model in another order and mark its results as out of date.
  ordered_regs <- function(regs, lvs) {
    regs <- lapply(regs, function(p) intersect(lvs, p))
    regs <- regs[lengths(regs) > 0]
    regs[intersect(union(names(B$regs), lvs), names(regs))]
  }
  s2_regs_builder <- reactive({
    gen <- rv$s2_gen
    lvs <- B$lvs
    if (is.null(input[[sprintf("s2_mark_%d", gen)]])) return(ordered_regs(B$regs, lvs))
    out <- list()
    for (i in seq_along(lvs)) {
      p <- input[[sprintf("pred_%d_%d", gen, i)]] %||% character()
      if (length(p)) out[[lvs[i]]] <- p
    }
    ordered_regs(out, lvs)
  })
  observeEvent(lv_now(), {
    lvs <- lv_now()
    regs <- isolate(s2_regs_builder())
    regs <- lapply(regs[intersect(names(regs), lvs)], intersect, lvs)
    rerender_s2(lvs, regs[lengths(regs) > 0])
  }, ignoreInit = TRUE)
  output$s2_builder <- renderUI({
    gen <- rv$s2_gen
    lvs <- B$lvs
    if (length(lvs) < 2) return(notice(L("s2_need_factors"), type = "info"))
    tagList(
      tags$div(style = "display:none", textInput(sprintf("s2_mark_%d", gen), NULL, value = "1")),
      lapply(seq_along(lvs), function(i) {
        selectizeInput(sprintf("pred_%d_%d", gen, i), L("predicted_by", lvs[i]),
                       choices = setdiff(lvs, lvs[i]), selected = B$regs[[lvs[i]]],
                       multiple = TRUE, width = "100%",
                       options = list(plugins = list("remove_button"),
                                      placeholder = L("pred_placeholder")))
      })
    )
  })
  observeEvent(input$s2_mode, {
    if (identical(input$s2_mode, "syntax") && !nzchar(norm_syntax(input$s2_text))) {
      updateTextAreaInput(session, "s2_text", value = build_s2(s2_regs_builder()))
    }
  }, ignoreInit = TRUE)
  S2 <- reactive({
    if (identical(input$s2_mode, "syntax")) norm_syntax(input$s2_text) else build_s2(s2_regs_builder())
  })
  s2_regs <- reactive({
    s <- S2()
    if (!nzchar(s)) return(list())
    tryCatch(parse_s2(s), error = function(e) structure(list(), error = conditionMessage(e)))
  })
  s2_issue <- reactive({
    r <- s2_regs()
    if (!is.null(attr(r, "error"))) return(paste(L("syntax_error"), attr(r, "error")))
    if (!length(r)) return(L("s2_empty"))
    codes <- check_s2(r, lv_now())
    if ("unknown_factor" %in% codes) return(L("s2_unknown"))
    if ("self" %in% codes) return(L("s2_self"))
    if ("cycle" %in% codes) return(L("s2_cycle"))
    NULL
  })
  s2_valid <- reactive(is.null(s2_issue()))
  output$s2_preview <- renderText({
    s <- S2()
    if (nzchar(s)) s else L("syntax_empty")
  })
  output$s2_problems <- renderUI({
    p <- s2_issue()
    if (!is.null(p)) notice(p, type = if (identical(p, L("s2_empty"))) "info" else "warn")
  })
  output$s2_plot <- renderPlot({
    req(s2_valid())
    plot_paths(s2_regs(), lv_now())
  }, res = 96)
  output$s2_footer <- renderUI({
    if (!s2_valid() || !mm_ok()) {
      if (s2_valid() && !mm_ok()) return(tags$div(class = "step-foot", hint(L("s2_need_mm"))))
      return(NULL)
    }
    tags$div(class = "step-foot", actionButton("next_s2", L("continue_clusters"), class = "btn-ink"))
  })

  # ---- Number of clusters -------------------------------------------------------------------
  ms_key <- reactive({
    list(S2 = S2(), endogenous_cov = isTRUE(input$endogenous_cov),
         endo_group_specific = isTRUE(input$endo_group_specific))
  })
  ms_ok <- reactive(mm_ok() && !is.null(rv$ms) && identical(rv$ms_spec$key, ms_key()))

  launch_queued <- function() {
    n_run <- sum(grepl("^k[0-9]+$", names(J$jobs)))
    while (length(J$queue) && n_run < job_workers()) {
      k <- J$queue[1]
      J$queue <- J$queue[-1]
      launch_k(k)
      n_run <- n_run + 1
    }
  }
  launch_k <- function(k) {
    J$kstatus$state[J$kstatus$k == k] <- "running"
    start_job(paste0("k", k), "clusters", c(J$ms_args, list(k = k)), function(res) {
      i <- which(J$kstatus$k == k)
      if (res$ok) {
        fit <- res$value$fit
        fit$MM <- isolate(rv$mm$fit)
        J$kfits[[as.character(k)]] <- fit
        J$kstatus$state[i] <- "done"
        J$kstatus$seconds[i] <- res$value$seconds
        J$kwarnings[[as.character(k)]] <- res$value$warnings
      } else {
        J$kstatus$state[i] <- "error"
        J$kstatus$message[i] <- res$message
      }
      if (!length(J$queue) && !any(J$kstatus$state == "running")) finish_ms()
    })
  }
  finish_ms <- function() {
    st <- J$kstatus
    if (any(st$state == "error")) {
      rv$ms_errors <- st[st$state == "error", c("k", "message")]
      return()
    }
    fits <- J$kfits[as.character(st$k)]
    ms <- tryCatch(assemble_selection(unname(fits)), error = function(e) {
      rv$ms_errors <- data.frame(k = NA, message = conditionMessage(e))
      NULL
    })
    if (is.null(ms)) return()
    # A job that finishes without an error has not necessarily converged, and
    # warnings raised during estimation must reach the user.
    notes <- lapply(st$k, function(k) {
      w <- J$kwarnings[[as.character(k)]] %||% character()
      # Notices about deprecated function names inside the packages are of no
      # use to the person running the analysis.
      w <- w[!grepl("deprecated", w, ignore.case = TRUE)]
      c(list(k = k, warnings = w),
        cluster_checks(fits[[as.character(k)]], J$ms_snapshot$max_it))
    })
    names(notes) <- as.character(st$k)
    rv$ms_notes <- notes
    rv$k_preset <- suggest_k(tidy_selection(ms)$picks) %||% st$k[1]
    if (is.na(rv$k_preset)) rv$k_preset <- st$k[1]
    rv$ms_spec <- J$ms_snapshot
    rv$se <- list()
    rv$ms <- ms
  }
  # Notices for the solutions that need attention (all, or one number of clusters).
  ms_note_ui <- function(only = NULL) {
    out <- list()
    for (n in rv$ms_notes) {
      if (!is.null(only) && !identical(as.integer(n$k), as.integer(only))) next
      if (isTRUE(n$hit_limit)) {
        out <- c(out, list(notice(L("ms_note_limit", n$k, rv$ms_spec$max_it %||% 10000L), type = "error")))
      }
      if (length(n$empty)) {
        out <- c(out, list(notice(L("ms_note_empty", n$k, paste(n$empty, collapse = ", ")), type = "warn")))
      }
      for (w in n$warnings) {
        out <- c(out, list(notice(tags$strong(L("ms_note_warn", n$k)), " ", w, type = "warn")))
      }
    }
    out
  }
  ms_flagged <- function(k) {
    n <- rv$ms_notes[[as.character(k)]]
    !is.null(n) && (isTRUE(n$hit_limit) || length(n$empty) > 0 || length(n$warnings) > 0)
  }
  observeEvent(input$run_ms, {
    if (is_running("^k[0-9]+$")) return()
    if (!mm_ok()) {
      showNotification(L("ms_need_mm"), type = "warning")
      return()
    }
    if (!s2_valid()) {
      showNotification(L("ms_need_s2"), type = "warning")
      return()
    }
    G <- rv$mm$summary$ngroups
    kmin <- suppressWarnings(as.integer(input$k_min))
    kmax <- suppressWarnings(as.integer(input$k_max))
    ns <- suppressWarnings(as.integer(input$nstarts))
    seed <- suppressWarnings(as.integer(input$seed))
    maxit <- suppressWarnings(as.integer(input$max_it))
    if (anyNA(c(kmin, kmax, ns, seed, maxit)) || kmin < 1 || kmax < kmin || ns < 1 || maxit < 1) {
      showNotification(L("ms_bad_numbers"), type = "error")
      return()
    }
    if (kmax > G) {
      showNotification(L("ms_too_many", G), type = "error")
      return()
    }
    clear_results("ms")
    ks <- seq.int(kmin, kmax)
    key <- ms_key()
    sp <- rv$mm$spec
    J$ms_args <- list(data = rv$mm$data,
                      S1 = if (isTRUE(sp$blocks)) split_blocks(sp$S1) else sp$S1,
                      S2 = key$S2, group = sp$group, s1_fit = rv$mm$fit, seed = seed,
                      nstarts = ns, max_it = maxit, endogenous_cov = key$endogenous_cov,
                      endo_group_specific = key$endo_group_specific,
                      missing = sp$missing %||% "listwise", ordinal = isTRUE(sp$ordinal),
                      noninv = sp$noninv)
    J$ms_snapshot <- list(key = key, regs = s2_regs(), k_min = kmin, k_max = kmax, seed = seed,
                          nstarts = ns, max_it = maxit)
    J$kwarnings <- list()
    J$kstatus <- data.frame(k = ks, state = "queued", seconds = NA_real_, message = "",
                            stringsAsFactors = FALSE)
    J$queue <- ks
    launch_queued()
    active(TRUE)
    tick(isolate(tick()) + 1L)
  })
  observeEvent(input$ms_stop, {
    J$queue <- integer()
    stop_jobs("^k[0-9]+$")
    J$kstatus <- NULL
    tick(isolate(tick()) + 1L)
  })
  output$ms_cancel <- renderUI({
    if (is_running("^k[0-9]+$")) actionButton("ms_stop", L("cancel"), class = "btn-quiet")
  })
  output$ms_progress <- renderUI({
    tick()
    running()
    st <- J$kstatus
    out <- list()
    if (!is.null(rv$ms_errors)) {
      e <- rv$ms_errors
      out <- c(out, list(notice(
        tags$strong(L("ms_failed")),
        tags$ul(lapply(seq_len(nrow(e)), function(i) {
          tags$li(if (!is.na(e$k[i])) paste0(e$k[i], " ", L("clusters_lc"), ": "), e$message[i])
        })),
        type = "error"
      )))
    }
    if (!is.null(st) && (any(st$state %in% c("queued", "running")))) {
      done <- sum(st$state == "done")
      out <- c(out, list(sheet(
        class = "busy",
        tags$p(class = "busy-title", L("ms_running", done, nrow(st))),
        progress_bar(100 * done / nrow(st)),
        tags$ul(class = "job-list", lapply(seq_len(nrow(st)), function(i) {
          job <- J$jobs[[paste0("k", st$k[i])]]
          txt <- switch(st$state[i],
                        queued = L("job_queued"),
                        running = L("job_running", fmt_duration(if (is.null(job)) 0 else job_seconds(job))),
                        done = L("job_done", fmt_duration(st$seconds[i])),
                        error = L("job_error"))
          tags$li(class = paste0("job-", st$state[i]),
                  tags$span(class = "job-name", L("n_clusters", st$k[i])),
                  tags$span(class = "job-state", txt))
        })),
        hint(L("ms_parallel", job_workers()))
      )))
    }
    if (!length(out) && is.null(rv$ms)) {
      out <- list(sheet(class = "empty", title = L("ms_empty_title"), tags$p(L("ms_empty_text"))))
    }
    if (!is.null(rv$ms) && !ms_ok()) out <- c(out, list(notice(L("ms_stale"), type = "warn")))
    tagList(out)
  })
  ms_tidy <- reactive({
    req(rv$ms)
    tidy_selection(rv$ms)
  })
  crit_names <- c(Chull = "Convex hull", BIC_G = "BIC (G)", BIC_N = "BIC (N)", AIC = "AIC",
                  AIC3 = "AIC3", ICL = "ICL")
  output$ms_summary <- renderUI({
    req(rv$ms)
    picks <- ms_tidy()$picks
    if (nrow(rv$ms$Overview) < 2) return(NULL)
    sug <- suggest_k(picks)
    n_agree <- sum(picks == sug, na.rm = TRUE)
    any_limit <- any(vapply(rv$ms_notes, function(n) isTRUE(n$hit_limit), logical(1)))
    sheet(
      title = L("ms_summary_title"),
      if (any_limit) notice(L("ms_summary_limit"), type = "error"),
      tags$p(class = "headline", L("ms_suggest", sug, n_agree, sum(!is.na(picks)))),
      tags$div(class = "picks", lapply(names(picks), function(cn) {
        tags$span(class = paste("pick", if (isTRUE(picks[[cn]] == sug)) "pick-agree"),
                  tags$span(class = "pick-name", crit_names[[cn]]),
                  tags$span(class = "pick-k", if (is.na(picks[[cn]])) "–" else picks[[cn]]))
      })),
      hint(L("ms_suggest_hint"))
    )
  })
  output$k_picker <- renderUI({
    req(rv$ms)
    ks <- as.integer(rv$ms$Overview$Clusters)
    sheet(
      class = "picker",
      radioButtons("selected_k", L("k_pick_label"), inline = TRUE,
                   choices = stats::setNames(ks, ks),
                   selected = isolate(rv$k_preset) %||% ks[1])
    )
  })
  selected_k <- reactive({
    req(rv$ms)
    k <- suppressWarnings(as.integer(input$selected_k %||% rv$k_preset))
    ks <- as.integer(rv$ms$Overview$Clusters)
    if (length(k) != 1 || is.na(k) || !k %in% ks) ks[1] else k
  })
  output$ms_table <- renderUI({
    req(rv$ms)
    t <- ms_tidy()
    tab <- t$table
    sel <- selected_k()
    best <- function(col, k) {
      v <- tab[[col]]
      txt <- if (col == "Chull") fmt_num(v, 3) else fmt_num(v, 1)
      lapply(seq_along(v), function(i) {
        if (isTRUE(tab$Clusters[i] == k)) tags$span(class = "best", txt[i]) else txt[i]
      })
    }
    starts <- vapply(rv$ms$Models, function(m) {
      d <- start_diagnostics(m)
      paste0(d$n_best, " / ", d$n_starts)
    }, character(1))
    sheet(
      title = L("ms_table_title"),
      html_table(
        list(lapply(as.integer(tab$Clusters), function(k) {
               if (ms_flagged(k)) tagList(tags$span(class = "flag", title = L("ms_flag_title"), "!"), " ", k) else k
             }),
             fmt_num(tab$LL, 1), fmt_int(as.integer(tab$nrpar)),
             best("Chull", t$picks[["Chull"]]), best("BIC_G", t$picks[["BIC_G"]]),
             best("BIC_N", t$picks[["BIC_N"]]), best("AIC", t$picks[["AIC"]]),
             best("AIC3", t$picks[["AIC3"]]), best("ICL", t$picks[["ICL"]]),
             fmt_num(tab$R2entropy, 3), unname(starts)),
        header = c(L("clusters"), L("loglik"), L("parameters"), unname(crit_names),
                   L("entropy"), L("starts_best")),
        align = c("r", rep("r", 10)),
        row_class = ifelse(tab$Clusters == sel, "is-selected", "")
      ),
      hint(L("ms_table_hint")),
      ms_note_ui()
    )
  })
  output$ms_plot_ui <- renderUI({
    req(rv$ms)
    if (nrow(rv$ms$Overview) < 2) return(NULL)
    sheet(
      title = L("ms_plot_title"),
      tags$div(class = "plot-pair",
               tags$div(tags$h3(L("ms_plot_chull")), plotOutput("ms_plot_ll", height = "240px")),
               tags$div(selectInput("ms_crit", NULL, width = "180px", selected = "AIC3",
                                    choices = stats::setNames(names(crit_names)[-1], crit_names[-1])),
                        plotOutput("ms_plot_crit", height = "240px"))),
      hint(L("ms_plot_hint"))
    )
  })
  output$ms_plot_ll <- renderPlot({
    req(rv$ms)
    plot_selection(rv$ms, "Chull", selected_k(), xlab = L("clusters"))
  }, res = 96)
  output$ms_plot_crit <- renderPlot({
    req(rv$ms)
    plot_selection(rv$ms, input$ms_crit %||% "AIC3", selected_k(), xlab = L("clusters"))
  }, res = 96)
  output$ms_footer <- renderUI({
    if (!ms_ok()) return(NULL)
    tags$div(class = "step-foot",
             actionButton("next_ms", L("continue_results", selected_k()), class = "btn-ink"))
  })

  # ---- Results ----------------------------------------------------------------------------------
  sel_fit <- reactive({
    req(ms_ok())
    mmgsem::extract(rv$ms, selected_k())
  })
  sel_se <- reactive({
    k <- selected_k()
    rv$se[[paste0(k, "|corrected")]] %||% rv$se[[paste0(k, "|naive")]]
  })
  sel_betas <- reactive({
    se <- sel_se()
    tidy_betas(sel_fit(), if (is.null(se)) NULL else se$se)
  })
  sel_regs <- reactive(rv$ms_spec$regs)

  output$res_body <- renderUI({
    if (!ms_ok()) return(notice(L("res_locked"), type = "info"))
    fit <- sel_fit()
    h <- fit_headline(fit)
    cl <- tidy_clusters(fit)
    k <- h$k
    b <- sel_betas()
    cols <- cluster_colours(k)
    wide <- lapply(seq_len(k), function(j) fmt_num(b$estimate[b$cluster == j], 3))
    params <- b$parameter[b$cluster == 1]
    members <- lapply(seq_len(k), function(j) {
      g <- cl[cl$cluster == j, , drop = FALSE]
      tags$div(
        class = "member-row",
        tags$div(class = "member-head", cluster_dot(j, k), tags$strong(paste(cw, j)),
                 tags$span(class = "member-n", L("n_groups", nrow(g)))),
        tags$div(class = "chips", lapply(seq_len(nrow(g)), function(i) {
          unsure <- g$probability[i] < 0.9
          tags$span(class = paste("chip", if (unsure) "chip-unsure"),
                    title = L("chip_title", fmt_num(g$probability[i], 3)),
                    g$group[i], if (unsure) tags$span(class = "chip-p", fmt_fit(g$probability[i])))
        }))
      )
    })
    lowest <- cl[which.min(cl$probability), ]
    n_rows <- ceiling(k / min(k, 2))
    tagList(
      tags$div(class = "tiles",
               tile(L("clusters"), k),
               tile(L("entropy"), fmt_num(h$entropy, 3), L("entropy_note")),
               tile(L("loglik_obs"), fmt_num(h$loglik_observed, 1)),
               tile(L("starts_best"), paste0(h$n_best, " / ", h$n_starts),
                    if (h$n_best < h$n_starts / 2) L("starts_warn") else L("starts_ok")),
               tile(L("iterations"), fmt_int(as.integer(h$iterations)))),
      ms_note_ui(only = k),
      sheet(
        title = L("res_paths_title"), class = "signature",
        tags$p(L(if (is.null(sel_se())) "res_paths_text" else "res_paths_text_se")),
        plotOutput("res_paths", height = paste0(40 + n_rows * 260, "px"))
      ),
      tags$div(
        class = "two",
        sheet(
          title = L("res_coef_title"),
          html_table(
            c(list(params), wide),
            header = c(list(L("relation")), lapply(seq_len(k), function(j) tagList(cluster_dot(j, k), paste(cw, j)))),
            align = c("l", rep("r", k))
          ),
          hint(L("res_coef_hint"))
        ),
        sheet(title = L("res_members_title"), members,
              hint(L("res_lowest", fmt_num(lowest$probability, 3), lowest$group)))
      ),
      sheet(
        title = L("res_post_title"),
        tags$p(L("res_post_text")),
        plotOutput("res_post", height = paste0(70 + 22 * nrow(cl), "px"))
      ),
      sheet(
        title = L("res_compare_title"),
        plotOutput("res_betas", height = paste0(90 + 34 * k, "px"))
      ),
      tags$details(
        class = "more sheet",
        tags$summary(L("res_starts_title")),
        tags$p(L("res_starts_text", h$n_best, h$n_starts)),
        plotOutput("res_starts", height = "220px")
      ),
      tags$details(
        class = "more sheet",
        tags$summary(L("res_r2_title")),
        uiOutput("res_r2")
      ),
      tags$details(
        class = "more sheet",
        tags$summary(L("verbatim_title")),
        hint(L("verbatim_hint")),
        code_out("res_verbatim")
      ),
      tags$div(class = "step-foot", actionButton("next_res", L("continue_tests"), class = "btn-ink"))
    )
  })
  output$res_paths <- renderPlot({
    fit <- sel_fit()
    cl <- tidy_clusters(fit)
    plot_paths(sel_regs(), names(rv$mm$factors), betas = sel_betas(),
               sizes = table(cl$cluster), cluster_word = cw, groups_word = L("groups_lc"))
  }, res = 96)
  output$res_post <- renderPlot(plot_posteriors(sel_fit(), cluster_word = cw), res = 96)
  output$res_betas <- renderPlot(plot_betas(sel_betas(), cluster_word = cw), res = 96)
  output$res_starts <- renderPlot({
    plot_starts(sel_fit(), xlab = L("starts_axis"), ylab = L("loglik_step2"))
  }, res = 96)
  output$res_r2 <- renderUI({
    r2 <- tidy_r2(sel_fit())
    k <- n_clusters(sel_fit())
    facs <- setdiff(names(r2), c("group", "cluster"))
    html_table(
      c(list(r2$group, lapply(r2$cluster, function(j) tagList(cluster_dot(j, k), j))),
        lapply(facs, function(f) fmt_num(r2[[f]], 3))),
      header = c(L("group"), cw, facs), align = c("l", "l", rep("r", length(facs)))
    )
  })
  output$res_verbatim <- renderText(verbatim(summary(sel_fit())))

  # ---- Hypothesis tests ---------------------------------------------------------------------------
  start_se <- function(naive) {
    type <- if (naive) "naive" else "corrected"
    id <- paste0("se_", type)
    if (!ms_ok() || is_running(paste0("^", id, "$"))) return()
    k <- selected_k()
    if (isTRUE(rv$ms_notes[[as.character(k)]]$hit_limit)) {
      showNotification(L("se_not_converged"), type = "error", duration = 10)
      return()
    }
    fit <- sel_fit()
    rv$se_error <- NULL
    start_job(id, "se", list(fit = fit, naive = naive), function(res) {
      if (!res$ok) {
        rv$se_error <- res$message
        return()
      }
      cur <- rv$se
      cur[[paste0(k, "|", type)]] <- list(se = res$value$se, type = type, source = "computed",
                                         seconds = res$value$seconds,
                                         warnings = res$value$warnings)
      rv$se <- cur
    })
  }
  observeEvent(input$se_naive, start_se(TRUE))
  observeEvent(input$se_corrected, start_se(FALSE))
  observeEvent(input$se_stop, stop_jobs("^se_"))
  # Corrected standard errors of the tutorial example take over an hour, so a
  # set computed in advance with the bundled version of mmgsem ships with the
  # interface. It is offered only for the model it belongs to.
  pre_se <- precomputed_se()
  tutorial_se_ok <- reactive({
    req(ms_ok())
    !is.null(pre_se) && isTRUE(rv$is_example) && selected_k() == 4 &&
      se_matches_fit(pre_se$se, sel_fit())
  })
  output$se_precomputed <- renderUI({
    if (!isTRUE(tryCatch(tutorial_se_ok(), error = function(e) FALSE))) return(NULL)
    tagList(tags$hr(), actionButton("se_tutorial", L("se_tutorial_btn"), class = "btn-quiet"),
            hint(L("se_tutorial_hint", pre_se$mmgsem, fmt_duration(pre_se$seconds))))
  })
  observeEvent(input$se_tutorial, {
    req(tutorial_se_ok())
    cur <- rv$se
    cur[["4|corrected"]] <- list(se = pre_se$se, type = "corrected", source = "tutorial",
                                 seconds = pre_se$seconds, warnings = character(),
                                 mmgsem = pre_se$mmgsem)
    rv$se <- cur
  })
  output$se_cancel <- renderUI({
    if (is_running("^se_")) tagList(tags$hr(), actionButton("se_stop", L("cancel"), class = "btn-quiet"))
  })
  output$se_status <- renderUI({
    out <- list()
    for (type in c("naive", "corrected")) {
      id <- paste0("se_", type)
      if (is_running(paste0("^", id, "$"))) {
        tick()
        job <- J$jobs[[id]]
        if (is.null(job)) next
        log <- tryCatch(paste(readLines(job$log, warn = FALSE), collapse = "\n"), error = function(e) "")
        stage <- if (grepl("Cross-derivatives", log, fixed = TRUE)) 3 else
          if (grepl("Step 1 parameters", log, fixed = TRUE)) 2 else 1
        out <- c(out, list(sheet(
          class = "busy",
          tags$p(class = "busy-title", L(paste0("se_running_", type))),
          progress_bar(job_progress(job)),
          hint(if (type == "corrected") L("se_stage", stage, 3) else NULL, " ",
               L("elapsed", fmt_duration(job_seconds(job))))
        )))
      }
    }
    if (!is.null(rv$se_error)) {
      out <- c(out, list(notice(tags$strong(L("se_failed")), tags$br(), rv$se_error, type = "error")))
    }
    tagList(out)
  })
  output$tests_body <- renderUI({
    if (!ms_ok()) return(notice(L("res_locked"), type = "info"))
    s <- sel_se()
    if (is.null(s)) {
      return(sheet(class = "empty", title = L("tests_empty_title"), tags$p(L("tests_empty_text"))))
    }
    fit <- sel_fit()
    k <- n_clusters(fit)
    b <- sel_betas()
    src <- if (identical(s$source, "tutorial")) L("se_src_tutorial", s$mmgsem %||% "") else
      L(paste0("se_src_", s$type), fmt_duration(s$seconds))
    zero <- sheet(
      title = L("tests_zero_title"),
      tags$p(L("tests_zero_text")),
      html_table(
        list(lapply(b$cluster, function(j) tagList(cluster_dot(j, k), paste(cw, j))),
             b$parameter, fmt_num(b$estimate, 3), fmt_num(b$se, 3), fmt_num(b$z, 2),
             lapply(b$p, function(p) if (!is.na(p) && p < 0.05) tags$strong(fmt_p(p)) else fmt_p(p)),
             paste0("[", fmt_num(b$ci_lower, 3), ", ", fmt_num(b$ci_upper, 3), "]")),
        header = c(cw, L("relation"), L("estimate"), "SE", "z", "p", L("ci95")),
        align = c("l", "l", "r", "r", "r", "r", "r"),
        row_class = ifelse(c(FALSE, diff(b$cluster) != 0), "group-start", "")
      )
    )
    across <- NULL
    if (k >= 2) {
      w <- tidy_wald(fit, s$se)
      pw <- tidy_pairwise(fit, s$se)
      across <- tagList(
        sheet(
          title = L("tests_wald_title"),
          tags$p(L("tests_wald_text")),
          html_table(
            list(w$parameter, fmt_num(w$wald, 3), fmt_int(w$df),
                 lapply(w$p, function(p) if (!is.na(p) && p < 0.05) tags$strong(fmt_p(p)) else fmt_p(p))),
            header = c(L("relation"), "Wald χ²", "df", "p"),
            align = c("l", "r", "r", "r")
          )
        ),
        sheet(
          title = L("tests_pair_title"),
          tags$p(L("tests_pair_text", attr(pw, "n_tests"),
                   format(signif(attr(pw, "alpha"), 3), scientific = FALSE))),
          html_table(
            list(pw$parameter,
                 lapply(seq_len(nrow(pw)), function(i) {
                   tagList(cluster_dot(pw$cluster_a[i], k), pw$cluster_a[i], " – ",
                           cluster_dot(pw$cluster_b[i], k), pw$cluster_b[i])
                 }),
                 fmt_num(pw$difference, 3), fmt_num(pw$z, 2), fmt_p(pw$p),
                 ifelse(pw$significant, L("differ"), "")),
            header = c(L("relation"), L("clusters"), L("difference"), "z", "p", L("after_correction")),
            align = c("l", "l", "r", "r", "r", "l"),
            row_class = ifelse(c(FALSE, pw$parameter[-1] != pw$parameter[-nrow(pw)]), "group-start", "")
          ),
          hint(L("tests_pair_hint"))
        )
      )
    }
    tagList(
      notice(src, " ", L("se_reminder"), type = if (s$type == "corrected") "ok" else "info"),
      lapply(s$warnings, function(w) notice(w, type = "warn")),
      zero, across,
      tags$details(
        class = "more sheet",
        tags$summary(L("verbatim_title")),
        hint(L("verbatim_hint")),
        code_out("tests_verbatim")
      )
    )
  })
  output$tests_verbatim <- renderText({
    s <- sel_se()
    req(s)
    fit <- sel_fit()
    out <- verbatim(summary(fit, se = s$se))
    if (n_clusters(fit) >= 2) {
      out <- paste(out, verbatim(mmgsem::test.mmgsem(model = fit, se = s$se)),
                   verbatim(mmgsem::test.mmgsem(model = fit, se = s$se, multiple_comparison = TRUE)),
                   sep = "\n")
    }
    out
  })

  # ---- Explaining the clusters -----------------------------------------------------------------------
  observeEvent(input$covfile, {
    f <- input$covfile
    df <- tryCatch(read_data_file(f$datapath, f$name), error = function(e) {
      showNotification(paste(L("data_read_error"), conditionMessage(e)), type = "error",
                       duration = 10)
      NULL
    })
    if (is.null(df)) return()
    rv$cov <- df
    groups <- tryCatch(tidy_clusters(sel_fit())$group, error = function(e) character())
    hits <- vapply(df, function(x) sum(as.character(x) %in% groups), integer(1))
    gcol <- names(df)[which.max(hits)]
    updateSelectInput(session, "cov_group", choices = names(df), selected = gcol)
    vars <- numeric_columns(df, exclude = gcol)
    updateSelectInput(session, "cov_var", choices = vars, selected = vars[1])
  })
  observeEvent(input$cov_group, {
    req(rv$cov)
    vars <- numeric_columns(rv$cov, exclude = input$cov_group)
    updateSelectInput(session, "cov_var", choices = vars,
                      selected = if (isTRUE(input$cov_var %in% vars)) input$cov_var else vars[1])
  }, ignoreInit = TRUE)
  cov_matched <- reactive({
    req(rv$cov, ms_ok(), input$cov_group %in% names(rv$cov))
    cl <- tidy_clusters(sel_fit())
    m <- match_covariates(rv$cov, input$cov_group, cl$group)
    list(clusters = cl, data = m$data, n = m$n_matched)
  })
  output$exp_body <- renderUI({
    if (!ms_ok()) return(notice(L("res_locked"), type = "info"))
    if (is.null(rv$cov)) {
      return(sheet(class = "empty", title = L("exp_empty_title"), tags$p(L("exp_empty_text"))))
    }
    m <- cov_matched()
    v <- input$cov_var
    if (m$n == 0) return(notice(L("exp_no_match"), type = "warn"))
    if (!isTRUE(v %in% names(m$data))) return(notice(L("exp_no_numeric"), type = "warn"))
    k <- max(m$clusters$cluster)
    val <- as.numeric(m$data[[v]])
    stat <- function(f) vapply(seq_len(k), function(j) {
      x <- val[m$clusters$cluster == j]
      x <- x[!is.na(x)]
      if (length(x)) f(x) else NA_real_
    }, numeric(1))
    tagList(
      notice(L("exp_matched", m$n, nrow(m$clusters)), " ", L("exp_caution"), type = "info"),
      sheet(title = L("exp_plot_title", v), plotOutput("exp_plot", height = "340px")),
      sheet(
        title = L("exp_table_title"),
        html_table(
          list(lapply(seq_len(k), function(j) tagList(cluster_dot(j, k), paste(cw, j))),
               fmt_int(vapply(seq_len(k), function(j) sum(m$clusters$cluster == j & !is.na(val)), integer(1))),
               fmt_num(stat(mean), 3), fmt_num(stat(stats::median), 3),
               fmt_num(stat(min), 3), fmt_num(stat(max), 3)),
          header = c(cw, L("groups"), L("mean"), L("median"), L("min"), L("max")),
          align = c("l", rep("r", 5))
        )
      )
    )
  })
  output$exp_plot <- renderPlot({
    m <- cov_matched()
    req(input$cov_var %in% names(m$data))
    plot_covariate(m$clusters, m$data[[input$cov_var]], label = input$cov_var, cluster_word = cw)
  }, res = 96)

  # ---- Script and downloads ------------------------------------------------------------------------------
  script_spec <- reactive({
    m <- rv$mm
    spec <- list(
      data_source = if (isTRUE(rv$is_example)) "example" else rv$raw_name,
      data_reader = rv$raw_reader,
      group = group_now()
    )
    if (!is.null(m) && mm_ok()) {
      spec$items <- m$items
      spec$S1 <- m$spec$S1
      spec$noninv <- m$spec$noninv
      spec$missing <- m$spec$missing %||% "listwise"
      spec$ordinal <- isTRUE(m$spec$ordinal)
      spec$blocks <- isTRUE(m$spec$blocks)
    } else {
      f <- mm_factors()
      o <- mm_opts()
      spec$items <- unique(unlist(f))
      spec$S1 <- S1()
      spec$noninv <- noninv_now()
      spec$missing <- o$missing
      spec$ordinal <- o$ordinal
      spec$blocks <- o$blocks && is.null(mm_option_problem())
    }
    spec$S2 <- if (s2_valid()) S2() else ""
    if (ms_ok()) {
      s <- rv$ms_spec
      spec[c("k_min", "k_max", "seed", "nstarts", "max_it")] <- s[c("k_min", "k_max", "seed", "nstarts", "max_it")]
      spec$endogenous_cov <- s$key$endogenous_cov
      spec$endo_group_specific <- s$key$endo_group_specific
      spec$selected_k <- selected_k()
      se <- sel_se()
      spec$se <- if (is.null(se)) "none" else se$type
    } else {
      spec$k_min <- as.integer(input$k_min %||% 1)
      spec$k_max <- as.integer(input$k_max %||% 6)
      spec$seed <- as.integer(input$seed %||% 1)
      spec$nstarts <- as.integer(input$nstarts %||% 20)
      spec$max_it <- as.integer(input$max_it %||% 10000)
      spec$endogenous_cov <- isTRUE(input$endogenous_cov)
      spec$endo_group_specific <- isTRUE(input$endo_group_specific)
    }
    spec
  })
  script_text <- reactive({
    if (!has_data()) return(L("script_empty"))
    make_script(script_spec())
  })
  output$script <- renderText(script_text())
  output$dl_script <- downloadHandler(
    filename = function() "mmgsem_analysis.R",
    content = function(file) writeLines(script_text(), file, useBytes = TRUE)
  )
  output$dl_list <- renderUI({
    if (!ms_ok()) return(tags$p(class = "hint", L("dl_none")))
    tags$div(class = "dl-list",
             downloadButton("dl_betas", L("dl_betas"), class = "btn-quiet"),
             downloadButton("dl_members", L("dl_members"), class = "btn-quiet"),
             downloadButton("dl_selection", L("dl_selection"), class = "btn-quiet"),
             downloadButton("dl_fit", L("dl_fit"), class = "btn-quiet"))
  })
  output$dl_betas <- downloadHandler(
    filename = function() "mmgsem_coefficients.csv",
    content = function(file) utils::write.csv(sel_betas(), file, row.names = FALSE)
  )
  output$dl_members <- downloadHandler(
    filename = function() "mmgsem_cluster_membership.csv",
    content = function(file) utils::write.csv(tidy_clusters(sel_fit()), file, row.names = FALSE)
  )
  output$dl_selection <- downloadHandler(
    filename = function() "mmgsem_model_selection.csv",
    content = function(file) utils::write.csv(rv$ms$Overview, file, row.names = FALSE)
  )
  output$dl_fit <- downloadHandler(
    filename = function() paste0("mmgsem_fit_", selected_k(), "_clusters.rds"),
    content = function(file) saveRDS(sel_fit(), file)
  )

  # ---- Project files ----------------------------------------------------------------------------------------
  output$dl_project <- downloadHandler(
    filename = function() paste0("mmgsem_project_", format(Sys.Date(), "%Y%m%d"), ".rds"),
    content = function(file) {
      ms <- rv$ms
      if (!is.null(ms)) ms$Models <- lapply(ms$Models, function(m) {
        m$MM <- NULL
        m
      })
      saveRDS(list(
        format = "mmgsemGUI-project-1",
        raw = rv$raw, raw_name = rv$raw_name, is_example = rv$is_example,
        group = group_now(), S1 = S1(), noninv = noninv_now(), S2 = S2(),
        mm = rv$mm, mm_config = rv$mm_config, screen = rv$screen,
        ms = ms, ms_spec = rv$ms_spec, ms_notes = rv$ms_notes, se = rv$se,
        selected_k = if (!is.null(ms)) selected_k(),
        inputs = list(mm_mode = input$mm_mode, s2_mode = input$s2_mode,
                      mm_missing = input$mm_missing, mm_ordinal = input$mm_ordinal,
                      mm_blocks = input$mm_blocks,
                      k_min = input$k_min, k_max = input$k_max, nstarts = input$nstarts,
                      seed = input$seed, max_it = input$max_it,
                      endogenous_cov = input$endogenous_cov,
                      endo_group_specific = input$endo_group_specific)
      ), file)
    }
  )
  observeEvent(input$project_file, {
    p <- tryCatch(readRDS(input$project_file$datapath), error = function(e) NULL)
    if (!is.list(p) || !identical(p$format, "mmgsemGUI-project-1") || is.null(p$raw)) {
      showNotification(L("project_bad"), type = "error", duration = 10)
      return()
    }
    f <- tryCatch(parse_s1(p$S1), error = function(e) list())
    rows <- lapply(names(f), function(n) list(name = n, items = f[[n]]))
    set_data(p$raw, p$raw_name, example = isTRUE(p$is_example),
             rows = if (length(rows)) rows else NULL, group = p$group)
    set_noninv(p$noninv %||% character(), free_loadings(f))
    regs <- tryCatch(parse_s2(p$S2), error = function(e) list())
    rerender_s2(names(f), regs)
    i <- p$inputs
    # Models written as syntax are restored as syntax, so that nothing the
    # builder cannot express (labels, cross-loadings, ...) is lost.
    mm_mode <- if (identical(i$mm_mode, "syntax")) "syntax" else "builder"
    s2_mode <- if (identical(i$s2_mode, "syntax")) "syntax" else "builder"
    updateRadioButtons(session, "mm_mode", selected = mm_mode)
    updateRadioButtons(session, "s2_mode", selected = s2_mode)
    updateTextAreaInput(session, "s1_text", value = if (mm_mode == "syntax") p$S1 else "")
    updateTextAreaInput(session, "s2_text", value = if (s2_mode == "syntax") p$S2 else "")
    updateRadioButtons(session, "mm_missing",
                       selected = if (identical(i$mm_missing, "fiml")) "fiml" else "listwise")
    updateCheckboxInput(session, "mm_ordinal", value = isTRUE(i$mm_ordinal))
    updateCheckboxInput(session, "mm_blocks", value = isTRUE(i$mm_blocks))
    updateNumericInput(session, "k_min", value = i$k_min)
    updateNumericInput(session, "k_max", value = i$k_max)
    updateNumericInput(session, "nstarts", value = i$nstarts)
    updateNumericInput(session, "seed", value = i$seed)
    updateNumericInput(session, "max_it", value = i$max_it)
    updateCheckboxInput(session, "endogenous_cov", value = isTRUE(i$endogenous_cov))
    updateCheckboxInput(session, "endo_group_specific", value = isTRUE(i$endo_group_specific))
    if (!is.null(p$mm)) {
      for (opt in c("ordinal", "blocks")) if (is.null(p$mm$spec[[opt]])) p$mm$spec[[opt]] <- FALSE
      if (is.null(p$mm$spec$missing)) p$mm$spec$missing <- "listwise"
      p$mm$spec <- p$mm$spec[c("data_id", "group", "S1", "noninv", "missing", "ordinal", "blocks")]
      p$mm$spec$data_id <- rv$data_id
      rv$mm <- p$mm
      rv$mm_config <- p$mm_config
      rv$screen <- p$screen
    }
    if (!is.null(p$ms) && !is.null(p$mm)) {
      p$ms$Models <- lapply(p$ms$Models, function(m) {
        m$MM <- p$mm$fit
        m
      })
      # A model made by clicking is compared in the order used above, also when
      # the project was saved with its regressions in another order.
      if (s2_mode == "builder" && !is.null(p$ms_spec$key$S2)) {
        key_regs <- tryCatch(parse_s2(p$ms_spec$key$S2), error = function(e) NULL)
        if (!is.null(key_regs)) p$ms_spec$key$S2 <- build_s2(ordered_regs(key_regs, names(f)))
      }
      rv$k_preset <- p$selected_k
      rv$ms_notes <- p$ms_notes
      rv$ms_spec <- p$ms_spec
      rv$se <- p$se %||% list()
      rv$ms <- p$ms
    }
    showNotification(L("project_opened"), type = "message")
    go(if (!is.null(p$ms)) "results" else if (!is.null(p$mm)) "structure" else "measure")
  })

  # ---- Step rail and About ---------------------------------------------------------------------------------------
  step_state <- reactive({
    d <- has_data() && !is.null(group_now())
    mm <- mm_ok()
    s2 <- s2_valid()
    ms <- ms_ok()
    se <- if (ms) sel_se() else NULL
    note_data <- if (d) L("rail_data", fmt_int(length(unique(rv$raw[[group_now()]]))), fmt_int(nrow(rv$raw))) else ""
    note_mm <- if (is_running("^mm$")) L("rail_running") else if (mm) {
      if (!is.null(rv$mm$summary$blocks)) L("rail_blocks", length(rv$mm$summary$blocks)) else
        paste("CFI", fmt_fit(rv$mm$summary$fit$cfi))
    } else if (mm_current()) L("rail_not_converged") else if (!is.null(rv$mm)) L("rail_stale") else ""
    note_s2 <- if (s2) L("rail_s2", sum(lengths(s2_regs()))) else ""
    note_ms <- if (is_running("^k[0-9]+$")) L("rail_running") else if (ms) L("rail_ms", selected_k()) else
      if (!is.null(rv$ms)) L("rail_stale") else ""
    note_se <- if (is_running("^se_")) L("rail_running") else if (!is.null(se)) L(paste0("rail_se_", se$type)) else ""
    list(
      data = list(enabled = TRUE, done = d, note = note_data),
      measure = list(enabled = d, done = mm, note = note_mm),
      structure = list(enabled = d && length(lv_now()) >= 2, done = s2, note = note_s2),
      clusters = list(enabled = mm && s2, done = ms, note = note_ms),
      results = list(enabled = ms, done = FALSE, note = ""),
      tests = list(enabled = ms, done = !is.null(se), note = note_se),
      explain = list(enabled = ms, done = !is.null(rv$cov) && ms, note = ""),
      export = list(enabled = d, done = FALSE, note = "")
    )
  })
  output$rail <- renderUI({
    cur <- input$step %||% "data"
    st <- step_state()
    tags$ol(class = "rail-list", lapply(seq_along(step_ids), function(i) {
      id <- step_ids[i]
      s <- st[[id]]
      cls <- c("action-button", "rail-step", if (id == cur) "is-current",
               if (s$done) "is-done", if (!s$enabled) "is-locked")
      tags$li(tags$button(
        id = paste0("go_", id), type = "button", class = paste(cls, collapse = " "),
        disabled = if (!s$enabled) NA, `aria-current` = if (id == cur) "step",
        tags$span(class = "rail-num", if (s$done) "✓" else i),
        tags$span(class = "rail-text",
                  tags$span(class = "rail-title", L(paste0("step_", id))),
                  if (nzchar(s$note)) tags$span(class = "rail-note", s$note))
      ))
    }))
  })
  observeEvent(input$about, {
    showModal(modalDialog(
      title = L("about_title"), easyClose = TRUE, footer = modalButton(L("close")),
      tags$p(L("about_text")),
      tags$p(tags$strong(L("about_cite"))),
      tags$ul(
        tags$li("Perez Alonso, A. F., Vermunt, J., Rosseel, Y., Zhao, H., & De Roover, K. (2026). Using mixture multigroup structural equation modeling to compare structural relations across many groups: An introduction to the mmgsem R package. ",
                tags$em("Structural Equation Modeling, 33"), "(5), 803–830. https://doi.org/10.1080/10705511.2026.2617170"),
        tags$li("Perez Alonso, A. F., Rosseel, Y., Vermunt, J. K., & De Roover, K. (2024). Mixture multigroup structural equation modeling: A novel method for comparing structural relations across many groups. ",
                tags$em("Psychological Methods."), " https://doi.org/10.1037/met0000667")
      ),
      tags$p(class = "hint", L("about_versions",
                               as.character(utils::packageVersion("mmgsemGUI")),
                               as.character(utils::packageVersion("mmgsem")),
                               as.character(utils::packageVersion("lavaan")),
                               paste(R.version$major, R.version$minor, sep = ".")))
    ))
  })
}
