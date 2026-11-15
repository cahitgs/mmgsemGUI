# Interface layout ----------------------------------------------------------------

step_ids <- c("data", "measure", "structure", "clusters", "results", "tests",
              "explain", "export")

step_page <- function(id, title, lede, ...) {
  tabPanelBody(
    id,
    tags$div(
      class = "step",
      tags$header(class = "step-head", tags$h1(title), tags$p(class = "lede", lede)),
      ...
    )
  )
}

work <- function(controls, stage, class = NULL) {
  tags$div(class = paste(c("work", class), collapse = " "),
           tags$div(class = "controls", controls),
           tags$div(class = "stage", stage))
}

app_ui <- function(request) {
  L <- translator()

  ui_data <- step_page(
    "data", L("data_title"), L("data_lede"),
    work(
      sheet(
        title = L("data_choose"),
        actionButton("load_example", L("data_example_btn"), class = "btn-quiet"),
        hint(L("data_example_hint")),
        tags$hr(),
        fileInput("datafile", L("data_file_label"),
                  accept = c(".csv", ".txt", ".tsv", ".dat", ".xlsx", ".xls", ".sav",
                             ".rds", ".rda", ".RData"),
                  buttonLabel = L("browse"), placeholder = L("no_file")),
        selectInput("group_var", L("data_group_label"), choices = character(0)),
        hint(L("data_group_hint"))
      ),
      uiOutput("data_stage")
    ),
    uiOutput("data_footer")
  )

  ui_measure <- step_page(
    "measure", L("mm_title"), L("mm_lede"),
    work(
      tagList(
        sheet(
          title = L("mm_define"),
          radioButtons("mm_mode", NULL, inline = TRUE,
                       choices = stats::setNames(c("builder", "syntax"),
                                                 c(L("mode_builder"), L("mode_syntax")))),
          conditionalPanel(
            "input.mm_mode == 'builder'",
            uiOutput("mm_builder"),
            tags$div(class = "row-actions",
                     actionButton("add_factor", L("add_factor"), class = "btn-quiet"),
                     actionButton("remove_factor", L("remove_factor"), class = "btn-quiet"))
          ),
          conditionalPanel(
            "input.mm_mode == 'syntax'",
            tags$div(class = "code-input",
                     textAreaInput("s1_text", L("mm_syntax_label"), rows = 7, width = "100%")),
            hint(L("mm_syntax_hint"))
          )
        ),
        sheet(
          title = L("noninv_title"),
          selectizeInput("noninv", L("noninv_label"), choices = NULL, multiple = TRUE,
                         width = "100%",
                         options = list(plugins = list("remove_button"),
                                        placeholder = L("noninv_placeholder"))),
          hint(L("noninv_hint"))
        ),
        sheet(
          title = L("opt_title"),
          radioButtons("mm_missing", L("opt_missing_label"),
                       choices = stats::setNames(c("listwise", "fiml"),
                                                 c(L("opt_listwise"), L("opt_fiml")))),
          hint(L("opt_missing_hint")),
          checkboxInput("mm_blocks", L("opt_blocks"), value = FALSE),
          hint(L("opt_blocks_hint")),
          checkboxInput("mm_ordinal", L("opt_ordinal"), value = FALSE),
          hint(L("opt_ordinal_hint"))
        ),
        tags$div(class = "run-row", actionButton("fit_mm", L("fit_mm_btn"), class = "btn-ink"),
                 uiOutput("mm_cancel", inline = TRUE))
      ),
      tagList(
        sheet(title = L("syntax_preview"), code_out("s1_preview")),
        uiOutput("mm_status"),
        uiOutput("mm_result")
      )
    ),
    uiOutput("mm_footer")
  )

  ui_structure <- step_page(
    "structure", L("s2_title"), L("s2_lede"),
    work(
      sheet(
        title = L("s2_define"),
        radioButtons("s2_mode", NULL, inline = TRUE,
                     choices = stats::setNames(c("builder", "syntax"),
                                               c(L("mode_builder"), L("mode_syntax")))),
        conditionalPanel("input.s2_mode == 'builder'", uiOutput("s2_builder")),
        conditionalPanel(
          "input.s2_mode == 'syntax'",
          tags$div(class = "code-input",
                   textAreaInput("s2_text", L("s2_syntax_label"), rows = 5, width = "100%")),
          hint(L("s2_syntax_hint"))
        )
      ),
      tagList(
        sheet(title = L("s2_diagram"), uiOutput("s2_problems"),
              plotOutput("s2_plot", height = "300px"),
              hint(L("s2_cov_hint"))),
        sheet(title = L("syntax_preview"), code_out("s2_preview"))
      )
    ),
    uiOutput("s2_footer")
  )

  ui_clusters <- step_page(
    "clusters", L("ms_title"), L("ms_lede"),
    work(
      tagList(
        sheet(
          title = L("ms_range"),
          tags$div(class = "pair",
                   numericInput("k_min", L("k_min"), value = 1, min = 1, step = 1),
                   numericInput("k_max", L("k_max"), value = 6, min = 1, step = 1)),
          hint(L("ms_range_hint")),
          tags$div(class = "pair",
                   numericInput("nstarts", L("nstarts"), value = 20, min = 1, step = 1),
                   numericInput("seed", L("seed"), value = 1, step = 1)),
          hint(L("nstarts_hint")),
          tags$details(
            class = "more",
            tags$summary(L("advanced")),
            checkboxInput("endogenous_cov", L("endo_cov"), value = TRUE),
            hint(L("endo_cov_hint")),
            checkboxInput("endo_group_specific", L("endo_gs"), value = TRUE),
            hint(L("endo_gs_hint")),
            numericInput("max_it", L("max_it"), value = 10000, min = 10, step = 100)
          )
        ),
        tags$div(class = "run-row", actionButton("run_ms", L("run_ms_btn"), class = "btn-ink"),
                 uiOutput("ms_cancel", inline = TRUE))
      ),
      tagList(
        uiOutput("ms_progress"),
        uiOutput("ms_summary"),
        uiOutput("k_picker")
      )
    ),
    uiOutput("ms_table"),
    uiOutput("ms_plot_ui"),
    uiOutput("ms_footer")
  )

  ui_results <- step_page(
    "results", L("res_title"), L("res_lede"),
    uiOutput("res_body")
  )

  ui_tests <- step_page(
    "tests", L("tests_title"), L("tests_lede"),
    work(
      tagList(
        sheet(
          title = L("se_title"),
          tags$p(L("se_intro")),
          actionButton("se_naive", L("se_naive_btn"), class = "btn-ink"),
          hint(L("se_naive_hint")),
          actionButton("se_corrected", L("se_corr_btn"), class = "btn-quiet"),
          hint(L("se_corr_hint")),
          uiOutput("se_precomputed"),
          uiOutput("se_cancel")
        )
      ),
      tagList(uiOutput("se_status"), uiOutput("tests_body")),
      class = "work-stack"     # wide tables: controls above them on a laptop screen
    )
  )

  ui_explain <- step_page(
    "explain", L("exp_title"), L("exp_lede"),
    work(
      sheet(
        title = L("exp_file_title"),
        fileInput("covfile", L("exp_file_label"),
                  accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls", ".sav", ".rds"),
                  buttonLabel = L("browse"), placeholder = L("no_file")),
        hint(L("exp_file_hint")),
        selectInput("cov_group", L("exp_group_label"), choices = character(0)),
        selectInput("cov_var", L("exp_var_label"), choices = character(0))
      ),
      uiOutput("exp_body")
    )
  )

  ui_export <- step_page(
    "export", L("exp2_title"), L("exp2_lede"),
    tags$div(
      class = "export-grid",
      sheet(
        title = L("script_title"),
        tags$p(L("script_intro")),
        code_out("script", tall = TRUE),
        downloadButton("dl_script", L("dl_script"), class = "btn-ink")
      ),
      tags$div(
        sheet(
          title = L("dl_title"),
          uiOutput("dl_list")
        ),
        sheet(
          title = L("project_title"),
          tags$p(L("project_intro")),
          downloadButton("dl_project", L("project_save"), class = "btn-quiet"),
          tags$div(class = "spacer"),
          fileInput("project_file", L("project_open"), accept = ".rds",
                    buttonLabel = L("browse"), placeholder = L("no_file"))
        )
      )
    )
  )

  # The page is the outermost element, so that its language reaches the <html> tag.
  bootstrapPage(
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title("mmgsem GUI"),
      tags$link(rel = "icon", type = "image/png", href = "mmgsemgui/icon.png"),
      tags$link(rel = "stylesheet", href = "mmgsemgui/fonts/fonts.css"),
      tags$link(rel = "stylesheet", href = "mmgsemgui/app.css"),
      tags$script(src = "mmgsemgui/app.js")
    ),
    tags$div(
      class = "app",
      tags$header(
        class = "topbar",
        tags$div(class = "brand",
                 tags$span(class = "brand-name", "mmgsem GUI"),
                 tags$span(class = "brand-sub", L("brand_sub"))),
        tags$div(class = "topbar-right",
                 uiOutput("job_badge", inline = TRUE),
                 actionLink("about", L("about"), class = "toplink"))
      ),
      tags$div(
        class = "shell",
        tags$nav(class = "rail", `aria-label` = L("steps"), uiOutput("rail")),
        tags$main(
          class = "main",
          tabsetPanel(id = "step", type = "hidden", selected = "data",
                      ui_data, ui_measure, ui_structure, ui_clusters, ui_results,
                      ui_tests, ui_explain, ui_export)
        )
      )
    )
  )
}
