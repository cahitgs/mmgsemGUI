# State shared by the sessions of one running application (desktop mode).
app_state <- new.env()
app_state$sessions <- 0L
app_state$ever <- FALSE

#' Start the mmgsem interface
#'
#' Opens the point-and-click interface. Everything runs on this computer; no
#' data leave it.
#'
#' @param port Port to listen on. By default a free port is chosen.
#' @param launch.browser Open the browser automatically?
#' @param desktop Run as a desktop application: the interface opens in its own
#'   window (Microsoft Edge in application mode when available) and R stops
#'   when that window is closed. Used by the Windows installer.
#' @return Called for its side effect; returns the value of [shiny::runApp()].
#' @examples
#' if (interactive()) run_app()
#' @export
run_app <- function(port = NULL, launch.browser = TRUE, desktop = FALSE) {
  # Uploaded data files can be large (the default limit is 5 MB).
  old <- options(mmgsemGUI.desktop = isTRUE(desktop), shiny.maxRequestSize = 1024^3)
  on.exit(options(old), add = TRUE)
  addResourcePath("mmgsemgui", system.file("app", "www", package = "mmgsemGUI"))
  if (isTRUE(desktop)) {
    app_state$sessions <- 0L
    app_state$ever <- FALSE
    if (isTRUE(launch.browser)) launch.browser <- open_app_window
    # If no window ever connects, do not keep R running in the background.
    later::later(function() {
      if (!app_state$ever) stopApp()
    }, delay = 180)
  }
  app <- shinyApp(ui = app_ui, server = app_server)
  runApp(app, port = port, launch.browser = launch.browser, host = "127.0.0.1")
}

# Open the interface in its own window: Edge in application mode on Windows,
# otherwise the default browser.
open_app_window <- function(url) {
  edge <- character()
  if (.Platform$OS.type == "windows") {
    edge <- file.path(c(Sys.getenv("ProgramFiles(x86)"), Sys.getenv("ProgramFiles"),
                        Sys.getenv("LOCALAPPDATA")),
                      "Microsoft", "Edge", "Application", "msedge.exe")
    edge <- edge[file.exists(edge)]
  }
  if (length(edge)) {
    system2(edge[1], c(paste0("--app=", url), "--window-size=1500,950"), wait = FALSE)
  } else {
    utils::browseURL(url)
  }
  invisible(NULL)
}

# Desktop mode: stop R shortly after the last window is closed. The delay lets
# a reload of the page reconnect.
track_session <- function(session) {
  if (!isTRUE(getOption("mmgsemGUI.desktop"))) return(invisible(NULL))
  app_state$sessions <- app_state$sessions + 1L
  app_state$ever <- TRUE
  session$onSessionEnded(function() {
    app_state$sessions <- app_state$sessions - 1L
    later::later(function() {
      if (app_state$sessions <= 0L) stopApp()
    }, delay = 5)
  })
  invisible(NULL)
}
