# Builds the Windows installer of mmgsem GUI.
#
#   Rscript installer/build_installer.R
#
# What it needs on the building computer:
#   * this R installation, with mmgsem and all packages mmgsemGUI imports
#     (plus readxl and haven) installed;
#   * Inno Setup 6 (https://jrsoftware.org/isinfo.php).
#
# What it produces: installer/output/mmgsemGUI-setup-<version>.exe, a single
# file that installs R, the packages and the application for one user, without
# administrator rights. The people who use the application need nothing else.

file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
here <- normalizePath(if (length(file_arg)) dirname(file_arg[1]) else "installer", winslash = "/")
root <- normalizePath(file.path(here, ".."), winslash = "/")
pkg_src <- file.path(root, "mmgsemGUI")
staging <- file.path(here, "staging")
out_dir <- file.path(here, "output")
version <- unname(read.dcf(file.path(pkg_src, "DESCRIPTION"))[1, "Version"])
step <- function(...) cat(format(Sys.time(), "%H:%M:%S"), " ", ..., "\n", sep = "")
win <- function(p) normalizePath(p, winslash = "\\", mustWork = FALSE)

robocopy <- function(from, to, exclude_dirs = character(), exclude_files = character()) {
  args <- c(shQuote(win(from)), shQuote(win(to)), "/E", "/NFL", "/NDL", "/NJH", "/NJS", "/NP", "/R:1", "/W:1")
  if (length(exclude_dirs)) args <- c(args, "/XD", shQuote(win(exclude_dirs)))
  if (length(exclude_files)) args <- c(args, "/XF", exclude_files)
  code <- system2("robocopy", args, stdout = FALSE)
  if (code >= 8) stop("Copying failed: ", from, call. = FALSE)   # robocopy: < 8 means success
  invisible(TRUE)
}

# ---- 1. Clean ---------------------------------------------------------------------
step("1/6 Cleaning the staging folder")
unlink(staging, recursive = TRUE, force = TRUE)
dir.create(staging, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE)

# ---- 2. R itself -------------------------------------------------------------------
step("2/6 Copying R ", R.version$major, ".", R.version$minor)
r_home <- normalizePath(R.home(), winslash = "/")
r_dst <- file.path(staging, "R")
robocopy(r_home, r_dst,
         exclude_dirs = file.path(r_home, c("doc", "Tcl", "tests")),
         exclude_files = c("unins*.exe", "unins*.dat"))
lib_dst <- file.path(r_dst, "library")

# ---- 3. Packages -----------------------------------------------------------------------
step("3/6 Collecting the packages")
imports <- read.dcf(file.path(pkg_src, "DESCRIPTION"))[1, "Imports"]
imports <- trimws(gsub("\\(.*?\\)", "", strsplit(imports, ",")[[1]]))
top <- c(imports, "readxl", "haven")
ip <- installed.packages()
missing <- setdiff(top, rownames(ip))
if (length(missing)) stop("Install these packages first: ", paste(missing, collapse = ", "), call. = FALSE)
deps <- tools::package_dependencies(top, db = ip, which = c("Depends", "Imports"), recursive = TRUE)
need <- sort(unique(c(top, unlist(deps))))
need <- setdiff(need, c("R", "mmgsemGUI"))
missing <- setdiff(need, rownames(ip))
if (length(missing)) stop("Install these packages first: ", paste(missing, collapse = ", "), call. = FALSE)
copied <- character()
for (p in need) {
  from <- normalizePath(find.package(p), winslash = "/")
  if (identical(dirname(from), normalizePath(.Library, winslash = "/"))) next   # ships with R
  robocopy(from, file.path(lib_dst, p))
  copied <- c(copied, p)
}
step("     ", length(copied), " packages copied; ", length(need) - length(copied), " ship with R")

# ---- 4. The application --------------------------------------------------------------------
step("4/6 Installing mmgsemGUI ", version)
out <- system2(file.path(R.home("bin"), "R.exe"),
               c("CMD", "INSTALL", "--no-multiarch", "--no-test-load",
                 paste0("--library=", shQuote(win(lib_dst))), shQuote(win(pkg_src))),
               stdout = TRUE, stderr = TRUE)
if (!any(grepl("DONE (mmgsemGUI)", out, fixed = TRUE))) {
  cat(out, sep = "\n")
  stop("mmgsemGUI could not be installed.", call. = FALSE)
}

# ---- 5. Settings, slimming, licences -----------------------------------------------------------
step("5/6 Writing settings and licence information")
# The application must use only the packages it ships with, whatever R
# packages the user may have installed elsewhere.
writeLines(c(
  "# mmgsem GUI: use only the packages that ship with the application.",
  ".libPaths(.Library)",
  "options(repos = c(CRAN = 'https://cloud.r-project.org'))"
), file.path(r_dst, "etc", "Rprofile.site"))
for (d in c("doc", "html", "tests", "include")) {
  unlink(Sys.glob(file.path(lib_dst, "*", d)), recursive = TRUE, force = TRUE)
}
file.copy(file.path(here, "app.ico"), file.path(staging, "app.ico"), overwrite = TRUE)

bundled <- sort(list.dirs(lib_dst, full.names = FALSE, recursive = FALSE))
lic <- vapply(bundled, function(p) {
  d <- tryCatch(read.dcf(file.path(lib_dst, p, "DESCRIPTION"), fields = c("Version", "License")),
                error = function(e) matrix(NA_character_, 1, 2))
  sprintf("  %-18s %-12s %s", p, d[1, 1], d[1, 2])
}, character(1))
writeLines(c(
  paste0("mmgsem GUI ", version),
  "",
  "This application bundles the following free software. Each component keeps",
  "its own licence; the licence texts are in the folders of the components.",
  "",
  paste0("R ", R.version$major, ".", R.version$minor,
         " (GPL-2 | GPL-3; see R\\COPYING). Source code: https://cran.r-project.org/"),
  "",
  "R packages (source code: https://cran.r-project.org/, and",
  "https://github.com/AndresFPA/mmgsem for mmgsem):",
  "",
  sprintf("  %-18s %-12s %s", "Package", "Version", "Licence"),
  lic,
  "",
  "Fonts (SIL Open Font License 1.1; see R\\library\\mmgsemGUI\\app\\www\\fonts):",
  "  IBM Plex Sans, IBM Plex Mono, STIX Two Text",
  "",
  "Method and estimation package to cite:",
  "  Perez Alonso, A. F., Vermunt, J., Rosseel, Y., Zhao, H., & De Roover, K. (2026).",
  "  Structural Equation Modeling, 33(5), 803-830.",
  "  https://doi.org/10.1080/10705511.2026.2617170"
), file.path(staging, "LICENSES.txt"))

size_mb <- sum(file.size(list.files(staging, recursive = TRUE, full.names = TRUE))) / 1024^2
step("     staging folder: ", round(size_mb), " MB in ", length(bundled), " packages")

# ---- 6. Installer ------------------------------------------------------------------------------------
step("6/6 Compiling the installer (this takes a few minutes)")
iscc <- c(file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Inno Setup 6", "ISCC.exe"),
          file.path(Sys.getenv("ProgramFiles(x86)"), "Inno Setup 6", "ISCC.exe"),
          file.path(Sys.getenv("ProgramFiles"), "Inno Setup 6", "ISCC.exe"))
iscc <- iscc[file.exists(iscc)]
if (!length(iscc)) stop("Inno Setup 6 was not found. Install it from https://jrsoftware.org/", call. = FALSE)
out <- system2(iscc[1], c("/Q", paste0("/DAppVersion=", version), shQuote(win(file.path(here, "mmgsemGUI.iss")))),
               stdout = TRUE, stderr = TRUE)
exe <- file.path(out_dir, paste0("mmgsemGUI-setup-", version, ".exe"))
if (!file.exists(exe)) {
  cat(out, sep = "\n")
  stop("The installer was not produced.", call. = FALSE)
}
step("Done: ", exe, " (", round(file.size(exe) / 1024^2), " MB)")
