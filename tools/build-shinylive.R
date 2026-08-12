#!/usr/bin/env Rscript

root <- normalizePath(file.path(getwd()))
if (!file.exists(file.path(root, "DESCRIPTION")) || !dir.exists(file.path(root, "R"))) {
  stop("Run this script from the mimosa repository root.", call. = FALSE)
}

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop(
    "Package 'shinylive' is required. Install it with install.packages('shinylive').",
    call. = FALSE
  )
}

stage_dir <- file.path(root, "_shinylive_app")
site_dir <- file.path(root, "docs")

if (dir.exists(stage_dir)) {
  unlink(stage_dir, recursive = TRUE)
}
on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)

if (dir.exists(site_dir)) {
  unlink(site_dir, recursive = TRUE)
}

dir.create(stage_dir, recursive = TRUE)
dir.create(file.path(stage_dir, "R"), recursive = TRUE)
dir.create(file.path(stage_dir, "data"), recursive = TRUE)

description <- read.dcf(file.path(root, "DESCRIPTION"))
build_version <- unname(description[1, "Version"])
build_commit <- Sys.getenv("GITHUB_SHA", unset = "")
if (!nzchar(build_commit)) {
  build_commit <- tryCatch(
    system2(
      "git",
      c("-C", root, "rev-parse", "--short=8", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) ""
  )
}
build_commit <- substr(trimws(build_commit[1]), 1, 8)

writeLines(
  c(
    "options(",
    "  mimosa.webR = TRUE,",
    paste0("  mimosa.version = ", encodeString(build_version, quote = "\""), ","),
    paste0("  mimosa.commit = ", encodeString(build_commit, quote = "\"")),
    ")"
  ),
  file.path(stage_dir, "build-info.R")
)

invisible(file.copy(file.path(root, "inst", "shinylive", "app.R"), stage_dir, overwrite = TRUE))
invisible(file.copy(file.path(root, "R", c(
  "load.R",
  "examples.R",
  "helper.R",
  "formula.R",
  "model.R",
  "output.R",
  "ui.R",
  "server.R"
)), file.path(stage_dir, "R"), overwrite = TRUE))
invisible(file.copy(file.path(root, "data", "popular2.rda"), file.path(stage_dir, "data"), overwrite = TRUE))

shinylive::export(stage_dir, site_dir)
unlink(stage_dir, recursive = TRUE)

message("Shinylive site written to: ", site_dir)
message("Build label: mimosa v", build_version, " (webR ", build_commit, ")")
message("Preview with: httpuv::runStaticServer('docs')")
