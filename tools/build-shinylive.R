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

invisible(file.copy(file.path(root, "inst", "shinylive", "app.R"), stage_dir, overwrite = TRUE))
invisible(file.copy(file.path(root, "R", c(
  "load.R",
  "examples.R",
  "helper.R",
  "formula.R",
  "output.R",
  "ui.R",
  "server.R"
)), file.path(stage_dir, "R"), overwrite = TRUE))
invisible(file.copy(file.path(root, "data", "popular2.rda"), file.path(stage_dir, "data"), overwrite = TRUE))

shinylive::export(stage_dir, site_dir)

message("Shinylive site written to: ", site_dir)
message("Preview with: httpuv::runStaticServer('docs')")
