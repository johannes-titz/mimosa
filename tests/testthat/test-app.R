test_that("UI and Shiny app objects are constructed without launching a browser", {
  old_options <- options(shiny.testmode = TRUE)
  on.exit(options(old_options), add = TRUE)

  ui <- myui()
  app <- run_app(host = "127.0.0.1", port = 12345)

  expect_s3_class(ui, "shiny.tag")
  ui_html <- as.character(ui)
  expect_true(grepl("mimosa v", ui_html, fixed = TRUE))
  expect_true(grepl("https://github.com/johannes-titz/mimosa/releases/latest",
                    ui_html, fixed = TRUE))
  expect_true(grepl(
    "https://johannestitz.com/post/2026-08-11-mimosa-0-6-1/",
    ui_html,
    fixed = TRUE
  ))
  expect_true(grepl('id="copy_citation_plain"', ui_html, fixed = TRUE))
  expect_true(grepl('id="copy_citation_html"', ui_html, fixed = TRUE))
  expect_true(grepl('id="download_citation_bib"', ui_html, fixed = TRUE))
  expect_match(
    ui_html,
    "<strong>[[:space:]]+Citation: [[:space:]]+Titz, J\\. \\(2020\\)\\. mimosa:"
  )
  expect_s3_class(app, "shiny.appobj")
  expect_identical(app$options$host, "127.0.0.1")
  expect_identical(app$options$port, 12345)
})

test_that("citation formats contain the published article metadata", {
  plain <- mimosa_citation_plain()
  html <- mimosa_citation_html()
  bib <- mimosa_citation_bibtex()

  expect_match(plain, "Titz, J. \\(2020\\)")
  expect_match(plain, "10.21105/joss.02116", fixed = TRUE)
  expect_match(html, "<em>Journal of Open Source Software, 5</em>", fixed = TRUE)
  expect_match(html, '<a href="https://doi.org/10.21105/joss.02116">', fixed = TRUE)
  expect_match(bib, "@article{titz2020mimosa", fixed = TRUE)
  expect_match(bib, "doi = {10.21105/joss.02116}", fixed = TRUE)
})

test_that("webR version labels identify the version and exact build", {
  old_options <- options(
    mimosa.webR = TRUE,
    mimosa.version = "0.6.9000",
    mimosa.commit = "1a2b3c4d"
  )
  on.exit(options(old_options), add = TRUE)

  expect_identical(
    mimosa_version_label(),
    "mimosa v0.6.9000 (webR 1a2b3c4d)"
  )
})

test_that("diagnostic dialogs require explicit acknowledgement", {
  dialog <- as.character(acknowledgement_modal("Warning", "Check the model."))

  expect_true(grepl('data-backdrop="static"', dialog, fixed = TRUE))
  expect_true(grepl('data-keyboard="false"', dialog, fixed = TRUE))
  expect_true(grepl(">OK</button>", dialog, fixed = TRUE))
})
