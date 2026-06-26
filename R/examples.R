#' Example data set metadata
#'
#' @noRd
example_dataset_info <- function() {
  list(
    "mlmRev::Exam" = list(
      label = "Exam",
      description = "Inner London exam data with pupils nested in schools."
    ),
    "mlmRev::Chem97" = list(
      label = "Chem97",
      description = "A-level chemistry exam scores from England in 1997, with students nested in schools."
    ),
    "mlmRev::Contraception" = list(
      label = "Contraception",
      description = "Contraceptive-use data from Bangladesh, with women nested in districts."
    ),
    "mlmRev::Early" = list(
      label = "Early",
      description = "Early-childhood intervention data with repeated cognitive scores nested within children."
    ),
    "mlmRev::Hsb82" = list(
      label = "Hsb82",
      description = "High School and Beyond 1982 data, with students nested in schools."
    ),
    "mlmRev::Mmmec" = list(
      label = "Mmmec",
      description = "Malignant melanoma mortality data for European regions."
    ),
    "mlmRev::Oxboys" = list(
      label = "Oxboys",
      description = "Oxford boys growth data, with repeated height measurements nested within subjects."
    ),
    "lme4::sleepstudy" = list(
      label = "sleepstudy",
      description = "Reaction-time data from a sleep-deprivation study, with repeated observations nested within subjects."
    ),
    "popular2" = list(
      label = "Popularity2",
      description = "Simulated pupil-popularity data included with mimosa, with pupils nested in classes."
    )
  )
}

#' Example data set choices
#'
#' @noRd
example_dataset_choices <- function() {
  info <- example_dataset_info()
  choices <- names(info)
  stats::setNames(choices, vapply(info, function(x) x$label, character(1)))
}

#' Example data set description
#'
#' @param key example data set key
#' @noRd
example_dataset_description <- function(key) {
  info <- example_dataset_info()
  key <- canonical_example_key(key)
  if (!key %in% names(info)) {
    return("")
  }
  info[[key]]$description
}

#' Canonical example data set key
#'
#' @param key example data set key
#' @noRd
canonical_example_key <- function(key) {
  if (!is.character(key) || length(key) != 1 || is.na(key)) {
    return("")
  }
  old_popular2_value <- paste0("mimosa", "::", "popular2")
  if (identical(key, old_popular2_value)) {
    return("popular2")
  }
  key
}

#' Load an example data set
#'
#' @param key example data set key
#' @noRd
load_example_dataset <- function(key) {
  key <- canonical_example_key(key)
  switch(
    key,
    "mlmRev::Exam" = mlmRev::Exam,
    "mlmRev::Chem97" = mlmRev::Chem97,
    "mlmRev::Contraception" = mlmRev::Contraception,
    "mlmRev::Early" = mlmRev::Early,
    "mlmRev::Hsb82" = mlmRev::Hsb82,
    "mlmRev::Mmmec" = mlmRev::Mmmec,
    "mlmRev::Oxboys" = mlmRev::Oxboys,
    "lme4::sleepstudy" = lme4::sleepstudy,
    "popular2" = load_package_dataset("popular2"),
    NULL
  )
}

#' Load a package dataset by name
#'
#' @param name dataset name
#' @noRd
load_package_dataset <- function(name) {
  data_env <- new.env(parent = emptyenv())
  utils::data(list = name, package = "mimosa", envir = data_env)
  data_env[[name]]
}

#' HTML option tags for example data sets
#'
#' @param selected selected example data set key
#' @noRd
example_dataset_options <- function(selected = "mlmRev::Exam") {
  info <- example_dataset_info()
  c(
    list(tags$option(value = "", "")),
    lapply(names(info), function(key) {
      tags$option(
        value = key,
        title = info[[key]]$description,
        selected = if (identical(key, selected)) "selected" else NULL,
        info[[key]]$label
      )
    })
  )
}

#' Example data set select input with option titles
#'
#' @param selected selected example data set key
#' @noRd
example_dataset_select <- function(selected = "mlmRev::Exam") {
  tags$div(
    class = "form-group shiny-input-container",
    style = "width: 150px;",
    tags$label("OR use example data sets:", `for` = "examplefile"),
    tags$select(
      id = "examplefile",
      class = "form-control",
      example_dataset_options(selected)
    )
  )
}
