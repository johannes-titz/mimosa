#' Load the data
#' 
#' Depending on file ending, the data is loaded.
#' 
#' @importFrom foreign read.spss
#' @importFrom utils read.csv read.csv2 count.fields
#' @param name name of file
#' @param datapath the actual path
#' @return data as an R object or an error
#' @noRd
load_data <- function(name, datapath) {
  ext <- tools::file_ext(name)
  d <- switch(ext,
              sav = read_sav(datapath),
              csv = load_csv(datapath),
              validate("Invalid file; Please upload a .csv or .sav file")
  )
  d
}

#' Read an SPSS data file
#'
#' A lightweight wrapper around [foreign::read.spss()] that preserves the
#' naming, variable-label, and integer-conversion behavior previously provided
#' by `Hmisc::spss.get()`.
#'
#' @param file path to an SPSS `.sav` file
#' @return a data frame
#' @export
read_sav <- function(file) {
  data <- foreign::read.spss(
    file,
    use.value.labels = FALSE,
    to.data.frame = TRUE,
    reencode = NA
  )
  variable_labels <- attr(data, "variable.labels")
  original_names <- names(data)
  names(data) <- gsub(
    "_",
    ".",
    make.names(original_names, unique = TRUE),
    fixed = TRUE
  )

  if (length(variable_labels)) {
    for (i in seq_along(data)) {
      label <- variable_labels[i]
      if (!is.na(label) && nzchar(label) && label != original_names[i]) {
        attr(data[[i]], "label") <- label
        class(data[[i]]) <- c("labelled", class(data[[i]]))
      }
    }
  }
  attr(data, "variable.labels") <- NULL

  for (name in names(data)) {
    value <- data[[name]]
    if (is.factor(value) || is.character(value)) {
      next
    }
    if (all(is.na(value)) || (
      max(abs(value), na.rm = TRUE) <= .Machine$integer.max &&
        all(floor(value) == value, na.rm = TRUE)
    )) {
      storage.mode(value) <- "integer"
      data[[name]] <- value
    }
  }
  data
}

#' R code for loading an uploaded data file
#'
#' The generated code assumes the original file is in the working directory.
#'
#' @param name original file name
#' @noRd
uploaded_dataset_code <- function(name) {
  filename <- paste(deparse(basename(name), control = "all"), collapse = "")
  ext <- tolower(tools::file_ext(name))
  switch(
    ext,
    csv = paste(
      "data <- utils::read.csv(",
      paste0("  ", filename),
      ")",
      "if (ncol(data) == 1L) {",
      "  data <- utils::read.csv2(",
      paste0("    ", filename),
      "  )",
      "}",
      sep = "\n"
    ),
    # Keep the package-qualified call in the generated code, but assemble the
    # namespace marker separately so dependency scanners do not try to install
    # mimosa inside the already self-contained Shinylive app.
    sav = paste0("data <- mimosa", "::read_sav(", filename, ")"),
    ""
  )
}

#' check for diferent csv types and encoding
#' @noRd
load_csv <- function(path) {
  L <- readLines(path, n = 1)
  numfields_semicolon <- count.fields(textConnection(L), sep = ";")
  numfields_colon <- count.fields(textConnection(L), sep = ",")
  if (numfields_semicolon == 1) {
    data <- read_csv_with_fallback(path, utils::read.csv)
  } else if (numfields_colon == 1) {
    data <- read_csv_with_fallback(path, utils::read.csv2)
  }
  data
}

#' Read CSV data using base R encodings
#'
#' @noRd
read_csv_with_fallback <- function(path, reader) {
  encodings <- c("", "UTF-8", "ISO-8859-1", "latin1")
  for (encoding in encodings) {
    data <- tryCatch(
      if (encoding == "") {
        reader(path)
      } else {
        reader(path, fileEncoding = encoding)
      },
      error = function(e) NULL
    )
    if (!is.null(data)) {
      return(data)
    }
  }
  reader(path)
}
