#' Load the data
#' 
#' Depending on file ending, the data is loaded.
#' 
#' @importFrom Hmisc spss.get
#' @importFrom utils read.csv read.csv2 count.fields
#' @param name name of file
#' @param datapath the actual path
#' @return data as an R object or an error
#' @noRd
load_data <- function(name, datapath) {
  ext <- tools::file_ext(name)
  d <- switch(ext,
              sav = Hmisc::spss.get(datapath, use.value.labels = F),
              csv = load_csv(datapath),
              validate("Invalid file; Please upload a .csv or .sav file")
  )
  d
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
