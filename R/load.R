#' Load the data
#' 
#' Depending on file ending, the data is loaded.
#' 
#' @importFrom Hmisc spss.get
#' @importFrom utils read.csv read.csv2 count.fields
#' @importFrom readr guess_encoding
#' @importFrom stringr str_match
#' @param name name of file
#' @param datapath the actual path
#' @return data as an R object or an error
#' @noRd
load_data <- function(name, datapath) {
  ext <- tools::file_ext(name)
  data <- tryCatch({
    d <- switch(ext,
                sav = Hmisc::spss.get(datapath, use.value.labels = F),
                csv = load_csv(datapath),
                validate("Invalid file; Please upload a .csv or .sav file")
    )
  },
  error = function(error_message) {
    msg <- "Sorry, I could not read your data. Please check that it is in the SPSS format .sav or a regular .csv file with a comma or a semicolon as the separator."
      showModal(modalDialog(
      title = "Error",
      msg,
      easyClose = TRUE
    ))
    # will grey out app
    #message(error_message)
  }
  )
  data
}

#' check for diferent csv types and encoding
#' @noRd
load_csv <- function(path) {
  encoding <- unlist(readr::guess_encoding(path)[1, 1])
  L <- readLines(path, n = 1)
  numfields_semicolon <- count.fields(textConnection(L), sep = ";")
  numfields_colon <- count.fields(textConnection(L), sep = ",")
  if (numfields_semicolon == 1) {
    data <- utils::read.csv(path, fileEncoding = encoding)
  } else if (numfields_colon == 1) {
    data <- utils::read.csv2(path, fileEncoding = encoding)
  }
  data
}