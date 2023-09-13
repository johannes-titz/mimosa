
#' Load the data
#' 
#' Depending on file ending, the data is loaded.
#' 
#' @importFrom Hmisc spss.get
#' @importFrom utils read.csv read.csv2 count.fields
#' @importFrom readr guess_encoding
#' @importFrom stringr str_match
#' @param datafile yep, the data file to upload
#' @return data as an R object or an error
#' @noRd
load_data <- function(datafile) {
  fileending <- stringr::str_match(datafile$datapath, "(\\..+$)")[1,1]
  data <- tryCatch({
    if (fileending == ".sav") {
      data <- Hmisc::spss.get(datafile$datapath, use.value.labels = F)
    } else if (fileending == ".csv") {
      encoding <- unlist(readr::guess_encoding(datafile$datapath)[1, 1])
      L <- readLines(datafile$datapath, n = 1)
      numfields_semicolon <- count.fields(textConnection(L), sep = ";")
      numfields_colon <- count.fields(textConnection(L), sep = ",")
      if (numfields_semicolon == 1) {
        data <- utils::read.csv(datafile$datapath, fileEncoding = encoding)
      } else if (numfields_colon == 1) {
        data <- utils::read.csv2(datafile$datapath, fileEncoding = encoding)
      }
    }
    data},
    error = function(error_message) {
      msg <- "Sorry, I could not read your data. Please check that it is in the SPSS format .sav or a regular .csv file with a comma as a separator (not a semicolon or any other delimiter)."
      shinyalert::shinyalert("Error", msg)
      message(error_message)
    }
    )
}