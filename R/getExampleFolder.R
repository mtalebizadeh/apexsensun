#' getExampleFolder
#' @description Creates a copy of the tutorial project inside working directory.
#' @param folderName A string representing tutorial folder name.
#'
#' @return Void/Creates a copy of the tutorial project inside working directory.
#' @export
#'
#' @examples
#' \dontrun{
#' getExampleFolder()
#' }
getExampleFolder <- function(folderName = "Example") {
  if (file.exists(folderName)) {
    stop(paste(folderName)," already exists! Please select a different name for tutorial folder.")
  } else {
    folderLocation <- system.file("extdata", "Exampleee", package = "APEXSENSUN")
    if (file.exists("Exampleee")) {
      stop("Can not copy the tutorial folder!")
    }
   file.copy(from = folderLocation, to = getwd(), overwrite = T, recursive = T)
   file.rename(from = "Exampleee", folderName)
  }
}
