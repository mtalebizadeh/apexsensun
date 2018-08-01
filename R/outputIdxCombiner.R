
#' outputIdxCombiner
#'@description Pulls out common indices from a number of integer vectors.
#' @param ... Any number of integer vectors.
#'
#' @return An interger vector representing idices that are common among all vectors.
#' @export
#'
#' @examples
#' \dontrun{
#' #Outputs an integer vector containing 7,8,9,10.
#'   combined_indices <- outputIdxCombiner(c(1:10),c(7:13))
#'   }
outputIdxCombiner <- function (...) {

  listOfArgs <- list(...)
  idxCommon <- listOfArgs[[1]]
  for (elm in listOfArgs) {
      idxCommon <- intersect(idxCommon, elm)
    }
  print(idxCommon)
  idxCommon
  }

