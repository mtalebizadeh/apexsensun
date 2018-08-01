

#' idx2parm
#'
#' @description Derives posterior distribution using prior distribution and vector of accepted indices.
#' @param xMat A data.frame object containing prior parameter distributions(i.e. prior distribution can be retrived from the output of MC4APEX function).
#' @param idxAccept An integer vector containing accepted simulation numbers.
#' @return A data.frame object containing posterior distribution.
#' @export
#'
#' @examples
#' \dontrun{
#' # Creating a copy of tutorial folder:
#'     getExampleFolder()
#' # Creating a list for setting inputs:
#'     globalInput <- inputGen()
#' # Setting parameter bounds:
#'     globalInput$apexPARM$Root_growth_soil[1] = 0.15
#'     globalInput$apexPARM$Root_growth_soil[2] = 0.2
#'     globalInput$apexPARM$Soil_evap_plant_cover[1] = 0
#'     globalInput$apexPARM$Soil_evap_plant_cover[2] = 0.5
#' # Performing Monte Carlo simulation:
#'     input4SA <- APEXSENSUN::mc4APEX(globalInput)
#' # Creating posterior distribution:
#'     posteriorParms <- idx2parm(xMat =input4SA$gsaObject$X,
#'                       idxAccept = c(20,23,56,776))
#'         }
idx2parm <- function (xMat, idxAccept) {

  idxParm <- 1:nrow(xMat)
  logicExtract <- idxParm %in% idxAccept
  xMatPost <- xMat[logicExtract, ]
  return(xMatPost)
}




