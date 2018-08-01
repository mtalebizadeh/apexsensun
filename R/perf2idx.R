#' @title perf2idx
#'
#' @description  Renders simulation numbers meeting selected performance criteria.
#'
#' @param perfMatrix A data.frame object containing performance matrix (i.e. output of dws2perf function).
#' @param lowLimit A vector contatining the lower limits for RMSE, NASH and PBIAS, Mean e.g. c(0,0,-25,0).
#' @param upLimit A vector contatining the upper limits for RMSE, NASH and PBIAS, Mean, e.g. c(10,1,+25,5).
#'
#' @return A data frame containing accepted simulations and their performance measure values.
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
#' # Calculating performance matrix:
#'     PerfMat <- dws2perf(observedFilePath ="Example/Observed_Files/observed.txt",
#'                     dwsFolderPath = "Example/Calculated_Outputs/DWS",
#'                     startDate="2002-01-01",
#'                     endDate="2003-01-01",
#'                     captionDwsVar="ET",captionObsVar="ET",TW="week")
#' # Extracting simulations meeting performance criteria:
#'     acceptedSimulations <- perf2idx(perfMatrix = PerfMat,
#'                                     lowLimit = c(0, 0, -2, 0),
#'                                     upLimit = c(10, 1, 2, 5))
#' }
perf2idx <- function (perfMatrix, lowLimit, upLimit) {

  idxAccept <- as.data.frame(array(data = NaN,dim = dim(perfMatrix)))
  names(idxAccept) <- names(perfMatrix)
  i = 1
  j = 1
  while (i <= nrow(perfMatrix)) {

    if (lowLimit[1] <= perfMatrix['RMSE'][i,1] && perfMatrix['RMSE'][i,1] <= upLimit[1] &&
        lowLimit[2] <= perfMatrix['NASH'][i,1] && perfMatrix['NASH'][i,1] <= upLimit[2] &&
        lowLimit[3] <= perfMatrix['PBIAS'][i,1] && perfMatrix['PBIAS'][i,1] <= upLimit[3] &&
        lowLimit[4] <= perfMatrix['MEAN'][i,1] && perfMatrix['MEAN'][i,1] <= upLimit[4])  {

        idxAccept$Index[j] <- i
        idxAccept$RMSE[j] <- perfMatrix['RMSE'][i,1]
        idxAccept$NASH[j] <- perfMatrix['NASH'][i,1]
        idxAccept$PBIAS[j] <- perfMatrix['PBIAS'][i,1]
        idxAccept$MEAN[j] <- perfMatrix['MEAN'][i,1]
      j = j + 1
      i = i + 1
    } else {
      i = i + 1
    }
  }

  return(na.omit(idxAccept))
}
