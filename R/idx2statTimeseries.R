


#' idx2statTimeseries
#' @description Generates time series statistics for accepted simulations using selected probabilities.
#' @param dwsFolderPath Folder path for .DWS output folder.
#' @param startDate Start date with format YYYY-MM-DD (e.g. "2002-01-01").
#' @param endDate End date with format YYYY-MM-DD (e.g. "2003-01-01").
#' @param captionDwsVar Variable caption as it appears in the .DWS output file.
#' @param TW Time averaging window could be: "day" , or "week", or "month" or "year" or any multiple of them, e.g. "3 month".
#' @param acceptedIndices "all" for all files or An integer vector containing accepted simulation numbers.
#' @param probs Vector of probabilities for which quantiles are calculated at each time step, e.g. c(0.025,0.5,0.975).
#'
#' @return A data frame containing mean and quantile time series for selected probabilities.
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
#' # Calculating stat time series:
#'     statTimeSeries <- idx2statTimeseries(dwsFolderPath = "Example/Calculated_Outputs/DWS/",
#'                                          startDate = "2002-01-01",
#'                                          endDate="2003-01-01",
#'                                          captionDwsVar="ET", TW = "week",
#'                                          acceptedIndices = "all",
#'                                          probs = c(0.025,0.5,0.85,0.975))
#' }
#'
idx2statTimeseries <- function(dwsFolderPath, startDate, endDate,
                                captionDwsVar, TW="day",
                                acceptedIndices="all",
                                probs=c(0.025,0.5,0.975)) {

  if (is.character(acceptedIndices)) {
  acceptedIndices=1:length(list.files(path = dwsFolderPath, pattern = ".DWS"))
  }

  ensembleMat <- createEnsembleMatrix(dwsFolderPath, startDate, endDate,
                                      captionDwsVar, TW, acceptedIndices)

  statTimeseriesMat <- as.data.frame(array(data = NaN,
                                           dim = c(nrow(ensembleMat),
                                                  (length(probs) + 2))))
  quantileNames<-c()
  for (i in (1:length(probs)))  {
  quantileNames[i] <- paste("Quantile_", toString(probs[i]), sep="")
  }

  statNames <- c("Date","Mean",quantileNames)
  names(statTimeseriesMat) <- statNames
  statTimeseriesMat['Date'] <- ensembleMat['Date']
  statTimeseriesMat['Mean'] <- apply(X = ensembleMat[,2:ncol(ensembleMat)], 1,
                                    function(x) mean(x = x))

  for (i in (1:length(probs))) {
  statTimeseriesMat[quantileNames[i]] <- apply(X = ensembleMat[,2:ncol(ensembleMat)], MARGIN = 1,
                                               FUN = function (x) stats::quantile(x=x,probs=probs[i], type=1))
  }
return(statTimeseriesMat)
}
#
#
# Helper Functions
createEnsembleMatrix <- function(dwsFolderPath, startDate, endDate,
                                 captionDwsVar, TW="day", acceptedIndices="all",
                                 timeSeriesFileName="dws_timeseries.txt") {
  #Detecting .DWS root name..
  namesList <- list.files(path = dwsFolderPath,pattern = ".DWS")
  dwsCaptions <- list.files(path = dwsFolderPath,pattern = "1.DWS")
  shortestCharLength <- .Machine$double.xmax
  for (caption in dwsCaptions) {
    shortestCharLength <- min(shortestCharLength, nchar(caption))
  }
  rootName<- substr(dwsCaptions[1], start = 1, stop = (shortestCharLength-5))

  if (is.character(acceptedIndices)) {
    acceptedIndices <- 1:length(namesList)

  }

  originalDws<- dws2timeseries(paste(dwsFolderPath,"/", rootName,toString(1), ".DWS", sep = ""),
                                captionDwsVar, startDate, endDate)
  originalAggDws <- agg4timeseries(originalDws, startDate, endDate,TW)
  ensembleMatrix <- array(data = NaN, dim = c(nrow(originalAggDws), (length(acceptedIndices)+1)))
  colnames(ensembleMatrix) <- c("Date", paste("Sim", acceptedIndices, sep=""))
  ensembleMatrix <- as.data.frame(ensembleMatrix)
  ensembleMatrix['Date'] <- originalAggDws['Date']

  for (i in (1:length(acceptedIndices))) {
    tempFileName <- paste(dwsFolderPath, "/", rootName, acceptedIndices[i], ".DWS", sep = "")
    temp_time_series <- dws2timeseries(tempFileName, captionDwsVar, startDate, endDate)
    ensembleMatrix[paste("Sim", acceptedIndices[i], sep="")] <-
    (agg4timeseries(temp_time_series, startDate, endDate, TW))[captionDwsVar]
  }
  return(ensembleMatrix)
}
