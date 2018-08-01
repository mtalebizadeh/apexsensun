
#' idx2Timeseries
#' @description Extracts output time series corresponding to accepted simulation. indices with a selected time window.
#' @param dwsFolderPath Folder path for .DWS output folder.
#' @param startDate Start date with format YYYY-MM-DD (e.g. "2002-01-01").
#' @param endDate End date with format YYYY-MM-DD (e.g. "2003-01-01").
#' @param captionDwsVar Variable caption as it appears in the .DWS output file.
#' @param TW Time averaging window could be: "day" , or "week", or "month" or "year" or any multiple of them, e.g. "3 month".
#' @param acceptedIndices An integer vector containing accepted simulation numbers.
#'
#' @return A dataframe containing aggregated time series for the selected TW.
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
#' # Extracting time series
#'   extractedTimeseries <- idx2Timeseries(dwsFolderPath = "Example/Calculated_Outputs/DWS",
#'                                         startDate="2002-01-01", endDate="2003-01-01",
#'                                         captionDwsVar = "ET",TW="month",
#'                                         acceptedIndices=c(1,15,10,8))
#'}
#'
idx2Timeseries <- function(dwsFolderPath, startDate, endDate,
                           captionDwsVar, TW="day", acceptedIndices="all") {
#Detecting .DWS root name..
  namesList <- list.files(path = dwsFolderPath,pattern = ".DWS")
  dwsCaptions <- list.files(path = dwsFolderPath,pattern = "1.DWS")
  shortestCharLength <- .Machine$double.xmax
  for (caption in dwsCaptions) {
    shortestCharLength <- min(shortestCharLength,nchar(caption))
  }
  rootName<- substr(dwsCaptions[1], start = 1, stop = (shortestCharLength-5))

  if (is.character(acceptedIndices)) {
  acceptedIndices <- 1:length(namesList)
  }


  originalDws<- dws2timeseries(paste(dwsFolderPath, "/", rootName, toString(1), ".DWS", sep = ""),
                               captionDwsVar, startDate, endDate)

  originalAggDws <- agg4timeseries(originalDws, startDate, endDate, TW)
  ensembleMatrix <- array(data = NaN, dim = c(nrow(originalAggDws), (length(acceptedIndices)+1)))
  colnames(ensembleMatrix) <- c("Date", paste("Sim", acceptedIndices, sep=""))
  ensembleMatrix <- as.data.frame(ensembleMatrix)
  ensembleMatrix['Date'] <- originalAggDws['Date']


  for(i in 1:length(acceptedIndices)) {
  temp_file_name <- paste(dwsFolderPath, "/", rootName, acceptedIndices[i], ".DWS", sep = "")
  temp_time_series <- dws2timeseries(temp_file_name, captionDwsVar, startDate, endDate)
  ensembleMatrix[paste("Sim", acceptedIndices[i], sep="")] <-
  (agg4timeseries(temp_time_series, startDate, endDate,TW))[captionDwsVar]
  }
  return(ensembleMatrix)
}



