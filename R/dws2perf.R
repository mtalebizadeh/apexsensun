
#' dws2perf
#' @description Calculates performance matrix using observed data and simulated .DWS files for a selected time averaging window.
#'
#' @param observedFilePath File path containing observed data.
#' @param dwsFolderPath Folder path containing .DWS output files.
#' @param perfMatrixFileName An arbitrary file name for saving performance matrix. Will not save if not assigned a value.
#' @param startDate Start date with format YYYY-MM-DD (e.g. "2004-05-18").
#' @param endDate End date with format YYYY-MM-DD (e.g. "2004-05-18").
#' @param captionDwsVar Variable caption as it appears in the .DWS output file.
#' @param captionObsVar variable caption as it appears in the observed file.
#' @param TW Time averaging window could be: "day" , or "week", or "month" or "year" or any multiple of them, e.g. "3 month".
#'
#' @return A data frame containing model performance values.
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
#'}
#'
dws2perf <- function(observedFilePath, dwsFolderPath,
                      startDate, endDate,
                      captionDwsVar, captionObsVar,
                      TW="day", perfMatrixFileName=NA) {

  perfMatrixFolderPath <- getwd()

  sampleSize <- length(list.files(path = dwsFolderPath, pattern = ".DWS"))
  namesList <- list.files(path = dwsFolderPath)
  dwsCaptions <- list.files(path = dwsFolderPath, pattern = "1.DWS")
  shortestCharLength <- .Machine$double.xmax
  for (caption in dwsCaptions) {
    shortestCharLength <- min(shortestCharLength, nchar(caption))
  }
  dwsRootName<- substr(dwsCaptions[1],start = 1 ,stop = (shortestCharLength-5))
  obsTimeseries <- obs2timeseries(observedFilePath, captionObsVar,
                                   startDate, endDate)

  perfMat <- data.frame(Index = rep(NaN, sampleSize[1]),
                        RMSE = rep(NaN, sampleSize[1]),
                        NASH = rep(NaN, sampleSize[1]),
                        PBIAS = rep(NaN, sampleSize[1]),
                        MEAN = rep(NaN, sampleSize[1]))

  perfMat$Index <- 1:sampleSize[1]

  for (i in 1:sampleSize[1]) {
    simTimeseries <- dws2timeseries(paste(dwsFolderPath,
                                           "/", dwsRootName,
                                           toString(i), ".dws",
                                           sep = ""),
                                    captionDwsVar, startDate, endDate)

    perfMat['RMSE'][i,1] <- outputRMSEProTW(simTimeseries, obsTimeseries,
                                            startDate, endDate, TW)
    perfMat['NASH'][i,1] <- outputNASHProTW(simTimeseries, obsTimeseries,
                                            startDate, endDate, TW)
    perfMat['PBIAS'][i,1] <- outputPBIASProTW(simTimeseries, obsTimeseries,
                                            startDate, endDate, TW)
    perfMat['MEAN'][i,1] <- outputMEANProTW(simTimeseries,
                                            startDate, endDate, TW)

    print(paste("Files Processed...", toString(i), sep = ""))

  }

  perfMat<-na.omit(perfMat)
  if (!is.na(perfMatrixFileName)) {
    save(perfMat,file = paste(perfMatrixFileName,".RData", sep = ""))
    print("performance measure matrix was written successfully!")
  }

return(perfMat)
}
#----
#
# Helper functions
#
obs2timeseries <- function(observedFilePath, captionObsVar,
                            startDate, endDate) {
  #Creates a dataframe containing observed variable.
  #
  ATemp <- read.table(observedFilePath, header = TRUE)

  firstDwsDate <- paste(substr(x = ATemp$Date[1],start = 1,stop = 4),
                        "-", substr(x = ATemp$Date[1],start = 5,stop = 6),
                        "-", substr(x = ATemp$Date[1],start = 7,stop = 8),
                        sep = "")

  lastDwsDate <- paste(substr(x= ATemp$Date[nrow(ATemp)], start = 1,stop = 4),
                         "-", substr(x= ATemp$Date[nrow(ATemp)], start = 5, stop = 6),
                         "-", substr(x= ATemp$Date[nrow(ATemp)], start = 7, stop = 8),
                         sep = "")

  ATemp$Date <- as.character(seq.Date(as.Date(firstDwsDate),
                                      as.Date(lastDwsDate),
                                      by = "day"))
  dateSeq <- as.character(seq.Date(as.Date(startDate),
                                   as.Date(endDate),
                                   by = "day"))
  idxAllDates <- 1:nrow(ATemp)
  idxStartDate <- idxAllDates[ATemp$Date == startDate]
  idxEndDate <- idxAllDates[ATemp$Date == endDate]
  timeSeriesObs <- array(NaN,
                         dim = c(length(dateSeq),
                                 length(captionObsVar) +1))
  timeSeriesObs[, 1] <- dateSeq
  colNames <- c("Date", captionObsVar)
  colnames(timeSeriesObs) <- colNames
  timeSeriesObs <- as.data.frame(timeSeriesObs)

  for (i in (1:(length(colNames) - 1))) {

    timeSeriesObs[colNames[i + 1]] <- ATemp[captionObsVar[i]][idxStartDate:idxEndDate,]
  }
  return(timeSeriesObs)
}
#
#
dws2timeseries <- function(fileName, captionDwsVar,
                           startDate, endDate) {

    nameLineNumber <-9
    connSource <- file(description = fileName,open = "r+",blocking = TRUE)
    readLineObj <- readLines(con = connSource, n = -1)
    nameLine <- strsplit(x = readLineObj[nameLineNumber], split = " ")
    nameLineVector <- array(data = NaN, dim = c(1, length(nameLine[[1]])))

    i=1
    for (elm in nameLine[[1]]) {
      nameLineVector[i] <- nameLine[[1]][i]
      i=i+1
    }

    nameLineVector <- nameLineVector[nameLineVector!=""]

    ATemp <- as.data.frame(array(data = NaN,
                                 dim = c(length(readLineObj)
                                         ,length(nameLineVector)-2)))

    colnames(ATemp) <- c("Date",nameLineVector[4:length(nameLineVector)])

    firstDwsDate <- paste(toString(strtoi(substr(readLineObj[nameLineNumber+1]
                                                   ,start = 1, stop = 5))),
                          "-",
                          toString(strtoi(substr(readLineObj[nameLineNumber+1]
                                                   ,start = 6, stop = 9))),
                          "-",
                          toString(strtoi(substr(readLineObj[nameLineNumber+1]
                                                   ,start = 10, stop = 13)))
                          ,sep="")

    ATemp['Date'] <- as.character(seq.Date(from = as.Date(firstDwsDate),
                                           by = "day",
                                           length.out = length(readLineObj)))
    close(connSource)

    try(ATemp <- read.table(fileName, header = TRUE, skip = 8), silent = TRUE)
    ATemp$Date <- as.character(seq.Date(from = as.Date(firstDwsDate),
                                        by = "day",
                                        length.out = nrow(ATemp)))

    if (captionDwsVar=="TN") {
      ATemp$TN <- ATemp$QN + ATemp$YN + ATemp$QDRN +
                  ATemp$RSFN + ATemp$QRFN + ATemp$SSFN
    }

    if(captionDwsVar=="TP") {
      ATemp$TP <- ATemp$QP + ATemp$YP + ATemp$QDRP + ATemp$QRFP
    }

    dateSeq <- as.character(seq.Date(as.Date(startDate),
                                     as.Date(endDate),
                                     by = "day"))
    idxAllDates <- 1:nrow(ATemp)
    idxStartDate <- idxAllDates[ATemp$Date == startDate]
    idxEndDate <- idxAllDates[ATemp$Date == endDate]
    timeSeriesDws <- array(NaN,
                           dim = c(length(dateSeq),
                                   length(captionDwsVar) + 1))
    timeSeriesDws[, 1] <- dateSeq
    colNames <- c("Date", captionDwsVar)
    colnames(timeSeriesDws) <- colNames
    timeSeriesDws <- as.data.frame(timeSeriesDws)

    for (i in (1:(length(colNames) - 1))) {
      timeSeriesDws[colNames[i + 1]] <- ATemp[captionDwsVar[i]][idxStartDate:idxEndDate,]
    }
    return(timeSeriesDws)
  }
#
#
outputNASHProTW <- function (simSeries, obsSeries,
                             startDate, endDate,
                             TW="day") {
  # Calculates Nash-Sutcliffe performance measure.
  simSeriesAgg <- agg4timeseries(timeSeries = simSeries,
                                 startDate = startDate,
                                 endDate = endDate, TW = TW)
  obsSeriesAgg <- agg4timeseries(timeSeries = obsSeries,
                                 startDate = startDate,
                                 endDate = endDate, TW = TW)
  NsNumerator <- sum((simSeriesAgg[[2]] - obsSeriesAgg[[2]])^2)
  NsDenominator <- sum((obsSeriesAgg[[2]] - mean(obsSeriesAgg[[2]], na.rm = TRUE))^2)
  NASH <- 1 - (NsNumerator/NsDenominator)

   if (is.na(NASH)) {
     NASH <- -999
   }
   return(NASH)
 }
#
#
outputRMSEProTW <- function (simSeries, obsSeries,
                               startDate, endDate, TW="day") {
  # Calculates RMSE performance measure
  simSeriesAgg <- agg4timeseries(timeSeries = simSeries, startDate = startDate,
                                   endDate = endDate, TW = TW)
  obsSeriesAgg <- agg4timeseries(timeSeries = obsSeries, startDate = startDate,
                                   endDate = endDate, TW = TW)
  RMSESeries <- sqrt(mean((simSeriesAgg[[2]] - obsSeriesAgg[[2]])^2, na.rm = TRUE))
  if (is.na(RMSESeries)) {
    RMSESeries <- -999
  }
  return(RMSESeries)
}
#
#
outputMEANProTW <- function (simSeries, startDate, endDate, TW="day") {
  #Calculates mean output (No observed data is used).
  simSeriesAgg <- agg4timeseries(timeSeries = simSeries, startDate = startDate,
                                 endDate = endDate, TW = TW)

  MEANSeries <- mean(simSeriesAgg[[2]], na.rm = TRUE)
  if (is.na(MEANSeries)) {
    MEANSeries <- -999
  }
  return(MEANSeries)
}
#
#
outputPBIASProTW <- function (simSeries, obsSeries,
                              startDate, endDate, TW="day") {
  #Calculates PBIAS performance measure
  simSeriesAgg <- agg4timeseries(timeSeries = simSeries, startDate = startDate,
                                   endDate = endDate, TW = TW)
  obsSeriesAgg <- agg4timeseries(timeSeries = obsSeries, startDate = startDate,
                                   endDate = endDate, TW = TW)
  NsNumerator <- sum(obsSeriesAgg[[2]] - simSeriesAgg[[2]])*100
  NsDenominator <- sum(obsSeriesAgg[[2]])
  PBIAS <- NsNumerator/NsDenominator
  if (is.na(PBIAS)) {
    PBIAS <- -999
  }
  return(PBIAS)
}
#
#
agg4timeseries <- function(timeSeries, startDate, endDate, TW="day") {
  #Aggregates daily time series.
  if (TW == "day") {
    return(timeSeries)
  } else {
    aggDateSeq <- as.character(seq.Date(from = as.Date(startDate), to = as.Date(endDate), by = TW))
    idxSeq <- 1:nrow(timeSeries)
    idxSeqStart <- idxSeq[timeSeries$Date %in% aggDateSeq[1:(length(aggDateSeq)-1)]]
    idxSeqEnd <- (idxSeq[timeSeries$Date %in% aggDateSeq][-1]) - 1
    idxSeqEnd[length(idxSeqEnd)] <- idxSeqEnd[length(idxSeqEnd)] + 1
    #Empty data frame for storing aggregated time series...
    aggTimeseries <- as.data.frame(array(data = NaN,dim = c(length(idxSeqStart), 2)))
    names(aggTimeseries) <- names(timeSeries)
    aggTimeseries$Date <- aggDateSeq[1:length(aggDateSeq)-1]

    for (i in 1:nrow(aggTimeseries)) {
      aggTimeseries[[2]][i] <- mean(c(timeSeries[[2]][idxSeqStart[i]:idxSeqEnd[i]]), na.rm = TRUE)
    }
    return(aggTimeseries)

  }
}
