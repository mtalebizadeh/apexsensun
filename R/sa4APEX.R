#' sa4APEX
#' @description Performs sensitivity analysis for RMSE, NASH, PBIAS, and MEAN perofrmance measures.
#' @param input A list object (or a file name) containing simulation and SA parameters.
#' @param input4SA A list containing inputs for SA (i.e. output of a call to mc4APEX function)
#' @param usingTextFile A logical variable (i.e. either TRUE or FALSE) specifying weather to read "Input" from a List object or a text file.
#'
#' @return Void (i.e., writes sensitivity results in text files).
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
#' # Performing sensitivity analysis:
#'     sa4APEX(globalInput,input4SA = input4SA)
#'     }
sa4APEX <- function (input,input4SA,usingTextFile=FALSE) {
  if (!isTRUE(usingTextFile)) {
    globalInputs <- input
  }

  if(isTRUE(usingTextFile) && missing(input4SA) ) {
    globalInputs <- text2InputSimulation(input)
    load(paste(globalInputs$folderPathGsaOutputs,
    "/", globalInputs$gsaType,"_","GSA_WorkSpace",".RData", sep = ""))
  }

  gsaType = globalInputs$gsaType
  gsaObject <- input4SA$gsaObject

  if (gsaType=="KSTEST") {
    for (j in 1:length(globalInputs$captionVarSim)) {
      perfFunc <- modelPerfDWS(globalInputs$folderPathObserved,
                               globalInputs$labelObservedVar.txt,
                               globalInputs$startDate[j],
                               globalInputs$endDate[j], 1,
                               nrow(gsaObject$X),
                               globalInputs$calculatedOutputFolderDWS,
                               globalInputs$labelOutputVariableDWS,
                               globalInputs$captionVarSim[j], "Date",
                               globalInputs$captionVarObs[j])


      subFolderPathGsaOutputs <- paste(globalInputs$folderPathGsaOutputs,
                                       "/", globalInputs$captionVarSim[j], "_",
                                       format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                                       sep = "")

      dir.create(subFolderPathGsaOutputs)
      write.table(perfFunc,
                  paste(subFolderPathGsaOutputs,
                        "/", "Perform.Fcn", "_",
                        gsaType, "_",
                        format(Sys.time(),"%Y_%B_%d_%H_%M"),
                        ".txt", sep = ""),
                  sep = "\t")
      #
      if (gsaObject$ksTestPerf=="NASH") {
        Y_PF <- perfFunc$NASH
        logicSeq <- (Y_PF > gsaObject$ksTestThreshold[j])
      }
      else if (gsaObject$ksTestPerf=="RMSE") {
          Y_PF <- perfFunc$RMSE
          logicSeq <- (Y_PF < gsaObject$ksTestThreshold[j])
      }
      else if (gsaObject$ksTestPerf=="PBIAS") {
        Y_PF <- perfFunc$PBIAS
        logicSeq <- (abs(Y_PF) < abs(gsaObject$ksTestThreshold[j]))
      } else {
        stop("Please enter a valid performance function:, i.e. NASH, RMSE, or PBIAS")
      }


      idxSeq <- 1:nrow(gsaObject$X)
      idxAccept <- as.numeric(idxSeq[logicSeq])
      xMat = gsaObject$X
      idxParm <- 1:nrow(xMat)
      logicExtract <- idxParm %in% idxAccept
      xPosterior <- xMat[logicExtract, ]
      dir.create(paste(subFolderPathGsaOutputs, "/",
                 gsaObject$ksTestPerf,
                 sep = ""))
      gsa_analysis(gsaObject,
                   xPosterior,
                   paste(subFolderPathGsaOutputs,
                         "/", gsaObject$ksTestPerf,
                         sep = ""),
                   gsaType)
    }
  } else {
    for (j in 1:length(globalInputs$captionVarSim)) {
    perfFunc <- modelPerfDWS(globalInputs$folderPathObserved,
                             globalInputs$labelObservedVar.txt,
                             globalInputs$startDate[j],
                             globalInputs$endDate[j], 1, nrow(gsaObject$X),
                             globalInputs$calculatedOutputFolderDWS,
                             globalInputs$labelOutputVariableDWS,
                             globalInputs$captionVarSim[j], "Date",
                             globalInputs$captionVarObs[j])

    subFolderPathGsaOutputs <- paste(globalInputs$folderPathGsaOutputs, "/",
                                     globalInputs$captionVarSim[j], "_",
                                     format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                                     sep = "")

    dir.create(subFolderPathGsaOutputs)
    write.table(perfFunc,
                paste(subFolderPathGsaOutputs, "/",
                      "Perform.Fcn", "_", gsaType,"_",
                      format(Sys.time(), "%Y_%B_%d_%H_%M"),
                      ".txt", sep = ""),
                sep = "\t")

    Y_RMSE <- perfFunc$RMSE
    dir.create(paste(subFolderPathGsaOutputs, "/",
                     "RMSE", sep = ""))
    gsa_analysis(gsaObject,
                 Y_RMSE,
                 paste(subFolderPathGsaOutputs,
                       "/", "RMSE", sep = ""),
                 gsaType)

     Y_MEAN <- perfFunc$MEAN
     dir.create(paste(subFolderPathGsaOutputs, "/", "MEAN", sep = ""))
     gsa_analysis(gsaObject,
                  Y_MEAN,
                  paste(subFolderPathGsaOutputs,
                        "/", "MEAN", sep = ""),
                  gsaType)

    Y_NASH <- perfFunc$NASH
    dir.create(paste(subFolderPathGsaOutputs, "/", "NASH", sep = ""))
    gsa_analysis(gsaObject,
                 Y_NASH,
                 paste(subFolderPathGsaOutputs,
                       "/", "NASH", sep = ""),
                 gsaType)

    Y_PBIAS <- perfFunc$PBIAS
    dir.create(paste(subFolderPathGsaOutputs, "/", "PBIAS", sep = ""))
    gsa_analysis(gsaObject,
                 Y_PBIAS,
                 paste(subFolderPathGsaOutputs,
                       "/", "PBIAS", sep = ""),
                 gsaType)
  }
  }
}
#
#
# Helper Functions...
#
gsa_analysis <- function (GSA_Object, Y, folder_path_GSA_Outputs, GSA_Type) {
  if (GSA_Type == "MORRIS") {
    GSA_Object <- sensitivity::tell(GSA_Object, y = Y)
    Mu_Sigma <- print(GSA_Object)

    write.table(Mu_Sigma,
                paste(folder_path_GSA_Outputs,
                      "/", GSA_Type, "_", format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                      ".txt", sep = ""),
                sep = "\t")

    save(GSA_Object,
         file =paste(folder_path_GSA_Outputs, "/", GSA_Type,
                     "_", "Xmat_and_Yvec", format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                     ".RData", sep = ""))
    return(GSA_Object)
  }
  else if (GSA_Type == "SRC") {
    GSA_Object <- sensitivity::src(GSA_Object$X, y = Y, rank = FALSE,
                                   nboot = 0, conf = 0.95)
    SRC_Coeff <- as.data.frame(sensitivity:::print.src(GSA_Object))
    colnames(SRC_Coeff) <- c("SRCs")
    write.table(SRC_Coeff,
                paste(folder_path_GSA_Outputs,
                      "/", GSA_Type, "_",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                      ".txt", sep = ""),
                sep = "\t")

    save(GSA_Object,
         file = paste(folder_path_GSA_Outputs, "/", GSA_Type,
                      "_", "Final_GSA_Object", format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                      ".RData", sep = ""))
    return(GSA_Object)
  }
  else if (GSA_Type == "SRRC") {
    GSA_Object <- sensitivity::src(GSA_Object$X, y = Y, rank = TRUE,
                                   nboot = 0, conf = 0.95)
    SRC_Coeff <- as.data.frame(sensitivity:::print.src(GSA_Object))
    colnames(SRC_Coeff) <- c("SRRCs")
    write.table(SRC_Coeff,
                paste(folder_path_GSA_Outputs,
                      "/", GSA_Type, "_", format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                      ".txt", sep = ""),
                sep = "\t")

    save(GSA_Object,
         file = paste(folder_path_GSA_Outputs, "/", GSA_Type,
                      "_", "Final_GSA_Object", format(Sys.time(), "%Y_%B_%d_%H_%M_%S")
                      ,".RData", sep = ""))
    return(GSA_Object)
  }
  else if (GSA_Type == "SOBOL") {

    GSA_Object$X1 <- scale(x = GSA_Object$X1,center = TRUE,scale = TRUE)
    GSA_Object$X2 <- scale(x = GSA_Object$X2,center = TRUE,scale = TRUE)
    Y = scale(x = Y,center = TRUE,scale = TRUE)
    GSA_Object <- sensitivity::tell(GSA_Object, y = Y)
    Sobol_Idx <- as.data.frame(sensitivity:::print.sobol(GSA_Object))
    colnames(Sobol_Idx) <- c("Sobol's Indices")
    write.table(Sobol_Idx,
                paste(folder_path_GSA_Outputs,
                      "/", GSA_Type, "_",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                      ".txt", sep = ""),
                sep = "\t")

    save(GSA_Object,
         file = paste(folder_path_GSA_Outputs, "/", GSA_Type,
                      "_", "Final_GSA_Object",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),".RData", sep = ""))
    return(GSA_Object)
  }
  else if (GSA_Type == "SOBOL2002") {

    GSA_Object$X1 <- scale(x = GSA_Object$X1,center = TRUE,scale = TRUE)
    GSA_Object$X2 <- scale(x = GSA_Object$X2,center = TRUE,scale = TRUE)
    Y = scale(x = Y,center = TRUE,scale = TRUE)
    GSA_Object <- sensitivity::tell(GSA_Object, y = Y)
    Sobol2002_Idx_S <- GSA_Object$S
    Sobol2002_Idx_T <- GSA_Object$T
    Sobol2002_Idx <- cbind(Sobol2002_Idx_S,Sobol2002_Idx_T)
    colnames(Sobol2002_Idx) <- c("Main Effect","Total Effect")

    write.table(Sobol2002_Idx,
                paste(folder_path_GSA_Outputs,
                      "/", GSA_Type, "_",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                      ".txt", sep = ""),
                sep = "\t")

    save(GSA_Object,
         file = paste(folder_path_GSA_Outputs,
                      "/", GSA_Type,
                      "_", "Final_GSA_Object",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),".RData",
                      sep = ""))
  }
  else if (GSA_Type == "SOBOL2007") {

    GSA_Object$X1 <- scale(x = GSA_Object$X1,center = TRUE,scale = TRUE)
    GSA_Object$X2 <- scale(x = GSA_Object$X2,center = TRUE,scale = TRUE)
    Y = scale(x = Y,center = TRUE,scale = TRUE)
    GSA_Object <- sensitivity::tell(GSA_Object, y = Y)
    SOBOL2007_Idx_S <- GSA_Object$S
    SOBOL2007_Idx_T <- GSA_Object$T
    SOBOL2007_Idx <- cbind(SOBOL2007_Idx_S,SOBOL2007_Idx_T)
    colnames(SOBOL2007_Idx) <- c("Main Effect","Total Effect")

    write.table(SOBOL2007_Idx,
                paste(folder_path_GSA_Outputs,
                      "/", GSA_Type, "_",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                      ".txt", sep = ""),
                sep = "\t")

    save(GSA_Object,
         file = paste(folder_path_GSA_Outputs,
                      "/", GSA_Type,
                      "_", "Final_GSA_Object",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),".RData",
                      sep = ""))
  }
  else if (GSA_Type == "SOBOLEFF") {
    tell(GSA_Object, y = Y)
    Soboleff_Idx <- print(GSA_Object)
    write.table(Soboleff_Idx,
                paste(folder_path_GSA_Outputs,
                      "/", GSA_Type, "_",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                      ".txt", sep = ""),
                sep = "\t")

    save(GSA_Object,
         file = paste(folder_path_GSA_Outputs,
                      "/", GSA_Type,
                      "_", "Final_GSA_Object",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),".RData",
                      sep = ""))
  }
  else if (GSA_Type == "SOBOLJANSEN") {

    sensitivity::tell(GSA_Object, y = Y)
    SOBOLJANSEN_Idx_S <- GSA_Object$S
    SOBOLJANSEN_Idx_T <- GSA_Object$T
    SOBOLJANSEN_Idx <- cbind(SOBOLJANSEN_Idx_S,SOBOLJANSEN_Idx_T)
    colnames(SOBOLJANSEN_Idx) <- c("Main Effect","Total Effect")

    write.table(SOBOLJANSEN_Idx,
                paste(folder_path_GSA_Outputs,
                      "/", GSA_Type, "_",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                      ".txt", sep = ""),
                sep = "\t")

    save(GSA_Object,
         file = paste(folder_path_GSA_Outputs,
                      "/", GSA_Type,
                      "_", "Final_GSA_Object",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),".RData",
                      sep = ""))
  }
  else if (GSA_Type == "SOBOLMARA") {

    GSA_Object$X <- scale(x = GSA_Object$X,center = TRUE,scale = TRUE)
    Y = scale(x = Y,center = TRUE,scale = TRUE)
    sensitivity::tell(GSA_Object, y = Y)
    SOBOLMARA_Idx <- GSA_Object$S
    colnames(SOBOLMARA_Idx) <- c("Main Effect")

    write.table(SOBOLMARA_Idx,
                paste(folder_path_GSA_Outputs,
                      "/", GSA_Type, "_",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_S%"),
                      ".txt", sep = ""),
                sep = "\t")

    save(GSA_Object,
         file = paste(folder_path_GSA_Outputs,
                      "/", GSA_Type,
                      "_", "Final_GSA_Object",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),".RData",
                      sep = ""))
  }
  else if (GSA_Type == "SOBOLMARTINEZ") {

    sensitivity::tell(GSA_Object, y = Y)
    SOBOLMARTINEZ_Idx_S <-  GSA_Object$S['original']
    SOBOLMARTINEZ_Idx_T <- GSA_Object$T['original']
    SOBOLMARTINEZ_Idx <- cbind(SOBOLMARTINEZ_Idx_S,SOBOLMARTINEZ_Idx_T)
    colnames(SOBOLMARTINEZ_Idx) <- c("Main Effect","Total Effect")

    write.table(SOBOLMARTINEZ_Idx,
                paste(folder_path_GSA_Outputs,
                      "/", GSA_Type, "_",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                      ".txt", sep = ""),
                sep = "\t")

    save(GSA_Object,
         file = paste(folder_path_GSA_Outputs,
                      "/", GSA_Type,
                      "_", "Final_GSA_Object",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),".RData",
                      sep = ""))

  } else if (GSA_Type=="FAST99") {


    GSA_Object$X <- scale(x = GSA_Object$X,center = TRUE,scale = TRUE)
    Y = scale(x = Y,center = TRUE,scale = TRUE)
    GSA_Object <- sensitivity::tell(GSA_Object, y = Y)
    FAST99_Idx <- as.data.frame(sensitivity:::print.fast99(GSA_Object))
    colnames(FAST99_Idx) <- c("Main Efffect","Total Effect")

    write.table(FAST99_Idx,
                paste(folder_path_GSA_Outputs,
                      "/", GSA_Type, "_",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                      ".txt", sep = ""),
                sep = "\t")

    save(GSA_Object,
         file = paste(folder_path_GSA_Outputs,
                      "/", GSA_Type,
                      "_", "Final_GSA_Object",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),".RData",
                      sep = ""))

  } else if (GSA_Type=="KSTEST") {

    #Generating an empty vector for storing KS-Test results
    KS_Test <- as.data.frame(array(NaN,dim = c(ncol(GSA_Object$X),3)))
    row.names(KS_Test) <- colnames(GSA_Object$X)
    colnames(KS_Test) <- c("Sensitivity_State","p-value","D-statistic")
    i=1
    X_behave <- Y;

    write.table(x = X_behave,
                file = paste(folder_path_GSA_Outputs,
                             "/", GSA_Type,
                             "_", "Behaviour",
                             ".txt", sep = ""),
                sep = "\t",row.names = T)

    write.table(x = GSA_Object$X,
                file = paste(folder_path_GSA_Outputs,
                             "/", GSA_Type,
                             "_", "Prior",
                             ".txt", sep = ""),
                sep = "\t",row.names = F)

    if (nrow(Y)==0) {
      print("No 'behavior' simulation detected!")
      X_Non_Behaviour = GSA_Object$X

    } else {
      idx_seq = 1:nrow(GSA_Object$X)
      idx_non_behavior = idx_seq[!(GSA_Object$X[,1] %in% Y[,1])]
      X_Non_Behaviour <- GSA_Object$X[idx_non_behavior,]
    }

    write.table(x = X_Non_Behaviour,
                file = paste(folder_path_GSA_Outputs,
                             "/", GSA_Type,
                             "_", "Non-Behaviour",
                             ".txt", sep = ""),
                sep = "\t",row.names = T)

    while (i<=ncol(GSA_Object$X)) {
      ks_test_temp = list(p.value=-999)
      try(ks_test_temp <- ks.test(x=(GSA_Object$X[,i][!(GSA_Object$X[,i]%in%Y[,i])]),
                                  y = Y[,i], alternative = "two.sided"),silent = TRUE)

      if((ks_test_temp$p.value <= GSA_Object$KS_TEST_sig_level)&&(ks_test_temp$p.value >= 0) ) {
        KS_Test[i,1] <-  "Sensitive"
        KS_Test[i,2] <-  ks_test_temp$p.value
        KS_Test[i,3] <-  as.numeric(ks_test_temp$statistic)
      }
      else if ( (ks_test_temp$p.value >= GSA_Object$KS_TEST_sig_level)&&(ks_test_temp$p.value <= 1)   ) {
        KS_Test[i,1] <-  "Non-sensitive"
        KS_Test[i,2] <-  ks_test_temp$p.value
        KS_Test[i,3] <-  as.numeric(ks_test_temp$statistic)
      }
      else if (ks_test_temp$p.value < 0){
        KS_Test[i,1] <-  "Inconclusive"
        KS_Test[i,2] <-  NaN
        KS_Test[i,3] <-  NaN
      }

      i=i+1
    }

    write.table(KS_Test, paste(folder_path_GSA_Outputs,
                               "/", GSA_Type, "_",
                               format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),
                               ".txt", sep = ""),
                sep = "\t")

    GSA_Object$Y <- Y
    save(GSA_Object,
         file = paste(folder_path_GSA_Outputs,
                      "/", GSA_Type,
                      "_", "Final_GSA_Object",
                      format(Sys.time(), "%Y_%B_%d_%H_%M_%S"),".RData",
                      sep = ""))
  } else {
    stop("Please Enter a Valid GSA Method")
  }
}
#
#
# Helper Functions




modelPerfDWS <- function (folderPathObserved, labelObservedVar.txt,
                          startDate, endDate, idxFirstFile,
                          idxLastFile, calculatedOutputFolderDWS,
                          labelOutputVariableDWS, captionVarSim,
                          captionDateObs = "Date", captionVarObs) {


  obsTimeseries <- obsDataframeDaily(paste(folderPathObserved,
                                           "/", labelObservedVar.txt,
                                           sep = ""),
                                     startDate, endDate,
                                     captionDateObs = "Date", captionVarObs)

  sampleSize <- (idxLastFile - idxFirstFile) + 1
  perfFunc <- data.frame(RMSE = rep(NaN, sampleSize[1]),
                         MEAN = rep(NaN, sampleSize[1]),
                         NASH = rep(NaN, sampleSize[1]),
                         PBIAS = rep(NaN, sampleSize[1]))

  for (i in 1:sampleSize[1]) {
    Sim_timeseries <- simDataframeDWS(paste(calculatedOutputFolderDWS,
                                            "/", labelOutputVariableDWS,
                                            toString(i + idxFirstFile -1),
                                            ".DWS", sep = ""),
                                      startDate, endDate,
                                      captionVarSim)

    perfFunc$RMSE[i] <- calculateRMSE(Sim_timeseries, obsTimeseries)
    perfFunc$MEAN[i] <- mean(Sim_timeseries$Signal, na.rm = TRUE)
    perfFunc$NASH[i] <- calculateNASH(Sim_timeseries, obsTimeseries)
    perfFunc$PBIAS[i] <- calculatePBIAS(Sim_timeseries, obsTimeseries)
    print(paste("Files Processed...", toString(i), sep = ""))

  }

  return(perfFunc)
}
#----
# Helper functions
obsDataframeDaily <- function (obsFile.txt, startDate, endDate,
                               captionDateObs = "Date", captionVarObs) {
  # Creates a dataframe with columns "Date" and "Signal" from a .DWS file.
  #
  #Args:
  #  SITE271.DWS: name of the .DWS file.
  #  startDate: start date.
  #  endDate: end date.
  #  captionVarSim: Variable caption as in the .DWS file.
  #Returns: Simulated series.
  #
  startDate <- gsub(" ", "", startDate)
  endDate <- gsub(" ", "", endDate)
  obsTable <- read.table(obsFile.txt, header = TRUE)
  namesVec <- names(obsTable)
  idxCaptionDateObs <- match(captionDateObs, namesVec)
  idxCaptionVarObs <- match(captionVarObs, namesVec)
  dateString <- as.character(obsTable[[idxCaptionDateObs]])
  signalVector <- obsTable[[idxCaptionVarObs]]
  startIdx <- match(startDate, dateString)
  endIdx <- match(endDate, dateString)
  obsTimeSeriesLength <- (endIdx - startIdx) + 1
  obsTimeSeries <- data.frame(Date = rep(1, times = obsTimeSeriesLength),
                              Signal = rep(1, times = obsTimeSeriesLength))
  obsTimeSeries$Date <- dateString[startIdx:endIdx]
  obsTimeSeries$Signal <- signalVector[startIdx:endIdx]
  obsTimeSeries
}
#----
#
simDataframeDWS <- function(SITE271.DWS, startDate, endDate, captionVarSim) {
  # Creates a dataframe with columns "Date" and "Signal" from a .DWS file.
  #
  #Args:
  #  SITE271.DWS: name of the .DWS file.
  #  startDate: start date.
  #  endDate: end date.
  #  captionVarSim: Variable caption as in the .DWS file.
  #Returns: Simulated series.
  #
  startDate <- gsub(" ", "", startDate)
  endDate <- gsub(" ", "", endDate)
  simTable <- read.table(SITE271.DWS, header = TRUE, skip = 8)
  simSize <- dim(simTable)
  dateString <- rep(1, times = simSize[1])

  for (i in 1:simSize[1]) {
    yearString <- toString(simTable$Y[i])
    if (simTable$M[i] < 10) {
      monthString <- paste("0", toString(simTable$M[i]), sep = "")
    } else {
      monthString <- toString(simTable$M[i])
    }

    if (simTable$D[i] < 10) {
      dayString <- paste("0", toString(simTable$D[i]),sep = "")

    } else {
      dayString <- toString(simTable$D[i])
    }

    dateString[i] <- paste(yearString, monthString, dayString, sep = "")
  }

  startIdx <- match(startDate, dateString)
  endIdx <- match(endDate, dateString)
  simTimeSeriesLength <- (endIdx - startIdx) + 1
  simTimeSeries <- data.frame(Date = rep(1, times = simTimeSeriesLength),
                              Signal = rep(1, times = simTimeSeriesLength))

  if (captionVarSim == "WYLD") {
    simTimeSeries$Signal <- simTable$WYLD[startIdx:endIdx]
  }
  else if (captionVarSim == "RFV") {
    simTimeSeries$Signal <- simTable$RFV[startIdx:endIdx]
  }
  else if (captionVarSim == "QDR") {
    simTimeSeries$Signal <- simTable$QDR[startIdx:endIdx]
  }
  else if (captionVarSim == "ET") {
    simTimeSeries$Signal <- simTable$ET[startIdx:endIdx]
  }
  else if (captionVarSim == "Q") {
    simTimeSeries$Signal <- simTable$Q[startIdx:endIdx]
  }
  else if (captionVarSim == "YN") {
    simTimeSeries$Signal <- simTable$YN[startIdx:endIdx]
  }
  else if (captionVarSim == "YP") {
    simTimeSeries$Signal <- simTable$YP[startIdx:endIdx]
  }
  else if (captionVarSim == "QN") {
    simTimeSeries$Signal <- simTable$QN[startIdx:endIdx]
  }
  else if (captionVarSim == "QP") {
    simTimeSeries$Signal <- simTable$QP[startIdx:endIdx]
  }
  else if (captionVarSim == "PRKP") {
    simTimeSeries$Signal <- simTable$PRKP[startIdx:endIdx]
  }
  else if (captionVarSim == "DPRK") {
    simTimeSeries$Signal <- simTable$DPRK[startIdx:endIdx]
  }
  else if (captionVarSim == "QDRN") {
    simTimeSeries$Signal <- simTable$QDRN[startIdx:endIdx]
  }
  else if (captionVarSim == "QDRP") {
    simTimeSeries$Signal <- simTable$QDRP[startIdx:endIdx]
  }
  else if (captionVarSim == "DN") {
    simTimeSeries$Signal <- simTable$DN[startIdx:endIdx]
  }
  else if (captionVarSim == "RSFN") {
    simTimeSeries$Signal <- simTable$RSFN[startIdx:endIdx]
  }
  else if (captionVarSim == "QRFN") {
    simTimeSeries$Signal <- simTable$QRFN[startIdx:endIdx]
  }
  else if (captionVarSim == "QRFP") {
    simTimeSeries$Signal <- simTable$QRFP[startIdx:endIdx]
  }
  else if (captionVarSim == "SSFN") {
    simTimeSeries$Signal <- simTable$SSFN[startIdx:endIdx]
  }
  else if (captionVarSim == "SSF") {
    simTimeSeries$Signal <- simTable$SSF[startIdx:endIdx]
  }
  else if (captionVarSim == "QRF") {
    simTimeSeries$Signal <- simTable$QRF[startIdx:endIdx]
  }
  else if (captionVarSim == "RSSF") {
    simTimeSeries$Signal <- simTable$RSSF[startIdx:endIdx]
  }
  else if (captionVarSim == "TN") {
    simTimeSeries$Signal <- simTable$QN[startIdx:endIdx] +
      simTable$YN[startIdx:endIdx] +
      simTable$QDRN[startIdx:endIdx] +
      simTable$RSFN[startIdx:endIdx] +
      simTable$QRFN[startIdx:endIdx] +
      simTable$SSFN[startIdx:endIdx]
  }
  else if (captionVarSim == "TP") {
    simTimeSeries$Signal <- simTable$QP[startIdx:endIdx] +
      simTable$YP[startIdx:endIdx] +
      simTable$QDRP[startIdx:endIdx] +
      simTable$QRFP[startIdx:endIdx]
  }
  else {
    stop("Please enter a valid variable name, see variables in .DWS files")
  }
  simTimeSeries$Date <- dateString[startIdx:endIdx]
  simTimeSeries
}
#----
#
calculateRMSE <- function (simSeries, obsSeries) {
  # Calculates Root Mean Squar Error (RMSE).
  #
  #Args:
  #  simSeries: Simulated data (i.e., output of a call to simDataframeDWS function).
  #  obsSeries: Observed data (i.e., output of a call to obsDataframeDaily function ).
  #
  #Returns: RMSE
  #
  SE_series <- (simSeries$Signal - obsSeries$Signal)^2
  RMSE <- sqrt(mean(SE_series, na.rm = TRUE))
  return(RMSE)
}
#----
#
calculatePBIAS <- function (simSeries, obsSeries) {
  # Calculates PBIAS performance measure.
  #
  #Args:
  #  simSeries: Simulated data (i.e., output of a call to simDataframeDWS function).
  #  obsSeries: Observed data (i.e., output of a call to obsDataframeDaily function ).
  #
  #Returns: PBIAS
  #
  NS_Numerator <- sum(obsSeries$Signal - simSeries$Signal, na.rm = TRUE) * 100
  NS_Denominator <- sum(obsSeries$Signal, na.rm = TRUE)
  PBIAS <- NS_Numerator/NS_Denominator
  return(PBIAS)
}
#----
#
calculateNASH <- function (simSeries, obsSeries) {
  # Calculates Nash-Sutcliffe (NS) performance measure.
  #
  #Args:
  #  simSeries: Simulated data (i.e., output of a call to simDataframeDWS function).
  #  obsSeries: Observed data (i.e., output of a call to obsDataframeDaily function ).
  #
  #Returns: NS
  #
  NS_Numerator <- sum((simSeries$Signal - obsSeries$Signal)^2,
                      na.rm = TRUE)
  NS_Denominator <- sum((obsSeries$Signal - mean(obsSeries$Signal,
                                                 na.rm = TRUE))^2, na.rm = TRUE)
  NASH <- 1 - (NS_Numerator/NS_Denominator)
  return(NASH)
}
















