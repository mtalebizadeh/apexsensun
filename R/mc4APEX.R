#' mc4APEX
#' @description Performs a Monte Carlo simulation using the settting in an input list or input text file generated from a call to "inputGen" function.
#'
#' @param input A list or a file name containing simulation and SA parameters.
#' @param usingTextFile A logical variable specifying weather to read input from a list object or a text file.
#' @param savingInput4SA A logical variable specifying weather to save outputs.
#'
#' @return A list object containing uncertainty and sensitivity analysis settings and prior parameter distributions.
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
#'}
mc4APEX <- function(input, usingTextFile = FALSE, savingInput4SA = TRUE) {
  if (!isTRUE(usingTextFile)) {
    globalInputs <- input
    sampleSize <- globalInputs$sampleSize
    type <- globalInputs$gsaType
    saParms <- globalInputs$saParms

    # Extracting uncertain model parameters from input list
    allBasinParameters <- input$apexPARM
    allControlParameters <- input$apexCONT
    uncertainBasinParameters <- paramExtract(allBasinParameters)
    uncertainControlParameters <- paramExtract(allControlParameters)
    if (ncol(uncertainBasinParameters) !=0 && ncol(uncertainControlParameters) != 0) {
      allUncertainModelParameters <- cbind(uncertainBasinParameters,
                                           uncertainControlParameters)
    }
    else if (ncol(uncertainBasinParameters) !=0 && ncol(uncertainControlParameters) == 0) {
      allUncertainModelParameters <- uncertainBasinParameters
      }
    else if (ncol(uncertainBasinParameters) == 0 && ncol(uncertainControlParameters) != 0) {
      allUncertainModelParameters <- uncertainControlParameters
    } else {
      stop("No uncertain parameter was found!")
    }

    # Creating gsaObject...
    gsaObject <- createGsaObject(sampleSize, allUncertainModelParameters,
                                 type, saParms)
    input4SA <- list(gsaObject = gsaObject, basinParms = uncertainBasinParameters,
                     controlParms = uncertainControlParameters)
  } else {
    globalInputs <- text2InputSimulation(input)
    input4SA <- input4MCSimulation(input)
  }


  if (isTRUE(savingInput4SA)) {
  save(input4SA,file = paste(globalInputs$folderPathGsaOutputs,
                             "/", globalInputs$gsaType, "_",
                             "GSA_WorkSpace", ".RData", sep = ""))
  }

  # Running Monte Carlo simulation
  runAPEXModel(input4SA, globalInputs$folderPathProject,
               globalInputs$labelWatershedParam, globalInputs$backUpPARM0806.dat,
               globalInputs$labelControlParam, globalInputs$backUpAPEXCONT.dat,
               globalInputs$labelAPEXExe, globalInputs$folderPathRCodes,
               globalInputs$storeFolderPathWatershed, globalInputs$storeFolderPathControl,
               globalInputs$labelOutputVariableAWP, globalInputs$calculatedOutputFolderAWP,
               globalInputs$labelOutputVariableACY, globalInputs$calculatedOutputFolderACY,
               globalInputs$labelOutputVariableDWS, globalInputs$calculatedOutputFolderDWS)

  input4SA

}

# Helper Functions
#
#
paramExtract <- function(paramRange) {
  # This function extracts uncertain parameters from a dataframe containing parameter bounds.
  #
  # Args:
  #   paramRange: A dataframe containing parameter bounds.
  #
  #
  # Returns:
  #   A dataframe containing uncertain model parameters only.
  #
  idx_seq <- 1:ncol(paramRange)
  paramRange <- paramRange[idx_seq[as.vector(paramRange[1,] != paramRange[2,])]]
  return(paramRange)
}
#
#
runAPEXModel <- function(input4SA, folderPathProject, labelWatershedParam,
                         backUpPARM0806.dat, labelControlParam, backUpAPEXCONT.dat,
                         labelAPEXExe, folderPathRCodes, storeFolderPathWatershed,
                         storeFolderPathControl, labelOutputVariableAWP, calculatedOutputFolderAWP,
                         labelOutputVariableACY, calculatedOutputFolderACY,
                         labelOutputVariableDWS, calculatedOutputFolderDWS) {
  # Performs Monte Carlo simulation
  #
  # Args:
  #  input4SA: A list contaning a gsaObject and uncertain parameter bounds.
  #  folderPathProject: Folder path to the APEX model folder.
  #  labelWatershedParam: File label containing watershed parameters.
  #  backUpPARM0806.dat: File label containing original watershed parameters.
  #  labelControlParam: File label containing control parameters.
  #  backUpAPEXCONT.dat: File label containing original control parameters.
  #  labelAPEXExe: File label representing APEX executable model.
  #  folderPathRCodes: Working directory
  #  storeFolderPathWatershed: Folder path storing watershed parameters for Monte Carlo simulations.
  #  storeFolderPathControl: Folder path storing control parameters for Monte Carlo simulations.
  #  labelOutputVariableAWP: File label for .AWP output files.
  #  calculatedOutputFolderAWP: Folder path storing .AWP output files for Monte Carlo simulations.
  #  labelOutputVariableACY: File label for .ACY output files.
  #  calculatedOutputFolderACY: Folder path storing .ACY output files for Monte Carlo simulations.
  #  labelOutputVariableDWS: File label for .DWS output files.
  #  calculatedOutputFolderDWS: Folder path storing .DWS output files for Monte Carlo simulations.
  #
  # Returns:
  #  Void
  #
  folderPathRCodes <-getwd()
  sampleSize <- nrow(input4SA$gsaObject$X)
  gsaObject <- input4SA$gsaObject
  uncertainBasinParameters <- input4SA$basinParms
  uncertainControlParameters <- input4SA$controlParms

  for (i in 1:sampleSize) {
    tempWatershedFilePath <- paste(folderPathProject,
                                   "/", labelWatershedParam, ".dat", sep = "")
    tempControlFilePath <- paste(folderPathProject,
                                 "/", labelControlParam, ".dat", sep = "")
    if (ncol(uncertainBasinParameters)!=0) {
      basinParm2Write <- as.data.frame(gsaObject$X[i, 1:ncol(uncertainBasinParameters)])
      colnames(basinParm2Write) <- colnames(uncertainBasinParameters)
      writeBasinParameters(basinParm2Write, tempWatershedFilePath, backUpPARM0806.dat)
    }

    if (ncol(uncertainControlParameters) != 0) {
      contParm2Write <- as.data.frame(gsaObject$X[i, (ncol(uncertainBasinParameters) +
                                                        1):(ncol(uncertainControlParameters) +
                                                            ncol(uncertainBasinParameters))])
      colnames(contParm2Write) <- colnames(uncertainControlParameters)
      writeControlParameters(contParm2Write, tempControlFilePath, backUpAPEXCONT.dat)
    }

    setwd(folderPathProject)
    system(paste(labelAPEXExe, ".exe", sep = ""))
    setwd(folderPathRCodes)
    file.copy(tempWatershedFilePath, paste(storeFolderPathWatershed,
                                           "/", labelWatershedParam, toString(i), ".dat",
                                           sep = ""),overwrite = TRUE)
    file.copy(tempControlFilePath, paste(storeFolderPathControl,
                                         "/", labelControlParam, toString(i),
                                         ".dat", sep = ""),overwrite = TRUE)
    tempOutputFilepathAWP <- paste(folderPathProject,
                                   "/", labelOutputVariableAWP, ".AWP", sep = "")
    file.copy(tempOutputFilepathAWP, paste(calculatedOutputFolderAWP,
                                           "/", labelOutputVariableAWP, toString(i), ".AWP",
                                           sep = ""),overwrite = TRUE)
    tempOutputFilepathACY <- paste(folderPathProject,
                                   "/", labelOutputVariableACY, ".ACY", sep = "")
    file.copy(tempOutputFilepathACY, paste(calculatedOutputFolderACY,
                                           "/", labelOutputVariableACY, toString(i), ".ACY",
                                           sep = ""),overwrite = TRUE)
    tempOutputFilepathDWS <- paste(folderPathProject,
                                   "/", labelOutputVariableDWS, ".DWS", sep = "")
    file.copy(tempOutputFilepathDWS, paste(calculatedOutputFolderDWS,
                                           "/", labelOutputVariableDWS, toString(i), ".DWS",
                                           sep = ""),overwrite = TRUE)

  }
}
#
#
createGsaObject <- function (sampleSize, allUncertainModelParameters, type,saParms){

  lowerLimits <- as.vector(allUncertainModelParameters[1,], mode = "numeric")
  upperLimits <- as.vector(allUncertainModelParameters[2,], mode = "numeric")
  type = toupper(type)
  if (type == "MORRIS") {
    gsaObject <- sensitivity::morris(model = NULL, factor = names(allUncertainModelParameters),
                                     r = as.numeric(saParms$morrisRFactor) ,
                                     design = list(type = "oat", levels = as.numeric(saParms$morrisLevels),                                                              grid.jump = 1),
                                     binf = lowerLimits, bsup = upperLimits, scale = TRUE)
    gsaObject$X <- as.data.frame(gsaObject$X);
    gsaObject
  }
  else if (type == "SRC") {
    X <- allUncertainModelParameters[-1, ]
    X[1, ] = array(NaN, dim = c(1, length(X)))
    X <- dataFrameAug(X, sampleSize)
    xSize <- dim(X)
    i = 1
    while (i <= xSize[2]) {
      X[i] <- runif(sampleSize, min = lowerLimits[i],
                    max = upperLimits[i])
      i = i + 1
    }
    gsaObject = list(X = X)
  }
  else if (type == "SRRC") {
    X <- allUncertainModelParameters[-1, ]
    X[1, ] = array(NaN, dim = c(1, length(X)))
    X <- dataFrameAug(X, sampleSize)
    xSize <- dim(X)
    i = 1
    while (i <= xSize[2]) {
      X[i] <- runif(sampleSize, min = lowerLimits[i],
                    max = upperLimits[i])
      i = i + 1
    }
    gsaObject = list(X = X)
  }
  else if (type == "SOBOL") {
    X1 <- allUncertainModelParameters[-1, ]
    X2 <- allUncertainModelParameters[-1, ]
    X1[1, ] = array(NaN, dim = c(1, length(X1)))
    X2[1, ] = array(NaN, dim = c(1, length(X2)))
    X1 <- dataFrameAug(X1, sampleSize)
    X2 <- dataFrameAug(X2, sampleSize)
    xSize <- dim(X1)
    i = 1
    while (i <= xSize[2]) {
      X1[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      X2[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      i = i + 1
    }
    gsaObject <- sensitivity::sobol(model = NULL, X1 = X1, X2 = X2,
                                    order = saParms$sobolOrder,
                                    nboot = 0, conf = 0.95)
  }
  else if (type == "SOBOL2002") {
    X1 <- allUncertainModelParameters[-1, ]
    X2 <- allUncertainModelParameters[-1, ]
    X1[1, ] = array(NaN, dim = c(1, length(X1)))
    X2[1, ] = array(NaN, dim = c(1, length(X2)))
    X1 <- dataFrameAug(X1, sampleSize)
    X2 <- dataFrameAug(X2, sampleSize)
    xSize <- dim(X1)
    i = 1
    while (i <= xSize[2]) {
      X1[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      X2[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      i = i + 1
    }
    gsaObject <- sensitivity::sobol2002(model = NULL, X1 = X1, X2 = X2,
                                        nboot = 0, conf = 0.95)
  }
  else if (type == "SOBOL2007") {
    X1 <- allUncertainModelParameters[-1, ]
    X2 <- allUncertainModelParameters[-1, ]
    X1[1, ] = array(NaN, dim = c(1, length(X1)))
    X2[1, ] = array(NaN, dim = c(1, length(X2)))
    X1 <- dataFrameAug(X1, sampleSize)
    X2 <- dataFrameAug(X2, sampleSize)
    xSize <- dim(X1)
    i = 1
    while (i <= xSize[2]) {
      X1[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      X2[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      i = i + 1
    }
    gsaObject <- sensitivity::sobol2007(model = NULL, X1 = X1, X2 = X2,
                                        nboot = 0, conf = 0.95)
  }
  else if (type == "SOBOLEFF") {
    X1 <- allUncertainModelParameters[-1, ]
    X2 <- allUncertainModelParameters[-1, ]
    X1[1, ] = array(NaN, dim = c(1, length(X1)))
    X2[1, ] = array(NaN, dim = c(1, length(X2)))
    X1 <- dataFrameAug(X1, sampleSize)
    X2 <- dataFrameAug(X2, sampleSize)
    xSize <- dim(X1)
    i = 1
    while (i <= xSize[2]) {
      X1[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      X2[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      i = i + 1
    }
    gsaObject <- sobolEff(model = NULL, X1 = X1, X2 = X2,
                          order = 1, nboot = 0, conf = 0.95)
  }
  else if (type == "SOBOLJANSEN") {
    X1 <- allUncertainModelParameters[-1, ]
    X2 <- allUncertainModelParameters[-1, ]
    X1[1, ] = array(NaN, dim = c(1, length(X1)))
    X2[1, ] = array(NaN, dim = c(1, length(X2)))
    X1 <- dataFrameAug(X1, sampleSize)
    X2 <- dataFrameAug(X2, sampleSize)
    xSize <- dim(X1)
    i = 1
    while (i <= xSize[2]) {
      X1[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      X2[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      i = i + 1
    }
    gsaObject <- sensitivity::soboljansen(model = NULL, X1 = X1, X2 = X2,
                                          nboot = 0, conf = 0.95)
  }
  else if (type == "SOBOLMARA") {
    X <- allUncertainModelParameters[-1, ]
    X[1, ] = array(NaN, dim = c(1, length(X)))
    X <- dataFrameAug(X, sampleSize)
    xSize <- dim(X)
    i = 1
    while (i <= xSize[2]) {
      X[i] <- runif(sampleSize, min = lowerLimits[i],
                    max = upperLimits[i])
      i = i + 1
    }
    gsaObject <- sensitivity::sobolmara(model = NULL, X1 = X)
  }
  else if (type == "SOBOLMARTINEZ") {
    X1 <- allUncertainModelParameters[-1, ]
    X2 <- allUncertainModelParameters[-1, ]
    X1[1, ] = array(NaN, dim = c(1, length(X1)))
    X2[1, ] = array(NaN, dim = c(1, length(X2)))
    X1 <- dataFrameAug(X1, sampleSize)
    X2 <- dataFrameAug(X2, sampleSize)
    xSize <- dim(X1)
    i = 1
    while (i <= xSize[2]) {
      X1[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      X2[i] <- runif(sampleSize, min = lowerLimits[i],
                     max = upperLimits[i])
      i = i + 1
    }
    gsaObject <- sensitivity::sobolmartinez(model = NULL, X1 = X1, X2 = X2,
                                            nboot = 0, conf = 0.95)
  }

  else if (type=="FAST99") {

    lowerLimits <- as.vector(allUncertainModelParameters[1,], mode = "numeric")
    upperLimits <- as.vector(allUncertainModelParameters[2,], mode = "numeric")
    limitList <- list()
    i=1
    while (i<=ncol(allUncertainModelParameters)) {
      limitList[[i]] <- list(min=lowerLimits[i],max=upperLimits[i])
      i=i+1
    }

    gsaObject <- sensitivity::fast99(model = NULL, factor = names(allUncertainModelParameters),
                                     n = sampleSize, M = 4,
                                     q = rep("qunif",ncol(allUncertainModelParameters)),q.arg = limitList)

  }
  else if (type=="KSTEST") {

    X <- allUncertainModelParameters[-1, ]
    X[1, ] = array(NaN, dim = c(1, length(X)))
    X <- dataFrameAug(X, sampleSize)
    xSize <- dim(X)
    i = 1
    while (i <= xSize[2]) {
      X[i] <- runif(sampleSize, min = lowerLimits[i],
                    max = upperLimits[i])
      i = i + 1
    }
    gsaObject = list(X = X)
    gsaObject$ksTestPerf <- saParms$ksTestPerf
    gsaObject$ksTestThreshold <- saParms$ksTestThreshold
    gsaObject$ksTestSigmaLevel <- saParms$ksTestSigmaLevel
  } else {
    stop("Please enter a valid method")
  }
  return(gsaObject)

}
#
#
text2InputSimulation <- function (inputFile) {
  # Read inputs from a text file.
  globalInputNames <- c("sampleSize","captionVarSim", "captionVarObs",
                        "startDate","endDate", "labelAPEXExe",
                        "labelWatershedParam", "labelControlParam", "labelOutputVariableAWP",
                        "labelOutputVariableACY", "labelOutputVariableDWS", "labelOutputXmat",
                        "labelObservedVar.txt", "labelTemplateBasin.txt", "labelTemplateControl.txt",
                        "backUpPARM0806.dat", "backUpAPEXCONT.dat", "folderPathProject",
                        "folderPathRCodes", "folderPathObserved", "folderPathGsaOutputs",
                        "storeFolderPathWatershed", "storeFolderPathControl", "calculatedOutputFolderAWP",
                        "calculatedOutputFolderACY", "calculatedOutputFolderDWS", "gsaType")

  globalInputLocations <- c(10, 15, 17,
                            22, 24, 29,
                            31, 33, 35,
                            37, 39, 41,
                            43, 48, 50,
                            52, 54, 56,
                            58, 60, 62,
                            64, 66, 68,
                            70, 72, 74)

  globalInputTypeVector <- c("integer",rep("character",26))


  saParmsInputNames <- c("morrisRFactor", "morrisLevels", "sobolOrder",
                         "ksTestPerf", "ksTestThreshold", "ksTestSigmaLevel")

  saParmsInputLocations <- c(78, 80, 83,
                             86, 88, 90)

  saParmsTypeVector <- c(rep("integer",3), "character", rep("double",2))



  # Creating global input list
  globalInputLength <- length(globalInputNames)
  globalInputs <- as.list(x = rep(NA,globalInputLength))
  names(globalInputs) <- globalInputNames
  for (variable in seq(1,globalInputLength)) {
    print(variable)
    globalInputs[variable] <- list2vec(inputFile = inputFile,
                                       lines2Skip = globalInputLocations[variable],
                                       varType = globalInputTypeVector[variable])
  }

  # Creating saParms input list
  saParmsInputLength <- length(saParmsInputNames)
  saParms <- as.list(x = rep(NA,saParmsInputLength))
  names(saParms) <- saParmsInputNames
  for (variable in seq(1,saParmsInputLength)) {
    saParms[variable] <- list2vec(inputFile = inputFile,
                                  lines2Skip = saParmsInputLocations[variable],
                                  varType = saParmsTypeVector[variable])
  }
  # Appending saParms to globalInputList
  globalInputs$saParms <- saParms

}
#
#
input4MCSimulation <- function (inputFile) {

  globalInputs <- text2InputSimulation(inputFile)
  labelTemplateBasin.txt <- globalInputs$labelTemplateBasin.txt
  labelTemplateControl.txt <- globalInputs$labelTemplateControl.txt
  sampleSize =globalInputs$sampleSize
  type=globalInputs$gsaType
  saParms <- globalInputs$saParms

  allBasinParameters <- read.table(labelTemplateBasin.txt,
                                   header = TRUE)
  allControlParameters <- read.table(labelTemplateControl.txt,
                                     header = TRUE)
  uncertainBasinParameters <- paramExtract(allBasinParameters)
  uncertainControlParameters <- paramExtract(allControlParameters)

  if (ncol(uncertainBasinParameters)!=0 && ncol(uncertainControlParameters)!=0) {
    allUncertainModelParameters <- cbind(uncertainBasinParameters,
                                         uncertainControlParameters)
  }
  else if (ncol(uncertainBasinParameters)!=0 && ncol(uncertainControlParameters)==0) {
    allUncertainModelParameters <- uncertainBasinParameters

  }else if (ncol(uncertainBasinParameters)==0 && ncol(uncertainControlParameters)!=0) {
    allUncertainModelParameters <- uncertainControlParameters

  }else {
    stop("No uncertain parameter was found!")
  }

  gsaObject <- createGsaObject(sampleSize,allUncertainModelParameters,
                               type,saParms)

  return(list(gsaObject = gsaObject,
              basinParms = uncertainBasinParameters,
              controlParms = uncertainControlParameters))
}
#
#
dataFrameAug <- function (dataFrame, repNumb) {
  dim_x <- dim(dataFrame)
  i = 1
  dataFrameAugment <- dataFrame
  i = 1
  while (i < repNumb) {
    dataFrameAugment <- rbind(dataFrameAugment, dataFrame,
                              make.row.names = FALSE)
    i = i + 1
  }
  dataFrameAugment
}
#
#
list2vec <- function (input_file, lines2skip, var_type) {
  list_of_vars <- read.table(input_file, skip = lines2skip,
                             colClasses = var_type, nrows = 1)
  vec_of_vars = NULL
  i = 1
  while (i <= length(list_of_vars)) {
    vec_of_vars[i] = list_of_vars[1, i]
    i = i + 1
  }
  return(vec_of_vars)
}
