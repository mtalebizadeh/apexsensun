#' inputGen
#' @description Generates a List object or text file for setting simulation and SA parameters.
#'
#' @param inputs Not required for setting simulation and SA parameters through a List object. Othereise, a File name for setting parameters through text file.
#'
#' @return A template list object for setting simulation and SA parameters, or Void if the inputs are to be read through text file (i.e. if 'input' is set to a value).
#' @export
#'
#' @examples
#' \dontrun{
#' input_list <- inputGen()   # Creates a List object For setting parameters.
#' inputGen("Input_File.txt") # Creates a text file For setting parameters (Deprecated) .
#' }
inputGen <- function (inputs) {

  if (hasArg(inputs)) {
  fileLocation <- system.file("extdata", "Packaged_Test.txt",
                               package = "APEXSENSUN")
  file.copy(from = fileLocation, to = inputs)
  } else {
    globalInputs <- list(sampleSize = 100 ,captionVarSim = "ET", captionVarObs = "ET",
                      startDate = "2002 01 01", endDate = "2003 01 01", labelAPEXExe = "APEX0806",
                      labelWatershedParam = "PARM", labelControlParam = "Apexcont",
                      labelOutputVariableAWP = "APEX001", labelOutputVariableACY = "APEX001",
                      labelOutputVariableDWS = "APEX001", labelObservedVar.txt = "observed.txt",
                      backUpPARM0806.dat = "Example/Back_Up/PARM.dat", backUpAPEXCONT.dat = "Example/Back_Up/Apexcont.dat",
                      folderPathProject = "Example/APEX",folderPathRCodes = getwd(),
                      folderPathObserved = "Example/Observed_Files", folderPathGsaOutputs = "Example/GSA_Outputs",
                      storeFolderPathWatershed = "Example/Generated_Inputs/PARM",
                      storeFolderPathControl = "Example/Generated_Inputs/CONT",
                      calculatedOutputFolderAWP = "Example/Calculated_Outputs/AWP",
                      calculatedOutputFolderACY = "Example/Calculated_Outputs/ACY",
                      calculatedOutputFolderDWS = "Example/Calculated_Outputs/DWS", gsaType = "SRC",
                      saParms = list(morrisRFactor = 15, morrisLevels = 8,  # Morris parameters.
                                     sobolOrder = 1,  # Sobol parameter.
                                     ksTestPerf = "NASH", ksTestThreshold = 0.2, ksTestSigmaLevel = 0.05),  #Ks-test.
                      apexPARM = genParmRange(), apexCONT = genContRange())  # Apex model parameters
    return(globalInputs)
  }

}



