#' @title genContRange
#' @description Creates a dataframe for setting lower and upper limits of uncertain model parameters inside the APEXCONT.dat file.
#'
#' @param controlRangeFile File name associated to parameter range file containing parameters inside the APEXCONT.dat file.
#'
#' @return Void
#' @export
#'
#' @examples
#' # Used only if the inputs are to be read through text file.
#' \dontrun{
#'     genContRange("controlRangeFile.txt")
#'     }
genContRange <- function (controlRangeFile) {

  contParamDataframe <- watershedParamDataframe <- as.data.frame(array(data = -1, dim = c(2,37)))
  controlParamNames <- c("RFN", "CO2", "CQN", "PSTX", "YWI", "BTA",
                         "EXPK", "QG", "QCF", "CHSO", "BWD", "FCW",
                         "FPSC", "GWSO", "RFTO", "RFPO", "SATO", "FL",
                         "FW","ANG", "UXP", "DIAM", "ACW", "GZL0",
                         "RTN0", "BXCT", "BYCT", "DTHY", "QTH", "STND",
                         "DRV", "PCO0","RCC0", "CSLT", "BUS1", "BUS2", "BUS3")

  names(contParamDataframe) <- controlParamNames


  if (hasArg(controlRangeFile)) {
    write.table(contParamDataframe, file = paste(controlRangeFile, sep=""), sep = "\t",
                row.names = FALSE)
  }
  contParamDataframe
}

