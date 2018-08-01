

########################### Author: Mansour Talebizadeh ####################################
#Description: Writing uncertain APEX control parametesr in control file...
#
#Inputs: 1) uncertainContParam: A dataframe containing uncertain APEX control parameters
#        2) "APEXCONT.dat" Fie path containing APEX control parameters (File must be allready created)
#        3) "backUpAPEXCONT.dat" Fie path containing DEFAULT APEX control parameters (File must be allready created)
#Outputs:
#        Void
############################################################################################

writeControlParameters <-function(uncertainContParam, APEXCONT.dat, backUpAPEXCONT.dat) {
  #
#Loading required functions...
  #source("watershed_cont_dataframe.r")

#Setting up connections...
  connSource <- file(description = backUpAPEXCONT.dat,open = "r+",blocking = TRUE)
  connTarget <- file(description = APEXCONT.dat,open ="r+",blocking = TRUE )

  #Reading from back up file into a readLines object...
  readlinesObj <- readLines(con = connSource,n = -1)
#
###Sub-functionn
  cont2loc <- function(APEXCONT) {
    switch(names(APEXCONT),

      #Line 3...
           "RFN"= return(c(3,1,8,"%8.2f")),
           "CO2"= return(c(3,9,16,"%8.2f")),
           "CQN"= return(c(3,17,24,"%8.2f")),
           "PSTX"= return(c(3,25,32,"%8.2f")),
           "YWI"= return(c(3,33,40,"%8.2f")),
           "BTA"= return(c(3,41,48,"%8.2f")),
           "EXPK"= return(c(3,49,56,"%8.2f")),
           "QG"= return(c(3,57,64,"%8.2f")),
           "QCF"= return(c(3,65,72,"%8.2f")),
           "CHSO"= return(c(3,73,80,"%8.2f")),

       #Line 4...
            "BWD"=return(c(4,1,8,"%8.2f")),
            "FCW"=return(c(4,9,16,"%8.2f")),
            "FPSC"=return(c(4,17,24,"%8.2f")),
            "GWSO"=return(c(4,25,32,"%8.2f")),
            "RFTO"=return(c(4,33,40,"%8.2f")),
            "RFPO"=return(c(4,41,48,"%8.2f")),
            "SATO"=return(c(4,49,56,"%8.2f")),
            "FL"=return(c(4,57,64,"%8.2f")),
            "FW"=return(c(4,65,72,"%8.2f")),
            "ANG"=return(c(4,73,80,"%8.2f")),

      #Line 5...
          "UXP"= return(c(5,1,8,"%8.2f")),
          "DIAM"= return(c(5,9,16,"%8.2f")),
          "ACW"= return(c(5,17,24,"%8.2f")),
          "GZL0"= return(c(5,25,32,"%8.2f")),
          "RTN0"=return(c(5,33,40,"%8.2f")),
          "BXCT"= return(c(5,41,48,"%8.2f")),
          "BYCT"=return(c(5,49,56,"%8.2f")),
          "DTHY"=return(c(5,57,64,"%8.2f")),
          "QTH"=return(c(5,65,72,"%8.2f")),
          "STND"=return(c(5,73,80,"%8.2f")),

      #Line 6...
          "DRV"=return(c(6,1,8,"%8.2f")),
          "PCO0"=return(c(6,9,16,"%8.2f")),
          "RCC0"=return(c(6,17,24,"%8.2f")),
          "CSLT"=return(c(6,25,32,"%8.2f")),
          "BUS1"=return(c(6,33,40,"%8.2f")),
          "BUS2"=return(c(6,41,48,"%8.2f")),
          "BUS3"=return(c(6,49,56,"%8.2f"))

    )
  }

####Sub-Function....
    cont2replace <- function(readlinesObj,APEXCONT) {
      locVec <- cont2loc(APEXCONT)
      substr(x = readlinesObj[strtoi(locVec[1])],start = strtoi(locVec[2]),
             stop = strtoi(locVec[3])) <- sprintf(fmt = locVec[4],APEXCONT[[1]])
      return(readlinesObj)

    }
################
    i=1
    colNumbers <-ncol(uncertainContParam)
    while(i<=colNumbers) {
      readlinesObj <- cont2replace(readlinesObj,uncertainContParam[i])
      i=i+1

    }
###Writing the final readLine object...
    writeLines(text = readlinesObj,con = connTarget)

    close(connSource)
    close(connTarget)
}




