#' Download the WFRC MM database from AGRC
#'
#' @details 
#' The Utah AGRC multimodal network database is available at 
#'   https://gis.utah.gov/data/transportation/street-network-analysis/#MultimodalNetwork
#'   
#' For this research we downloaded the information from that file in August 2020.
#' The file we downloaded is available on Box, but is not committed to the 
#' repository for space reasons. This file contains code to download the archived 
#' AGRC network, extract it. 
#' 
#' This function requires the user to have the `7z` command line utility installed
#' and available on the PATH.
#' 
#' @importFrom stringr str_c
download_gbd <- function(){
  filegdb <- "data/MM_NetworkDataset_03042021.gdb"
  if(!file.exists(filegdb)) {
    zippedgdb <- "data/agrc_network.zip"
    if(!file.exists(zippedgdb)) {
      download.file("https://byu.box.com/shared/static/bowvzfgciuggw936n0fmw1je9a3t5794.zip",
                    zippedgdb)
    }
    system2("7z", c("e", zippedgdb, stringr::str_c("-o", filegdb)) )
    file.remove("data/agrc_network.zip")
  } else {
    message("File is already available")
  }
}

