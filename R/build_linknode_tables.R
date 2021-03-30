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

#' Extract the roadway links and associated nodes from the WFRC network 
#' based on a bounding box
#' 
#' @param bb An object of class `sf` defining a study area.
#' @param gdb_path A path to the WFRC MM database
#' 
#' @return A list with two objects:
#'   - `links` An object of class `sf` with the link shapes and line attributes
#'   - `nodes` An object of class `sf` with the node shapes
#' 
#' @details
#' The algorithm works as follows:
#'   - extract junctions from geodatabase and filter to junctions in `bb`
#'   - extract links from gdb and filter to links in `bb`
#'   - Identify coordinates for ending and starting points of links
#'   - Determine which junctions are closes
#' 
#' @importFrom sf st_read st_transform st_filter
#' @importFrom dplyr mutate row_number
#' 
extract_roads <- function(bb, gdb_path){
  
  # to see the layers in the database
  # st_layers(gdb_path)
  
  # get nodes from the dataset
  nodes <- sf::st_read(gdb_path, layer = "NetworkDataset_ND_Junctions") %>%
    sf::st_transform(4326) %>%
    # create a node id
    dplyr::mutate(id = dplyr::row_number()) %>%
    sf::st_filter(bb)
    
  # get auto_links
  links <- sf::st_read(gdb_path, layer = "AutoNetwork") %>%
    sf::st_transform(4326) %>%
    # create a link id and get other attributes
    dplyr::transmute(
      link_id = dplyr::row_number(), 
      aadt = ifelse(AADT == 0, NA, AADT),
      oneway = ifelse(is.na(Oneway), "FT", Oneway),
      length = Length_Miles,
      speed = Speed, 
      ftype = gsub(".*?([0-9]+).*", "\\1", RoadClass),
    ) %>%
    sf::st_filter(bb)
    
  
  # Node identification =======
  # The links don't have any node information on them. So let's extract the
  # first and last points from each polyline. This actually extracts all of them
  link_points <- links %>%
    dplyr::select(link_id) %>%
    sf::st_cast("POINT") # WARNING: will generate n points for each point.
  
  # now we get the first point of each feature and find the nearest node
  start_nodes <- link_points %>% 
    dplyr::group_by(link_id) %>% dplyr::slice(1) %>%
    sf::st_join(nodes, join = sf::st_nearest_feature) %>%
    dplyr::rename(start_node = id)
  
  # and do the same for the last n() point of each feature
  end_nodes <- link_points %>% 
    dplyr::group_by(link_id) %>% dplyr::slice(dplyr::n()) %>%
    sf::st_join(nodes, join = sf::st_nearest_feature) %>%
    dplyr::rename(end_node = id)
  
  # put the node id's onto the links dataset
  mylinks <- links %>%
    dplyr::left_join(start_nodes %>% sf::st_set_geometry(NULL), by = "link_id") %>%
    dplyr::left_join(end_nodes   %>% sf::st_set_geometry(NULL), by = "link_id")
 
  # remove any nodes that are not part of link endpoints
  mynodes <- nodes %>% 
    dplyr::filter(id %in% mylinks$start_node | id %in% mylinks$end_node)
  
 
  # Return list of links and nodes ============
  list(
    links = mylinks,
    nodes = mynodes
  )
  
}



#' Write link and node sets to CSV files
#' 
#' @param linknodeset A list with two SF objects named `links` and `nodes`
#' @param folder Where the files will be written.
#' 
#' @details 
#' Writes out three files in the target folder:
#'   - `network.geojson` A map for visualization
#'   - `links.csv` A CSV file of all the links with attributes
#'   - `nodes.csv` A CSV file of all the nodes with coordinates
#' 
#' 
write_linknodes <- function(linknodeset, folder){
  
  # check if folder exists
  if(!dir.exists(folder)) dir.create(folder)
  
  # write links as a geojson for mapping
  sf::st_write(linknodeset$links, file.path(folder, "network.geojson"), 
               delete_dsn = TRUE)
   
  # write links as CSV file
  readr::write_csv(linknodeset$links %>% sf::st_set_geometry(NULL), file.path(folder, "links.csv"))
  

  
  # write nodes file
  linknodeset$nodes %>%
    dplyr::mutate(
      x = sf::st_coordinates(.)[, 1],
      y = sf::st_coordinates(.)[, 2]
    ) %>%
    sf::st_set_geometry(NULL) %>%
    readr::write_csv(file.path(folder, "nodes.csv"))
  
  # if there is a centroid frame, write it out also
  if(!is.null(linknodeset$centroids)) {
    linknodeset$nodes %>%
      dplyr::mutate(
        x = sf::st_coordinates(.)[, 1],
        y = sf::st_coordinates(.)[, 2]
      ) %>%
      sf::st_set_geometry(NULL) %>%
      readr::write_csv(file.path(folder, "centroids.csv"))
  }
}



