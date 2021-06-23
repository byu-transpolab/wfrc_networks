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
download_gdb <- function(filegdb){
  if(!file.exists(filegdb)) {
    zippedgdb <- "data/agrc_network.zip"
    if(!file.exists(zippedgdb)) {
      download.file("https://byu.box.com/shared/static/o2sc6ozzb0j62n3u5gdw2uakj8pfhrvr.zip",
                    zippedgdb)
    }
    system2("7z", c("e", zippedgdb, stringr::str_c("-o", filegdb)) )
    file.remove("data/agrc_network.zip")
  } else {
    message("File is already available")
  }
  
  return(filegdb)
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
  links <- sf::st_read(gdb_path, layer = "BikePedAuto") %>%
    sf::st_transform(4326) %>%
    dplyr::filter(AutoNetwork == "Y") %>%
    st_cast(., "MULTILINESTRING") %>% 
    sf::st_filter(bb)  %>%
    
    # create a link id and get other attributes
    dplyr::transmute(
      link_id = dplyr::row_number(), 
      aadt = ifelse(AADT == 0, NA, AADT),
      # the "oneway" field can take three values
      oneway = case_when(
        Oneway == "B" ~ 0,  # link goes in both directions
        Oneway == "TF" ~ 1, # link goes in drawn order
        Oneway == "" ~ 2    # link goes against drawn order!
      ),
      length = Length_Miles,
      speed = Speed, 
      ftype = gsub(".*?([0-9]+).*", "\\1", CartoCode),
      fdesc = case_when(
        ftype %in% c(1, 2, 4) ~ "Freeway",
        ftype %in% c(3, 5) ~ "Principal Arterial",
        ftype %in% c(7) ~ "Ramp",
        ftype %in% c(8) ~ "Arterial",
        ftype %in% c(10) ~ "Collector",
        ftype %in% c(11) ~ "Local",
        TRUE ~ as.character(NA)
      )
    ) 
    
  
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
    dplyr::rename(a = id)
  
  # and do the same for the last n() point of each feature
  end_nodes <- link_points %>% 
    dplyr::group_by(link_id) %>% dplyr::slice(dplyr::n()) %>%
    sf::st_join(nodes, join = sf::st_nearest_feature) %>%
    dplyr::rename(b = id)
  
  # put the node id's onto the links dataset
  mylinks <- links %>%
    dplyr::left_join(start_nodes %>% sf::st_set_geometry(NULL), by = "link_id") %>%
    dplyr::left_join(end_nodes   %>% sf::st_set_geometry(NULL), by = "link_id") %>%
    # If the link goes against the drawn order, replace a and b
    mutate(
      new_a = ifelse(oneway == 2, b, a),
      new_b = ifelse(oneway == 2, a, b),
      a = new_a,
      b = new_b
    ) %>%
    select(-new_a, -new_b)
 
  # remove any nodes that are not part of link endpoints
  mynodes <- nodes %>% 
    dplyr::filter(id %in% mylinks$a | id %in% mylinks$b)
  
 
  # Return list of links and nodes ============
  list(
    links = mylinks,
    nodes = mynodes
  )
  
}


#' Get the TDM segments and related information
#' 
#' @param summaries_file Path to the TDM model summaries file
#' @param nodes_file Path to the TDM model summaries nodes file
#' @param bb A bounding box polygon
#' 
#' @return A list with two sf objects: links and nodes
get_segments <- function(summaries_file, nodes_file, bb){
  
  
  # get nodes with xy coords from travel demand model network file
  tdm_nodes <- foreign::read.dbf(nodes_file) %>%
    st_as_sf(coords = c("X", "Y"), crs = 3719) %>%
    st_filter(st_transform(bb, 3719)) %>%
    select(N)
  
  # get link information from travel demand model network file
  tdm_links <- foreign::read.dbf(summaries_file) %>%
    as_tibble() %>%
    filter(STREET != "Cent") %>%
    filter(A %in% tdm_nodes$N & B %in% tdm_nodes$N) %>%
    transmute(
      LINKID, A, B, 
      LANES,
      FT, FTCLASS,
      CAPACITY = CAP1HR1LN * LANES
    ) 
  
  # make a linestring geometry for each link.
  tdm_linestrings <- tdm_links %>%
    select(LINKID, A, B) %>%
    pivot_longer(A:B, names_to = "end", values_to = "N") %>%
    left_join(tdm_nodes, by = "N") %>%
    st_set_geometry("geometry")  %>%
    group_by(LINKID) %>%
    summarise() %>%
    st_cast("MULTILINESTRING")
  
  
  tdm_links_sf <- tdm_links %>%
    left_join(tdm_linestrings, by = "LINKID") %>%
    st_set_geometry("geometry") 
  
  
  list(
    "links" = tdm_links_sf,
    "nodes" = tdm_nodes
  )
}


#' Join segment data to network
#' 
#' @param linknodes A list of `links` and `nodes` sf objects with roadway links
#' @param segment_data A list of `links` and `nodes` sf objects with travel demand
#'   model network information
#' @param link_types A character vector describing values of `linknodes$links$fdesc` 
#'  that should be kept
#' @param segment_types A character vector describing values of `segment_data$links$FTCLASS`
#'  that should be kept
#' @param buffer A distance (in meters) to buffer the links and segments when 
#'
#' @details Filters links from the streets network and the TDM segments to 
#'   similar facility types. Buffers the 
#' 
#' @return link nodes with added columns
#' 
join_segments <- function(linknodes, segment_data, link_types, segment_types, buffer = 60){
  
  # make a buffer around the links in the specified link_types
  links <-  linknodes$links %>%
    st_transform(st_crs(segment_data$links)) %>%
    filter(fdesc %in% link_types)  %>%
    st_buffer(buffer)
    
  # keep segments in the TDM network from the specified segment_types
  segments <- segment_data$links %>%
    filter(FTCLASS %in% segment_types)
  
  # find nodes in the TDM network that lie inside the buffers.
  segment_nodes <- segment_data$nodes %>%
    filter(N %in% segments$A | N %in% segments$B) 
  
  # filter the segments down to the matched links, and buffer them.
  segment_buffer <- segments %>%
    filter(A %in% segment_nodes$N & B %in% segment_nodes$N)  %>%
    st_buffer(buffer) 
   
  # calculate intersection between two buffers
  intersecting_areas <- st_intersection(
    links, 
    segment_buffer %>% 
      # fields from TDM segments to keep
      select(LINKID, LANES, CAPACITY)) %>%
    
    # calculate area of intersection, and pick the single intersection with the 
    # most overlap 
    mutate(intersect_area = st_area(.)) %>%
    group_by(link_id)  %>%
    arrange(-intersect_area, .by_group = TRUE) %>%
    slice(1)
  
  
  intersecting_areas %>%
    st_set_geometry(NULL)
    
}
  

#' Join all segments to network
#' 
#' @param linknodes A list of `links` and `nodes` sf objects with roadway links
#' @param segment_data A list of `links` and `nodes` sf objects with travel demand
#'   model network information
#' 
#' 
#' 
join_all_segments <- function(){
  
  
  
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



