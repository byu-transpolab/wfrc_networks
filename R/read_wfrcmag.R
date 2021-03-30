#' Read dbf files from the WFRC / MAG TDM Network
#' 
#' @param node_file Path to a dbf node file exported from CUBE
#' @param link_file Path to a dbf link file exported from CUBE
#' 
#' @return A list with two objects:
#'   - `links` An object of class `sf` with the link shapes and line attributes
#'   - `nodes` An object of class `sf` with the node shapes
#' 
#' @importFrom foreign read.dbf 
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter transmute
#' 
#' 
#' 
read_wfrcmag <- function(node_file, link_file, crs = 32612){
  
  # Read links table and filter ===================
  links <- foreign::read.dbf(link_file) %>%
    tibble::as_tibble()
  
  my_links <- links %>%
    # filter out centroid connectors and roads with no lanes (under construction)
    dplyr::filter(FT != 1) %>%
    dplyr::filter(LANES > 0) %>%
    # select the columns we want
    dplyr::transmute(
      link_id = LINKID, 
      a = A, b = B,
      aadt = DY_VOL,
      length = DISTANCE,
      # speed in meters per second
      speed = FF_SPD * 0.44704,
      ftype = FT,
      lanes = LANES,
      capacity = CAP1HR1LN * LANES
    )
  
  # Read nodes table =====================
  nodes <- foreign::read.dbf(node_file) %>%
    tibble::as_tibble()
  
  # turn into sf
  my_nodes <- nodes %>%
    dplyr::select( n = N,  x = X, y = Y ) %>%
    sf::st_as_sf(coords = c('x', 'y'), crs = crs)
  
  # get centroids as separate file
  taz_ids <- unique(nodes$TAZID)
  
  centroids <- my_nodes %>%
    dplyr::filter(n %in% taz_ids)
  
  # Append node coords to links ====================
  link_geom <- bind_rows(
    my_links %>%
      transmute(link_id, end = 1, n = a) %>%
      left_join(my_nodes),
    my_links %>%
      transmute(link_id, end = 2, n = b) %>%
      left_join(my_nodes)
  )%>%
    select(-n) %>%
    st_as_sf(crs = crs) %>%
    group_by(link_id) %>%
    arrange(end, .by_group = TRUE) %>%
    select(-end) %>%
    summarise(do_union = FALSE) %>%
    st_cast("MULTILINESTRING")
    
    
  mylinks <- my_links %>%
    left_join(link_geom, by = "link_id") %>%
    st_as_sf()
  
  
  # Return list of links and nodes ============
  list(
    links = mylinks,
    nodes = my_nodes,
    centroids = centroids
  )
}