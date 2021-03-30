#' Join the segment information to the links
#' 
#' @param net A network table of links and nodes
#' @param segs_with_factors A geographic layer with segment ID's joinable to the
#'   TDM outputs.
#' @param segment_summaries TDM outputs
#' 
#' 
join_segments_data <- function(net, segs_with_factors, segment_summaries){
  
  # limit the segments to the same region as the network information
  segments <- segs_with_factors %>%
    st_filter(st_convex_hull(st_union(net$nodes)))
    
  
  # A basic spatial join isn't going to work very well for two reasons:
  #    - First, the segments data inexplicably only has the northbound half of I-15.
  #    - Second, some shapes might be close enough to intersect or even be parallel
  
  
  
}