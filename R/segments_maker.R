library(sf)
library(tidyverse)
library(foreign)

#read the WFRC shapefile and convert to geojson to stash the outputs
#in the repo 
segs_with_factors <- st_read("/Users/gregmacfarlane/Box/Macfarlane/research/strategic_transit/Utah Network/WFRC Network Stuff/TravelDemandModel/TDM_master_segments/Master_Segs_withFactors_20200504.shp")
st_write(segs_with_factors, "data/segs_with_factors.geojson", )

#read the output summary from the TDM
segment_summaries <- read.dbf("/Users/gregmacfarlane/Box/Macfarlane/research/strategic_transit/Utah Network/WFRC Network Stuff/TravelDemandModel/TDM_segment_summaries/v831_SE19_Net19_Summary_SEGID_Detailed.dbf")

segment_summaries %>% 
  as_tibble()  %>%
  write_rds("data/segment_summaries.rds")
