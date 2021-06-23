library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/build_linknode_tables.R")
source("R/join_segments_data.R")


bounding_box <- "data/payson_bb.geojson"
output_folder <- "data/payson"

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf"))

# End this file with a list of target objects.
list(
  tar_target(gdb, download_gdb("data/MM_NetworkDataset_06032021.gdb"), 
             format = "file"),
  tar_target(bb,  st_read(bounding_box)),
  tar_target(linknodes, extract_roads(bb, gdb)),
  
  tar_target(segment_data, 
             get_segments("data/v831_SE19_Net19__Summary.dbf", 
                          "data/v831_SE19_Net19__Summary_nodes.dbf",
                          bb)),
  
  tar_target(linknodes_attr, join_all_segments(linknodes, segment_data)),
  tar_target(write, write_linknodes(linknodes_attr, output_folder), 
             format = "file"),
  
  message("Done")
)
