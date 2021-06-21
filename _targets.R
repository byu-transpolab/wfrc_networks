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

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "sf"))

# End this file with a list of target objects.
list(
  tar_target(gdb, download_gdb("data/UtahRoadsNetworkAnalysis.gdb"), 
             format = "file"),
  tar_target(bb,  st_read("data/payson_bb.geojson")),
  tar_target(linknodes, extract_roads(bb, gdb)),
  
  
  tar_target(segment_data, get_segments("data/v831_SE19_Net19__Summary.dbf", 
                                        "data/segs_with_factors.geojson", 
                                        bb)),
  
  #tar_target(attributed_net)
  
  message("Done")
)
