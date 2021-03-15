
arcGisJoins <- function(){
  #how to create a box download: https://www.joelgrayson.com/box-download-link-generator
  basic <- "https://app.box.com/index.php?rm=box_download_shared_file&shared_name=o8jgfhglivfc7unhvnvb91lcg17nl6xx&file_id=f_787368551269"
  freeway <- "https://app.box.com/index.php?rm=box_download_shared_file&shared_name=r478xknaemxecb7g4tqdgo3uhoubc2et&file_id=f_785273290555"
  ramp <- "https://app.box.com/index.php?rm=box_download_shared_file&shared_name=rgp4ano9rwz2i0dyv6rfaxoqsou4sg3d&file_id=f_785276833664"
  
  unzip(basic)
  unzip(freeway)
  unzip(ramp)
}

unzip <- function(file){
  download.file(file,destfile = "data/join.zip", mode = "wb")
  utils::unzip("data/join.zip", exdir = "data/ArcGisJoins")
  file.remove("data/join.zip")
}