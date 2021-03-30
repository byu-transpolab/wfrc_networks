
arcGisJoins <- function(){
  #how to create a box download: https://www.joelgrayson.com/box-download-link-generator
  basic <- "https://app.box.com/index.php?rm=box_download_shared_file&shared_name=o8jgfhglivfc7unhvnvb91lcg17nl6xx&file_id=f_787368551269"
  freeway <- "https://app.box.com/index.php?rm=box_download_shared_file&shared_name=r478xknaemxecb7g4tqdgo3uhoubc2et&file_id=f_785273290555"
  ramp <- "https://app.box.com/index.php?rm=box_download_shared_file&shared_name=rgp4ano9rwz2i0dyv6rfaxoqsou4sg3d&file_id=f_785276833664"
  basic_agrc <- "https://app.box.com/index.php?rm=box_download_shared_file&shared_name=c5rvkxlt47ngl6ei72i6kmw368nuymin&file_id=f_793086644944"
  freeway_agrc <- "https://app.box.com/index.php?rm=box_download_shared_file&shared_name=uc9x8782t3f062yqg78ay6jbxgidzq6o&file_id=f_793087592273"
  ramp_agrc <- "https://app.box.com/index.php?rm=box_download_shared_file&shared_name=lo5dzzccq89ait7a8x9q09s63qhzisqj&file_id=f_793085996098"
  
  unzip(basic)
  unzip(freeway)
  unzip(ramp)
  unzip(basic_agrc)
  unzip(freeway_agrc)
  unzip(ramp_agrc)
}

unzip <- function(file){
  download.file(file,destfile = "data/join.zip", mode = "wb")
  utils::unzip("data/join.zip", exdir = "data/ArcGisJoins")
  file.remove("data/join.zip")
}