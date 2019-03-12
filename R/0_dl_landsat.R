# PANGEA file for 2016 is faulty - replace with the one provided by the authors


path <- "data/landsat/"
link <- "http://store.pangaea.de/Publications/Camara-etal_2018/MatoGrosso-LandCover-"
files <- c("2001-2005_v3.zip", "2006-2008_v3.zip", "2009-2011_v3.zip", 
           "2012-2014_v3.zip", "2015-2017_v3.zip")

for(i in seq_along(files)) {
  download.file(paste0(link, files[i]), 
                destfile = paste0(path, files[i]))
}
for(i in seq_along(files)) {
  unzip(paste0(path, files[i]),
        exdir = path,
        unzip = "/usr/bin/unzip")
}
