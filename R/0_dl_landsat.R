
# Version 3 - https://doi.pangaea.de/10.1594/PANGAEA.899706 ---------------

path <- "data/landsat/"
link <- "http://store.pangaea.de/Publications/Camara-etal_2019/"
files <- paste0("mt_20", formatC(1:17, width = 2, flag = 0), "_v3_1.tif")

for(i in seq_along(files)) {
  download.file(paste0(link, files[i]),
                destfile = paste0(path, files[i]))
}

# Version 2 - https://doi.pangaea.de/10.1594/PANGAEA.895495 ---------------

# File for 2016 is faulty

path <- "data/landsat/v2"
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
