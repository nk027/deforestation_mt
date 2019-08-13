
# SPEI from http://spei.csic.es/ ------------------------------------------

path <- "data/spei/"
timescales <- c("03", "06", "12", "24", "36") # 1:48 are available
link <- paste0("http://soton.eead.csic.es/spei/10/nc/spei", timescales, ".nc")
files <- paste0("spei", timescales, ".nc")

for(i in seq_along(link)) {
  if(!file.exists(paste0(path, files[i]))){
    download.file(link[i], destfile = paste0(path, files[i]))
  }
}
