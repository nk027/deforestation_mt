
# Municipio borders from IBGE ---------------------------------------------

path <- "data/municipios/"
link <- paste0("ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/",
               c(paste0("municipio_20", 18:15, "/Brasil/BR/br_municipios.zip"),
               paste0("municipio_20", c(14, 13, 10), "/mt/mt_municipios.zip"),
               paste0("municipio_2007/escala_2500mil/proj_geografica_sirgas2000/brasil/55mu2500gsr.zip"),
               "municipio_2005/escala_2500mil/proj_geografica/arcview_shp/brasil/55mu2500gc.zip",
               "municipio_2001/mt/51mu2500g.zip",
               "municipio_2000/mt/mt_municipios.zip"))
years <- c(2018:2013, 2010, 2007, 2005, 2001, 2000)
files <- paste0("shp", years, ".zip")


for(i in seq_along(link)) {
  if(!file.exists(paste0(path, files[i])))
    download.file(paste0(link[i]), destfile = paste0(path, files[i]))
}
for(i in seq_along(files)) {
  unzip(paste0(path, files[i]),
        exdir = paste0(path, years[i]),
        unzip = "/usr/bin/unzip")
}
