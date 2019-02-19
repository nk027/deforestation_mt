
# To-do: Check changes of municipios over time

path <- "data/municipios/"
link <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2017/Brasil/BR/"
files <- c("br_municipios.zip")

for(i in seq_along(files)) {
  download.file(paste0(link, files[i]), 
                destfile = paste0(path, files[i]))
}
for(i in seq_along(files)) {
  unzip(paste0(path, files[i]),
        exdir = path,
        unzip = "/usr/bin/unzip")
}
