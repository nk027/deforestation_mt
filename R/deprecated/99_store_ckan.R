
# devtools::install_github("fineprint-global/fineprintutils", 
#                          auth_token = <TOKEN>)
library("fineprintutils")

ck_setup()

files_tab <- paste0("~/land_use/data/sidra/", 
                    list.files("~/land_use/data/sidra/", "[.]ods"))
files_sat <- paste0("~/land_use/data/landsat/",
                       list.files("~/land_use/data/landsat/", "[.]tif"))
files_mun <- paste0("~/land_use/data/municipios/",
                    list.files("~/land_use/data/municipios/", "[.]zip"))
files_cli <- paste0("~/land_use/data/spei/",
                    list.files("~/land_use/data/spei", "[.]nc"))
files_data <- "~/land_use/data/data.rds"

files <- c(files_tab, files_sat, files_mun, files_cli, files_data)
name <- "defor_sp"
title <- "Investigating Spatial Dependence in Deforestation"
author <- "Nikolas Kuschnig"
descr <- "Data used to create the analysis in <https://github.com/fineprint-global/defor_sp>."
version <- "v1"
org <- "other"
res_names <- gsub(".*/(.*)$", "\\1", files)
res_descr <- c(
  rep("Tabular data from IBGE SIDRA", length(files_tab)),
  rep("Remotely sensed land-cover-change raster data from Camara et al. (2019)", length(files_sat)),
  rep("Shapefiles wth municipal and state borders of Mato Grosso / Brazil from IBGE", length(files_mun)),
  rep("Climatological raster data with the SPEI from Vicente-Serrano et al. (2012)", length(files_cli)),
  "Resulting RDS file with an sf-tibble of all data, including transformed variables"
)

ck_post_dataset(files, name, title, author, descr, version, org, 
                res_names = res_names, res_descr = res_descr)
