download.file("http://api.worldbank.org/v2/en/indicator/PA.NUS.FCRF?downloadformat=csv",
              destfile = "data/other/fx_brlusd.zip")
unzip("data/other/fx_brlusd.zip", exdir = "data/other")
