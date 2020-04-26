pkgs <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "highcharter",
  "shiny",
  "flexdashboard",
  "devtools",
  "brazilmaps",
  "sf",
  "leaflet",
  "geobr",
  "stringr"
)

install.packages(pkgs, repos = "https://cran.rstudio.com")
devtools::install_github("Freguglia/datacovidbr")