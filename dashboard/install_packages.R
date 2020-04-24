pkgs <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "shiny",
  "flexdashboard",
  "devtools",
  "brazilmaps",
  "sf"
)

install.packages(pkgs, repos = "https://cran.rstudio.com")
devtools::install_github("Freguglia/datacovidbr")