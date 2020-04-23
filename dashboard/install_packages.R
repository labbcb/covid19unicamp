pkgs <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "shiny",
  "shinydashboard",
  "devtools"
)

install.packages(pkgs, repos = "https://cran.rstudio.com")
devtools::install_github("Freguglia/datacovidbr")