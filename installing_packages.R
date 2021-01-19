install.packages(
  c("dplyr", "lubridate", "tidyverse", "reshape2", "ggplot2", "shiny",
    "shinyBS", "shinyWidgets", "shinythemes", "plotly", "shinycssloaders",
    "rgdal", "leaflet", "leaflet.extras", "tigris", "spdplyr",
    "ggdendro", "scales", "shinyalert", "shinybusy", "RcppRoll",
    "zoo", "readxl", "Jmisc", "cowplot", "survival", "reticulate", "devtools", "git2r"
  ),
  repos = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"
)

devtools::install_github('Displayr/flipTime')
devtools::install_github('hamilton-institute/hamiltonThemes')
