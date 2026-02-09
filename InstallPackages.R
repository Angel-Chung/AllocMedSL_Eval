# Install packages
install.packages(c(
  "dplyr",
  "lubridate",
  "data.table",
  "ggplot2",
  "plotly",
  "tidyverse"
))

# synthdid must be installed from GitHub
install.packages("devtools")
devtools::install_github("synth-inference/synthdid")
