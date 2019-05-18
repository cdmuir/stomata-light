# Install CRAN packages
install.packages("distr")
install.packages("tidyverse")

# Install remotes for GitHub packages
install.packages("remotes")

# Install archived versions of ecophys packages
remotes::install_github("cdmuir/gunit", ref = "v1.0.1")
remotes::install_github("cdmuir/tealeaves", ref = "v1.0.1")
remotes::install_github("cdmuir/photosynthesis", ref = "v1.0.1")
remotes::install_github("cdmuir/leafoptimizer", ref = "v0.0.2")
