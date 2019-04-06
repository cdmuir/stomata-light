rm(list = setdiff(ls(), "run"))
graphics.off()

# remove before final publication
file.copy(
  "/Volumes/GoogleDrive/My Drive/resources/bibliography/refs.bib",
  "/Volumes/GoogleDrive/My Drive/research/01_active/stomata-light/ms/refs.bib", 
  overwrite = TRUE)

file.exists("/Volumes/GoogleDrive/My Drive/professional/resources/bibliography/refs.bib")
message("delete PAT and make leafoptimizer public before putting on GitHub")

# Libraries
library(cowplot)
library(distr)
library(magrittr)
library(plyr)
library(tidyverse)
library(units)

source("r/functions.R")

palette(colorRampPalette(c("tomato", "steelblue"), alpha = TRUE)(5))
