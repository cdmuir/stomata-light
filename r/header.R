rm(list = setdiff(ls(), "run"))
graphics.off()

if (file.exists("r/private.R")) source("r/private.R")

# Libraries
library(cowplot)
library(distr)
library(magrittr)
library(plyr)
library(tidyverse)
library(units)

source("r/functions.R")

palette(colorRampPalette(c("tomato", "steelblue"), alpha = TRUE)(5))
