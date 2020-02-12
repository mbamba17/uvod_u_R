if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
library(pacman)

pacman::p_load(reshape2, shiny, visNetwork, DT)