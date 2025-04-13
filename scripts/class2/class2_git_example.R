# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Analyses, most updated
# Authors: G.Carteny
# last update: 2025-03-25
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Admin 

admin <- T
if(admin) {
  want = c("dplyr", "magrittr", "rio",  "ggplot2",
           "here", "haven", "labelled", "nnet",
           "ggridges", "ggthemes", "stargazer")
  have = want %in% rownames(installed.packages())
  if ( any(!have) ) { install.packages( want[!have] ) }
  # load packages
  junk <- lapply(want, library, character.only = TRUE)
  rm(have, want, junk)
  options(scipen = 99)
  rm(list = ls())
}
