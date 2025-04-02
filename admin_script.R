# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Title: Admin script 
# Author: G.Carteny  
# Last update: 2023.06.23
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

want <- c("here")

have <- want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
rm(have,want,junk)

options(scipen = 99)
rm(list = ls())
# Create folders if they don't exist

folders <- c('data','docs','scripts','output')

mainfolder <- list.files()

junk <- lapply(folders, function(fldr) if(!(fldr %in% mainfolder)) { dir.create(here(fldr))})

rm(list=ls())