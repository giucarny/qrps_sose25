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

# Foo # ------------------------------------------------------------------------
# Wrapper as.numeric 
asn <- function(x) {as.numeric(x)}

# for checking labels
see_labels <- 
  function(cols, re = T, data=itanes) {
    
    
    if (re) {
      coltosel <- names(data)[grepl(paste0(cols, collapse = '|'), names(data))]
    } else {
      coltosel <- names(data)[names(data) %in% cols]  
    }
    
    data[,c(coltosel)] %>% sapply(.,var_label) # 2006
    
  }

# Load wrangled data # ---------------------------------------------------------

itanes <- read_dta(here('data', 'merge box', 'Merge 2.0.dta'))

ess <- read_dta(here('data', 'merge box', 'ESS2013 Addendum', 'ESS 2013.dta'))


# itanes2001 <- itanes %>% filter(year==2001)
# itanes2001[,colSums(is.na(itanes2001))<nrow(itanes2001)] 
# colnames(itanes)[grepl('^e',colnames(itanes))]

# Rescaled variables for immi # -----------------------------------------------

sel_cols_i <- grepl('immi_cult|immi_eco|immieco|immicult|immi_troppi|immi_trust', names(itanes))

names(itanes)[sel_cols_i]  <- 
  names(itanes)[sel_cols_i] %>% 
  gsub('_', '',.) %>% 
  gsub('immieco','immie',.) %>% 
  gsub('immicult','immic',.)  