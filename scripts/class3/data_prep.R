# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 - Data
# Author: G.Carteny
# last update: 2025-04-28
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


admin = T

if (admin) {
  want = c(
    "tidyverse",
    "gridExtra",
    "magrittr",
    "haven",
    "data.table",
    "labelled",
    "here",
    "rio",
    "gtools",
    "DescTools",
    "countrycode"
  )
  have = want %in% rownames(installed.packages())
  if (any(!have)) {
    install.packages(want[!have])
  }
  junk <- lapply(want, library, character.only = TRUE)
  rm(have, want, junk)
  options(scipen = 99)
  rm(list = ls())
}


# Load the data # ==============================================================

ees2019 <- haven::read_sav(here('data', 'ees', '2019', 'ZA7581_v1-0-0_full.sav'),
                           encoding = 'latin1')

# wrangle gender/sex # =========================================================

ees2019 %<>%
  mutate(sex = as.numeric(D3),
         sex = case_when(D3_rec == 3 ~ NA_real_, T ~ D3_rec - 1))

# Mutate attitude toward EU integration # ======================================

EES2019 %<>% mutate(eu_int = case_when(as.numeric(Q23) > 10 ~ NA_real_, T ~ as.numeric(Q23)))
