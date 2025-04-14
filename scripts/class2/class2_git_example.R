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


# Load wrangled data # ---------------------------------------------------------

ess <- read_dta(here('data', 'ess', 'ESS 2013.dta'))


# Prepare the poltrst vrbl from ESS (KIS example) # ----------------------------

# No
maximum_trust_party <- max(x = ess[["trstprt"]], na.rm = TRUE)
minimum_trust_party <- min(x = ess[["trstprt"]], na.rm = TRUE)
ess[["trst_prty"]] <- 
  (maximum_trust_party + minimum_trust_party) - ess[["trstprt"]]

maximum_trust_parliament <- max(x = ess[["trstprl"]], na.rm = TRUE)
minimum_trust_parliament <- min(x = ess[["trstprl"]], na.rm = TRUE)
ess[["trst_parl"]] <- 
  (maximum_trust_parliament + minimum_trust_parliament) - ess[["trstprl"]]

ess[["poltrst"]] <- ess[["trst_prty"]] + ess[["trst_parl"]]


# Yes (base)
ess$trst_prty <- max(ess$trstprt, na.rm = TRUE) + 
  min(ess$trstprt, na.rm = TRUE) - 
  ess$trstprt

ess$trst_parl <- max(ess$trstprl, na.rm = TRUE) + 
  min(ess$trstprl, na.rm = TRUE) - 
  ess$trstprl

ess$poltrst <- ess$trst_prty + ess$trst_parl


# Yes (dplyr)
ess %<>%
  mutate(
    trst_prty = max(trstprt,na.rm=T) + min(trstprt,na.rm=T) - trstprt,
    trst_parl = max(trstprl,na.rm=T) + min(trstprl,na.rm=T) - trstprl,
    poltrst = trst_prty + trst_parl
  )


# Yes, but better (dplyr)
ess %<>%
  mutate(
    # Flip the scales
    trst_prty = max(trstprt,na.rm=T) + min(trstprt,na.rm=T) - trstprt,
    trst_parl = max(trstprl,na.rm=T) + min(trstprl,na.rm=T) - trstprl,
    # Create aggregate measur
    poltrst = trst_prty + trst_parl
  )


# Prepare the cdx rad vrbl from ESS # ------------------------------------------

ess %<>%
  mutate(
    cdx_rad = case_when(
      prtvtbit %in% c(9, 10, 13) ~ 2,         
      prtvtbit %in% 1:6 ~ 0,                  
      prtvtbit %in% 8  ~ 1,                 
      prtvtbit == 12 ~ 0,                   
      prtvtbit == 14 ~ NA_real_,             
      TRUE ~ as.numeric(prtvtbit)
    )
  )

# wrangle and select ess data for reg  # ------------------------------------------------

ess_reg <-
  ess %>%
  mutate(year = 2013) %>%
  dplyr::select(
    year,
    age,
    sex,
    edu,
    job,
    polint,
    lr4,
    geo,
    prt_close,
    religion,
    immi,
    immi_z,
    immi_2z,
    trst_prty,
    trst_parl,
    poltrst,
    cdx,
    cdx_rad
  ) %>%
  mutate(cdx_rad_past = NA)

# Mean visualization # ---------------------------------------------------------

ess_reg %>% 
  ggplot(aes(x=immi)) +
  geom_density(fill='orange2') +
  theme_classic()


