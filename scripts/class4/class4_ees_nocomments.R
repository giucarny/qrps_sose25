# Title: Class 4 code (almost without any comment)
# Author: Giuseppe Carteny
# Last update: 05.05.2025

# When going through the code check the supplemental information marks here and there
# And then check the `~/scripts/class4/class4_ees_supplinfo.R` script

# It would be very useful if you take a look at the third class code, namely 
# `~/scripts/class3/class3_code.R`

# 1. Admin # -------------------------------------------------------------------

## 1.1 Packages # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

want <- c('dplyr', 'magrittr', 'here', 'labelled', 'ggplot2')
have <- want %in% installed.packages()[,1]            # <- - - - - - - - - - - # Check supplemental info script*, A.1
if (any(!have)) { install.packages(want[!have]) }     # <- - - - - - - - - - - # Check supplemental info script, A.2        
for(pckg in want) {library(pckg, character.only = T)} # <- - - - - - - - - - - # Check supplemental info script, A.3        

# * class4_ees_supplinfo.R

## 1.2 Clean the environment # - - - - - - - - - - - - - - - - - - - - - - - - -
rm(list=ls())

# 2. Functions  # --------------------------------------------------------------

asn <- function(x) {as.numeric(x)} 
se <- function(gino) {sd(gino, na.rm = F)/sqrt(length(gino[!is.na(gino)]))}

# 3. Load the data # -----------------------------------------------------------

## 3.1 File path # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

file_dir <- here('data', 'ees', '2019', 'ZA7581_v1-0-0_full.sav')

## 3.2 Load the file # - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ees2019 <- 
  haven::read_sav(
    file = file_dir, 
    encoding = 'latin1' # this is just to prperly read some some string variables in the dataset that use non-latin characters
  ) 


## 3.3 Inspect the data # - - - - - - - - - - - - - - - - - - - - - - - -

obs_n <- nrow(ees2019) # the number of observations
vrbls_n <- ncol(ees2019) # the number of variables

# Obs per country
obs_x_country <- 
  ees2019 %>% 
  count(countrycode) %>% 
  print(n=Inf) 

# 4 Wrangle the data # -----------------------------------------------------------

## 4.1 Basic wrangling # - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### 4.1.1 First thing: change all the variable names to lower letters. 

names(ees2019)[1:20] # first 20 names
names(ees2019) <- tolower(names(ees2019))
names(ees2019)[1:20] # first 20 names

### 4.1.2 Then, we check the variables that identify our observations
ees2019 %>% 
  dplyr::select(
    respid,          # code for each unique responder
    countrycode,     # code for each country
    meta_start_date, # identifier of the start date of the interview
    meta_end_date    # identifier of the end date of the interview
  )

### 4.1.3 Add country names to the dataset 

country_df <-
  data.frame(
    countrycode = ees2019$countrycode %>% val_labels(.) %>% as.numeric(.),
    countryname = ees2019$countrycode %>% val_labels(.) %>% attr(., 'names')
  )

ees2019 <- merge(ees2019, country_df, by.x = "countrycode", by.y = "countrycode") %>% as_tibble

ees2019 %>% count(countrycode, countryname) %>% print(n=Inf)


# 4.3. Variables of interest: attitudes toward EU integration 

ees2019$eu_int <- ees2019$q23

ees2019 %>% 
  ggplot(aes(x=eu_int)) +  
  geom_bar()

ees2019 %>% count(eu_int)
# weird

ees2019 <- 
  ees2019 %>% 
  mutate(eu_int = ifelse(eu_int > 10, NA, eu_int))

ees2019 %>% count(q23, eu_int)

ees2019 %>% 
  ggplot(aes(x=eu_int)) +  
  geom_bar()
# Ok, it looks fine 

# 4.4. Variables of interest: others # - - - - - - - - - - - - - - - - - - - - - -

# NOTE: you can add yours using the logic (and checks) used above for EU int
#       Here I already know the variables, so I just quickly wrangle them 

ees2019 <- 
  ees2019 %>% 
  mutate(
    age = 2019 - d4_1, # birth year
    gender = d3,
    urbrur = d8,
    marital = case_when(d5>14 ~ NA_real_,
                        d5>=1 & d5<=8 ~ 1, 
                        d5>=9 & d5<=14 ~ 0),
    self_lr = ifelse(q11>10,NA,q11),
    regulation = ifelse(q14_1>10,NA,q14_1),
    redistr = ifelse(q14_2>10,NA,q14_2),
    ss_marriage = ifelse(q14_3>10,NA,q14_3),
    civ_lib = ifelse(q14_4>10,NA,q14_4),
    immigr = ifelse(q14_5>10,NA,q14_5),
    envir = ifelse(q14_6>10,NA,q14_6),
  )


# 4.5 shorter dataframe? # - - - - - - - - - - - - - - - - - - - - - - - - - - - -

vrbls2keep <- 
  c(# Identifiers
    'respid', 'countryname', 
    # Sociodemograpic variables
    'age','gender', 'urbrur', 'marital',
    # Variables of interest
    'self_lr','eu_int', 'regulation', 'redistr', 
    'ss_marriage', 'civ_lib', 'immigr', 'envir'
  )

ees2019_short <- ees2019 %>% dplyr::select(all_of(vrbls2keep)) 
ees2019_short

# 5. Analyses # -------------------------------------------------------------------

## 5.1 Univariate analyses: EU integration # - - - - - - - - - - - - - - - - - -
### 5.1.1 prepare your data for the analyses #

# Now, we can get all the needed information. 
# The data.frame you need depends on what kind of information you want to include 
# in your analyses. 

# You are interested in preserving the original variable distribution? 
# Then go for mutate: 
eu_int_df <- 
  ees2019_short %>% 
  dplyr::select(eu_int) %>% # We select this variable to make the dataset tidier
  mutate(
    eu_int_mean = mean(eu_int, na.rm=T), 
    eu_int_sd = sd(eu_int, na.rm=T),
    eu_int_n = length(eu_int[!is.na(eu_int)]),
    eu_int_se = eu_int_sd/sqrt(eu_int_n),
    eu_int_68dist_upper = eu_int_mean+(1*eu_int_sd),
    eu_int_68dist_lower = eu_int_mean-(1*eu_int_sd),
    eu_int_mean_95ci_upper = eu_int_mean+(1.96*eu_int_se),
    eu_int_mean_95ci_lower = eu_int_mean-(1.96*eu_int_se),
  ) 


# Plot the distribution and some summary statistics 
eu_int_df %>% 
  ggplot(aes(x=eu_int)) +
  geom_bar(alpha=.5) +
  geom_vline(aes(xintercept=eu_int_mean), colour='red4') +
  geom_vline(aes(xintercept=eu_int_68dist_upper), colour='blue4') +
  geom_vline(aes(xintercept=eu_int_68dist_lower), colour='blue4') + 
  geom_vline(aes(xintercept=eu_int_mean_95ci_upper), colour='indianred4') +
  geom_vline(aes(xintercept=eu_int_mean_95ci_lower), colour='indianred4') 

# The standard error is SUPER SMALL!            # <- - - - - - - # Check supplemental info script, A.3, and class3 code
# 
# eu_int_df %>% 
#   ggplot(aes(x=eu_int)) +
#   geom_vline(aes(xintercept=eu_int_mean), colour='red4') +
#   geom_vline(aes(xintercept=eu_int_68dist_upper), colour='blue4') +
#   geom_vline(aes(xintercept=eu_int_68dist_lower), colour='blue4') + 
#   geom_vline(aes(xintercept=eu_int_mean_95ci_upper), colour='indianred4') +
#   geom_vline(aes(xintercept=eu_int_mean_95ci_lower), colour='indianred4') +
#   coord_cartesian(xlim=c(5.3,5.45))






