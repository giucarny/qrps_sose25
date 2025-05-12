# Title: Class 5 code - data preparation
# Author: Giuseppe Carteny
# Last update: 10.05.2025

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

# # Obs per country
# obs_x_country <- 
#   ees2019 %>% 
#   count(countrycode) %>% 
#   print(n=Inf) 

## 4.1 Basic wrangling # -------------------------------------------------------

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

# ees2019 %>% count(countrycode, countryname) %>% print(n=Inf)


# 4.2. Variables of interest: attitudes toward EU integration -----------------


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

# 4.3. Variables of interest: others # ----------------------------------------

# NOTE: you can add yours using the logic (and checks) used above for EU int
#       Here I already know the variables, so I just quickly wrangle them 

ees2019 <- 
  ees2019 %>% 
  mutate(
    age = 2019 - d4_1, # birth year
    gender = d3,
    urbrur = d8,
    edu_rec = as.numeric(d2_1),
    edu_rec = case_when(
      edu_rec == 97 ~ age - 6, 
      edu_rec %in% c(4, 99) ~ NA_real_, 
      T ~ edu_rec
    ),
    edu_rec = case_when(
      edu_rec <= 15 | edu==1 ~ 1,
      edu_rec > 15 &  edu_rec <= 19 | edu==2 ~ 2,
      edu_rec > 19  | edu==3 ~ 3,
      T ~ edu_rec
    ),
      soc_class     = case_when(
        as.numeric(d7) <= 2 ~ 1,
        as.numeric(d7) == 3 ~ 2,
        as.numeric(d7) %in% c(4, 5) ~ 3,
        as.numeric(d7) >  5 ~ NA_real_
      ),
    marital = case_when(d5>14 ~ NA_real_,
                        d5>=1 & d5<=8 ~ 1, 
                        d5>=9 & d5<=14 ~ 0),
    self_lr = ifelse(q11>10,NA,q11),
    regulation = ifelse(q14_1>10,NA,q14_1),
    redistr = ifelse(q14_2>10,NA,q14_2),
    ss_marriage = ifelse(q14_3>10,NA,q14_3),
    civ_lib = ifelse(q14_4>10,NA,q14_4),
    immigr = ifelse(q14_5>10,NA,abs(as.numeric(q14_5)-10)),
    envir = ifelse(q14_6>10,NA,q14_6),
    satwithdem = case_when(as.numeric(q3) >  4 ~ NA_real_,
                           as.numeric(q3) <= 4 ~ abs(as.numeric(q3)-5),
                           T ~ as.numeric(q3))
  )




# 4.5 shorter dataframe? # ----------------------------------------------------

vrbls2keep <- 
  c(# Identifiers
    'respid', 'countryname', 
    # Sociodemograpic variables
    'age','gender', 'urbrur', 'marital', 'edu_rec', 'soc_class', 
    # Variables of interest
    'self_lr','eu_int', 'regulation', 'redistr', 
    'ss_marriage', 'civ_lib', 'immigr', 'envir', 
    'satwithdem'
  )

ees2019_short <- ees2019 %>% dplyr::select(all_of(vrbls2keep)) 

ees2019_short[,c('immigr', 'self_lr')] %>% na.omit %>% ggplot(aes(x=self_lr, y=immigr))+ geom_jitter(alpha=.1) + geom_smooth()
ees2019_short[,c('redistr', 'self_lr')] %>% na.omit %>% ggplot(aes(x=self_lr, y=redistr))+ geom_jitter(alpha=.1) + geom_smooth()

                                                              