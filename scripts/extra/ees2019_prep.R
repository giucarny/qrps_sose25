# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 - Data  
# Author: G.Carteny
# last update: 2023-02-03
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


admin = T

if (admin) {
  want = c("tidyverse", "gridExtra", "magrittr", "haven", "data.table", "labelled", "here", "rio",
           "gtools", "DescTools", "countrycode")
  have = want %in% rownames(installed.packages())
  if ( any(!have) ) { install.packages( want[!have] ) }
  junk <- lapply(want, library, character.only = TRUE)
  rm(have,want,junk)
  options(scipen = 99)
  rm(list = ls())
}


# Load the data # ======================================================================================

# EES2019 <- haven::read_dta(here('Data', 'EES2019', 'ZA7581_v1-0-0.dta'))

EES2019 <- haven::read_sav(here('data', 'ees', '2019', 'ZA7581_v1-0-0_full.sav'), encoding = 'latin1') 


# Create a 'countryname' variable # ====================================================================

EES2019 <-
  EES2019 %>% 
  left_join(.,
            data.frame(countrycode = EES2019$countrycode %>% val_labels() %>% as.numeric,
                       countryname = EES2019$countrycode %>% val_labels() %>% attr(.,'names')),
            by='countrycode')

# Create a 'countryshort' variable # ===================================================================

EES2019 %<>% 
  mutate(countryshort = region_NUTS1 %>% str_extract(pattern = '^.{0,2}')) %>% 
  mutate(countryshort = case_when(countrycode==1250 ~ 'FR',
                                  countrycode==1428 ~ 'LV',
                                  countrycode==1442 ~ 'LU',
                                  countrycode==1470 ~ 'MT',
                                  T ~ countryshort))

# Create a 'partysys' variable # ===================================================================

EES2019 %<>%
  mutate(
    partysys = case_when(
      meta_lang_be==1 ~ 'BE_fl',
      meta_lang_be==2 ~ 'BE_wa',
      T               ~ countryshort
    )
  )

# Create an 'age' variable # ===========================================================================

EES2019 %<>% mutate(D4_age = 2019 - D4_1) 

# Mutate gender/sex # =================================================================================

#                         gndr_lab = EES2019$D3 %>% val_labels %>% attr(., 'names'))

EES2019 %<>%
  mutate(D3_rec = as.numeric(D3),
         D3_rec = case_when(D3_rec==3 ~ NA_real_, T ~ D3_rec-1))

# Mutate urban rural # =================================================================================

# urbrur_summ <- data.frame(urbrur     = EES2019$D8 %>% val_labels %>% unique(),
#                          urbrur_lab = EES2019$D8 %>% val_labels %>% attr(., 'names'))

EES2019 %<>%
  mutate(D8_rec = as.numeric(D8),
         D8_rec = case_when(D8_rec==1 ~ 0,
                            D8_rec==2 | D8_rec==3 ~ 1, 
                            T ~ D8_rec))  

# Mutate religious denomination and invert religiosity # ===============================================

# D9_summ <- data.frame(D9 = EES2019$D9 %>% val_labels %>% unique(),
#                            D9_labs = EES2019$D9 %>% val_labels %>% attr(., 'names'))


EES2019 %<>% 
  mutate(D9_rec  = as.numeric(D9),
         D10_rec = as.numeric(D10)) %>%
  mutate(D9_rec = case_when(D9_rec==1 ~ 1, # 'Catholic', 
                            D9_rec==2 ~ 2, # 'Orthodox',  
                            D9_rec==3 ~ 3, # 'Protestant',  
                            D9_rec==4 ~ 4, # 'Other Christian',  
                            D9_rec>=5 & D9_rec<10 ~ 5, # 'Other', 
                            D9_rec==10 | D9_rec==11 ~ 0, # 'Non-believer',
                            D9_rec==12 ~ 5, # 'Other'
                            D9_rec>12  ~ NA_real_)) %>%
  mutate(D10_rec = case_when(D10_rec>8 ~ NA_real_, T~D10_rec),
         D10_rec = abs(D10_rec-8),
         D10_rec = case_when(D9_rec==0 & D10==as.numeric(96)  ~ 0,
                             T            ~ D10_rec),
         D10_dic_rec = case_when(D10_rec >= 5 ~ 1, is.na(D10_rec)~NA_real_, T ~ 0),
         D10_dic_rec = as.factor(D10_dic_rec))  


# Mutate marital status # ==============================================================================

# mrtlst_summ <- data.frame(mrtlst     = EES2019$D5 %>% val_labels %>% unique(),
#                           mrtlst_lab = EES2019$D5 %>% val_labels %>% attr(., 'names'))

EES2019 %<>%
  mutate(D5_rec = as.numeric(D5),
         D5_rec = case_when(D5_rec>14 ~ NA_real_,
                            D5_rec>=1 & D5_rec<=8 ~ 1, 
                            D5_rec>=9 & D5_rec<=14 ~ 0))


# Mutate left-right self-placement # ===================================================================

EES2019 %<>%
  mutate(Q11_rec = case_when(as.numeric(Q11)>10 ~ NA_real_,
                             T ~ as.numeric(Q11)))


# # Check 
# EES2019 %>% 
#   dplyr::select(starts_with('Q11')) %>% 
#   print(., n=200)


# Create a 'leftist' variable # 

EES2019 %<>% 
  mutate(
    Q11_lef_rec = case_when(
      Q11_rec <  3 ~ 1, 
      Q11_rec >= 3 ~ 0,
      T             ~ NA_real_       
    ),
    Q11_ext_rec = case_when(
      is.na(Q11_rec) ~ NA_real_,
      Q11_rec <  3 | Q11_rec >  7 ~ 1,
      Q11_rec >= 3 & Q11_rec <= 7 ~ 0      
    )
  )
# 
# EES2019 %>%
#   dplyr::select(starts_with('Q11')) %>%
#   distinct %>%
#   arrange(Q11)

# Mutate attitude toward EU integration # ==============================================================

EES2019 %<>%
  mutate(Q23_rec = case_when(as.numeric(Q23)>10 ~ NA_real_,
                             T ~ as.numeric(Q23)))
# ,
#          Q23_rec = abs(Q23_rec-10))


# # Check
# EES2019 %>%
#   dplyr::select(starts_with('Q23')) %>%
#   print(., n=200)


EES2019 %<>% 
  mutate(
    Q23_pro_rec = case_when(
      Q23_rec <= 6 ~ 0, 
      Q23_rec >  6 ~ 1,
      T             ~ NA_real_       
    ),
    Q23_ext_rec = case_when(
      is.na(Q23_rec) ~ NA_real_,
      Q23_rec <  3 | Q23_rec >  7 ~ 1,
      Q23_rec >= 3 & Q23_rec <= 7 ~ 0      
    )
  )

# EES2019 %>%
#   dplyr::select(starts_with('Q23')) %>%
#   distinct %>%
#   arrange(Q23)

# Mutate attitude toward R's country EU membership  # ==================================================

EES2019 %<>%
  mutate(Q22_rec = case_when(as.numeric(Q22)   >    3  ~ NA_real_,
                             as.numeric(Q22) %in% c(1) ~ 2,
                             as.numeric(Q22) %in% c(2) ~ 0,
                             as.numeric(Q22) %in% c(3) ~ 1,
                             T                         ~ as.numeric(Q22))) 


# # Check
# EES2019 %>%
#   dplyr::select(starts_with('Q22')) %>%
#   print(., n=200)



# Mutate attitudes toward state regulation of the economy self-placement # =============================

EES2019 %<>%
  mutate(Q14_1_rec = case_when(as.numeric(Q14_1) >  10 ~ NA_real_,
                               as.numeric(Q14_1) <= 10 ~ abs(as.numeric(Q14_1)-10),
                               T ~ as.numeric(Q14_1)))

# # Check
# EES2019 %>%
#   dplyr::select(starts_with('Q14_1')) %>%
#   print(., n=200)


# Mutate attitudes toward redistribution # =============================================================

# red <- data.frame(red     = EES2019$Q14_2 %>% val_labels %>% unique(),
#                   red_lab = EES2019$Q14_2 %>% val_labels %>% attr(., 'names'))

EES2019 %<>%
  mutate(Q14_2_rec = case_when(as.numeric(Q14_2) >  10 ~ NA_real_,
                               # as.numeric(Q14_2) <= 10 ~ abs(as.numeric(Q14_2)-10),
                               T ~ as.numeric(Q14_2)))

# # Check
# EES2019 %>%
#   dplyr::select(starts_with('Q14_2')) %>%
#   print(., n=200)


# Mutate attitudes toward same-sex marriage # ==========================================================

# ssm <- data.frame(ssm     = EES2019$Q14_3 %>% val_labels %>% unique(),
#                   ssm_lab = EES2019$Q14_3 %>% val_labels %>% attr(., 'names'))

EES2019 %<>%
  mutate(Q14_3_rec = case_when(as.numeric(Q14_3) >  10 ~ NA_real_,
                               # as.numeric(Q14_3) <= 10 ~ abs(as.numeric(Q14_3)-10),
                               T ~ as.numeric(Q14_3)))

# # Check
# EES2019 %>%
#   dplyr::select(starts_with('Q14_3')) %>%
#   print(., n=200)



# Mutate attitudes toward civil liberties # ============================================================

EES2019 %<>%
  mutate(Q14_4_rec = case_when(as.numeric(Q14_4) >  10 ~ NA_real_,
                               as.numeric(Q14_4) <= 10 ~ abs(as.numeric(Q14_4)-10),
                               T ~ as.numeric(Q14_4)))

# # Check
# EES2019 %>%
#   dplyr::select(starts_with('Q14_4')) %>%
#   print(., n=200)


# Mutate attitudes toward immigration # ================================================================

EES2019 %<>%
  mutate(Q14_5_rec = case_when(as.numeric(Q14_5) >  10 ~ NA_real_,
                               as.numeric(Q14_5) <= 10 ~ abs(as.numeric(Q14_5)-10),
                               T ~ as.numeric(Q14_5)))

# # Check
# EES2019 %>%
#   dplyr::select(starts_with('Q14_5')) %>%
#   print(., n=200)


# Mutate attitudes toward environmental policies # =====================================================

EES2019 %<>%
  mutate(Q14_6_rec = case_when(as.numeric(Q14_6) >  10 ~ NA_real_,
                               as.numeric(Q14_6) <= 10 ~ abs(as.numeric(Q14_6)-10),
                               T ~ as.numeric(Q14_6)))

# # Check
# EES2019 %>%
#   dplyr::select(starts_with('Q14_6')) %>%
#   print(., n=200)




# Mutate satisfaction with democracy # =================================================================

EES2019 %<>%
  mutate(Q3_rec = case_when(as.numeric(Q3) >  4 ~ NA_real_,
                            as.numeric(Q3) <= 4 ~ abs(as.numeric(Q3)-5),
                            T ~ as.numeric(Q3)))

# # Check
# EES2019 %>%
#   dplyr::select(starts_with('Q3')) %>%
#   print(., n=200)

# Mutate government approval/disapproval # =============================================================

EES2019 %<>%
  mutate(Q5_rec = case_when(as.numeric(Q5) >  2 ~ NA_real_,
                            as.numeric(Q5) <= 2 ~ abs(as.numeric(Q5)-2),
                            T ~ as.numeric(Q5)))

# # Check
# EES2019 %>%
#   dplyr::select(starts_with('Q5')) %>%
#   print(., n=200)


# Mutate participation # ===============================================================================

EES2019 %<>%
  # dplyr::select(Q6) %>%
  mutate(
    Q6_rec = case_when(
      as.numeric(Q6) >  2 ~ NA_real_,
      as.numeric(Q6) == 2 ~ 0,
      as.numeric(Q6) == 1 ~ 1
    )
  )

# Mutate demovalues # ==================================================================================

EES2019 %<>%
  mutate(
    across(
      starts_with('Q17'),
      list(
        rec =~case_when(
          as.numeric(.)>5 ~ NA_real_,
          T               ~ abs(.-6)
        )
      )
    )
  )


# Mutate confidence measures # =========================================================================

# trust <- data.frame(np_trst      = EES2019$Q18_1 %>% val_labels %>% unique(),
#                     np_trst_labs = EES2019$Q18_1 %>% val_labels %>% attr(., 'names'),
#                     ep_trst      = EES2019$Q18_1 %>% val_labels %>% unique(),
#                     ep_trst_labs = EES2019$Q18_1 %>% val_labels %>% attr(., 'names'))


EES2019 %<>%
  mutate(across(starts_with('Q18_'), list(rec = ~ case_when(as.numeric(.) > 5 ~ NA_real_,
                                                            T                 ~ abs(.-5)))))

## Check
# EES2019 %>%
#   dplyr::select(starts_with('Q18')) %>%
#   print(., n=200)

# Select data for the analyses # =======================================================================

EES2019_merged <- 
  EES2019 %>% 
  dplyr::select(countryshort, partysys, 
                respid, 
                meta_lang_be, 
                ends_with('_age'), 
                ends_with('rec'), 
                ends_with('_lo'), 
                ends_with('_hi'), 
                ends_with('_une'),
                starts_with('WGT')) 



# Not essential variables # ============================================================================

run = F

if (run) {
  
  
}

rm(run)