# load the necessary packages
library(tidyverse)
library(quanteda)
library(haven)

# install and load a new packages for today
# only run this code if you have not already run class3a
install.packages("openxlsx")
library(openxlsx)

# install remotes if not already installed
install.packages("remotes")

# Install quanteda.sentiment from GitHub
# this gives you some useful sentiment dictionaries
remotes::install_github("quanteda/quanteda.sentiment")


#########import/ clean German political tweets if necessary#########
# import tweets from Dropbox folder
# if you have not already done so
twt_de <- read.xlsx("https://www.dropbox.com/scl/fi/2gcyo0f8i4pdrcoqzxyn7/mdb_twt.xlsx?rlkey=t3yycjvxzhc39l98xjm5nle4g&st=ov9y8aaf&dl=1")

# import German politician details
# if you have not already done so
pol_de <- read.csv("https://www.dropbox.com/scl/fi/4st89lqg8soj9gj3z4lj0/de_all.csv?rlkey=h163w1p8dy1t8p6mzvtcn36ve&st=fzqc309o&dl=1",
                   encoding = "UTF-8")


# rename variables for merging
pol_de <- pol_de %>%
  rename(username = Twitter)

# rename and select for merging
twt_sub <- twt_de %>%
  select(AUTHOR_USERNAME, TEXT) %>%
  rename(username = AUTHOR_USERNAME,
         text = TEXT)

# merge politician details and tweets
twt_all <- merge(twt_sub, pol_de, by = "username", all.x = TRUE)

# note the different row numbers between twt_sub and twt_all
# we need to add some details...
twt_all$party <- ifelse(twt_all$username == "Jan_Nolte_AfD", 
                        "AfD", 
                        twt_all$party)

################

############
# start with the twt_all data frame
# select name and text for the corpus file
twt_selected <- twt_all %>%
  select(name, text)


# create the corpus, tokens, and dfm
twt_corp <- corpus(twt_selected)

# rename the rows
docnames(twt_corp) <- paste0(1:nrow(twt_selected),"_",
                             twt_selected$name)
twt_toks <- tokens(twt_corp)

twt_dfm <- dfm(twt_toks)

# check the head
head(twt_dfm)

# how do the Tweets discuss migration?
# create a migration dictionary
mig_dict <- dictionary(list(migration = c("migr*", "flucht*", "asyl*", "zuwand*", "integrat*")))

# Extract context around migration terms (window = 5 words on each side)
mig_context <- tokens_select(
  twt_toks,
  pattern = mig_dict,
  window = 5,
  selection = "keep",
  valuetype = "glob"
)

# view some of the results
mig_context[123:125,]

# Load Rauh sentiment dictionary
data("data_dictionary_Rauh", package = "quanteda.sentiment")

# Create dfm from context windows
dfm_context <- dfm(mig_context)

# Apply the Rauh sentiment dictionary
sent_scores <- dfm_lookup(dfm_context, dictionary = data_dictionary_Rauh)


# Convert to data.frame
sent_df <- convert(sent_scores, to = "data.frame")

# Create score: positive - negative
sent_df$sentiment_score <- sent_df$positive - sent_df$negative

# Add document IDs for merging
twt_all$doc_id <- docnames(twt_corp)
sent_df$doc_id <- docnames(dfm_context)


# Merge scores
twt_all_scored <- merge(twt_all, 
                        sent_df[, c("doc_id", "sentiment_score")], 
                        by = "doc_id", 
                        all.x = TRUE)

# Replace NA with 0 (i.e. tweets without migration content)
twt_all_scored <- twt_all_scored %>%
  filter(sentiment_score != "NA")


agg_scores <- aggregate(sentiment_score ~ party, 
                        data = twt_all_scored, 
                        FUN = mean)

# create a plot of the results
# first specify party colours
party_colors <- c(
  "AfD" = "#009EE0",         # AfD blue
  "CDU/CSU" = "#000000",     # CDU/CSU black
  "Die Linke" = "#BE3075",   # Left party magenta
  "FDP" = "#FFED00",         # FDP yellow
  "Greens" = "#64A12D",      # Green party green
  "Independent" = "#A9A9A9", # OPTIONAL
  "SPD" = "#E3000F"          # SPD red
)


# plot the scores
agg_scores %>%
  filter(party != "Independent") %>%
ggplot(aes(x = sentiment_score, y = reorder(party, sentiment_score))) +
  geom_point(aes(color = party), size = 4) +
  scale_color_manual(values = party_colors) +
  labs(
    x = "Sentiment Score",
    y = "",
    color = "Party"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


# now calculate Dalton's polarisation index
# first we need a dataset of electoral results
# import the ParlGov dataset for this

parl_gov <- read.xlsx("https://www.dropbox.com/scl/fi/s2ot4peowrqa2yz7zfz5e/parlgov24.xlsx?rlkey=bs4i7mg852pcb0pcvdz6bndcc&st=d4tka1ua&dl=1")

# select the necessary information
# recode the key party name details
parl_gov_de <- parl_gov %>%
  filter(country_name_short == "DE", election_date == "2021-09-26") %>%
  select(party_name_short, vote_share) %>%
  mutate(party = case_when(
    party_name_short == "B90/Gru" ~ "Greens",
    party_name_short == "PDS|Li" ~ "Die Linke",
    party_name_short == "SSW" ~ "Independent",
    party_name_short %in% c("CDU", "CSU") ~ "CDU/CSU",
    TRUE ~ party_name_short
  )) %>%
  group_by(party) %>%
  summarise(vote_share = sum(vote_share), .groups = "drop")

# merge the party vote shares with the party positions
de_agg <- merge(agg_scores, parl_gov_de, by = "party", all.x = TRUE)

de_agg <- de_agg %>%
  filter(!is.na(vote_share))

# view the results
de_agg


# compute weighted mean positions
weighted_mean <- de_agg %>%
  summarise(w_mean = sum(vote_share * sentiment_score)) %>%
  pull(w_mean)

# compute Dalton's Polarisation Index
dpi <- sum(de_agg$vote_share * abs(de_agg$sentiment_score - weighted_mean))

dpi <- sqrt(dpi)

# show the result
dpi

# Try this whole process with a different dictionary 
# and policy measures
# what level of polarisation do you find?
# how does it compare with migration?
# what other points of comparison could you include?