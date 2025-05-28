#####SETTING UP#####

# install and load the necessary packages for today
admin <- TRUE
if (admin) {
  want <- c(
    "tidyverse", "quanteda", "quanteda.textplots",
    "quanteda.textstats", "readr", "stringr"
  )
  
  have <- want %in% rownames(installed.packages())
  if (any(!have)) {
    install.packages(want[!have])
  }
  
  # Load packages
  junk <- lapply(want, library, character.only = TRUE)
  
  # Clean up
  rm(have, want, junk)
  options(scipen = 99)
  rm(list = ls())
}



#####IMPORT THE DATA#####
# import the dataset from GitHub
first_debate <- read_csv("https://raw.githubusercontent.com/AlexDataPlatz/qrps_sose25/main/data/us_election_2020_1st_presidential_debate.csv")


# examine the file
head(first_debate)


# edit the speaker variable with regular expressions
first_debate <- first_debate %>%
  mutate(speaker=str_extract(speaker,"[A-z]*$")) 

# examine the file again
head(first_debate)
# notice the change to the speaker variable


#####FIRST OBJECT: CREATING A CORPUS#####

# transform the data frame into a CORPUS
# this makes it easier for Quanteda to work with
# use the Quanteda command corpus()
debate_corp <- corpus(first_debate)

# examine the corpus
head(debate_corp)
# notice the content. What is missing?

# let's rename the documents
docnames(debate_corp) <- paste0(1:nrow(first_debate),"_",
                                first_debate$speaker)

# examine the corpus again
head(debate_corp)
# now the doc names are a bit more "useful"

# look at a summary of the corpus
summary(debate_corp) %>% head()
# what do the different headings mean:
# Text, Types, Tokens?

#####SECOND OBJECT: CREATING TOKENS#####

# you can create different sizes of tokens
# examine the different outputs
tokens(debate_corp,"sentence")
tokens(debate_corp,"character")

# but most often, we will work with individual word-like chunks
tokens(debate_corp, what="word")[[1]][1:10]

# tokenise the corpus into word-like features
debate_toks <- tokens(debate_corp)

# examine the output
head(debate_toks)

# examine the earlier output
head(debate_corp)

# what is the difference?

#####THIRD OBJECT: CREATING A DOCUMENT FEATURE MATRIX#####

# use the dfm() command to create the object
debate_dfm <- dfm(debate_toks)

# examine the output
head(debate_dfm)
# what do you think it represents?

# Let's go back to the slides...




#####GETTING STARTED WITH DESCRIPTIVE ANALYSIS####

# what can we learn from our data?
# let's get an overview


# TASK 1: CONTEXT 
kwic(debate_toks, "America",window=4)

# try different words and windows
# Obama, economy, climate
# who talks most about each word? in what context?

# you can also word combinations via phrase()
kwic(debate_toks, phrase("Vice President"), window = 4)
# try other words and word combinations and tell me what you find


# TASK 2: COMPLEXITY
# how simple or complex is each speaker's language
# view for each doc
textstat_readability(debate_corp) %>% 
  head()

# create an object calculating readability scores
readability <- textstat_readability(debate_corp)

# examine the object
head(readability)
# it should look the same as above

# extract speaker names from the document IDs
readability <- readability %>%
  mutate(speaker = str_extract(document, "[^_]+$")) 
# this code uses Regex (in this case, "[^_]+$"), more on this later...

# calculate average Flesch score by speaker
readability_summary <- readability %>%
# use group_by() to get the average per speaker
  group_by(speaker) %>%
# create a new variable, avg_flesch, using mean()
# na.rm = TRUE removes any missing values
# R can only calculate the mean for numbers, not strongs or NAs
# So without na.rm = TRUE, you sometimes get an error
  summarise(avg_flesch = mean(Flesch, na.rm = TRUE)) 

# view the results
readability_summary %>%
# use filter() to select the speakers of interest
# the | translates as "or"
# similar commands are & ("and") and ! ("not")
  filter(speaker == "Biden"| speaker == "Trump")


# TASK 3: INTERRUPTIONS
# when do interruptions happen?
# look for token "crosstalk"
kwic(debate_toks, "crosstalk") %>%
  # plot only the first 15 occurances
  head(15) %>%
  textplot_xray()
# who gets interrupted the most?

# which package is textplot_xray() from?
# this code will tell you the answer
?textplot_xray

# which package is str_extract from?
# what does the 'str' part mean?


# TASK 4: SPECIFIC VOCABULARY
# what words does each speaker use most frequently?
# extract speaker names
speakers <- str_extract(docnames(debate_dfm), "(?<=_)\\w+")

# assign as docvar in the DFM
head(debate_dfm)

docvars(debate_dfm, "speaker") <- speakers

# now view the speaker variable
head(debate_dfm$speaker)

# remove docs where speaker is NA
dfm_biden <- debate_dfm %>%
  filter(speaker == "Biden")
# what went wrong?

# we cannot use tidyverse with dfm 
# instead, we need to use 'base' R 
# but base R is very reliable, usually fast, and less changeable than most packages

# first remove NAs
debate_dfm_clean <- debate_dfm[!is.na(docvars(debate_dfm, "speaker")), ]


# then subset Biden docs only
dfm_biden <- debate_dfm_clean[docvars(debate_dfm_clean, "speaker") == "Biden", ]


# plot the word cloud  
  textplot_wordcloud(
    dfm_biden,
    color = "steelblue")

# now do the same for Trump
# what differences do you notice?
# what problems do you notice with the results?
  
  
# TASK 5: KEYNESS
# calculate the "keyness" of words for each speaker
# which words have a higher chi-squared score for Biden?
# i.e. which words does he use more frequently than Trump?
keyness <- dfm_group(debate_dfm,speaker) %>%
  textstat_keyness("Biden")

head(keyness)

# plot the results
keyness %>%
  textplot_keyness()

# now do the same for Trump
# which words are "key"? what differences can you see?
# as you see, we have similar issues as with the word clouds
# we will discuss the way to deal with this in more detail next week


# Exercises

# You can do the following tasks either on the same corpus or another corpus you find interesting.

# - e.g. [Parlspeech](https://search.gesis.org/research_data/SDN-10.7802-2824)
# - e.g. [Party Manifestos](https://manifesto-project.wzb.eu/datasets/MPDS2024a)
# - [Collection of example datasets](https://www.ics.uci.edu/~smyth/courses/cs175/text_data_sets.html)

## Exercise: Keywords in context

# Using the commands above, check in which context the concepts of women and men are used.

# *RegEx Tip: You can use the question mark as replacement of a single letter 
# or the asteriks sign as a replacement of multiple letters, e.g. to include singular and plural.*


## Exercise: Keyness statistics

# Using the commands above, find out what are the most typical words for one of actors in your dataset. 
# If you are still using the debates, look at a different actor than before and try not to copy the code.
# After that, tell me something new you discovered about the words in the dataset

