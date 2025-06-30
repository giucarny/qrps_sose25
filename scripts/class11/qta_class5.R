# load some existing packages
library(tidyverse)
library(broom)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(openxlsx)

# install new packages for today's exercises
install.packages("tidytext")
install.packages("textdata")
install.packages("text2vec")
install.packages("pheatmap")

library(tidytext)
library(textdata)
library(text2vec)
library(pheatmap)

# import the parliament subset file
hoc_2019 <- read.xlsx("https://www.dropbox.com/scl/fi/wpkf9bsgohqojru968cfh/hoc_2019.xlsx?rlkey=7fyq0yp5cz3yfm14db1hg4muo&st=xeo186kk&dl=1")

# Or work with the larger file from class 3b if you already have it
# subset the data to create a smaller file
# hoc_2019 <- corp_hoc %>%
#  filter(date == "2019-01-08")


# create a quanteda corpus object
hoc_corp <- corpus(hoc_2019)


# Add party and speaker name as document variables
docvars(hoc_corp, "party") <- hoc_2019$party
docvars(hoc_corp, "speaker") <- hoc_2019$speaker

# create tokens
hoc_toks <- tokens(hoc_corp)

# create the document feature matrix
feats <- dfm(hoc_toks, verbose = TRUE) %>%
  dfm_trim(min_termfreq = 5) %>%
  featnames()

# run some descriptive analysis on the objects 
# (as in Class 1)

# add padding to all rows for any missing words
hoc_toks <- tokens_select(hoc_toks, 
                           feats, 
                           padding = TRUE)


# create a feature co-occurrence matrix
hoc_fcm <- fcm(hoc_toks, 
                context = "window", 
                count = "weighted", 
                weights = 1 / (1:5), 
                tri = TRUE)

# shows how often words are followed by other words
head(hoc_fcm)

# import the GloVe algorithm from the text2vec package
glove <- GlobalVectors$new(rank = 50, x_max = 10)

# rank controls the number of dimensions in the vector
# x_max controls the weighting of frequent words
# use higher values for larger corpuses

# apply the algorithm to our fcm
wv_main <- glove$fit_transform(hoc_fcm, 
                               n_iter = 10,
                               convergence_tol = 0.01, 
                               n_threads = 8)

head(wv_main)
# now we have the embedding vectors for each word

dim(wv_main)
# here we are only asking to calculate 50 dimensions
# we can increase, but the resulting matrix will be bigger

# GloVe also calculates vectors for each dimension within a text
# extract these too
wv_context <- glove$components

# inspect as before
dim(wv_context)

head(wv_context)

# transpose the matrix
head(t(wv_context))

# averaging word and context vectors is not necessary, 
# but can give a more accurate value for each word
word_vectors <- wv_main + t(wv_context)


head(word_vectors)
# this object is the basis for understanding our corpus
# let's discuss what we have done


# common example from word embeddings tutorials:
# paris - france + germany = berlin
# we can try this with the UK corpus
# (but it will work better with a larger corpus)
result_vector <- word_vectors["Department", , drop = FALSE] - 
  word_vectors["Conservative", , drop = FALSE] + 
  word_vectors["Labour", , drop = FALSE]

cos_sim <- textstat_simil(x = as.dfm(word_vectors),
                          y = as.dfm(result_vector),
                          method = "cosine")

head(sort(cos_sim[, 1], decreasing = TRUE), 5)


# measuring party positions
# calculate average embedding per party
# function to get mean word vector for a document
get_doc_vector <- function(tokens, word_vectors) {
  tokens <- tokens[tokens %in% rownames(word_vectors)]
  
  if (length(tokens) == 0) {
    return(rep(NA, ncol(word_vectors)))  # Handle empty cases
  }
  
  colMeans(word_vectors[tokens, , drop = FALSE])
}

# Apply to all documents (rows = speeches)
doc_vectors <- t(sapply(hoc_toks, get_doc_vector, word_vectors = word_vectors))

# Combine with speaker and party labels
doc_vectors_df <- as.data.frame(doc_vectors)
doc_vectors_df$speaker <- docvars(hoc_corp, "speaker")
doc_vectors_df$party <- docvars(hoc_corp, "party")

# Filter out rows with NA vectors
doc_vectors_df <- doc_vectors_df %>%
  filter(!is.na(V1))  # Assumes first dimension is V1

# Group by speaker and party, compute mean vector
speaker_vectors <- doc_vectors_df %>%
  group_by(speaker, party) %>%
  summarise(across(starts_with("V"), mean, na.rm = TRUE), .groups = "drop")

# Perform PCA
pca <- prcomp(speaker_vectors %>% select(starts_with("V")), scale. = TRUE)
pca_df <- as.data.frame(pca$x[, 1:2])  # Take first 2 PCs
pca_df$speaker <- speaker_vectors$speaker
pca_df$party <- speaker_vectors$party

# Plot with color by party
ggplot(pca_df, aes(PC1, PC2, color = party)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Lab" = "red", "Con" = "blue")) +
  theme_minimal() +
  labs(title = "PCA of MP Speech Embeddings",
       x = "PC1", y = "PC2") +
  theme(legend.title = element_blank())



# Create annotation for parties
annotation <- data.frame(Party = speaker_vectors$party)
rownames(annotation) <- speaker_vectors$speaker



# Sample 20 unique speakers
sample_speakers <- sample(speaker_vectors$speaker, 20)

# Filter speaker_vectors to sampled speakers
sample_vectors <- speaker_vectors %>%
  filter(speaker %in% sample_speakers)

# Extract only vector columns
vector_mat <- as.matrix(sample_vectors %>% 
                          select(starts_with("V")))

# Compute cosine similarity matrix
similarity_mat <- as.matrix(textstat_simil(as.dfm(vector_mat), 
                                           method = "cosine"))

# Add speaker names
rownames(similarity_mat) <- sample_vectors$speaker
colnames(similarity_mat) <- sample_vectors$speaker


# Party color annotations
annotation <- data.frame(Party = sample_vectors$party)
rownames(annotation) <- sample_vectors$speaker

party_colors <- list(Party = c(Lab = "red", Con = "blue"))

# Heatmap with annotations
pheatmap(similarity_mat,
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         annotation_row = annotation,
         annotation_col = annotation,
         annotation_colors = party_colors,
         color = colorRampPalette(c("white", "purple"))(100),
         main = "Cosine Similarity Between 50 Sampled MPs")


# Additional exercises
# run the same process with more of the House of Commons corpus
# plot the results
# what differences do you observe?
# how can you measure the gaps between the parties?
# how can we use this process to calculate e.g. polarisation?

# try the same process with our twitter file from class 3b
# calculate average embeddings for each party/ MP

