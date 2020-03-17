# Term Weighting, Term Ranking, Corpus Description.

# preliminaries: clear working idrectory
rm(list = ls())

# for more infromation on installing SpeedReader, see:
# https://github.com/matthewjdenny/SpeedReader

install.packages("devtools")
library("devtools")
#Now we can install from Github using the following line:
devtools::install_github("matthewjdenny/SpeedReader")

# load packages
library(quanteda)
library(SpeedReader)
library(ggplot2)

# load in a corpus object:
corp <- corpus(data_corpus_inaugural)

# pull out some basic summary data:
summary_data <- as.data.frame(summary(corp))

# look at the variation in number of tokens per document
ggplot(summary_data, aes(x = Text, y = Tokens)) +
    geom_col() +
    coord_flip()

# look at the variation in number of types per document
ggplot(summary_data, aes(x = Text, y = Types)) +
    geom_col() +
    coord_flip()

# create a quanteda dfm object:
dtm <- dfm(corp,
           remove_punct = TRUE,
           remove_numbers = TRUE)

# check dimensions:
dtm

# now we are going to convert this into a simple triplet matrix format:
dtm_triplet <- SpeedReader::convert_quanteda_to_slam(dtm)

# now we convert the result into a dense matrix format:
dtm_dense <- SpeedReader::sparse_to_dense_matrix(dtm_triplet)

# Information Theoretic Quantities:

# we are fortunate that there is a function to calculate PMI ready to go for us
pmi_table <- pmi(dtm_triplet)

# we can also calculate the mutual information of the joint distribution
# implied by the dtm:
mutual_information(dtm_triplet)

# let's try removing stopwors and see what happens to the mutual information
# of the join distribution impled by the resulting dtm:
dtm2 <- dfm(corp,
            remove_punct = TRUE,
            remove_numbers = TRUE,
            remove = stopwords("en"))
# convert to a simple triplet matrix
dtm_triplet2 <- SpeedReader::convert_quanteda_to_slam(dtm2)

# now we calcualte it's mutual information. Note that we can do this for
# simple triplet matrices or dense matrices. This function is mutch faster (for
# very large matrices) when using a sparse representation of the matrix.
mutual_information(dtm_triplet2)

# We can also calcualte entropy of term distributions:
calc_entropy <- function(input) {
    # normalize
    input <- input/sum(input)
    rem <- which(input == 0)
    if (length(rem) > 0) {
        input <- input[-rem]
    }
    log_input <- log(input)
    return(-sum(input*log_input))
}

# calculate entropy
entropies <- apply(FUN = calc_entropy,
                   X = dtm_dense,
                   MARGIN = 2)

# find highest entropy terms
entropies <- entropies[order(entropies,decreasing = T)]
entropies[1:40]

# find lowest entropy terms
entropies <- entropies[order(entropies,decreasing = F)]
entropies[1:40]

# Now lets try creating an alternate DTM using 2-4 grams (either with a POS
# filter or just all 2-4 grams if you cannot get phrasemachine to work :/):
# if phrasemachine works for you, you can try out the following:
library(phrasemachine)
corp <- quanteda::data_corpus_inaugural
documents <- texts(corp)
phrase_documents <- phrasemachine(
    documents,
    regex = "Phrases",
    maximum_ngram_length = 4,
    minimum_ngram_length = 1,
    return_phrase_vectors = TRUE,
    return_tag_sequences = FALSE,
    memory = "-Xmx512M")
temp <- unlist(lapply(phrase_documents,paste0,collapse = " "))
texts(corp) <- temp
phrase_tokens <- tokens(corp,
                        what = "fastestword")
dtm3 <- quanteda::dfm(phrase_tokens,
                                 tolower = TRUE,
                                 stem = FALSE,
                                 remove_punct = FALSE)

# if not, just try with all 1-4 grams
corp <- quanteda::data_corpus_inaugural
dtm3 <- dfm(corp,
            remove_punct = TRUE,
            remove_numbers = TRUE,
            ngrams = 1:4)


# we can see how this affects pmi top terms
dtm_triplet3 <- SpeedReader::convert_quanteda_to_slam(dtm3)

# we are fortunate that there is a function to calculate PMI ready to go for us
pmi_table3 <- pmi(dtm_triplet3)



######### TF-IDF weighting: ###########


# applies TF_t,d * log(num_docs/DF_t) weighting to each entry in the document term
# matrix and returns a dtm where the i,j entries are no longer the number of
# times the term appeared in the document, but have now had idf weighting
# multipled for each term:
Q_tfidf <- quanteda::dfm_tfidf(dtm3)


# take a look a the top 20 terms across the entire corpus:
Q_tfidf <- as.matrix(Q_tfidf)
raw_dtm <- as.matrix(dtm3)
top_n <- 20
temp <- colSums(Q_tfidf)
temp <- temp[order(temp,decreasing = T)]
temp <- data.frame(term = names(temp)[1:top_n],
                   score = temp[1:top_n],
                   stringsAsFactors = FALSE)
row.names(temp) <- NULL
print(temp)

# and in individual documents:
for (i in 50:58) {
    cat("Top terms for",rownames(Q_tfidf)[i],"\n")
    temp <- Q_tfidf[i,]
    temp2 <- raw_dtm[i,]
    temp <- temp[order(temp,decreasing = T)]
    temp2 <- temp2[order(temp2,decreasing = T)]

    to_print <- data.frame(tfidf_term = names(temp)[1:top_n],
                           tfidf_score = temp[1:top_n],
                           freq_term = names(temp2)[1:top_n],
                           freq_score = temp2[1:top_n],
                           stringsAsFactors = FALSE)
    row.names(to_print) <- NULL
    print(to_print)
    cat("\n\n")
}



# applies TF * log(num_docs/DF) weighting to terms where TF is the total number
# of times that term is used in the corpus, and the IDF multiplication only
# happens once. Useful for evaluating "important" terms at the corpus level:
SR_tfidf <- SpeedReader::tfidf(dtm_triplet,
                               vocabulary = colnames(dtm_triplet),
                               only_calculate_corpus_level_statistics = TRUE)

# a comparison of different term weightings and their resultant top terms
comparison <- compare_tf_idf_scalings(dtm_dense)

# run through the different weighting methods and look
top_n <- 20
for (i in 1:length(comparison)) {
    cat("Showing results for Scaling Method:",comparison[[i]]$scaling,"\n")
    temp <- colSums(comparison[[i]]$dtm)
    temp <- temp[order(temp,decreasing = T)]
    temp <- data.frame(term = names(temp)[1:top_n],
                       score = temp[1:top_n],
                       stringsAsFactors = FALSE)
    row.names(temp) <- NULL
    print(temp)
}

