# Dcoument Similarities and Text Reuse
rm(list = ls())

# load in packages
library(quanteda)
# Please reinstall
# devtools::install_github("matthewjdenny/SpeedReader")
library(SpeedReader)
library(slam)
# library(quanteda.corpora)

############## Text Similarity Metrics ################
# lets start by looking at the text similarity functionality we have available
# to us just in quanteda:
# Drawn from: https://quanteda.io/articles/quickstart.html#similarities-between-texts

# take a look at our corpus:
summary(data_corpus_inaugural)

# we start by creating a document term matrix:
dtm <- dfm(data_corpus_inaugural,
           remove = stopwords("english"),
           stem = TRUE,
           remove_punct = TRUE,
           remove_numbers = TRUE)

# calculate all pairwise similarities to given input documents:
cos_sim <- textstat_simil(
    x = dtm, # the input dtm for comparison
    y = dtm[c("2009-Obama", "2017-Trump"), ], # the rows we want to compare everything to
    margin = "documents", # are we comparing documents or words?
    method = "cosine") # the comparison method

# take a look at the results:
cos_sim

# lets see how similarities change with a different preprocessing specification:
dtm2 <- dfm(data_corpus_inaugural,
            stem = TRUE,
            remove_punct = TRUE,
            remove_numbers = TRUE)

# calculate all pairwise similarities to given input documents:
cos_sim2 <- textstat_simil(
    dtm2, # the input dtm for comparison
    dtm2[c("2009-Obama", "2017-Trump"), ], # the rows we want to compare everything to
    margin = "documents", # are we comparing documents or words?
    method = "cosine") # the comparison method


# take a look at the results:
comp <- cbind(cos_sim[,1],cos_sim2[,1])
colnames(comp) <- c("no_stopwords","stopwords")

# we can see that including stopwords dramatically (and likely artificially)
# increases all pairwise similarities:
comp

# compare with tfidf_weighting
dtm_tfidf <- quanteda::dfm_tfidf(dtm2)

# cosine similarity on tfidf weighted dtm:
cos_sim3 <- textstat_simil(
    dtm_tfidf, # the input dtm for comparison
    dtm_tfidf[c("2009-Obama", "2017-Trump"), ], # the rows we want to compare everything to
    margin = "documents", # are we comparing documents or words?
    method = "cosine")

# take a look at the results:
comp <- cbind(cos_sim[,1],cos_sim3[,1])
colnames(comp) <- c("no_stopwords","tfidf_stopwords")

# we can see that including stopwords dramatically (and likely artificially)
# increases all pairwise similarities:
comp

# a way to download an .rds file from the internet:
data_corpus_sotu <- readRDS(url("https://quanteda.org/data/data_corpus_sotu.rds"))

# take a loook at the corpus:
summary(data_corpus_sotu)

# subset to only SOTUs since 1980:
data_corpus_sotu_1980 <-  corpus_subset(
    data_corpus_sotu,
    Date > as.Date("1980-01-01"))

# turn into a dtm:
sotu <- dfm(data_corpus_sotu_1980,
            stem = TRUE,
            remove_punct = TRUE,
            remove = stopwords("english"))

# look at sparsity:
sotu

# here is a fun little function that can trim infrequently used terms. In this
# case, we set a threshold that all terms we keep in the dfm have to occur
# at least 10 total times, and in at least 5 documents. The idea behind this
# is to assess similarities for documents based on actual shared terms:
sotu <- dfm_trim(sotu,
                 min_termfreq = 10,
                 min_docfreq = 5)

# look at sparsity again:
sotu

# hierarchical clustering - get distances on normalized dfm where the term counts
# are replaced by term proportions in documents:
dists <- textstat_dist(dfm_weight(sotu, scheme = "prop"),
                       method = "euclidean")

# take a look at the distances:
round(dists[38:42,38:42],4)

# now we can run a hiarchical clustering algorithm on the distance object.
pres_cluster <- hclust(as.dist(dists))
# label with document names
pres_cluster$labels <- docnames(sotu)


pdf(file = "~/Desktop/dendrogram.pdf",
    width = 10,
    height = 5)
# plot as a dendrogram
plot(pres_cluster, xlab = "", sub = "",
     main = "Euclidean Distance on Normalized Token Frequency")
dev.off()

# we can also play around with similarities between terms and other terms:
term_sim <- textstat_simil(sotu,
                           sotu[, c("fair", "health", "terror")],
                           method = "cosine",
                           margin = "features")

# take a look at the most similar terms:
lapply(as.list(term_sim), head, 10)


################ Text Reuse ##################
# Now lets move on to assessing document editing and text reuse methods:
# There is a more detailed tutorial available at the end of this page:
# http://www.mjdenny.com/getting_started_with_SpeedReader.html

summary(data_corpus_sotu_1980)
# pull out two example texts to look for overlap in:
obama_2009_exerpt <- "They tell us that even in the most trying times, amid the most difficult circumstances, there is a generosity, a resilience, a decency, and a determination that perseveres, a willingness to take responsibility for our future and for posterity. Their resolve must be our inspiration. Their concerns must be our cause. And we must show them and all our people that we are equal to the task before us."
obama_2009_imposter <- "They tell us that even in the most trying times, amid the most difficult circumstances, there is a generosity, a resilience, a decency, and a determination that perseveres, a willingness to take responsibility for our future and for posterity. They tell us that mario kart is a great game, that was made for quarantine. Their resolve must be our inspiration. Their concerns must be our cause. And we must show them and all our people that we are equal to pi, and to the task before us."

two_doc_comparison <- ngram_sequence_matching(
    document_1 = obama_2009_exerpt,
    document_2 = obama_2009_imposter,
    ngram_size = 5,
    use_hashmap = TRUE)

pdf(file = "~/Desktop/match_sequence.pdf",
    width = 6,
    height = 2)
ngram_sequnce_plot(two_doc_comparison)
dev.off()

two_doc_comparison <- ngram_sequence_matching(
    document_1 = obama_2009_exerpt,
    document_2 = obama_2009_imposter,
    ngram_size = 1,
    use_hashmap = TRUE)

pdf(file = "~/Desktop/match_sequence_unigram.pdf",
    width = 6,
    height = 2)
ngram_sequnce_plot(two_doc_comparison)
dev.off()

# switch the order of documents to see other half of text reuse:
two_doc_comparison <- ngram_sequence_matching(
    document_1 = obama_2009_imposter,
    document_2 = obama_2009_exerpt,
    ngram_size = 5,
    use_hashmap = TRUE)

pdf(file = "~/Desktop/match_sequence_switch.pdf",
    width = 6,
    height = 2)
ngram_sequnce_plot(two_doc_comparison)
dev.off()


# lets try another example:
data("congress_bills")

# take a look at two versions of the same bill:
congress_bills[29]
congress_bills[30]

# Find the locations of overlapping n-gram matches and mismatches in the
# document pair.
matches <- ngram_sequence_matching(congress_bills[29],
                                   congress_bills[30],
                                   ngram_size = 5)

# and take a look at the plot:
ngram_sequnce_plot(matches,
                   custom_title = "Example Comparison of 103-HR-5-IH and 103-HR-5-EH.")

# We can also do generalized comparison using the shingled n-grams sequence
# statistics approach:
bill_tr <- document_similarities(
    filenames = NULL,
    documents = congress_bills,
    input_directory = NULL,
    ngram_size = 5,
    output_directory = NULL,
    doc_pairs = NULL,
    cores = 1,
    max_block_size = NULL,
    prehash = TRUE,
    ngram_match_only = FALSE,
    document_block_size = NULL,
    add_ngram_comparisons = NULL,
    unigram_similarity_threshold = NULL,
    doc_lengths = NULL)


# and tak a look at the results!
head(bill_tr)

# now lets try some additional options
bill_tr2 <- document_similarities(filenames = NULL,
                                 documents = congress_bills,
                                 input_directory = NULL,
                                 ngram_size = 5,
                                 output_directory = NULL,
                                 doc_pairs = NULL,
                                 cores = 1,
                                 max_block_size = NULL,
                                 prehash = TRUE,
                                 ngram_match_only = FALSE,
                                 document_block_size = NULL,
                                 add_ngram_comparisons = c(1,2,10),
                                 unigram_similarity_threshold = 0.9,
                                 doc_lengths = NULL)


# and tak a look at the results!
head(bill_tr2)



