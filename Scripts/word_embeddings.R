# Word Embeddings
# We are going to broadly follow:
# https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html
# and
# https://keras.rstudio.com/articles/examples/pretrained_word_embeddings.html
# for some parts of this lab.
# You can find more pre-trained word embeddings here:
# http://vectors.nlpl.eu/repository/
# You can get the full word2vec pretrained embeddings from a download link here:
# https://code.google.com/archive/p/word2vec/

# Start by installing the R package:
install.packages(c("keras","text2vec","Rtsne","ggrepel","rio"))
library(keras)
library(stringr)
library(text2vec)
library(Rtsne)
library(ggplot2)
library(ggrepel)
library(rio)

# If you want to try computing your own word embeddings, you will need to
# actually install the underlying software on our computers:
# install_keras()

# if this fails on a Mac, try:
# sudo /usr/bin/easy_install pip
# Then enter your login password.
# sudo /usr/local/bin/pip install --upgrade virtualenv
# Then enter your login password.

# we are going to use the pre-trained GLOVE embeddings. We could train our own
# embeddings, and I could sort of wave my hands and pretend to have taught you
# how to train a word embeddings model, but realistically this is beyond the
# scope of this class. You can find code in the cbail.github link to train a
# model if you are felling adventerous:
setwd("~/Desktop")
GLOVE_DIR <- 'glove.6B'

# download the GLOVE embeddings:
download_data <- function(data_dir, url_path, data_file) {
    if (!dir.exists(data_dir)) {
        download.file(paste0(url_path, data_file), data_file, mode = "wb")
        if (tools::file_ext(data_file) == "zip")
            unzip(data_file, exdir = tools::file_path_sans_ext(data_file))
        else
            untar(data_file)
        unlink(data_file)
    }
}
download_data(GLOVE_DIR, 'http://nlp.stanford.edu/data/', 'glove.6B.zip')


# read in the word vectors (50 dimensions):
word_embeddings_200 <- rio::import(file.path(GLOVE_DIR, 'glove.6B.200d.txt'),
                               quote = "")

# pull out the columns with the embeddings:
embedding_matrix_200 <- as.matrix(word_embeddings_200[,2:201])
# assign the words themselves as the row names:
rownames(embedding_matrix_200) <- word_embeddings_200[,1]

# function to find and print similar terms:
find_similar_words <- function(word,
                               embedding_matrix,
                               n = 5,
                               norm = "l2") {
    similarities <- embedding_matrix[word, , drop = FALSE] %>%
        sim2(embedding_matrix, y = ., method = "cosine", norm = norm)

    similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}

# lets find similar terms in the embedding space
find_similar_words("republican", embedding_matrix_200,  n = 20)
find_similar_words("republican", embedding_matrix_200, norm = "none", n = 20)
find_similar_words("cheese", embedding_matrix_200, n =10)

find_similar_words("partisan", embedding_matrix_200, n = 20)


# find most similar terms to abortion:
find_similar_words("abortion", embedding_matrix_200, n = 20)

# We can also use vector addition/subtraction to find (for example) non partisan
# synnonyms to the term abortion:
analogy = embedding_matrix_200["abortion", , drop = FALSE] -
    embedding_matrix_200["partisan", , drop = FALSE]

similarities <- sim2(x = embedding_matrix_200,
                     y = analogy,
                     method = "cosine")

similarities[,1] %>% sort(decreasing = TRUE) %>% head(20)


# we can also use word embeddings to solve analogies:
analogy = embedding_matrix_200["touchdown", , drop = FALSE] +
    embedding_matrix_200["basketball", , drop = FALSE] -
    embedding_matrix_200["football", , drop = FALSE]

similarities <- sim2(x = embedding_matrix_200,
                     y = analogy,
                     method = "cosine",
                     norm = "l2")

similarities[,1] %>% sort(decreasing = TRUE) %>% head(10)



# read in the word vectors (50 dimensions):
word_embeddings <- rio::import(file.path(GLOVE_DIR, 'glove.6B.50d.txt'),
                               quote = "")

# pull out the columns with the embeddings:
embedding_matrix <- as.matrix(word_embeddings[,2:51])
# assign the words themselves as the row names:
rownames(embedding_matrix) <- word_embeddings[,1]

# try the same thing but with 50d vectors:
analogy = embedding_matrix["abortion", , drop = FALSE] -
    embedding_matrix["partisan", , drop = FALSE]

similarities <- sim2(x = embedding_matrix,
                     y = analogy,
                     method = "cosine")

similarities[,1] %>% sort(decreasing = TRUE) %>% head(20)


# lets try out this dataset of 9799 tweets from elected officials in the US:
load(url("https://cbail.github.io/Elected_Official_Tweets.Rdata"))

# We want to use original tweets, not retweets:
inds <- which(elected_official_tweets$is_retweet == FALSE)
tweets <- elected_official_tweets[inds,c("screen_name", "text")]

# Many tweets contain URLs, which we don't want considered in the model:
tweets$text <- str_replace_all(string = tweets$text,
                               pattern = "https.+",
                               replacement = "")

# We can also generate document embeddings. These tend to make more sense for
# short documents like tweets than very long documents, or doucments that deal
# with multiple issues. The following will take several hours to run, so below I
# have provided you with the end results:
text <- tolower(tweets$text)
tokens <- stringr::str_split(text,"\\s+")

# use word embeddings for document similarity:
document_embeddings <- function(doc_tokens,
                                embedding_matrix) {

    # allocate a vector to embed the document in:
    doc_embedding <- rep(0,ncol(embedding_matrix))

    # remove blank tokens
    rem <- which(doc_tokens == "")
    if (length(rem) > 0) {
        doc_tokens <- doc_tokens[-rem]
    }

    # determine which tokens in the document match the vocaubulary in the
    # word embeddings matrix:
    inds <- match(doc_tokens,rownames(embedding_matrix))

    # remove any terms that do not appear in the embeddings matrix:
    rem <- which(is.na(inds))
    if(length(rem) > 0) {
        inds <- inds[-rem]
    }

    # take the column sums to find the vector addition of the words. Deal with
    # case of multiple matching words, only one, and none:
    if (length(inds) > 1) {
        doc_embedding <- colSums(embedding_matrix[inds,])
    } else if(length(inds) > 0) {
        doc_embedding <- embedding_matrix[inds,]
    } else {
        doc_embedding <- rep(0,ncol(embedding_matrix))
    }
    # return the document embedding vector
    return(doc_embedding)
}

# create a blank matrix to store the document embeddings:
embedded_docs <- matrix(0, nrow = length(tokens),
                        ncol = ncol(embedding_matrix))

# loop through your documents and generate document embeddings from word
# embeddings:
for (i in 1:length(tokens)) {
    if (i %% 10 == 0) {
        print(i)
    }
    embedded_docs[i,] <- document_embeddings(
        tokens[[i]],
        embedding_matrix = embedding_matrix)
}

# make sure that the rownames are the text of the documents:
# rownames(embedded_docs) <- tweets$text
# save the output:
# save(embedded_docs, file = "~/Desktop/Embedded_Political_Docs.RData")

# load in the pore-saved document embeddings from the git repo:
load("~/Desktop/Embedded_Political_Docs.RData")

# lets take a look at some tweets to compare:
head(tweets$text)

# and some more:
tail(tweets$text)

# now lets find similar tweets based on their embedding:
find_similar_words(tweets$text[2],embedded_docs, n = 10)

find_similar_words(tail(tweets$text)[3],embedded_docs, n = 5)

# we can also go one step further and look at author similarity using their
# document embeddings:
tweeters <- unique(tweets$screen_name)
tweeter_embeddings <- matrix(0,
                             nrow = length(tweeters),
                             ncol = ncol(embedded_docs))

# loop throuhg unique legislators and take the column sums of their tweet embeddings:
for (i in 1:length(tweeters)) {
    inds <- which(tweets$screen_name == tweeters[i])

    if (length(inds) > 1) {
        tweeter_embeddings[i,] <- colSums(embedded_docs[inds,])
    } else if(length(inds) > 0) {
        tweeter_embeddings[i,] <- embedded_docs[inds,]
    }
}

# make sure we add on the rownames to these embeddings:
rownames(tweeter_embeddings) <- tweeters



# now we are going to use a Barnes-Hut t-Distributed Stochastic Neighbor
# Embedding algorithm to reduce the dimensionality to 2 dimensions so that we
# canplot legislators against eachother:
# Set seed:
set.seed(12345)
# Generate the 2-dimenstional reduced embeddings:
tsne <- Rtsne(tweeter_embeddings, pca = FALSE)

# take the results and put them in a data frame for easy plotting:
tweeter_results <- data.frame(tweeter = rownames(tweeter_embeddings),
                              x = tsne$Y[,1],
                              y = tsne$Y[,2],
                              stringsAsFactors = FALSE)

# generate a plot:
pdf(file = "~/Desktop/Legislator_Embeddings.pdf",
    width = 12,
    height = 12)
ggplot(tweeter_results, aes(x = x, y = y, label = tweeter)) +
    geom_point(color = "red") +
    geom_text_repel()
dev.off()
