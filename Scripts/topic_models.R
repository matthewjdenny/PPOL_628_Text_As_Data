# Supervised Learning with Text
rm(list = ls())

# load in packages
library(quanteda)
# devtools::install_github("matthewjdenny/SpeedReader")
library(SpeedReader)
# install.packages("Rcpp",dependencies = TRUE)
library(ggplot2)

# for running topic models:
# install.packages("stm",dependencies = TRUE)
library(stm)

# to get access to one of the corpora we are using today:
# devtools::install_github("quanteda/quanteda.corpora")
library(quanteda.corpora)

############### Dirichlet Distributions ###################

# We need to start by sourcing our rdirichlet_cpp() function.
# If you cannot get Rcpp to work on your computer, you can also try
# the version included in MCMCpack:
# install.packages("MCMCpack", dependencies = TRUE)
# library(MCMCpack)
# and substitute this function for rdirichlet_cpp() in the code:
# MCMCpack::rdirichlet(draws, alpha_m)

set.seed(14578)
alpha <- 5
draws <- 1
dimen <- 20
alpha_m <- rep(alpha/dimen, dimen)
x <- rdirichlet_cpp(draws,alpha_m)

dat <- data.frame(Category = factor(1:dimen),
                  Density = as.vector(t(x)))

# plot the multinomial distribution we got from a draw from our dirichlet
# distribution:
ggplot(dat,aes(x = Category,y = Density,ymin = 0, ymax = Density)) +
    geom_point(colour = "darkblue",fill = "darkblue") +
    geom_linerange(colour = "darkblue") +
    scale_y_continuous(lim=c(0,0.5))

set.seed(14578)
alpha <- 100
draws <- 1
dimen <- 20
alpha_m <- rep(alpha/dimen, dimen)
x <- rdirichlet_cpp(draws,alpha_m)

dat <- data.frame(Category = factor(1:dimen),
                  Density = as.vector(t(x)))

# plot the multinomial distribution we got from a draw from our dirichlet
# distribution:
ggplot(dat,aes(x = Category,y = Density,ymin = 0, ymax = Density)) +
    geom_point(colour = "darkblue",fill = "darkblue") +
    geom_linerange(colour = "darkblue") +
    scale_y_continuous(lim=c(0,0.5))


set.seed(233478)
alpha <- 1
draws <- 1
dimen <- 20
alpha_m <- rep(alpha/dimen, dimen)
x <- rdirichlet_cpp(draws,alpha_m)

dat <- data.frame(Category = factor(1:dimen),
                  Density = as.vector(t(x)))

# plot the multinomial distribution we got from a draw from our dirichlet
# distribution:
ggplot(dat,aes(x = Category,y = Density,ymin = 0, ymax = Density)) +
    geom_point(colour = "darkblue",fill = "darkblue") +
    geom_linerange(colour = "darkblue") +
    scale_y_continuous(lim=c(0,1))

############### Topic Models using STM package #################

# lets start with a small corpus following the example in the quanteda
# quick start guide. We are going to use a package from the quanteda.corpora
# package, but you can substitute data_corpus_irishbudget2010 from the base
# quanteda package instead if you have trouble downloading the package:
data_corpus_guardian <- download("data_corpus_guardian")

# subset to the first 1000 documents:
data_corpus_guardian <- data_corpus_guardian[1:1000,]

# create a dfm object
dtm <- dfm(data_corpus_guardian,
           remove_punct = TRUE,
           remove_numbers = TRUE,
           remove = stopwords("english"))

# look at number of features:
dtm

# in order to do some preliminary checks, lets convert our quanteda dfm object
# into an stm object. In general, we do not need to do this, but in this
# instance it will let us make a cool plot:
check <-quanteda::convert(dtm,
                          to = "stm")

# we can make a plot to see how removing terms appearing in less than x documents
# would affect or corpus:
pdf(file = "~/Desktop/Removing_Terms.pdf",
    width = 9,
    height = 3)
plotRemoved(check$documents,
            lower.thresh=seq(1,500, by=10))
dev.off()

# now lets trim terms that appear very infrequently, and terms that appear in
# the overwhelming majority of documents:
dtm <- dfm_trim(dtm,
                min_termfreq = 20,
                max_docfreq = 700)

# look at number of features again:
dtm

# to make things easier down the road, I am going to convert this to an
# stm object and pull out some variables for use later:
out <- quanteda::convert(dtm,
                         to = "stm")

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# lets fit a simple topic model with twenty topics, and the default
# hyperparameters using the STM package. The stm package will fit this topic
# model using a variational approximation which is different from the collapsed
# Gibbs sampler we saw in the lecture, but effectively does the same thing:
lda_fit <- stm(dtm,
               K = 20,
               seed = 12345,
               verbose = TRUE)

# we can start by looking at the top 8 terms in each of the topics,
# along with thier overall proportion in the corpus:
pdf(file = "~/Desktop/Topic_Summaries.pdf",
    width = 12,
    height = 7)
plot.STM(lda_fit,
         type="summary",
         n = 8)
dev.off()

# we can also just display the top 20 words in each topic:
pdf(file = "~/Desktop/Topic_Words.pdf",
    width = 8,
    height = 11)
plot.STM(lda_fit,
         type="labels",
         topics = 1:10)
dev.off()

# we can also pull out documents that are most highly associated associated
# with a topic:
findThoughts(lda_fit,
             texts = texts(data_corpus_guardian),
             topics = 8,
             n = 1)

# finally we can get out estimates of topic correlcations within documents:
cors <- topicCorr(lda_fit)

# and make a network plot:
plot(cors)


################ Working with a larger corpus in MALLET ##############
# load in data from term category associations lab:
load("~/Desktop/Example_Bills_Corpus_Object.RData")

summary(bills)

# process our data:
dtm <- dfm(bills,
           remove_punct = TRUE,
           remove = stopwords("english"),
           ngrams = 1:3)

# look at vocabulary size
dtm
summary(rowSums(dtm))

# now lets trim the vocabulary to make things easier to work with:
dtm <- dfm_trim(dtm,
                min_termfreq = 10)

# look at vocabulary size
dtm
summary(rowSums(dtm))

# You are welcome to try running this, will take ~15-30 minutes+ depending on
# your computer setup
# lda_fit <- stm(dtm,
#                K = 30,
#                verbose = TRUE)
# plot.STM(lda_fit,
#          n = 4)

# now we convert to a simple triplet matrix
dtm_trip <- SpeedReader::convert_quanteda_to_slam(dtm)

# Lets try running a topic model using mallet:
# http://mallet.cs.umass.edu/topics.php
# This implementation is much more scalable than the version in stm, but
# requires a bit more legwork to get it working

setwd("~/Desktop")
lda_mallet <- mallet_lda(
    documents = dtm_trip,
    topics = 30,
    iterations = 500,
    burnin = 100,
    alpha = 1,
    beta = 0.01,
    hyperparameter_optimization_interval = 20,
    cores = 1,
    delete_intermediate_files = FALSE)

# lets look at the top words in each topic:
topic_output <- cbind(lda_mallet$topic_metadata,
                      lda_mallet$topic_top_words)



# we can see how our results would change is we were to use ACMI preprocessing:
# start by pulling out the document covariates
document_covariates <- docvars(dtm)
# generate a contingency table:
topic_party_table <- contingency_table(
    metadata = document_covariates,
    document_term_matrix = dtm_trip,
    variables_to_use = c("topic","party"),
    threshold = 100
)

# get ACMI contributions:
acmi_contribs <- ACMI_contribution(topic_party_table)

# remove all the negative contribution terms:
dtm_acmi <- dtm_trip[,-acmi_contribs$negative_vocab]

# now find the documents that no longer have any words in them:
rem <- which(slam::row_sums(dtm_acmi) == 0)

# remove these documents:
docvars_acmi <- document_covariates[-rem,]
dtm_acmi <- dtm_acmi[-rem,]


setwd("~/Desktop")
lda_acmi <- mallet_lda(
    documents = dtm_acmi,
    topics = 30,
    iterations = 1300,
    burnin = 800,
    alpha = 1,
    beta = 0.01,
    hyperparameter_optimization_interval = 20,
    cores = 1,
    delete_intermediate_files = TRUE)

# lets look at the top words in each topic:
topic_output_acmi <- cbind(lda_acmi$topic_metadata,
                           lda_acmi$topic_top_words)










