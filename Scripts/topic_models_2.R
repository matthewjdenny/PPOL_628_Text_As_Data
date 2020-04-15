# Supervised Learning with Text
rm(list = ls())

# load in packages
library(quanteda)
# devtools::install_github("matthewjdenny/SpeedReader")
library(SpeedReader)
library(ggplot2)

# for running topic models:
# install.packages("stm",dependencies = TRUE)
library(stm)

# optional fun package that makes cool topic clustering plots:
install.packages("stmCorrViz",dependencies = TRUE)
library(stmCorrViz)

############### Topic Models using STM package #################
# much of what we are going to cover today can be found in written form here:
# https://github.com/dondealban/learning-stm and here:
# https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf

# load in data from term category associations lab:
load("~/Desktop/Example_Bills_Corpus_Object.RData")

# take a look at the corpus:
head(summary(bills))

# take a subset to only senate bill sections:
bills_subset <- corpus_subset(bills, chamber == "1" & party != "Other")

# create a dfm object
dtm <- dfm(bills_subset,
           remove_punct = TRUE,
           remove_numbers = TRUE,
           remove = stopwords("english"))

# look at number of features:
dtm

# now lets trim terms that appear very infrequently, and terms that appear in
# the overwhelming majority of documents:
dtm <- dfm_trim(dtm,
                min_docfreq = 20,
                max_docfreq = 1800)

# look at number of features again:
dtm
summary(rowSums(dtm))


# lets fit a topic model with covariate effects. Note that this will take
# 10 minutes, so for the purposes of this lab, I am going to save and upload
# the results.
stm_fit <- stm(dtm,
               prevalence = as.formula("~topic+num_cosponsors"),
               content = as.formula("~party"),
               data = docvars(dtm),
               K = 30,
               seed = 12345,
               verbose = TRUE)

# alternatively, we can just load the model run back in:
# save(stm_fit, file = "~/Desktop/STM_fit.RData")
load("~/Desktop/STM_fit.RData")

# we can start by looking at the top 8 terms in each of the topics,
# along with thier overall proportion in the corpus:
pdf(file = "~/Desktop/Topic_Summaries.pdf",
    width = 12,
    height = 10)
plot.STM(stm_fit,
         type="summary",
         n = 5)
dev.off()

# optional argument when no content covariates: labeltype = "frex"

# lets take a look at topic quality
out <- quanteda::convert(dtm, to = "stm")
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
# will make a plot for models without content covariates, only returns coherence
# scores for each topic when content covariates are specified:
topicQuality(model=stm_fit, documents=docs)

# much more detailed term ranking within topics:
print(sageLabels(stm_fit))

# get estimated effects
estimates <- estimateEffect(
    1:30~topic+num_cosponsors,
    stm_fit,
    metadata = meta,
    uncertainty = "Global")

# plot
pdf(file = "~/Desktop/Cosponsor_Estimates.pdf",
    width = 6,
    height = 6)
plot(estimates,
     "num_cosponsors",
     method = "continuous",
     topics = c(20,25),
     xlab = "Number of Cosponsors",
     labeltype = "custom",
     custom.labels = c("Medicare Reform",
                       "Mental Health (Veterans)"))
dev.off()


# pull out estimates for
estimates2 <- estimateEffect(
    1:30~party,
    stm_fit,
    metadata = meta,
    uncertainty = "Global")

# plot
pdf(file = "~/Desktop/Party_Estimates.pdf",
    width = 6,
    height = 6)
plot(estimates2,
     covariate = "party",
     topics = c(15,28),
     model = stm_fit,
     method = "difference",
     cov.value1 = "Democrat",
     cov.value2 = "Republican",
     xlab = "More Democratic ... More Republican",
     main = "Effect of Democrat vs. Republican Sponsorship",
     labeltype = "custom",
     xlim=c(-.15,.05),
     custom.labels = c("Immigration Reform",
                       "Charter Schools"))
dev.off()

# we can also make a "perspectives" plot with words most strongly associated
# to each party within topic:
pdf(file = "~/Desktop/Party_Terms.pdf",
    width = 8,
    height = 8)
plot(stm_fit, type="perspectives", topics=15, n = 60)
dev.off()

# we can also get an interesting visualization of the topic heirarchy:
stmCorrViz(stm_fit, "~/Desktop/stm-interactive-correlation.html",
           documents_raw = texts(bills_subset),
           documents_matrix = out$documents)

# use findThoughts function for topic validation
findThoughts(stm_fit,
             texts = texts(bills_subset),
             topics = 15,
             n = 2)

# we can also extracta trace plot of the approximate model log likelihood:
pdf(file = "~/Desktop/Trace_Plot.pdf",
    width = 6,
    height = 4)
plot(stm_fit$convergence$bound,
     ylab="Approximate Objective",
     main="Trace Plot")
dev.off()



################ Convergence Diagnostics in MALLET ##############

# process our data:
dtm <- dfm(bills,
           remove_punct = TRUE,
           remove = stopwords("english"),
           ngrams = 1:3)

# now lets trim the vocabulary to make things easier to work with:
dtm <- dfm_trim(dtm,
                min_termfreq = 10)

# now we convert to a simple triplet matrix
dtm_trip <- SpeedReader::convert_quanteda_to_slam(dtm)

# ACMI preprocessing
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

# run the topic model (this will take half an hour or so):
setwd("~/Desktop")
lda_acmi <- mallet_lda(
    documents = dtm_acmi,
    topics = 30,
    iterations = 20000,
    burnin = 10000,
    alpha = 1,
    beta = 0.01,
    hyperparameter_optimization_interval = 20,
    cores = 1,
    delete_intermediate_files = TRUE)

# alternatively, we can just load the model run back in:
# save(lda_acmi, file = "MALLET_LDA_fit.RData")
load("~/Desktop/MALLET_LDA_fit.RData")

# code that generates the trace plot and geweke diagnostic

geweke_plot <- function(mallet_lda_output,
                        burnin) {
    # pull out some intermediate variables
    LL_Token <- mallet_lda_output$lda_trace_stats$LL_Token
    iteration <- mallet_lda_output$lda_trace_stats$iteration

    # color for dots:
    UMASS_BLUE <- rgb(51,51,153,255,maxColorValue = 255)
    plot( y = LL_Token[ceiling(burnin/10):length(LL_Token)],
        x = iteration[ceiling(burnin/10):length(LL_Token)],
        pch = 19, col = UMASS_BLUE,
        main = paste(
            "Un-Normalized Topic Model Log Likelihood \n",
            " Geweke Statistic for Last",
            length(ceiling(burnin/10):length(LL_Token)),
            "Iterations:",
            round(coda::geweke.diag(
                LL_Token[ceiling(burnin/10):length(LL_Token)])$z,
                2)),
        xlab = "Iteration", ylab = "Log Likelihood",
        cex.lab = 2, cex.axis = 1.4, cex.main = 1.4)
}

# try it out with different burnin values:
geweke_plot(lda_acmi,
            1)

geweke_plot(lda_acmi,
            5000)

geweke_plot(lda_acmi,
            10000)

geweke_plot(lda_acmi,
            19000)

geweke_plot(lda_acmi,
            15000)

# there is also a function to calculate topic coherence for arbitrary top
# terms lists :
topic_coherence(lda_acmi$topic_top_words$top_word_1,
                as.matrix(dtm_acmi),
                vocabulary = colnames(dtm_acmi))






