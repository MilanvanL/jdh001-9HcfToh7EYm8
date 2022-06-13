# This is the script for the analysis of parliamentary texts as used in the article: 
# 'Dialects of Discord. Changing vocabularies in the Dutch Cruise Missile Discussion', Journal of Digital History, 2022.
# By Ralf Futselaar and Milan van Lange.

# This script applies the Word2Vec technique on Dutch parliamentary data (Handelingen der Staten-Generaal).
# The bnosac Word2Vec package is used to train Word Embedding Models. See also https://github.com/bnosac/word2vec. 

# This script is meantfor demonstration purposes. It uses data from the 1970-1975 period. The article uses mroe data, 
# for more periods, but these are not included in this example, because of space constraints. The authors will be happy 
# to provide all data on request. 

# Load installed packages from library
library(magrittr)
library(tm)
library(word2vec)
library(XML)
library(qdap)
library(stringr)
library(dplyr)
library(readtext)
library(readr)
library(RCurl)

# Load the text data
# A pre-processed version of parliamentary texts is used. 
# Punctuation and stop words are removed from the texts and all letters are set to lower case. 
# Functions removePunctuation, removeWords, and tolower from package 'tm' are used.
# The pre-processed text data are stored as a single .csv-file on GitHub.
# This file can be downloaded from GitHub using the following command:
files <- getURL("https://raw.githubusercontent.com/jdh-observer/jdh001-9HcfToh7EYm8/main/parl_texts_7075.csv")

# Read the file into the RStudio Environment as data.frame
files2 <- read.csv(text = files, encoding="UTF-8")

# Subset party-specific data and train word2vec model for each party--------------------------------------------------

# 1. CPN (Communistische Partij Nederland)
# Subset period and party, set column "text" as character vector to serve as input for word2vec package
files2 %>%
  filter(str_detect(doc_id, "cpn")) -> cpn.7075
cpn.7075 <- as.character(cpn.7075$text)

# Train word2vec model
model.cpn.7075 <- word2vec(x = cpn.7075, min_count = 5, threads = 4, type = 'skip-gram', dim = 100, iter = 20 )

# Create matrix
emb.cpn.7075 <- as.matrix(model.cpn.7075)

# 2. VVD (Volkspartij voor Vrijheid en Democratie)
# Subset period and party, set column "text" as character vector to serve as input for word2vec package
files2 %>%
  filter(str_detect(doc_id, "vvd")) -> vvd.7075
vvd.7075 <- as.character(vvd.7075$text)

# Train word2vec model
model.vvd.7075 <- word2vec(x = vvd.7075, min_count = 5, threads = 4, type = 'skip-gram', dim = 100)

# Create matrix
emb.vvd.7075 <- as.matrix(model.vvd.7075)

# 3. PvdA (Partij van de Arbeid)
# Subset period and party, set column "text" as character vector to serve as input for word2vec package
files2 %>%
  filter(str_detect(doc_id, "pvda")) -> pvda.7075
pvda.7075 <- as.character(pvda.7075$text)

# Train word2vec model
model.pvda.7075 <- word2vec(x = pvda.7075, min_count = 5, threads = 4, type = 'skip-gram', dim = 100)

# Create matrix
emb.pvda.7075 <- as.matrix(model.pvda.7075)

# 4. CDA (Christen-Democratische Appel) (NOTE: this party did not yet exist before 1980) 
# Subset specific year, set as character vector to serve as input for word2vec package
files2 %>%
  filter(str_detect(doc_id, "cda")) -> cda.7075
cda.7075 <- as.character(cda.7075$text)

# Train word2vec model
model.cda.7075 <- word2vec(x = cda.7075, min_count = 5, threads = 4, type = 'skip-gram', dim = 100)

# Create matrix
emb.cda.7075 <- as.matrix(model.cda.7075)

# Now that we have trained the models we continue with the analysis.

# Create the word lists in the RStudio environment. These lists are generic as they are used for every party.
words.weapon <-  c("kernwapen", "kernwapens", "atoomwapen", "atoomwapens", "kruisraket", "kruisraketten", "kruisvluchtwapen", "kruisvluchtwapens", "lanceraket", "lanceraketten", "navokernwapen", "navokernwapens" )
words.prol <- c("afschrikking",  "tactischnucleaire",  "proliferatieverdrag",  "strategischnucleaire", "proliferatie", "afschrikkingsevenwicht",  "afschrikkingsstrategie", "atoomparaplu", "waarborgenstelsel", "kernwapenstrategie" , "afschrikwekkende", "afschrikkingstheorie", "deterrent", "afschrikkingsfunctie", "afschrikkingspolitiek", "afschrikkingsrol", "afschrikkingswapen", "afschrikkingswapens", "afschrikkingsmacht") 
words.nonprol <- c("ontwapening", "nonproliferatieverdrag", "kernwapenvrije", "kernwapenvrij", "denuclearisatie", "atoomvrije", "nonproliferatie",  "wapenbeperking", "wapenbeheersingsbesprekingen", "ontwapeningsbesprekingen", "kernstop", "ontwapeningsoverleg", "denuclearisering", "atoomvrij",   "gedenucleariseerd", "wapenbeheersing", "ontwapeningsonderhandelingen", "wapenbeheersingsonderhandelingen", "wapenvermindering") 

# Use the trained word2vec models and the word lists for analysis

# 1. CPN
# Create 'kernwapen' vector 
wv.cpn <- predict(model.cpn.7075, newdata = words.weapon, type = "embedding")
wv.cpn <- na.omit(wv.cpn)
comb.wv.cpn <- colMeans(wv.cpn) 

# Extract 100 nearest neighbours from 'kernwapen' in vector space
nns_wv.cpn <- predict(model.cpn.7075, newdata = comb.wv.cpn, type="nearest", top_n=100)
vecs_nns_nuc.100.cpn <- emb.cpn.7075[nns_wv.cpn$term,]

# Create 'proliferation' vector
pv.cpn <- predict(model.cpn.7075, newdata = words.prol, type = "embedding")
pv.cpn <- na.omit(pv.cpn)
comb.pv.cpn <- colMeans(pv.cpn) 

# Create 'non-proliferation' vector
nv.cpn <- predict(model.cpn.7075, newdata = words.nonprol, type = "embedding")
nv.cpn <- na.omit(nv.cpn)
comb.nv.cpn <- colMeans(nv.cpn) 

# Calculate distance (cosine similarity). We use these scores to plot the results 

# Create scores for plotting by comparing vectors of 'kernwapen' and proliferation vs non-proliferation
score_prol.cpn <- as.data.frame(word2vec_similarity(vecs_nns_nuc.100.cpn, comb.pv.cpn))
score_nonprol.cpn <- as.data.frame(word2vec_similarity(vecs_nns_nuc.100.cpn, comb.nv.cpn))

# 2. VVD
# Create 'kernwapen' vector 
wv.vvd <- predict(model.vvd.7075, newdata = words.weapon, type = "embedding")
wv.vvd <- na.omit(wv.vvd)
comb.wv.vvd <- colMeans(wv.vvd) 

# Extract 100 nearest neighbours from 'kernwapen' in vector space
nns_wv.vvd <- predict(model.vvd.7075, newdata = comb.wv.vvd, type="nearest", top_n=100)
vecs_nns_nuc.100.vvd <- emb.vvd.7075[nns_wv.vvd$term,]

# Create 'proliferation' vector
pv.vvd <- predict(model.vvd.7075, newdata = words.prol, type = "embedding")
pv.vvd <- na.omit(pv.vvd)
comb.pv.vvd <- colMeans(pv.vvd) 

# Create 'non-proliferation' vector
nv.vvd <- predict(model.vvd.7075, newdata = words.nonprol, type = "embedding")
nv.vvd <- na.omit(nv.vvd)
comb.nv.vvd <- colMeans(nv.vvd) 

# Calculate distance (cosine similarity). We use these scores to plot the results

# Create scores for plotting by comparing vectors of 'kernwapen' and proliferation vs non-proliferation
score_prol.vvd <- as.data.frame(word2vec_similarity(vecs_nns_nuc.100.vvd, comb.pv.vvd))
score_nonprol.vvd <- as.data.frame(word2vec_similarity(vecs_nns_nuc.100.vvd, comb.nv.vvd))

# 3. PvdA
# Create 'kernwapen' vector 
wv.pvda <- predict(model.pvda.7075, newdata = words.weapon, type = "embedding")
wv.pvda <- na.omit(wv.pvda)
comb.wv.pvda <- colMeans(wv.pvda)

# Extract 100 nearest neighbours from 'kernwapen' in vector space
nns_wv.pvda <- predict(model.pvda.7075, newdata = comb.wv.pvda, type="nearest", top_n=100)
vecs_nns_nuc.100.pvda <- emb.pvda.7075[nns_wv.pvda$term,]

# Create 'proliferation' vector
pv.pvda <- predict(model.pvda.7075, newdata = words.prol, type = "embedding")
pv.pvda <- na.omit(pv.pvda)
comb.pv.pvda <- colMeans(pv.pvda) 

# Create 'non-proliferation' vector
nv.pvda <- predict(model.pvda.7075, newdata = words.nonprol, type = "embedding")
nv.pvda <- na.omit(nv.pvda)
comb.nv.pvda <- colMeans(nv.pvda) 

# Calculate distance (cosine similarity). We use these scores to plot the results 

# Create scores for plotting by comparing vectors of 'kernwapen' and proliferation vs non-proliferation
score_prol.pvda <- as.data.frame(word2vec_similarity(vecs_nns_nuc.100.pvda, comb.pv.pvda))
score_nonprol.pvda <- as.data.frame(word2vec_similarity(vecs_nns_nuc.100.pvda, comb.nv.pvda))

# 4. CDA
# Create 'kernwapen' vector 
wv.cda <- predict(model.cda.7075, newdata = words.weapon, type = "embedding")
wv.cda <- na.omit(wv.cda)
comb.wv.cda <- colMeans(wv.cda) 

# Extract 100 nearest neighbours from 'kernwapen' in vector space
nns_wv.cda <- predict(model.cda.7075, newdata = comb.wv.cda, type="nearest", top_n=100)
vecs_nns_nuc.100.cda <- emb.cda.7075[nns_wv.cda$term,]

# Create 'proliferation' vector
pv.cda <- predict(model.cda.7075, newdata = words.prol, type = "embedding")
pv.cda <- na.omit(pv.cda)
comb.pv.cda <- colMeans(pv.cda) 

# Create 'non-proliferation' vector
nv.cda <- predict(model.cda.7075, newdata = words.nonprol, type = "embedding")
nv.cda <- na.omit(nv.cda)
comb.nv.cda <- colMeans(nv.cda) 

# Calculate distance (cosine similarity). We use these scores to plot the results

# Create scores for plotting by comparing vectors of 'kernwapen' and proliferation vs non-proliferation
score_prol.cda <- as.data.frame(word2vec_similarity(vecs_nns_nuc.100.cda, comb.pv.cda))
score_nonprol.cda <- as.data.frame(word2vec_similarity(vecs_nns_nuc.100.cda, comb.nv.cda))

# Plot the results ----------------------------------------------------------------
# Set working directory to make sure the outputs appear in the right place
setwd(here("output"))

# Create the plot, plot scores for each party, use a different colour for each party.
plot(score_prol.pvda$V1, score_nonprol.pvda$V1,ylim=c(0, 1.0), xlim=c(0,1.0), xlab = "Proliferation", ylab = 'Non-proliferation',
type='p', pch=16, col=c("blue"),
main="
     Party-specific nuclear weapon vocabularies plotted by their
     similarity to proliferation words(x) and non-proliferation words(y) 1970-1975")
points(score_prol.cpn$V1, score_nonprol.cpn$V1, pch=16, col=c("red"))
points(score_prol.vvd$V1, score_nonprol.vvd$V1, pch=16, col=c("black"))
#points(score_prol.cda$V1, score_nonprol.cda$V1, pch=16, col=c("green"))

# Plot abline
abline(a=0,b=1)

# Create legend
legend('bottomright', legend=c("CPN", "VVD", "PvdA"),
       col=c("red", "black", "blue"), lwd=11, cex=0.8)
           
# The End 



