# This script tries to use Word2Vec technique on Handelingen data
# For Dialects of Discord on the party level
# Milan van Lange, June 2021

library(magrittr)
library(tm)
remotes::install_github("bnosac/word2vec")
library(word2vec)
library(XML)
library(qdap)
library(stringr)
library(dplyr)
library(lumberjack)
library(readtext)

# Pre-processed .txt files are used (without interpunction, stowords removed, all letters to lower case, etc.)

# Train word2vec model --------------------------------------------------

# load prepared txts in envir
# create object with directory where files are in

# Train party specific models ---------------------------------------------

# create object with directory where files are in # this is workstation-specific!
data.dir2 <- c("https://raw.github.com/MilanvanL/jdh001-9HcfToh7EYm8/data/")


# load all files with .txt file type. make sure only the txt files needed are in the directory!
files2 <- readtext(paste0(data.dir2, "raw_texts_parl*" ))


# CPN
# Subset specific year, set as character vector to serve as input for word2vec package
files2 %>%
  filter(str_detect(doc_id, "cpn")) -> cpn.7075

# set as character
cpn.7075 <- as.character(cpn.7075$text)

# train model
# train word2vec
model.cpn.7075 <- word2vec(x = cpn.7075, min_count = 5, threads = 4, type = 'skip-gram', dim = 100, iter = 20 )

# create matrix
emb.cpn.7075 <- as.matrix(model.cpn.7075)

# VVD

# Subset specific year, set as character vector to serve as input for word2vec package
files2 %>%
  filter(str_detect(doc_id, "vvd")) -> vvd.7075

# set as character
vvd.7075 <- as.character(vvd.7075$text)

# train model
# train word2vec
model.vvd.7075 <- word2vec(x = vvd.7075, min_count = 5, threads = 4, type = 'skip-gram', dim = 100)

# create matrix
emb.vvd.7075 <- as.matrix(model.vvd.7075)

# PvdA

# Subset specific year, set as character vector to serve as input for word2vec package
files2 %>%
  filter(str_detect(doc_id, "pvda")) -> pvda.7075

# set as character
pvda.7075 <- as.character(pvda.7075$text)

# train model
# train word2vec
model.pvda.7075 <- word2vec(x = pvda.7075, min_count = 5, threads = 4, type = 'skip-gram', dim = 100)

# create matrix
emb.pvda.7075 <- as.matrix(model.pvda.7075)

# CDA
# Subset specific year, set as character vector to serve as input for word2vec package
files2 %>%
  filter(str_detect(doc_id, "cda")) -> cda.7075

# set as character
cda.7075 <- as.character(cda.7075$text)

# train model
# train word2vec
model.cda.7075 <- word2vec(x = cda.7075, min_count = 5, threads = 4, type = 'skip-gram', dim = 100)

# create matrix
emb.cda.7075 <- as.matrix(model.cda.7075)

# Training complete, go to analysis in next script 2_analyse_plot_7075_parties.R



