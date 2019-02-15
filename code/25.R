library(tidyverse)
library(tidytext)
library(data.table)
library(Matrix)
library(qdapRegex)
library(keras)
library(reticulate)
seed <- 3809360 #sample(1:9999999, 1)
cat("using seed: ", seed, "\n")
py_set_seed(seed, disable_hash_randomization = TRUE)
keras::use_session_with_seed(seed, F, F, F)
set.seed(seed)

# setwd("D:/kaggle/quora/R/")
# setwd("M:/kaggle/quora/R/")
# setwd("/mnt/nas/volume1/DataScience/kaggle/quora/R")

options(scipen=99999) # turn off scientific display

tr <- fread('../input/train.csv', data.table = FALSE, encoding = "UTF-8")

tr <- tr[sample(nrow(tr)),]
te <- fread('../input/test.csv', data.table = FALSE, encoding = "UTF-8")
tri <- 1:nrow(tr)

setDT(tr)
tr[,question_text:= str_replace_all(question_text, "aren't", "are not") ] 
tr[,question_text:= str_replace_all(question_text, "can't" , "cannot") ] 
tr[,question_text:= str_replace_all(question_text, "couldn't" , "could not") ] 
tr[,question_text:= str_replace_all(question_text, "didn't" , "did not") ] 
tr[,question_text:= str_replace_all(question_text, "doesn't" , "does not") ] 
tr[,question_text:= str_replace_all(question_text, "don't" , "do not") ] 
tr[,question_text:= str_replace_all(question_text, "hadn't" , "had not") ] 
tr[,question_text:= str_replace_all(question_text, "hasn't" , "has not") ] 
tr[,question_text:= str_replace_all(question_text, "haven't" , "have not") ] 
tr[,question_text:= str_replace_all(question_text, "he'd" , "he would") ] 
tr[,question_text:= str_replace_all(question_text, "he'll" , "he will") ] 
tr[,question_text:= str_replace_all(question_text, "he's" , "he is") ] 
tr[,question_text:= str_replace_all(question_text, "i'd" , "I would") ] 
tr[,question_text:= str_replace_all(question_text, "i'll" , "I will") ] 
tr[,question_text:= str_replace_all(question_text, "i'm" , "I am") ] 
tr[,question_text:= str_replace_all(question_text, "isn't" , "is not") ] 
tr[,question_text:= str_replace_all(question_text, "it's" , "it is") ] 
tr[,question_text:= str_replace_all(question_text, "it'll","it will") ] 
tr[,question_text:= str_replace_all(question_text, "i've" , "I have") ] 
tr[,question_text:= str_replace_all(question_text, "let's" , "let us") ] 
tr[,question_text:= str_replace_all(question_text, "mightn't" , "might not") ] 
tr[,question_text:= str_replace_all(question_text, "mustn't" , "must not") ] 
tr[,question_text:= str_replace_all(question_text, "shan't" , "shall not") ] 
tr[,question_text:= str_replace_all(question_text, "she'd" , "she would") ] 
tr[,question_text:= str_replace_all(question_text, "she'll" , "she will") ] 
tr[,question_text:= str_replace_all(question_text, "she's" , "she is") ] 
tr[,question_text:= str_replace_all(question_text, "shouldn't" , "should not") ] 
tr[,question_text:= str_replace_all(question_text, "that's" , "that is") ] 
tr[,question_text:= str_replace_all(question_text, "there's" , "there is") ] 
tr[,question_text:= str_replace_all(question_text, "they'd" , "they would") ] 
tr[,question_text:= str_replace_all(question_text, "they'll" , "they will") ] 
tr[,question_text:= str_replace_all(question_text, "they're" , "they are") ] 
tr[,question_text:= str_replace_all(question_text, "they've" , "they have") ] 
tr[,question_text:= str_replace_all(question_text, "we'd" , "we would") ] 
tr[,question_text:= str_replace_all(question_text, "we're" , "we are") ] 
tr[,question_text:= str_replace_all(question_text, "weren't" , "were not") ] 
tr[,question_text:= str_replace_all(question_text, "we've" , "we have") ] 
tr[,question_text:= str_replace_all(question_text, "what'll" , "what will") ] 
tr[,question_text:= str_replace_all(question_text, "what're" , "what are") ] 
tr[,question_text:= str_replace_all(question_text, "what's" , "what is") ] 
tr[,question_text:= str_replace_all(question_text, "what've" , "what have") ] 
tr[,question_text:= str_replace_all(question_text, "where's" , "where is") ] 
tr[,question_text:= str_replace_all(question_text, "who'd" , "who would") ] 
tr[,question_text:= str_replace_all(question_text, "who'll" , "who will") ] 
tr[,question_text:= str_replace_all(question_text, "who're" , "who are") ] 
tr[,question_text:= str_replace_all(question_text, "who's" , "who is") ] 
tr[,question_text:= str_replace_all(question_text, "who've" , "who have") ] 
tr[,question_text:= str_replace_all(question_text, "won't" , "will not") ] 
tr[,question_text:= str_replace_all(question_text, "wouldn't" , "would not") ] 
tr[,question_text:= str_replace_all(question_text, "you'd" , "you would") ] 
tr[,question_text:= str_replace_all(question_text, "you'll" , "you will") ] 
tr[,question_text:= str_replace_all(question_text, "you're" , "you are") ] 
tr[,question_text:= str_replace_all(question_text, "you've" , "you have") ] 
tr[,question_text:= str_replace_all(question_text, "'re", " are") ] 
tr[,question_text:= str_replace_all(question_text, "wasn't", "was not") ] 
tr[,question_text:= str_replace_all(question_text, "we'll"," will") ] 
tr[,question_text:= str_replace_all(question_text, "didn't", "did not") ] 
tr[,question_text:= str_replace_all(question_text, "tryin'","trying")]
tr[,question_text:= str_replace_all(question_text, "gey","gay")]
tr[,question_text:= str_replace_all(question_text, "fck","fuck")]
tr[,question_text:= str_replace_all(question_text, "gayyy","gay")]
tr[,question_text:= str_replace_all(question_text, "mem","mother")]
tr[,question_text:= str_replace_all(question_text, "dont","do not")]
tr[,question_text:= str_replace_all(question_text, "wont","would not")]
tr[,question_text:= str_replace_all(question_text, "cant","could not")]

tr[,question_text:= str_replace_all(question_text,'colour', 'color')] 
tr[,question_text:= str_replace_all(question_text,'centre', 'center')] 
tr[,question_text:= str_replace_all(question_text,'favourite', 'favorite')] 
tr[,question_text:= str_replace_all(question_text,'travelling', 'traveling')] 
tr[,question_text:= str_replace_all(question_text,'counselling', 'counseling')] 
tr[,question_text:= str_replace_all(question_text,'theatre', 'theater')]
tr[,question_text:= str_replace_all(question_text,'cancelled', 'canceled')] 
tr[,question_text:= str_replace_all(question_text,'labour', 'labor')]
tr[,question_text:= str_replace_all(question_text,'organisation', 'organization')]
tr[,question_text:= str_replace_all(question_text,'wwii', 'world war 2')]
tr[,question_text:= str_replace_all(question_text,'citicise', 'criticize')] 
tr[,question_text:= str_replace_all(question_text,'youtu ', 'youtube ')]
tr[,question_text:= str_replace_all(question_text,'Qoura', 'Quora')]
tr[,question_text:= str_replace_all(question_text,'sallary', 'salary')] 
tr[,question_text:= str_replace_all(question_text,'Whta', 'What')]
tr[,question_text:= str_replace_all(question_text,'narcisist', 'narcissist')]
tr[,question_text:= str_replace_all(question_text,'howdo', 'how do')]
tr[,question_text:= str_replace_all(question_text,'whatare', 'what are')]
tr[,question_text:= str_replace_all(question_text,'howcan', 'how can')]
tr[,question_text:= str_replace_all(question_text,'howmuch', 'how much')] 
tr[,question_text:= str_replace_all(question_text,'howmany', 'how many')]
tr[,question_text:= str_replace_all(question_text,'whydo', 'why do')]
tr[,question_text:= str_replace_all(question_text,'doI', 'do I')]
tr[,question_text:= str_replace_all(question_text,'theBest', 'the best')]
tr[,question_text:= str_replace_all(question_text,'howdoes', 'how does')]
tr[,question_text:= str_replace_all(question_text,'mastrubation', 'masturbation')] 
tr[,question_text:= str_replace_all(question_text,'mastrubate', 'masturbate')]
tr[,question_text:= str_replace_all(question_text,"mastrubating", 'masturbating')] 
tr[,question_text:= str_replace_all(question_text,'pennis', 'penis')]
tr[,question_text:= str_replace_all(question_text,'Etherium', 'Ethereum')] 
tr[,question_text:= str_replace_all(question_text,'narcissit', 'narcissist')] 
tr[,question_text:= str_replace_all(question_text,'bigdata', 'big data')]
tr[,question_text:= str_replace_all(question_text,'2k17', '2017')]
tr[,question_text:= str_replace_all(question_text,'2k18', '2018')]
tr[,question_text:= str_replace_all(question_text,'qouta', 'quota')] 
tr[,question_text:= str_replace_all(question_text,'exboyfriend', 'ex boyfriend')] 
tr[,question_text:= str_replace_all(question_text,'airhostess', 'air hostess')]
tr[,question_text:= str_replace_all(question_text,"whst", 'what')] 
tr[,question_text:= str_replace_all(question_text,'watsapp', 'whatsapp')] 
tr[,question_text:= str_replace_all(question_text,'demonitisation', 'demonetization')]
tr[,question_text:= str_replace_all(question_text,'demonitization', 'demonetization')]
tr[,question_text:= str_replace_all(question_text,'demonetisation', 'demonetization')]

print("train done")

setDT(te)
te[,question_text:= str_replace_all(question_text, "aren't", "are not") ] 
te[,question_text:= str_replace_all(question_text, "can't" , "cannot") ] 
te[,question_text:= str_replace_all(question_text, "couldn't" , "could not") ] 
te[,question_text:= str_replace_all(question_text, "didn't" , "did not") ] 
te[,question_text:= str_replace_all(question_text, "doesn't" , "does not") ] 
te[,question_text:= str_replace_all(question_text, "don't" , "do not") ] 
te[,question_text:= str_replace_all(question_text, "hadn't" , "had not") ] 
te[,question_text:= str_replace_all(question_text, "hasn't" , "has not") ] 
te[,question_text:= str_replace_all(question_text, "haven't" , "have not") ] 
te[,question_text:= str_replace_all(question_text, "he'd" , "he would") ] 
te[,question_text:= str_replace_all(question_text, "he'll" , "he will") ] 
te[,question_text:= str_replace_all(question_text, "he's" , "he is") ] 
te[,question_text:= str_replace_all(question_text, "i'd" , "I would") ] 
te[,question_text:= str_replace_all(question_text, "i'll" , "I will") ] 
te[,question_text:= str_replace_all(question_text, "i'm" , "I am") ] 
te[,question_text:= str_replace_all(question_text, "isn't" , "is not") ] 
te[,question_text:= str_replace_all(question_text, "it's" , "it is") ] 
te[,question_text:= str_replace_all(question_text, "it'll","it will") ] 
te[,question_text:= str_replace_all(question_text, "i've" , "I have") ] 
te[,question_text:= str_replace_all(question_text, "let's" , "let us") ] 
te[,question_text:= str_replace_all(question_text, "mightn't" , "might not") ] 
te[,question_text:= str_replace_all(question_text, "mustn't" , "must not") ] 
te[,question_text:= str_replace_all(question_text, "shan't" , "shall not") ] 
te[,question_text:= str_replace_all(question_text, "she'd" , "she would") ] 
te[,question_text:= str_replace_all(question_text, "she'll" , "she will") ] 
te[,question_text:= str_replace_all(question_text, "she's" , "she is") ] 
te[,question_text:= str_replace_all(question_text, "shouldn't" , "should not") ] 
te[,question_text:= str_replace_all(question_text, "that's" , "that is") ] 
te[,question_text:= str_replace_all(question_text, "there's" , "there is") ] 
te[,question_text:= str_replace_all(question_text, "they'd" , "they would") ] 
te[,question_text:= str_replace_all(question_text, "they'll" , "they will") ] 
te[,question_text:= str_replace_all(question_text, "they're" , "they are") ] 
te[,question_text:= str_replace_all(question_text, "they've" , "they have") ] 
te[,question_text:= str_replace_all(question_text, "we'd" , "we would") ] 
te[,question_text:= str_replace_all(question_text, "we're" , "we are") ] 
te[,question_text:= str_replace_all(question_text, "weren't" , "were not") ] 
te[,question_text:= str_replace_all(question_text, "we've" , "we have") ] 
te[,question_text:= str_replace_all(question_text, "what'll" , "what will") ] 
te[,question_text:= str_replace_all(question_text, "what're" , "what are") ] 
te[,question_text:= str_replace_all(question_text, "what's" , "what is") ] 
te[,question_text:= str_replace_all(question_text, "what've" , "what have") ] 
te[,question_text:= str_replace_all(question_text, "where's" , "where is") ] 
te[,question_text:= str_replace_all(question_text, "who'd" , "who would") ] 
te[,question_text:= str_replace_all(question_text, "who'll" , "who will") ] 
te[,question_text:= str_replace_all(question_text, "who're" , "who are") ] 
te[,question_text:= str_replace_all(question_text, "who's" , "who is") ] 
te[,question_text:= str_replace_all(question_text, "who've" , "who have") ] 
te[,question_text:= str_replace_all(question_text, "won't" , "will not") ] 
te[,question_text:= str_replace_all(question_text, "wouldn't" , "would not") ] 
te[,question_text:= str_replace_all(question_text, "you'd" , "you would") ] 
te[,question_text:= str_replace_all(question_text, "you'll" , "you will") ] 
te[,question_text:= str_replace_all(question_text, "you're" , "you are") ] 
te[,question_text:= str_replace_all(question_text, "you've" , "you have") ] 
te[,question_text:= str_replace_all(question_text, "'re", " are") ] 
te[,question_text:= str_replace_all(question_text, "wasn't", "was not") ] 
te[,question_text:= str_replace_all(question_text, "we'll"," will") ] 
te[,question_text:= str_replace_all(question_text, "didn't", "did not") ] 
te[,question_text:= str_replace_all(question_text, "tryin'","trying")]
te[,question_text:= str_replace_all(question_text, "gey","gay")]
te[,question_text:= str_replace_all(question_text, "fck","fuck")]
te[,question_text:= str_replace_all(question_text, "gayyy","gay")]
te[,question_text:= str_replace_all(question_text, "mem","mother")]
te[,question_text:= str_replace_all(question_text, "dont","do not")]
te[,question_text:= str_replace_all(question_text, "wont","would not")]
te[,question_text:= str_replace_all(question_text, "cant","could not")]

te[,question_text:= str_replace_all(question_text,'colour', 'color')] 
te[,question_text:= str_replace_all(question_text,'centre', 'center')] 
te[,question_text:= str_replace_all(question_text,'favourite', 'favorite')] 
te[,question_text:= str_replace_all(question_text,'travelling', 'traveling')] 
te[,question_text:= str_replace_all(question_text,'counselling', 'counseling')] 
te[,question_text:= str_replace_all(question_text,'theatre', 'theater')]
te[,question_text:= str_replace_all(question_text,'cancelled', 'canceled')] 
te[,question_text:= str_replace_all(question_text,'labour', 'labor')]
te[,question_text:= str_replace_all(question_text,'organisation', 'organization')]
te[,question_text:= str_replace_all(question_text,'wwii', 'world war 2')]
te[,question_text:= str_replace_all(question_text,'citicise', 'criticize')] 
te[,question_text:= str_replace_all(question_text,'youtu ', 'youtube ')]
te[,question_text:= str_replace_all(question_text,'Qoura', 'Quora')]
te[,question_text:= str_replace_all(question_text,'sallary', 'salary')] 
te[,question_text:= str_replace_all(question_text,'Whta', 'What')]
te[,question_text:= str_replace_all(question_text,'narcisist', 'narcissist')]
te[,question_text:= str_replace_all(question_text,'howdo', 'how do')]
te[,question_text:= str_replace_all(question_text,'whatare', 'what are')]
te[,question_text:= str_replace_all(question_text,'howcan', 'how can')]
te[,question_text:= str_replace_all(question_text,'howmuch', 'how much')] 
te[,question_text:= str_replace_all(question_text,'howmany', 'how many')]
te[,question_text:= str_replace_all(question_text,'whydo', 'why do')]
te[,question_text:= str_replace_all(question_text,'doI', 'do I')]
te[,question_text:= str_replace_all(question_text,'theBest', 'the best')]
te[,question_text:= str_replace_all(question_text,'howdoes', 'how does')]
te[,question_text:= str_replace_all(question_text,'mastrubation', 'masturbation')] 
te[,question_text:= str_replace_all(question_text,'mastrubate', 'masturbate')]
te[,question_text:= str_replace_all(question_text,"mastrubating", 'masturbating')] 
te[,question_text:= str_replace_all(question_text,'pennis', 'penis')]
te[,question_text:= str_replace_all(question_text,'Etherium', 'Ethereum')] 
te[,question_text:= str_replace_all(question_text,'narcissit', 'narcissist')] 
te[,question_text:= str_replace_all(question_text,'bigdata', 'big data')]
te[,question_text:= str_replace_all(question_text,'2k17', '2017')]
te[,question_text:= str_replace_all(question_text,'2k18', '2018')]
te[,question_text:= str_replace_all(question_text,'qouta', 'quota')] 
te[,question_text:= str_replace_all(question_text,'exboyfriend', 'ex boyfriend')] 
te[,question_text:= str_replace_all(question_text,'airhostess', 'air hostess')]
te[,question_text:= str_replace_all(question_text,"whst", 'what')] 
te[,question_text:= str_replace_all(question_text,'watsapp', 'whatsapp')] 
te[,question_text:= str_replace_all(question_text,'demonitisation', 'demonetization')]
te[,question_text:= str_replace_all(question_text,'demonitization', 'demonetization')]
te[,question_text:= str_replace_all(question_text,'demonetisation', 'demonetization')]

print("test done")

# prep texts
space_before_punct <- function(sentence) {
  str_replace_all(sentence, "([?.!])", " \\1")
}

puncts <- c(
  '\u002C','\u002E','\u0022','\u003A','\u0029','\u0028','\u002D','\u0021','\u003B','\u0027','\u0024','\u0026','\u002F','\u005B','\u005D','\u003E','\u0025','\u003D','\u0023','\u002A','\u002B','\u005C',
  '\u2022','\u007E','\u0040','\u00A3','\u00B7','\u005F','\u007B','\u007D','\u00A9','\u005E','\u00AE','\u0060','\u003C','\u2192','\u00B0','\u20AC','\u2122','\u203A','\u2665','\u2190','\u00D7','\u00A7','\u2033',
  '\u2032','\u00C2','\u2588','\u00BD','\u00E0','\u2026','\u201C','\u2605','\u201D','\u2013','\u25CF','\u00E2','\u25BA','\u2212','\u00A2','\u00B2','\u00AC','\u2591','\u00B6','\u2191','\u00B1','\u00BF','\u25BE',
  '\u2550','\u00A6','\u2551','\u2015','\u00A5','\u2593','\u2014','\u2039','\u2500','\u2592','\uFF1A','\u00BC','\u2295','\u25BC','\u25AA','\u2020','\u25A0','\u2019','\u2580','\u00A8','\u2584','\u266B','\u2606',
  '\u00E9','\u00AF','\u2666','\u00A4','\u25B2','\u00E8','\u00B8','\u00BE','\u00C3','\u22C5','\u2018','\u221E','\u2219','\uFF09','\u2193','\u3001','\u2502','\u00BB','\uFF0C','\uFF08','\u266A','\u2569','\u00B3',
  '\u30FB','\u2566','\u2563','\u2554','\u2557','\u25AC','\u2764','\u00EF','\u00D8','\u00B9','\u2264','\u221A','\u2021','\u5350','\u534D','\u003F', '\u002A','\u007C'
)

replace_special_chars <- function(sentence) {
  stringi::stri_replace_all_fixed(sentence, puncts, paste0(" ", puncts, " "), vectorize_all = F)
}

preprocess_sentence <- compose(#add_tokens,
  replace_special_chars
  #replace_nations,
  #replace_years,
  #replace_gods
)

# Setup some parameters
maxlen <- as.integer(60)
max_words <- NULL #as.integer(150000)           
emb_dim <- as.integer(300)

# Prepare to tokenize the text
dat <- tr %>% 
  bind_rows(te) %>% 
  mutate(group = ifelse(is.na(target), "Test", "Train") %>% factor,
         question_text = ifelse(is.na(question_text),"_na_",question_text),
         question_text = str_replace_all(question_text, "\\s+", " ")) ## remove multiple ws)
rm(tr, te);gc()

dat$question_text <- preprocess_sentence(dat$question_text)
dat[sample(nrow(dat)),] %>% filter(target == 1) %>% select(question_text) %>% head

tokenizer <- text_tokenizer(num_words = max_words, filters = "") %>% 
  fit_text_tokenizer(dat[dat$group == "Train",]$question_text)

word_idx <- tokenizer$word_index

sequences <- texts_to_sequences(tokenizer, dat$question_text) %>% 
  pad_sequences(maxlen = maxlen)

# Params
max_words <- length(word_idx) #as.integer(70000)
print(paste0("MAX FEATURES: ", max_words))

library(reticulate)

read_glove <- 
  "
import io
import os
import time
import numpy as np 

def load_glove(word_index, max_features, embed_size, type = 'glove'):
\tprint('.... reading EMB ', type)
\tif(type == 'glove'):
\t\tEMBEDDING_FILE = '../input/embeddings/glove.840B.300d/glove.840B.300d.txt'
\tif(type == 'fast_text'):
\t\tEMBEDDING_FILE = '../input/embeddings/wiki-news-300d-1M/wiki-news-300d-1M.vec'
\tif(type == 'para'):
\t\tEMBEDDING_FILE = '../input/embeddings/paragram_300_sl999/paragram_300_sl999.txt'

\tdef get_coefs(word,*arr): return word, np.asarray(arr, dtype='float32')
\tif(type == 'glove'):
\t\tembeddings_index = dict(get_coefs(*o.split(' ')) for o in io.open(EMBEDDING_FILE, errors='ignore'))
\tif(type == 'fast_text'):
\t\tembeddings_index = dict(get_coefs(*o.split(' ')) for o in io.open(EMBEDDING_FILE, encoding='utf8', errors='ignore') if len(o)>100)
\tif(type == 'para'):
\t\tembeddings_index = dict(get_coefs(*o.split(' ')) for o in io.open(EMBEDDING_FILE, encoding='utf8', errors='ignore') if len(o)>100)

\tall_embs = np.stack(embeddings_index.values())
\temb_mean,emb_std = all_embs.mean(), all_embs.std()
\tembed_size = all_embs.shape[1]
\tprint('reading done')

\tnb_words = min(max_features, len(word_index.items()))
\tembedding_matrix = np.zeros((nb_words+1, embed_size))   #embedding_matrix = np.random.normal(emb_mean, emb_std, (nb_words+1, embed_size))

\tfor word, i in word_index.items():
\t\tif i >= max_features: continue
\t\tembedding_vector = embeddings_index.get(word)
\t\tif embedding_vector is not None: embedding_matrix[i] = embedding_vector

\treturn embedding_matrix
"

writeLines(read_glove, "import_glove.py")
reticulate::source_python("import_glove.py")

ensemble_embs <- function(emb_list=list()){
  tmp = Reduce('+', emb_list)
  return(tmp/length(emb_list))
}

do_embeddings <- function(){
  
  emb1 <- load_glove(word_idx,as.integer(max_words),as.integer(emb_dim), type = "glove")
  # emb1 <- matrix(c(1,1,1,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0), ncol = 3, byrow = T)
  # emb1
  emb1_rowsum <- rowSums(emb1)
  #cat("emb 1 zero count ", sum(emb1_rowsum == 0)/nrow(emb1), "\n")
  
  emb2 <- load_glove(word_idx,as.integer(max_words),as.integer(emb_dim), type = "para")
  # emb2 <- matrix(c(2,2,2,0,0,0,2,2,2,2,2,2,0,0,0,0,0,0), ncol = 3, byrow = T)
  # emb2
  emb2_rowsum <- rowSums(emb2)
  #cat("emb 2 zero count ", sum(emb2_rowsum == 0)/nrow(emb2), "\n")
  gc()
  ensemble <- tibble(e1 = emb1_rowsum, e2 = emb2_rowsum) %>% mutate( 
    sub = case_when(
      e1!=0 & e2==0  ~ "e1",
      e1==0 & e2!=0  ~ "e2",
      #e1==0 & e2==0 & e3 != 0 ~ "e3",
      e1!=0 & e2!=0 ~ "e12"
    )
  )
  
  e1 <- which(ensemble$sub == "e1")
  e2 <- which(ensemble$sub == "e2")
  e12 <- which(ensemble$sub == "e12")
  
  emb1[e2,] <- emb2[e2,]
  emb1[e12,] <- ensemble_embs(list(emb1, emb2))[e12,]
  rm(emb2);gc()
  
  return(emb1)
}

emb_mat_ensemble  <- do_embeddings()
gc()

filter_nr <- 64
filter_size <- 3
max_pool_size <- 3
max_pool_strides <- 2
dense_nr <- 182
spatial_dropout <- 0.15
dense_dropout <- 0.05


conv_kern_reg <- keras::regularizer_l2(0)
conv_bias_reg <- keras::regularizer_l2(0)

get_model <- function(emb_mat = emb_mat, train_embed = FALSE){
  COMMENT_SEQUENCE_LENGTH <- maxlen
  EMBEDDING_DIM <- emb_dim
  num_words <- max_words+1
  
  comment  <- keras::layer_input(shape = list(COMMENT_SEQUENCE_LENGTH), dtype = "int32", name = "keras_comment_input")
  
  emb_comment <- comment %>% keras::layer_embedding(
    input_dim = num_words,
    output_dim = EMBEDDING_DIM,
    weights = list(emb_mat),
    # input_length = maxlen,
    trainable = train_embed,
    name = "embedding_layer"
  ) 
  
  emb_comment <- emb_comment %>% 
    keras::layer_spatial_dropout_1d(spatial_dropout) 
  
  x <- emb_comment %>% keras::bidirectional(keras::layer_cudnn_lstm(units = 64, return_sequences = T, kernel_initializer = initializer_glorot_uniform(seed = 27))) 
  
  block1 <-  x %>%
    keras::layer_conv_1d(filters = filter_nr, kernel_size=filter_size, padding='same', activation='linear',
                         kernel_regularizer=conv_kern_reg, bias_regularizer=conv_bias_reg, kernel_initializer = initializer_glorot_uniform(seed = 1)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_conv_1d(filters = filter_nr, kernel_size=filter_size, padding='same', activation='linear',
                         kernel_regularizer=conv_kern_reg, bias_regularizer=conv_bias_reg, kernel_initializer = initializer_glorot_uniform(seed = 3)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_parametric_relu()
  
  resize_emb <-  emb_comment %>%
    keras::layer_conv_1d(filters = filter_nr, kernel_size=1, padding='same', activation='linear',
                         kernel_regularizer=conv_kern_reg, bias_regularizer=conv_bias_reg, kernel_initializer = initializer_glorot_uniform(seed = 5)) %>%
    keras::layer_activation_parametric_relu()
  
  block1_output <- list(block1, resize_emb) %>% layer_add() %>%
    keras::layer_max_pooling_1d(pool_size=max_pool_size, strides=max_pool_strides, name = 'max_p_1')
  
  block2 <-  block1_output %>%
    keras::layer_conv_1d(filters = filter_nr, kernel_size=filter_size, padding='same', activation='linear',
                         kernel_regularizer=conv_kern_reg, bias_regularizer=conv_bias_reg, kernel_initializer = initializer_glorot_uniform(seed = 7)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_conv_1d(filters = filter_nr, kernel_size=filter_size, padding='same', activation='linear',
                         kernel_regularizer=conv_kern_reg, bias_regularizer=conv_bias_reg, kernel_initializer = initializer_glorot_uniform(seed = 8)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_parametric_relu()
  
  block2_output <- list(block2, block1_output) %>% layer_add() 
  
  output <- block2_output %>% keras::layer_global_max_pooling_1d(name = 'global_max_p')
  
  output <- output %>% #list(output,attention) %>% layer_concatenate() %>%
    keras::layer_dense(dense_nr, activation ='linear', kernel_initializer = initializer_glorot_uniform(seed = 9)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_parametric_relu() %>%
    keras::layer_dropout(dense_dropout, seed = 15) %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model <- keras::keras_model(inputs = comment, outputs = output)
  
  optimizer <- keras::optimizer_adam(lr = 0.0009)
  model %>% compile(loss = "binary_crossentropy", optimizer = optimizer, metrics = c('binary_accuracy'))
  # model %>% compile(loss = f1_loss, optimizer = optimizer, metrics = c('binary_accuracy'))
  return(model)
}


calc_f1_tresh <- function(y_true = y_val, y_pred = pred_y_val, v_loss = v_loss){
  best_thresh <- 0
  best_score <- 0
  for(thresh in (seq(0.1,0.6,0.01))){
    #thresh <- round(thresh, 2)
    score <- MLmetrics::F1_Score(y_true = as.factor(y_true), y_pred = as.factor((ifelse(y_pred >= thresh,1,0))),positive = "1")
    
    if(score > best_score){
      best_thresh <- thresh
      best_score <- score
    }
  }
  
  # print(paste0("_________ Epoch", m, " Val F1 Score: ", best_score, " val_loss: ",v_loss," best thresh: ", best_thresh, "\n"))
  output <- data_frame(best_score, best_thresh)
  return(output)
}

# Prepare a validation set
library(magrittr)
set.seed(seed)
y <- dat %>% filter(group == "Train") %$% target
X_te <- sequences[-tri, ]

#---- cv 
n_folds <- 6
val_ind <- caret::createFolds(y, k = n_folds, returnTrain = F)

te_preds <- list()
val_preds <- list()
te_preds_raw <- list()
cv_thresholds <- vector()
cv_scores <- vector()
cv_loss <- vector()

rs <- rowSums(emb_mat_ensemble)
zero_embedding_rows <- which(rs == 0)

for(f in 1:n_folds){
  X_tr <- sequences[tri,][-val_ind[[f]], ];y_tr <- y[-val_ind[[f]]]
  X_val <- sequences[tri,][val_ind[[f]], ];y_val <- y[val_ind[[f]]]
  
  ## calculate rowsums
  rs <- rowSums(emb_mat_ensemble)
  cat(">>>>>", length(rs[rs==0]), " rows = 0 ...\n")
  
  model <- get_model(emb_mat = emb_mat_ensemble, train_embed = F)
  
  # summary(model)
  l_rate <- c(0.003, 0.003, 0.003, 0.001)
  
  fold_score <- 0
  fold_thresh <- 0
  fold_loss <- 0
  bs <- 512*2
  for(m in 1:length(l_rate)){
    k_set_value(model$optimizer$lr, l_rate[m])
    
    if(m == (length(l_rate))){
      #cat("unfreezing embedding layer \n")
      # keras::unfreeze_weights(model, from = "embedding_layer", to = "embedding_layer")
      # keras::freeze_weights(model, from = "embedding_layer", to = "embedding_layer")
      rm(model);k_clear_session()
      model <- get_model(emb_mat = emb_mat_ensemble, train_embed = T)
      model <- model %>% keras::load_model_weights_hdf5("model.hdf5")  
      bs <- 512*1.5
      #layer.trainable=False
      ## save model weights an use in following runs
    } 
    
    history <- model %>% fit(
      X_tr,
      y_tr,
      batch_size = bs,
      callbacks = list(
        callback_model_checkpoint(filepath = "model.hdf5",save_best_only = F, verbose = F, mode ="min")
      ),
      validation_data = list(X_val, y_val),
      epochs = 1,
      #shuffle = T,
      verbose = 0
    )
    v_loss <- history$metrics$val_loss
    
    pred_y_val <- keras:::predict.keras.engine.training.Model(model, X_val, batch_size = bs, verbose = 0)
    scoring <- calc_f1_tresh(y_true = y_val, y_pred = pred_y_val, v_loss = v_loss)
    
    #if(scoring$best_score > fold_score){
    fold_score <- scoring$best_score
    fold_thresh <- scoring$best_thresh
    fold_loss <- v_loss
    best_model <- keras::clone_model(model)
    best_model <- best_model %>% keras::load_model_weights_hdf5("model.hdf5")  
    #}
  }
  
  #cat("updating embeddings ... \n")
  layer <- model %>% keras::get_layer(name = "embedding_layer")
  emb_mat_ensemble2 <- layer %>% keras::get_weights() %>% extract2(1)
  emb_mat_ensemble[zero_embedding_rows,] <- emb_mat_ensemble2[zero_embedding_rows,] 
  rm(emb_mat_ensemble2);gc()
  
  ## calculate rowsums
  #rs <- rowSums(emb_mat_ensemble)
  #cat(length(rs[rs==0]), " rows = 0 ...\n")
  
  print(paste0("Fold: ", f, " Val F1 Score: ", fold_score," Val Loss: ", fold_loss," best thresh: ", fold_thresh, "\n"))
  
  predictions <- keras:::predict.keras.engine.training.Model(best_model, X_te, batch_size = bs, verbose = 0)
  
  #predictions <- keras:::predict.keras.engine.training.Model(model, X_te, batch_size = 256, verbose = 0)
  te_preds_raw[[f]] <- predictions[,1]
  
  predictions <- ifelse(predictions >= fold_thresh, 1, 0)
  te_preds[[f]] <- predictions[,1]
  
  val_preds[[f]] <- data_frame(row = val_ind[[f]], y_pred = pred_y_val[,1], y_true = y_val)
  cv_thresholds <- c(cv_thresholds, fold_thresh)
  cv_scores <- c(cv_scores, fold_score)
  cv_loss <- c(cv_loss, fold_loss)
  
  rm(X_tr, X_val, best_model, model);gc()
  k_clear_session()
}

te_pred <- rlist::list.cbind(te_preds)
te_pred_raw <- rlist::list.cbind(te_preds_raw) %>% as.data.frame()
tr_pred <- rlist::list.rbind(val_preds)

print(paste0("CV SCORE: ", mean(cv_scores)))
print("TEST COR: ")
print(cor(te_pred_raw))

sub <- fread("../input/sample_submission.csv") %>% select(qid) %>%
  bind_cols(te_pred_raw) %>% group_by(qid) %>% mutate(prediction = (V1+V2+V3+V4+V5+V6)/6) %>% 
  select(qid, prediction)

best_thresh_final <- 0
best_score_final <- 0
for(thresh in (seq(0.1,0.501,0.01))){
  # thresh <- round(thresh, 2)
  
  score <- MLmetrics::F1_Score(y_true = as.factor(tr_pred$y_true), y_pred = as.factor((ifelse(tr_pred$y_pred >= thresh,1,0))),positive = "1")
  
  if(score > best_score_final){
    best_thresh_final <- thresh
    best_score_final <- score
  }
}

cv_results <- data_frame(fold = 1:n_folds, f1_score = cv_scores, threshold = cv_thresholds)
print(cv_results)

mean_thresh <-  mean(cv_thresholds)
max_fold_thresh <- cv_results[which.max(cv_results$f1_score),]$threshold

print(paste0("F1 best THRESH: ",best_thresh_final, " with score ", best_score_final))
print(paste0("all mean THRESH: ", mean_thresh))
print(paste0("best cv THRESH: ", max_fold_thresh))

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

t <- best_thresh_final
print(paste0("USING THRESH: ", t))

sub <- sub %>% mutate(prediction = ifelse(prediction >= t, 1, 0))
fwrite(sub, "submission.csv")