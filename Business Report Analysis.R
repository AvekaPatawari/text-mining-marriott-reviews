# business insight report
library(textshape)
library(textreadr)
library(dplyr)

######################################################
### Putting the vector in a data frame ##########
######################################################

setwd("/Users/aveka/Desktop/files/marriot reviews")
nm <- list.files(path="/Users/aveka/Desktop/files/marriot reviews")
my_doc_text <- do.call(rbind, lapply(nm, function(x) read_doc(file=x)))
mydf <- data.frame(text=my_doc_text)
print(mydf)

######################################################
########  tokenizing the mydf data frame #########
######################################################

library(tidytext)
library(tidyverse)
token_list <- mydf %>%
  unnest_tokens(word, text) #no punctuation, no upper case letters
print(token_list)

#######################################################
########## token frequencies ####################
#######################################################

frequencies_tokens <- mydf %>%
  unnest_tokens(word, text) %>%
  count(word, sort=TRUE)
print(frequencies_tokens)

#######################################################
######### stop words #########################
#######################################################

#stop words are words that are commonly used in English 
# e.g. is, I, are, you, me, the, of, etc.
#we will use the anti_join(stop_words) to remove the stop words
library(stringr)

data(stop_words)
frequencies_tokens_nostop <- mydf%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)


print(frequencies_tokens_nostop)

#######################################################
##### token frequency histograms################
#######################################################

library(ggplot2)
freq_hist <- mydf %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  filter(n > 3) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)


