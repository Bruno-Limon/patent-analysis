library(tidyverse)
library(igraph)
library(udpipe)
library(ggraph)
library(ggplot2)
library(textrank)
library(wordcloud)

# loading data
patents_tagged <- read_csv("C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/Patents_tagged.csv")
patents_tagged$phrase_tag <- as_phrasemachine(patents_tagged$upos, type = "upos")

## Collocation (words following one another)
stats <- keywords_collocation(x = patents_tagged, term = "token", ngram_max = 4,
                              group = c("doc_id", "paragraph_id", "sentence_id"))

## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(patents_tagged, upos %in% c("NOUN", "ADJ")),
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))

## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = patents_tagged$lemma,
                      relevant = patents_tagged$upos %in% c("NOUN", "ADJ"))

## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats <- cooccurrence(x = patents_tagged$lemma,
                      relevant = patents_tagged$upos %in% c("NOUN", "ADJ"), skipgram = 3)
head(stats, 10)


wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective")

stats <- textrank_keywords(patents_tagged$lemma,
                          relevant = patents_tagged$upos %in% c("NOUN", "ADJ"),
                          ngram_max = 4, sep = " ")

stats <- subset(stats$keywords, ngram > 2 & freq >= 50)

## Simple noun phrases (a adjective+noun, pre/postposition, optional determiner and another adjective+noun)
stats <- keywords_phrases(x = patents_tagged$phrase_tag, term = patents_tagged$token,
                         pattern = "(A|N)+N(P+D*(A|N)*N)*",
                         is_regex = TRUE, ngram_max = 4, detailed = FALSE)
stats <- subset(stats, ngram > 2)

wordcloud(words = stats$keyword, freq = stats$freq, scale = c(3.5, 0.25),
          max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Set2"))

wordnetwork <- head(stats, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = freq, edge_alpha = freq), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective")
