library(tidyverse)
library(igraph)
library(udpipe)
library(ggraph)
library(ggplot2)
library(textrank)
library(wordcloud)

patents_df <- read_csv("C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/Patents_specific.csv")

patents_tagged_title <- read_csv("C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/patents_tagged_title.csv")
patents_tagged_abstract <- read_csv("C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/patents_tagged_abstract.csv")

patents_tagged_title$phrase_tag <- as_phrasemachine(patents_tagged_title$upos, type = "upos")
patents_tagged_abstract$phrase_tag <- as_phrasemachine(patents_tagged_abstract$upos, type = "upos")

patents_tagged <- patents_tagged_abstract

# top 10 nouns
patents_tagged[patents_tagged$upos == "NOUN", ] %>%
    count(lemma) %>%
    top_n(10, n) %>%
    ggplot(aes(x = reorder(lemma, n), y = n, fill = lemma)) +
    geom_col() + coord_flip() + theme_light() + guides(fill = "none") +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 nouns",
         x = "Nouns",
         y = "Number of words")
ggsave("Nouns.png", height = 9, width = 16, dpi = 1080)

# top 10 adjectives
patents_tagged[patents_tagged$upos == "ADJ", ] %>%
    count(lemma) %>%
    top_n(10, n) %>%
    ggplot(aes(x = reorder(lemma, n), y = n, fill = lemma)) +
    geom_col() + coord_flip() + theme_light() + guides(fill = "none") +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 adjectives",
         x = "Adjective",
         y = "Number of words")
ggsave("Adjectives.png", height = 9, width = 16, dpi = 1080)

# top 10 Verbs
patents_tagged[patents_tagged$upos == "VERB", ] %>%
    count(lemma) %>%
    top_n(10, n) %>%
    ggplot(aes(x = reorder(lemma, n), y = n, fill = lemma)) +
    geom_col() + coord_flip() + theme_light() + guides(fill = "none") +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 verbs",
         x = "Verb",
         y = "Number of words")
ggsave("Verbs.png", height = 9, width = 16, dpi = 1080)

# top 10 Prepositions
patents_tagged[patents_tagged$upos == "PROPN", ] %>%
    count(lemma) %>%
    top_n(10, n) %>%
    ggplot(aes(x = reorder(lemma, n), y = n, fill = lemma)) +
    geom_col() + coord_flip() + theme_light() + guides(fill = "none") +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 prepositions",
         x = "Preposition",
         y = "Number of words")
ggsave("Prepositions.png", height = 9, width = 16, dpi = 1080)

# extracting keywords (NOUN + VERB + ADJ)
patents_kw3 <- keywords_rake(x = patents_tagged, term = "lemma", group = "doc_id",
                             relevant = patents_tagged$upos %in% c("NOUN", "ADJ"))
patents_kw3$key <- factor(patents_kw3$keyword, levels = rev(patents_kw3$keyword))

patents_kw3 %>%
    count(rake, key) %>%
    top_n(10, key) %>%
    ggplot(aes(x = reorder(key, n), y = rake)) +
    geom_col(fill = "#4da0ff") + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 n-grams with nouns, verbs & adjectives",
         x = "n-gram",
         y = "RAKE")
ggsave("Keywords-NounVerbAdj.png", height = 9, width = 16, dpi = 1080)

# Cooccurrences
patents_cooc <- cooccurrence(x = subset(patents_tagged, upos %in% c("NOUN", "ADJ")),
                     term = "lemma",
                     group = c("doc_id", "paragraph_id", "sentence_id"))
write.csv(patents_cooc, "C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/cooc.csv",
          row.names = FALSE)

wordnetwork <- head(patents_cooc, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "#4da0ff") +
    geom_node_text(aes(label = name), col = "#00043f", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    labs(title = "Cooccurrences within patents")
ggsave("CoocNetwork-NounAdj.png", height = 9, width = 16, dpi = 1080)

# Correlations
patents_tagged$id <- unique_identifier(patents_tagged, fields = c("sentence_id", "doc_id"))
dtm <- subset(patents_tagged, upos %in% c("NOUN", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 300)
termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]

write.csv(y, "C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/corr.csv",
          row.names = FALSE)


# Simple noun phrases (a adjective+noun, pre/postposition, optional determiner and another adjective+noun)
kwords <- keywords_phrases(x = patents_tagged$phrase_tag, term = patents_tagged$token,
                         pattern = "(A|N)+N(P+D*(A|N)*N)*",
                         is_regex = TRUE, ngram_max = 4, detailed = FALSE)
kwords <- subset(kwords, ngram > 2 & freq >= 20)
write.csv(kwords, "C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/keywords.csv",
          row.names = FALSE)

wordcloud(words = kwords$keyword, freq = kwords$freq, scale = c(3.5, 0.25),
          max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Set2"))
