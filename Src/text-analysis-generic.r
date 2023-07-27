library(tidyverse)
library(igraph)
library(udpipe)
library(ggraph)
library(ggplot2)
library(textrank)
library(wordcloud)
library(topicmodels)
library(qgraph)

# loading data
patents_tagged <- read_csv("C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/Patents_tagged.csv")
patents_tagged$phrase_tag <- as_phrasemachine(patents_tagged$upos, type = "upos")

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
    ggplot(aes(x = reorder(key, n), y = rake, fill = key)) + guides(fill = "none") +
    geom_col() + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 keywords (RAKE)",
         x = "Keyword",
         y = "RAKE")
ggsave("Keywords-Rake.png", height = 9, width = 16, dpi = 1080)

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

########################## TOPIC MODELING ##########################
#### Using Keywords

## Recode terms to keywords
patents_tagged$term <- patents_tagged$token
patents_tagged$term <- txt_recode_ngram(patents_tagged$term,
                           compound = kwords$keyword, ngram = kwords$ngram)

## Keep keyword or just plain nouns
patents_tagged$term <- ifelse(patents_tagged$upos %in% "NOUN", patents_tagged$term,
                              ifelse(patents_tagged$term %in% kwords$keyword,
                                     patents_tagged$term, NA))

# Build document/term/matrix
dtm <- document_term_frequencies(patents_tagged, document = "topic_level_id", term = "term")
dtm <- document_term_matrix(x = dtm)
# Cleaning
# Remove words which do not occur that much
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
# Remove nouns which you really do not like (mostly too common nouns)
dtm <- dtm_remove_terms(dtm, terms = c("JP", "CN", "PRINTER", "Assay"))
# Or keep of these nouns the top 50 based on mean term-frequency-inverse document frequency
dtm <- dtm_remove_tfidf(dtm, top = 50)

m <- LDA(dtm, k = 5, method = "Gibbs",
         control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))

topicterminology <- predict(m, type = "terms", min_posterior = 0.10, min_terms = 5)
View(topicterminology)

# Visualization
x_topics <- merge(patents_tagged, scores, by.x = "topic_level_id", by.y = "doc_id")
wordnetwork <- subset(x_topics, topic %in% 1 & lemma %in% topicterminology[[1]]$term)
wordnetwork <- cooccurrence(wordnetwork, group = c("topic_level_id"), term = "lemma")
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink")  +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words in topic 1 ", subtitle = "Nouns & Adjective cooccurrence")

topicterminology <- predict(m, type = "terms", min_posterior = 0.05, min_terms = 10)
termcorrs <- subset(x_topics, topic %in% 1 & lemma %in% topicterminology[[1]]$term)
termcorrs <- document_term_frequencies(termcorrs, document = "topic_level_id", term = "lemma")
termcorrs <- document_term_matrix(termcorrs)
termcorrs <- dtm_cor(termcorrs)
termcorrs[lower.tri(termcorrs)] <- NA
diag(termcorrs) <- NA

qgraph(termcorrs, layout = "spring", labels = colnames(termcorrs), directed = FALSE,
       borders = FALSE, label.scale = FALSE, label.cex = 1, node.width = 0.5)