library(tidyverse)
library(udpipe)
library(igraph)
library(ggraph)
library(ggplot2)

# loading data
patents_df <- read_csv("C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/Patents.csv")

# Transforming data
patents_df$Year <- as.character(patents_df$Publication_Year)
patents_df$Applicants <- strsplit(patents_df$Applicants, ";;")
patents_df$Inventors <- strsplit(patents_df$Inventors, ";;")
patents_df$Owners <- strsplit(patents_df$Owners, ";;")
patents_df$CPC <- strsplit(patents_df$"CPC Classifications", ";;")
patents_df$IPC <- strsplit(patents_df$"IPCR Classifications", ";;")

# frequency of jurisdictions
patents_df %>%
    count(Jurisdiction) %>%
    ggplot(aes(x = reorder(Jurisdiction, n), y = n)) +
    geom_col(fill = "#4da0ff") + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Patent frequency by jurisdiction",
         x = "Jurisdiction",
         y = "Number of patents")
ggsave("Jurisdiction-Frequency.png", height = 9, width = 16, dpi = 1080)

# frequency of document type
patents_df %>%
    count(Document_Type) %>%
    ggplot(aes(x = reorder(Document_Type, n), y = n)) +
    geom_col(fill = "#4da0ff") + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Patent frequency by document type",
         x = "Document type",
         y = "Number of patents")
ggsave("DocType-Frequency.png", height = 9, width = 16, dpi = 1080)

# frequency of legal status
patents_df %>%
    count(Legal_Status) %>%
    ggplot(aes(x = reorder(Legal_Status, n), y = n)) +
    geom_col(fill = "#4da0ff") + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Patent frequency by legal status",
         x = "Legal status",
         y = "Number of patents")
ggsave("LegalStatus-Frequency.png", height = 9, width = 16, dpi = 1080)

# frequency of publication year by jurisdiction
patents_df %>%
    count(Publication_Year, Jurisdiction, sort = TRUE) %>%
    ggplot(aes(x = Publication_Year, y = n, fill = Jurisdiction)) +
    geom_col() + theme_light() +
    scale_x_continuous("Year", breaks = seq(min(patents_df$Publication_Year),
                                            max(patents_df$Publication_Year), 5)) +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 28)) +
    labs(title = "Patent frequency by jurisdiction across time",
         x = "Year",
         y = "Number of patents")
ggsave("YearByJurisdiction.png", height = 9, width = 16, dpi = 1080)


# frequency of publication Year by legal status
patents_df %>%
    count(Publication_Year, Legal_Status) %>%
    ggplot(aes(x = Publication_Year, y = n, fill = Legal_Status)) +
    geom_col() + theme_light() +
    scale_x_continuous("Year", breaks = seq(min(patents_df$Publication_Year),
                                            max(patents_df$Publication_Year), 5)) +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 28)) +
    labs(title = "Patent frequency by legal status across time",
         x = "Year",
         y = "Number of patents")
ggsave("YearByLegalStatus.png", height = 9, width = 16, dpi = 1080)

# top 10 applicants
as.data.frame(table(unlist(patents_df$Applicants))) %>%
    arrange(desc(Freq)) %>%
    top_n(10, Freq) %>%
    ggplot(aes(x = reorder(Var1, Freq), y = Freq)) +
    coord_flip() + geom_col(fill = "#4da0ff") + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 applicants",
         x = "Applicants",
         y = "Number of patents")
ggsave("Applicants.png", height = 9, width = 16, dpi = 1080)

# trends of top 5 applicants
top5_app <- c("CLIMATE CORP", "DEERE & CO", "CELERA CORP", "PROCTER & GAMBLE", "XYLECO INC")
app_tibbles <- vector("list", length(top5_app))
for (i in seq_along(app_tibbles)){
    app_tibbles[[i]] <- patents_df %>%
                            filter(grepl(top5_app[i], Applicants)) %>%
                            count(Publication_Year, name = paste0("n", i)) %>%
                            as.data.frame()
}

app_list <- list(app_tibbles[[1]], app_tibbles[[2]], app_tibbles[[3]],
                 app_tibbles[[4]], app_tibbles[[5]])
app_df <- app_list %>% reduce(full_join, by = "Publication_Year")
app_df[is.na(app_df)] <- 0
colnames(app_df) <- c("Year", top5_app)

app_df %>%
    pivot_longer(., cols = top5_app, names_to = "Applicants", values_to = "Number_of_Patents") %>%
    as.data.frame() %>%
    ggplot(aes(x = Year, y = Number_of_Patents, group = Applicants, color = Applicants)) +
    geom_line() + geom_point() + theme_light() +
    scale_x_continuous("Year", breaks = seq(min(app_df$Year),
                                            max(app_df$Year), 4)) +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 28)) +
    labs(title = "Top 5 applicants across time",
         x = "Year",
         y = "Number of patents")
ggsave("Applicants-Trends.png", height = 5, width = 16, dpi = 1080)

# top 10 inventors
as.data.frame(table(unlist(patents_df$Inventors))) %>%
    arrange(desc(Freq)) %>%
    top_n(10, Freq) %>%
    ggplot(aes(x = reorder(Var1, Freq), y = Freq)) +
    coord_flip() + geom_col(fill = "#4da0ff") + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 inventors",
         x = "Inventors",
         y = "Number of patents")
ggsave("Inventors.png", height = 9, width = 16, dpi = 1080)

# top 10 owners
as.data.frame(table(unlist(patents_df$Owners))) %>%
    mutate(Var1 = gsub("\\s*\\([^\\)]+\\)", "", as.character(Var1))) %>%
    arrange(desc(Freq)) %>%
    top_n(10, Freq) %>%
    ggplot(aes(x = reorder(Var1, Freq), y = Freq)) +
    coord_flip() + geom_col(fill = "#4da0ff") + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 owners",
         x = "Owners",
         y = "Number of patents")
ggsave("Owners.png", height = 9, width = 16, dpi = 1080)

# trends of top 5 owners
top5_owners <- c("CELERA CORPORATION", "WALMART", "CLEARAG INC", "DTN LLC", "MONSANTO")
owners_tibbles <- vector("list", length(top5_owners))
for (i in seq_along(owners_tibbles)){
    owners_tibbles[[i]] <- patents_df %>%
                            filter(grepl(top5_owners[i], Owners)) %>%
                            count(Publication_Year, name = paste0("n", i)) %>%
                            as.data.frame()
}

owners_list <- list(owners_tibbles[[1]], owners_tibbles[[2]], owners_tibbles[[3]],
                    owners_tibbles[[4]], owners_tibbles[[5]])
owners_df <- owners_list %>% reduce(full_join, by = "Publication_Year")
owners_df[is.na(owners_df)] <- 0
colnames(owners_df) <- c("Year", top5_owners)

owners_df %>%
    pivot_longer(., cols = top5_owners, names_to = "Owners", values_to = "Number_of_Patents") %>%
    as.data.frame() %>%
    ggplot(aes(x = Year, y = Number_of_Patents, group = Owners, color = Owners)) +
    geom_line() + geom_point() + theme_light() +
    scale_x_continuous("Year", breaks = seq(min(owners_df$Year),
                                            max(owners_df$Year), 4)) +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 28)) +
    labs(title = "Top 5 owners across time",
         x = "Year",
         y = "Number of patents")
ggsave("Owner-Trends.png", height = 5, width = 16, dpi = 1080)

# top 10 CPC
as.data.frame(table(unlist(patents_df$CPC))) %>%
    arrange(desc(Freq)) %>%
    top_n(10, Freq) %>%
    ggplot(aes(x = reorder(Var1, Freq), y = Freq)) +
    coord_flip() + geom_col(fill = "#4da0ff") + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 CPC",
         x = "CPC",
         y = "Number of patents")
ggsave("CPC.png", height = 9, width = 16, dpi = 1080)

# trends of top 5 CPC
top5_cpc <- c("A01B79/005", "G06V20/188", "G06Q50/02", "B64C39/024", "B64U2101/00")
cpc_tibbles <- vector("list", length(top5_cpc))
for (i in seq_along(cpc_tibbles)){
    cpc_tibbles[[i]] <- patents_df %>%
                            filter(grepl(top5_cpc[i], CPC)) %>%
                            count(Publication_Year, name = paste0("n", i)) %>%
                            as.data.frame()
}

cpc_list <- list(cpc_tibbles[[1]], cpc_tibbles[[2]], cpc_tibbles[[3]],
                 cpc_tibbles[[4]], cpc_tibbles[[5]])
cpc_df <- cpc_list %>% reduce(full_join, by = "Publication_Year")
cpc_df[is.na(cpc_df)] <- 0
colnames(cpc_df) <- c("Year", top5_cpc)

cpc_df %>%
    pivot_longer(., cols = top5_cpc, names_to = "CPC", values_to = "Number_of_Patents") %>%
    as.data.frame() %>%
    ggplot(aes(x = Year, y = Number_of_Patents, group = CPC, color = CPC)) +
    geom_line() + geom_point() + theme_light() +
    scale_x_continuous("Year", breaks = seq(min(cpc_df$Year),
                                            max(cpc_df$Year), 4)) +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 28)) +
    labs(title = "Top 5 CPC groups across time",
         x = "Year",
         y = "Number of patents")
ggsave("CPC-Trends.png", height = 9, width = 16, dpi = 1080)

# top 10 IPC
as.data.frame(table(unlist(patents_df$IPC))) %>%
    arrange(desc(Freq)) %>%
    top_n(10, Freq) %>%
    ggplot(aes(x = reorder(Var1, Freq), y = Freq)) +
    coord_flip() + geom_col(fill = "#4da0ff") + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 IPC",
         x = "IPC",
         y = "Number of patents")
ggsave("IPC.png", height = 9, width = 16, dpi = 1080)

# trends of top 5 IPC
top5_ipc <- c("A01B79/00", "G06K9/00", "G06Q50/02", "B64C39/02", "A01M7/00")
ipc_tibbles <- vector("list", length(top5_ipc))
for (i in seq_along(ipc_tibbles)){
    ipc_tibbles[[i]] <- patents_df %>%
                            filter(grepl(top5_ipc[i], IPC)) %>%
                            count(Publication_Year, name = paste0("n", i)) %>%
                            as.data.frame()
}

ipc_list <- list(ipc_tibbles[[1]], ipc_tibbles[[2]], ipc_tibbles[[3]],
                 ipc_tibbles[[4]], ipc_tibbles[[5]])
ipc_df <- ipc_list %>% reduce(full_join, by = "Publication_Year")
ipc_df[is.na(ipc_df)] <- 0
colnames(ipc_df) <- c("Year", top5_ipc)

ipc_df %>%
    pivot_longer(., cols = top5_ipc, names_to = "IPC", values_to = "Number_of_Patents") %>%
    as.data.frame() %>%
    ggplot(aes(x = Year, y = Number_of_Patents, group = IPC, color = IPC)) +
    geom_line() + geom_point() + theme_light() +
    scale_x_continuous("Year", breaks = seq(min(ipc_df$Year),
                                            max(ipc_df$Year), 5)) +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 28)) +
    labs(title = "Top 5 IPC across time",
         x = "Year",
         y = "Number of patents")
ggsave("IPC-Trends.png", height = 9, width = 16, dpi = 1080)

######################################### TEXT ANALYSIS #########################################

# tokenizing title and abstract
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

patents_tagged <- udpipe_annotate(ud_model,
                                  x = patents_df$Title,
                                  doc_id = patents_df$"Application Number") %>%
as.data.frame()

# top 10 UPOS
patents_tagged %>%
    count(upos) %>%
    top_n(10, n) %>%
    ggplot(aes(x = reorder(upos, n), y = n)) +
    geom_col(fill = "#4da0ff") + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "POS frequency in title & abstract",
         x = "POS",
         y = "Number of words")
ggsave("POS.png", height = 9, width = 16, dpi = 1080)

# top 10 nouns
patents_tagged[patents_tagged$upos == "NOUN", ] %>%
    count(lemma) %>%
    top_n(10, n) %>%
    ggplot(aes(x = reorder(lemma, n), y = n)) +
    geom_col(fill = "#4da0ff") + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 nouns",
         x = "Noun",
         y = "Number of words")
ggsave("Nouns.png", height = 9, width = 16, dpi = 1080)

# top 10 propositions
patents_tagged[patents_tagged$upos == "PROPN", ] %>%
    count(lemma) %>%
    top_n(10, n) %>%
    ggplot(aes(x = reorder(lemma, n), y = n)) +
    geom_col(fill = "#4da0ff") + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 propositions",
         x = "Proposition",
         y = "Number of words")
ggsave("Prepositions.png", height = 9, width = 16, dpi = 1080)

# top 10 verbs
patents_tagged[patents_tagged$upos == "VERB", ] %>%
    count(lemma) %>%
    top_n(10, n) %>%
    ggplot(aes(x = reorder(lemma, n), y = n)) +
    geom_col(fill = "#4da0ff") + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 verbs",
         x = "Verb",
         y = "Number of words")
ggsave("Verbs.png", height = 9, width = 16, dpi = 1080)

# top 10 adjectives
patents_tagged[patents_tagged$upos == "ADJ", ] %>%
    count(lemma) %>%
    top_n(10, n) %>%
    ggplot(aes(x = reorder(lemma, n), y = n)) +
    geom_col(fill = "#4da0ff") + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 adjectives",
         x = "Adjective",
         y = "Number of words")
ggsave("Adjectives.png", height = 9, width = 16, dpi = 1080)

# extracting keywords (NOUN + ADJECTIVE)
patents_kw <- keywords_rake(x = patents_tagged, term = "lemma", group = "doc_id",
                            relevant = patents_tagged$upos %in% c("NOUN", "ADJ"))
patents_kw$key <- factor(patents_kw$keyword, levels = rev(patents_kw$keyword))

patents_kw %>%
    count(rake, key) %>%
    top_n(10, key) %>%
    ggplot(aes(x = reorder(key, n), y = rake)) +
    geom_col(fill = "#4da0ff") + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 n-grams with nouns & adjectives",
         x = "n-gram",
         y = "RAKE")
ggsave("Keywords-NounAdj.png", height = 9, width = 16, dpi = 1080)

# extracting keywords (NOUN + VERB)
patents_kw2 <- keywords_rake(x = patents_tagged, term = "lemma", group = "doc_id",
                             relevant = patents_tagged$upos %in% c("NOUN", "VERB"))
patents_kw2$key <- factor(patents_kw2$keyword, levels = rev(patents_kw2$keyword))

patents_kw2 %>%
    count(rake, key) %>%
    top_n(10, key) %>%
    ggplot(aes(x = reorder(key, n), y = rake)) +
    geom_col(fill = "#4da0ff") + coord_flip() + theme_light() +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 32)) +
    labs(title = "Top 10 n-grams with nouns & verbs",
         x = "n-gram",
         y = "RAKE")
ggsave("Keywords-NounVerb.png", height = 9, width = 16, dpi = 1080)

# extracting keywords (NOUN + VERB + ADJ)
patents_kw3 <- keywords_rake(x = patents_tagged, term = "lemma", group = "doc_id",
                             relevant = patents_tagged$upos %in% c("NOUN", "VERB", "ADJ"))
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

# cooccurrences NOUN + ADJ
patents_cooc <- cooccurrence(x = subset(patents_tagged, upos %in% c("NOUN", "ADJ")),
                     term = "lemma",
                     group = c("doc_id", "paragraph_id", "sentence_id"))

wordnetwork <- head(patents_cooc, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "#4da0ff") +
    geom_node_text(aes(label = name), col = "#00043f", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    labs(title = "Cooccurrences within patents",
         subtitle = "Nouns & Adjective")
ggsave("CoocNetwork-NounAdj.png", height = 9, width = 16, dpi = 1080)

# cooccurrences NOUN + VERB
patents_cooc2 <- cooccurrence(x = subset(patents_tagged, upos %in% c("NOUN", "VERB")),
                     term = "lemma",
                     group = c("doc_id", "paragraph_id", "sentence_id"))

wordnetwork <- head(patents_cooc2, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "#4da0ff") +
    geom_node_text(aes(label = name), col = "#00043f", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    labs(title = "Cooccurrences within patents",
         subtitle = "Nouns & Verbs")
ggsave("CoocNetwork-NounVerb.png", height = 9, width = 16, dpi = 1080)

# cooccurrences NOUN + VERB + ADJ
patents_cooc3 <- cooccurrence(x = subset(patents_tagged, upos %in% c("NOUN", "VERB", "ADJ")),
                              term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))

wordnetwork <- head(patents_cooc3, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "#4da0ff") +
    geom_node_text(aes(label = name), col = "#00043f", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    labs(title = "Cooccurrences within patents",
         subtitle = "Nouns, Verbs & Adjectives")
ggsave("CookNetwork-NounVerbAdj.png", height = 9, width = 16, dpi = 1080)