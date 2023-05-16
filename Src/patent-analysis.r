library(udpipe)
library(tidyverse)
library(ggplot2)

list_patents <- list.files(path = "C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/Patents",
                           pattern = "\\.txt$",
                           full.names = TRUE)

#set how many patents we want to read (for time purposes)
n_patents <- 10
col_names <- c("id", "filing_date", "publication_date", "ipc_classes",
               "assignee", "inventors", "title", "abstract", "claims")

# Create an empty dataframe, that will store all the information
patents <- data.frame(matrix(nrow = n_patents, ncol = length(col_names)))
colnames(patents) <- col_names

for (i in 1:n_patents){
    raw_text <- read_file(list_patents[[i]])
    # id
    patents[[i, 1]] <- str_remove_all(string = list_patents[[i]],
                                      pattern = "C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/Patents/|.txt")

    for (j in 2:length(col_names)){
        patents[[i, j]] <- str_extract(string = raw_text,
                                       pattern = paste0("<", col_names[j],
                                                        ">\n(.*?)\n</",
                                                        col_names[j], ">")) %>%
        str_remove_all(paste0("<", col_names[j], ">\n|\n</", col_names[j], ">"))
    }
}

ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
patents_tagged <- udpipe_annotate(ud_model,
                                  x = patents$abstract,
                                  doc_id = patents$id) %>%
as.data.frame()

# asdasdasd
patents_tagged %>%
count(upos) %>%
ggplot(aes(x = reorder(upos, n), y = n)) +
geom_bar(stat = "identity") +
coord_flip()
