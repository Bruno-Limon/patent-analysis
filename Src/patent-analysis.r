library(udpipe)
library(tidyverse)
library(ggplot2)

list_patents <- list.files(path = "C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/Patents",
                           pattern = "\\.txt$",
                           full.names = TRUE)

#set how many patents we want to read (for time purposes)
n_patents <- 10
# Create an empty tibble, that will store all the information
patents <- tibble(id = rep("", n_patents),
                  IPC = rep("", n_patents),
                  title = rep("", n_patents),
                  abstract = rep("", n_patents),
                  claims = rep("", n_patents))

for (i in 1:n_patents){
    raw_text <- read_file(list_patents[[i]])
    # id
    patents[[i, 1]] <- str_remove_all(string = list_patents[[i]],
                                      pattern = "C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/Patents/|.txt")
    # IPC
    patents[[i, 2]] <- str_extract(string = raw_text,
                                   pattern = "<ipc_classes>\n(.*?)\n</ipc_classes>") %>%
    str_remove_all("<ipc_classes>\n|\n</ipc_classes>")

    # title
    patents[[i, 3]] <- str_extract(string = raw_text,
                                   pattern = "<title>\n(.*?)\n</title>") %>%
    str_remove_all("<title>\n|\n</title>")

    # abstract
    patents[[i, 4]] <- str_extract(string = raw_text,
                                   pattern = "<abstract>\n(.*?)\n</abstract>") %>%
    str_remove_all("<abstract>\n|\n</abstract>")

    # claims
    # patents[[i, 5]] <- str_extract(string = raw_text,
    #                                pattern = "<claims>\n(.*?)\n</claims>") %>%
    # str_remove_all("<claims>\n|\n</claims>")
    }

head(patents)