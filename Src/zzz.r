library(tidyverse)

list_patents <- list.files(path = "C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/Patents",
                           pattern = "\\.txt$",
                           full.names = TRUE)

#set how many patents we want to read (for time purposes)
n_patents <- 100
col_names <- c("id", "filing_date", "inventors")

# Create an empty dataframe, that will store all the information
patents <- data.frame(matrix(nrow = n_patents, ncol = length(col_names)))
colnames(patents) <- col_names


for (i in 1:n_patents){
    raw_text <- read_file(list_patents[[i]])
    # id
    patents[[i, 1]] <- str_remove_all(string = list_patents[[i]],
                                      pattern = "C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/Patents/|.txt")

    for (j in 2:length(col_names)){
        patents[[i, j]] <- str_extract_all(string = raw_text,
                                           pattern = "<inventors>([^*]+)</inventors>") %>%
        str_remove_all("<inventors>\n|\n</inventors>") %>%
        str_split("\n")
    }
}

View(patents)

    # patents[[i, 3]] <- str_extract_all(string = raw_text,
    #                                    pattern = "<inventors>([^*]+)</inventors>") %>%
    # str_remove_all("<inventors>\n|\n</inventors>") %>%
    # str_split("\n")


# raw_text <- read_file(list_patents[[4]])
# x <- str_extract(string = raw_text,
#                  pattern = "<inventors>([^*]+)</inventors>") %>%
# str_remove_all("<inventors>\n|\n</inventors>") %>%
# str_split("\n")

# View(x)
