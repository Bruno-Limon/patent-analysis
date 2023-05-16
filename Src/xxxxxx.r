
# patents <- tibble(id = rep("", n_patents), filing_date = rep("", n_patents),
#                   publication_date = rep("", n_patents), IPC = rep("", n_patents),
#                   asignee = rep("", n_patents), inventors = rep("", n_patents),
#                   title = rep("", n_patents), abstract = rep("", n_patents),
#                   claims = rep("", n_patents))

# for (i in 1:n_patents){
#     raw_text <- read_file(list_patents[[i]])
#     # id
#     patents[[i, 1]] <- str_remove_all(string = list_patents[[i]],
#                                       pattern = "C:/Users/Pavilion/Desktop/Github/patent-analysis/Data/Patents/|.txt")
#     # filing_date
#     patents[[i, 2]] <- str_extract(string = raw_text,
#                                    pattern = "<filing_date>\n(.*?)\n</filing_date>") %>%
#     str_remove_all("<filing_date>\n|\n</filing_date>")

#     # publication_date
#     patents[[i, 3]] <- str_extract(string = raw_text,
#                                    pattern = "<publication_date>\n(.*?)\n</publication_date>") %>%
#     str_remove_all("<publication_date>\n|\n</publication_date>")

#     # IPC
#     patents[[i, 4]] <- str_extract(string = raw_text,
#                                    pattern = "<ipc_classes>\n(.*?)\n</ipc_classes>") %>%
#     str_remove_all("<ipc_classes>\n|\n</ipc_classes>")

#     # assignee
#     patents[[i, 5]] <- str_extract(string = raw_text,
#                                    pattern = "<assignee>\n(.*?)\n</assignee>") %>%
#     str_remove_all("<assignee>\n|\n</assignee>")

#     # inventors
#     patents[[i, 6]] <- str_extract(string = raw_text,
#                                    pattern = "<inventors>\n(.*?)\n</inventors>") %>%
#     str_remove_all("<inventors>\n|\n</inventors>")

#     # title
#     patents[[i, 7]] <- str_extract(string = raw_text,
#                                    pattern = "<title>\n(.*?)\n</title>") %>%
#     str_remove_all("<title>\n|\n</title>")

#     # abstract
#     patents[[i, 8]] <- str_extract(string = raw_text,
#                                    pattern = "<abstract>\n(.*?)\n</abstract>") %>%
#     str_remove_all("<abstract>\n|\n</abstract>")

#     # claims
#     patents[[i, 9]] <- str_extract(string = raw_text,
#                                    pattern = "<claims>\n(.*?)\n</claims>") %>%
#     str_remove_all("<claims>\n|\n</claims>")
# }