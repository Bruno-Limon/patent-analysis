library(udpipe)
library(tidyverse)
library(ggplot2)

udmodel <- udpipe_download_model(language = "english")

list_of_files <- list.files(path = "C:/Users/Pavilion/Desktop/Github/
                                    patent-analysis/Patents",
                            pattern = "\\.txt$",
                            full.names = TRUE)

x <- 12
y <- 10
z <- x + y
print(z)
