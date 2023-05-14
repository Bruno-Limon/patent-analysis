library(udpipe)
library(tidyverse)
library(ggplot2)

udmodel <- udpipe_download_model(language = "english")

x1 <- udpipe(x = "Competitive intelligence (CI) is the process and forward-looki
                ng practices used in producing knowledge
                about the competitive environment to improve organizational
                performance.", object = udmodel)
str(x1)

x <- 12
y <- 10
z <- x + y
print(z)
