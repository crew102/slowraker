library(fulltext)
library(dplyr)
library(magrittr)
library(devtools)

dog_pubs <- ft_search(query = 'dog', from = 'plos', limit = 50)
x <- ft_get(dog_pubs)

raw_dog_pubs <- data.frame(
  doi = x %>%
    chunks("doi") %>%
    unlist(),
  title = x %>%
    chunks("title") %>%
    unlist(),
  abstract = x %>%
    chunks("abstract") %>%
    extract2("plos") %>%
    sapply(function(x) paste(x$abstract, collapse = " ")),
  stringsAsFactors = FALSE, row.names = NULL
)

# Section headers abut words in abstracts (e.g., "BackgroundHi there").
# We'll remove cases of this so it doesn't influence keyword extraction.
dog_pubs <-
  raw_dog_pubs %>%
    mutate_all(funs(iconv(., "UTF-8", "latin1"))) %>%
    mutate(abstract = gsub("(Background|Methodology/Principal Findings|Author Summary|Conclusions/significance)([[:upper:]])", " \\2", abstract)) %>%
    filter(nchar(abstract) > 10) %>%
    slice(1:30)

write.csv(dog_pubs, "data-raw/dog_pubs.csv", row.names = FALSE)
use_data(dog_pubs, internal = FALSE, overwrite = TRUE)