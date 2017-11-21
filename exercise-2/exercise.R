# Exercise-2
# What are informatics courses about?

# Set up
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
#install.packages("rvest")
library(rvest)

# Use the `read_html` function to load this webpage:
# https://www.washington.edu/students/crscat/info.html
webpage <- read_html("https://www.washington.edu/students/crscat/info.html")

# Extract the text of each course title from the page by using the `html_nodes`
# function to identify the *bold paragraphs*. 
# Extract the text by passing those element to the `html_text` function
titles <- webpage %>% html_nodes('b') %>% html_text()

# Extract the *descriptions* of each course in the same process as above, searching for 
# paragraphs (p)
descriptions <- webpage %>% html_nodes("p") %>% html_text()

# Create a dataframe by combinding your course titles and descriptions (skip the first description)
courses <- data.frame(title = titles, 
                      description = descriptions[2:length(descriptions)],
                      stringsAsFactors = F)

# How many courses are in the catalogue?
num.of.courses <- as.numeric(length(courses$title))

# Create a tidytext sturcture of all words
words <- courses %>% unnest_tokens(word, description)

# Which words do we use to describe our courses?
word.count <- words %>% 
  group_by(word) %>%
  summarise(count = n()) %>% 
  arrange(-count)

# Create a set of stop words by adding (more) irrelevant words to the stop_words dataframe
more.words <- data.frame(
  word = c("myplan", "info", "course"),
  lexicon = "custom"
)
all.stop.words <- rbind(more.words, stop_words)
# Remove stop words by performing an anti_join with the stop_words dataframe
no.stop.words <- word.count %>% 
  anti_join(all.stop.words, by = "word")

# Which non stop-words are most common?


# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
no.stop.words %>% 
  filter(count > 10) %>% 
  mutate(word = reorder(word, count)) %>% 
  ggplot(aes(word, count)) +
  geom_col() +
  coord_flip()
