library(tidytext)
library(newsanchor)
library(lubridate)
library(coronavirus)
library(tidyverse)
library(wordcloud2)
library(stringr)

## GOAL: Get word clouds from a set range of dates 
get_description <- function() {
  return(glue("
  Get the most frequent words in newspaper articles of week before the chosen date.
         Will be pulled from the following newspapers: \n Fox News, Google News, Daily Mail, Time, Independent,
         New York Times, Wall Street Journal, Washington Post, USA Today
              "))
}

# save api key
#set_api_key(path = "~/.Renviron") # key: c8919073179f4417aac29c2a5a3481ec
wordcloud_slider = "2021-01-01" ## for testing

get_wordcloud <- function(df, wordcloud_slider) {

    # set largest possible from and to date back in the days:
  from = as.Date(wordcloud_slider) - weeks(1)
  to = as.Date(wordcloud_slider)

  results <- get_everything_all(query = "corona", language = "en",
                                from = from, to = to,
                                # sort_by = "popularity",
                                sources = c("fox-news", "google-news", "daily-mail", "time", "independent",
                                            "the-new-york-times", "the-wall-street-journal", "the-washington-post",
                                            "usa-today"))[[2]]
  
  
  # get description, title and content united:
  
  results_united <- results %>% 
    unite(col = content, content, title, description, sep = " — ", remove = FALSE)
  
  
  # remove annotation that there are more chars
  results$content <- gsub("\\[.*","", results$content)
  
  
  tokenized <- results %>% 
    # tokinize reviews at word level
    unnest_tokens(output = token, input = content) %>%
    # count tokens within reviews as 'n'
    # (keep rest in the result)
    count(token)
   # count(id, name, title, description, published_at, token)
  
  
  tokenized_clean <- tokenized %>% 
    anti_join(get_stopwords(), by = c(token = "word")) %>% 
    filter(token != "coronavirus") %>% 
    select(token, n)
  
  
  
  return(wordcloud2(data = tokenized_clean, size = 0.2, gridSize = 5))
}

#wordcloud2(data = tokenized_clean, size = 0.1, gridSize = 5)
