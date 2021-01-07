
## GOAL: Get word clouds from a set range of dates 
get_description <- function() {
  return(glue("
  Get the most frequent words in newspaper articles related to the Coronavirus in the week before the chosen date.
         Will be pulled from the following newspapers: \n Fox News, Google News, Daily Mail, Time, Independent,
         New York Times, Wall Street Journal, Washington Post, USA Today
              "))
}

# save api key
#set_api_key(path = "~/.Renviron") # key: c8919073179f4417aac29c2a5a3481ec
#wordcloud_slider = Sys.Date()-2 ## for testing

get_wordcloud <- function(wordcloud_params, wordcloud_slider) {

  results <- wordcloud_params$results
  stopwords <- wordcloud_params$stopwords
  

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
    filter(!token %in% c("coronavirus", stopwords)) %>% 
    select(token, n)

  wordcloud2(data = tokenized_clean, size = 0.25, gridSize = 5)
}

#wordcloud2(data = tokenized_clean, size = 0.1, gridSize = 5)
