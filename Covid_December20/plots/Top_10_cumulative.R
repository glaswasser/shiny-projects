
# description
get_description <- function() {
  return("Cumulative cases for top 10 countries")
}



options(scipen = 999)
#input = "2020-12-01"
get_title <- function() {
  return("Cumulative cases for top 10 countries")
}



get_plot <- function(df, input, time_range, countries, relative_cum, relative_overtime) {
  
  
  if (relative_cum == TRUE) {
    df$cases <- df$relative_cases
  }
  # create cumulative cases
  df <- df %>% 
    filter(type == "confirmed") %>%
    #select(date, country, cases) %>% 
    group_by(country) %>%
    mutate(confirmed = cumsum(cases)) %>% 
    ungroup() 
  

  conf_plot <- df %>% 
    filter(date == as.Date(input, origin = "1970-01-01")) %>% 
    # remove provinces and get only top value per country:
    group_by(country) %>% 
    top_n(1, wt = confirmed) %>% 
    ungroup() %>% 
    # get top 10 countries:
    top_n(10, wt = confirmed) %>% 
    # plot
    ggplot(aes(x = reorder(country, confirmed), y = confirmed, fill = country, label = population)) +
    geom_bar(stat = "identity") +
    guides(fill = FALSE) +
    theme(legend.position = "none") +
    labs(title = "Cumulative Confirmed Cases",
         subtitle = "Top 10 Countries") +
    ylab(glue::glue("cumulative confirmed cases on ", {format(as.Date(input, origin = "1970-01-01"), "%Y-%b-%d")})) +
    xlab("Country") +
    coord_flip() +
    scale_y_continuous(labels = comma)

  ggplotly(conf_plot, tooltip = c("confirmed", "population"))
}


