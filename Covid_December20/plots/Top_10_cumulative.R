
# description
get_description <- function() {
  return("\"Top 10\" countries in terms of Cumulative cases over time")
}


options(scipen = 999)


get_plot <- function(df, input, time_range, countries, relative_cum, relative_overtime, cum_type) {
  
  
  if (relative_cum == TRUE) {
    df$cases <- df$relative_cases
  }
  # create cumulative cases
  df <- df %>% 
    filter(type == cum_type) %>%
    #select(-province, -lat, -long) %>% 
    select(date, country, cases, type, population) %>% 
    group_by(country) %>%
    arrange(date) %>% 
    mutate(confirmed = cumsum(cases)) %>% 
    ungroup() 


  conf_plot <- df %>% 
    filter(date == as.Date(input, origin = "1970-01-01")) %>% 
    # remove provinces and get only top value per country:
    select(-cases) %>% 
    group_by(country) %>% 
    top_n(1, wt = confirmed) %>% 
    unique() %>% 
    ungroup() %>% 
    # get top 10 countries:
    top_n(10, wt = confirmed) %>% 
    head(10) %>% 
    # remove duplicates
    unique() %>% 
    # plot
    ggplot(aes(x = reorder(country, confirmed), y = confirmed, label = population)) +
    geom_bar(stat = "identity") +
    guides(fill = FALSE) +
    theme(legend.position = "none") +
    labs(title = glue::glue("Cumulative {cum_type} cases"),
         subtitle = "Top 10 Countries") +
    ylab(glue::glue("cumulative {cum_type} cases on ", {format(as.Date(input, origin = "1970-01-01"), "%Y-%b-%d")})) +
    xlab("Country") +
    coord_flip() +
    scale_y_continuous(labels = comma)

  ggplotly(conf_plot, tooltip = c("confirmed", "population"))
}


