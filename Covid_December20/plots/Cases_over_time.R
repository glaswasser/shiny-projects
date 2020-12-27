


# description
get_description <- function() {
  return("Daily cases over time. Click on the legend to omit lines")
}

get_title <- function() {
  return("Daily cases over time. Click on the legend to omit lines")
}

get_plot <- function(df, input, time_range, countries, relative) {
  
  # auto-select afghanistan to avoid errors
  if (length(countries) == 0) {
    countries = "Afghanistan"
  }
  # if relative checkbox is checked:
  if (relative == TRUE) {
    df$cases <- df$relative_cases
  }
  
  
  
  # create plot
  over_time_plot <- df %>% 
    filter(cases > 0,
           country %in% countries,
           # filter custom reactive dates:
           between(date, as.Date(time_range[1], origin = "1970-01-01"),
                   as.Date(time_range[2], origin = "1970-01-01"))) %>% 
    ggplot(aes(x = date, y = cases, colour = type, group = country, linetype = country)) +
    geom_line() +
    scale_y_continuous(labels = comma) +
    labs(title = glue("Daily cases over time for Countries: {glue_collapse(countries, sep = \", \")}"),
         subtitle = "Click on the legend to omit lines")
  
  ggplotly(over_time_plot)
}
