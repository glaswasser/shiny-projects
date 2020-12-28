


# description
get_description <- function() {
  return("Weekly new cases over time. For smoothness, sums per week are used instead of single days.
         Click on the legend to omit lines. Double click in the legend to isolate lines.")
}

options(scipen = 999)

#coronavirus %>% group_by(week = lubridate::week(date)) %>%  summarize(week_cases = mean(cases))


get_plot <- function(df, input, time_range, countries, relative_cum, relative_overtime, cum_type) {
  
  # auto-select afghanistan to avoid errors
  if (length(countries) == 0) {
    countries = "Germany"
  }
  # if relative checkbox is checked:
  if (relative_overtime == TRUE) {
    df$cases <- df$relative_cases
  }
  # get mean per week because of smoothness
  df %<>%
    group_by(date = cut(date, "week"), country, type) %>%
    summarise(cases = sum(cases),
              type = type) 
  # convert date to date format
  df$date <- as.Date(df$date)
  
  # create plot
  over_time_plot <- df %>% 
    filter(country %in% countries,
           # filter custom reactive dates:
           between(date, as.Date(time_range[1], origin = "1970-01-01"),
                   as.Date(time_range[2], origin = "1970-01-01"))) %>% 
    ggplot(aes(x = date, y = cases, colour = type, group = country, linetype = country)) +
    geom_line() +
    scale_y_continuous(labels = comma) +
    labs(title = glue("Weekly new cases over time for Countries: {glue_collapse(countries, sep = \", \")}"),
         subtitle = "Click on the legend to omit lines")

  ggplotly(over_time_plot)
}
