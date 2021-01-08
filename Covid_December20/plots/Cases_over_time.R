


# description
get_description <- function() {
  return("Weekly new cases over time. For smoothness, sums over the last 7 days are used instead of single days.
         Click on the legend to omit lines. Double click in the legend to isolate lines.")
}

options(scipen = 999)

#coronavirus %>% group_by(week = lubridate::week(date)) %>%  summarize(week_cases = mean(cases))

#countries = "Germany"
#time_range = c("2021-01-01", "2021-01-03")


get_plot <- function(df, input, time_range, countries, relative_cum, relative_overtime, cum_type) {
  
  # auto-select afghanistan to avoid errors
  if (length(countries) == 0) {
    countries = "Germany"
  }
  # if relative checkbox is checked:
  if (relative_overtime == TRUE) {
    df$cases <- df$relative_cases
  }
  
  # create seven days case sum and date range
  df %<>% 
    group_by(country, type) %>% 
    mutate(seven_days_case_sum =  reduce(map(0:6, ~ lag(cases, ., 0)), `+`), # see https://stackoverflow.com/questions/47463429/r-group-lag-sum
           date_range = paste(lag(date, 6, default = min(date)),"-", date))
    
    
  df$type <- factor(df$type, levels = c("death", "recovered", "confirmed"))
  
  
  # create plot
  over_time_plot <- df %>% 
    filter(country %in% countries,
           cases > 0,
           # filter custom reactive dates:
           between(date, as.Date(time_range[1], origin = "1970-01-01"),
                   as.Date(time_range[2], origin = "1970-01-01"))) %>% 
    ggplot(aes(x = date, y = seven_days_case_sum, colour = type, group = country, linetype = country, label = date_range,
               text = paste(" Country: ", country, "\n",
                            "Date: ", date, "\n",
                            "Type of cases: ", type, "\n",
                            "Cases over 7 days: ", seven_days_case_sum, "\n",
                            "Date range: ", date_range
               ))) +
    geom_line() +
    scale_y_continuous(labels = comma) +
    labs(title = glue("Weekly new cases over time for Countries: {glue_collapse(countries, sep = \", \")}"),
         subtitle = "Click on the legend to omit lines") 

  ggplotly(over_time_plot, tooltip = "text")
  
  #ggplotly(over_time_plot, tooltip = c("colour", "group", "x", "label", "y"))
}

