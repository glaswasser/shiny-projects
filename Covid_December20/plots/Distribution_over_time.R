get_description <- function() {
  if (countries == unique(df$country)){
    countries = "Worldwide"
  }
  return(glue("Covid 19 Cases - evolution over time in selected countries. If no country is selected,
              worldwide cases are used."))
}



get_plot <- function(df, input, time_range, countries, relative_cum, relative_overtime, cum_type) {
  
  if (length(countries) == 0) {
    countries = unique(df$country)
  }

df %>% 
  filter(country %in% countries) %>% 
  group_by(type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date) %>%
  mutate(active = confirmed - death - recovered) %>%
  mutate(active_total = cumsum(active),
         recovered_total = cumsum(recovered),
         death_total = cumsum(death)) %>%
  plot_ly(x = ~ date,
          y = ~ active_total,
          name = 'Active', 
          fillcolor = '#1f77b4',
          type = 'scatter',
          mode = 'none', 
          stackgroup = 'one') %>%
  add_trace(y = ~ death_total, 
            name = "Death",
            fillcolor = '#E41317') %>%
  add_trace(y = ~recovered_total, 
            name = 'Recovered', 
            fillcolor = 'forestgreen') %>%
  layout(title = glue("Distribution of Covid19 Cases {if(all(countries==unique(df$country))){(\"Worldwide\")} else{glue_collapse(countries, sep = \", \")}}"),
         legend = list(x = 0.1, y = 0.9),
         yaxis = list(title = "Number of Cases"),
         xaxis = list(title = "Source: Johns Hopkins University Center for Systems Science and Engineering"))
}


all(countries == unique(df$country))

