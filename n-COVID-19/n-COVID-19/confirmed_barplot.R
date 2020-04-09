get_description <- function() {
  return("Test description about item plot")
}

# confirmed cases:

get_plot <- function(dat) {
  
  exciting <- d %>% 
    filter(country != "China") %>% 
    filter(time == as.Date(input$sliderNum, origin = "1899-12-30")) %>% 
    top_n(n = 10, wt = cum_confirm) %>% 
    ggplot(aes(x = reorder(country, cum_confirm), y = cum_confirm, fill = country)) +
    geom_bar(stat = "identity") +
    guides(fill = FALSE) +
    ggtitle("Confirmed cases in countries (top 10 outside china)") +
    ylab("cumulative confirmed cases") +
    xlab("Country") +
    coord_flip()
  ggplotly(p, key = "text")
}
