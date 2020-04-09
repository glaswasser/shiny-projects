library(gganimate)

get_description <- function() {
  return("Test description about item plot")
}

# confirmed cases:

get_plot <- function(d, input) {
  
  exciting <- d %>% 
    filter(country != "China") %>% 
    top_n(n = 10, wt = cum_confirm) %>% 
    ggplot(aes(x = reorder(country, cum_confirm), y = cum_confirm, fill = country)) +
    geom_bar(stat = "identity") +
    guides(fill = FALSE) +
    ggtitle("Confirmed cases in countries (top 10 outside china)") +
    ylab("cumulative confirmed cases") +
    xlab("Country") +
    coord_flip()
  
  plotly <- ggplotly(exciting, key = "text")
}


exciting
exciting

d$time

two <- exciting + transition_time(time) +
  labs(title = "time: {frame_time}")

two

d %>% 
  filter(country != "China") %>% 
  top_n(n = 10, wt = cum_confirm) %>% 
  ggplot(aes(x = reorder(country, cum_confirm), y = cum_confirm, fill = country)) +
  geom_bar(stat = "identity") +
  guides(fill = FALSE) +
  ggtitle("Confirmed cases in countries (top 10 outside china)") +
  ylab("cumulative confirmed cases") +
  xlab("Country") +
  coord_flip()
