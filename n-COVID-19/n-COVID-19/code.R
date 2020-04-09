
# create a bar plot
#boring:
d %>% 
  filter(country != "China") %>% 
  filter(time == as.Date("2020-03-05", origin = "1899-12-30")) %>% 
  arrange(desc(cum_confirm)) %>% 
  top_n(n = 10, wt = cum_confirm) %>% 
  ggplot(aes(x = factor(country), y = cum_confirm))+
  stat_summary(fun="mean", geom="bar") +  
  ggtitle("Confirmed cases in countries (top 10 outside china)") +
  ylab("cumulative confirmed cases") +
  xlab("Country")


#exciting:
exciting <- d %>% 
  filter(country != "China") %>% 
  filter(time == as.Date("2020-03-05", origin = "1899-12-30")) %>% 
  top_n(n = 10, wt = cum_confirm) %>% 
  ggplot(aes(x = reorder(country, cum_confirm), y = cum_confirm, fill = country)) +
  geom_bar(stat = "identity") +
  guides(fill = FALSE) +
  ggtitle("Confirmed cases in countries (top 10 outside china)") +
  ylab("cumulative confirmed cases") +
  xlab("Country") +
  coord_flip()

exciting

ggplotly(exciting, key = "text")

