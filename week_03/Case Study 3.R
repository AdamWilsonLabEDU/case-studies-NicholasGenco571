## Install and load necessary packages
library(ggplot2)
library(gapminder)
library(dplyr)
gapminder_filter <- gapminder %>%
  filter(country != "Kuwait")

P1=ggplot(gapminder_filter, aes(x = lifeExp, y = gdpPercap, color = continent, size = pop/100000)) +
  geom_point() +
  facet_wrap(~year ,nrow=1)  +
  scale_y_continuous(trans = "sqrt") +
  theme_bw() +
  labs("continent", size="Pop(100K)")

gapminder_continent <- gapminder_filter %>% group_by(continent, year) %>% summarize(gdpPercap = weighted.mean(x = gdpPercap, w = pop), pop = sum(as.numeric(pop)))

P2=ggplot(gapminder_filter, aes(x = year, y = gdpPercap, color = continent)) +
  geom_point() +
  geom_line(aes(group = country)) +
  geom_line(data=gapminder_continent, color = "black") +
  geom_point(data=gapminder_continent, aes(size = pop), color = "black") +
  facet_wrap(~continent ,nrow=1)  +
  scale_y_continuous(trans = "sqrt") +
  theme_bw() +
  labs("continent", size="Pop(100K)")
ggsave(P1, width = 15, filename = "P1.png")
ggsave(P2, width = 15, filename = "P2.png")
