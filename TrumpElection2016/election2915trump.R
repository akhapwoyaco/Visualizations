## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------

library(tidyverse)
election_2016 <- read_csv("election_2016.csv")
head(election_2016)



## --------------------------------------------------------------------------------------------------------------------

election_2016_margin <- election_2016 |> 
  mutate(
    trump_percent = (trump/totalvotes)*100,
    clinton_percent = (clinton/totalvotes)*100,
    trump_margin = trump_percent - clinton_percent
  )

  
head(election_2016_margin[,c(-5,-7,-6,-8)]) 



## --------------------------------------------------------------------------------------------------------------------

summary(election_2016_margin)



## ----message=FALSE---------------------------------------------------------------------------------------------------
trump_hist <- election_2016_margin |> 
  ggplot(aes(x = trump_margin)) +
  geom_histogram(fill = "blue", colour = "black", alpha = .4) + 
  scale_x_continuous(labels = scales::label_percent(scale = 1) ) +
  labs(title = "Histogram of Trump's Percentage Vote Margin",
       x = "Percent Margin",
       caption = "https://github.com/akhapwoyaco"
       ) +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5) )

trump_hist

ggsave(plot = trump_hist, filename = "trump_hist.png", width = 35, height = 25, units = "cm", dpi = 400)


## --------------------------------------------------------------------------------------------------------------------

election_2016_poverty <- election_2016 |> 
  mutate( totalpov = (poverty/100) * population )

knitr::kable( head(election_2016_poverty[,c(-3,-4,-5,-6)]) )



## ----message=FALSE---------------------------------------------------------------------------------------------------

election_2016_state <- election_2016_poverty |> group_by(stname) |> 
  summarize(totalpov_state = sum(totalpov),
            population_state = sum(population)) |> 
  mutate(statepovrate = totalpov_state/population_state ) |> 
  arrange(desc(population_state) )

knitr::kable( election_2016_state ) 


## --------------------------------------------------------------------------------------------------------------------

povrate_scat <- election_2016_state |> 
  ggplot(aes(y = reorder(stname, statepovrate), x = statepovrate)) +
  geom_point() + 
  labs(
    x = "State Poverty Rate (Percentage)",
    caption = "https://github.com/akhapwoyaco") + 
  scale_x_continuous(
    labels = scales::label_percent(scale = 100) ) + 
  geom_text(
    data = subset(election_2016_state, population_state > 10000000),
    aes(label = population_state), hjust = -0.4, size = 3.5) +
  theme_bw() + 
  theme(
    axis.title.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey60" , linetype = "dashed")
  ) 

povrate_scat

ggsave(
  plot = povrate_scat, filename = "povrate_scatter.png",
  width = 40, height = 35, units = "cm", dpi = 400)

