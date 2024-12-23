library(readxl)
Medals <- read_excel(
  "data/Medals.xlsx", 
  col_types = c("numeric","text","numeric","numeric","numeric","numeric","numeric"))
View(Medals)

medals_long_data <- Medals %>% 
  filter(Rank<=10) %>% 
  select(-Rank, -`Rank by Total`) %>% 
  pivot_longer(
    Gold:Bronze, values_to = "count",
    names_to = "title"
  ) 
medals_long_plot <- medals_long_data%>% 
  ggplot(aes(x = count, y = reorder(`Team/NOC`,Total), fill = title)) +
  geom_col(position = "dodge") + 
  labs(caption = "https://github.com/akhapwoyaco", x = "Top 10 Medal Count") +
  theme_bw() + theme(
    axis.title.y = element_blank(), 
    legend.title = element_blank(),
    legend.position = 'inside',
    legend.position.inside = c(0.9,0.2), 
    legend.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
medals_long_plot  
ggsave("plots/medals_long_plot.png", medals_long_plot, 
       width = 30, height = 25, dpi = 750, units = "cm")
