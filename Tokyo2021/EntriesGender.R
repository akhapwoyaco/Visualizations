#
library(readxl)
EntriesGender <- read_excel(
  "data/EntriesGender.xlsx", 
  col_types = c("text", "numeric", "numeric", "numeric"))
head(EntriesGender)
tail(EntriesGender)
#
library(tidyr)
library(magrittr)
library(ggplot2)
library(dplyr)

# #Gender -----------------------------------------------------------------

gender_datas <- EntriesGender %>% 
  pivot_longer(
    cols = !Discipline,
    values_to = "Entries",
    names_to = "gender_category"
  ) %>% 
  filter(!grepl("Total", gender_category)) 
#plot
gender_datas_plot <- gender_datas  %>% 
  ggplot(aes(x = Entries, y = reorder(Discipline,Entries), fill = gender_category)) + 
  geom_col(position = "dodge")+ 
  labs(title = "Total Entries", 
       caption = "https://github.com/akhapwoyaco") +
  theme_bw() + theme(
    axis.title.y = element_blank(), 
    legend.title = element_blank(),
    legend.position = 'inside',
    legend.position.inside = c(0.7,0.7), 
    legend.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
gender_datas_plot

ggsave("plots/gender_datas_plot.png", gender_datas_plot, 
       width = 30, height = 25, dpi = 750, units = "cm")


# Total Entries -----------------------------------------------------------

total_datas <- EntriesGender %>% 
  pivot_longer(
    cols = !Discipline,
    values_to = "Entries",
    names_to = "gender_category"
  ) %>% 
  filter(grepl("Total", gender_category))
#plot
total_datas_plot <- total_datas %>% 
  ggplot(aes(x = Entries,y = reorder(Discipline,Entries))) + 
  geom_col(position = "dodge") + 
  labs(caption = "https://github.com/akhapwoyaco", title = "Total Entries") +
  theme_bw() + theme(
    axis.title.y = element_blank(), 
    legend.title = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
total_datas_plot

ggsave("plots/total_datas_plot.png", total_datas_plot, 
       width = 30, height = 25, dpi = 750, units = "cm")
