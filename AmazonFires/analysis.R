# load libraries

library(tidyverse)
library(scales)
library(readr)
library(lubridate)
MONTH_LEVELS = c(
  "Janeiro", "Fevereiro",
  parse_character("Mar\xe7o", locale = locale(encoding = "ISO-8859-1")),
  "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro",
  "Outubro", "Novembro", "Dezembro")

library(readr)
amazon <- read_csv(
  "data/amazon.csv", 
  col_types = cols(
    year = col_number(), 
    date = col_date(format = "%Y-%m-%d")),
  locale = locale(encoding = "ISO-8859-1", grouping_mark = "." )
) |>
  mutate(
    month_en = month(date, label = T),
  )
View(amazon)
#
# # import data
# amazon <- read_csv(
#   "data/amazon.csv", 
#   col_types = cols_only(
#     month = col_factor( 
#       palette = "Dark2",
#       levels = c(
#         "Janeiro", "Fevereiro",
#         parse_character("Mar\xe7o", locale = locale(encoding = "ISO-8859-1")),
#         "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro",
#         "Outubro", "Novembro", "Dezembro"), ordered = T, pal),
#     state = col_factor(
#       palette = "Dark2",
#       levels = c(
#         "Acre", "Alagoas", "Amapa",
#         "Amazonas", "Bahia", "Ceara",
#         "Distrito Federal", "Espirito Santo",
#         "Goias", "Maranhao", "Mato Grosso",
#         "Minas Gerais",
#         parse_character("Par\xe1",
#                         locale = locale(encoding = "ISO-8859-1")),
#         "Paraiba", "Pernambuco", "Piau", "Rio", "Rondonia",
#         "Roraima", "Santa Catarina", "Sao Paulo",
#         "Sergipe", "Tocantins"), ordered = F)#,
#     date = col_date(format = "%Y-%m-%d"),
#     year = col_factor(
#       # levels = c("1998", "1999", "2000", "2001", "2002", "2003","2004",
#       #            "2005", "2006", "2007", "2008","2009", "2010", "2011",
#       #            "2012", "2013", "2014", "2015", "2016", "2017"), ordered = T)
#   ),
#   locale = locale(
#     encoding = "ISO-8859-1", grouping_mark = "." )
# )
amazon
# wider format
amazon[,-c(2,5)] %>% 
  tidyr::pivot_wider( values_from = number, 
                      names_from = (month_en), 
                      values_fn = list(number = sum) ) %>% 
  print(n = Inf)
#
amazon %>% dplyr::group_by(state) %>% 
  summarise(total = sum(number)) %>% 
  arrange(total) %>% print(n = Inf)
#
amazon %>% dplyr::group_by(month) %>% 
  summarise(total = sum(number)) %>% 
  arrange(total) %>% print(n = Inf)
amazon %>% dplyr::group_by(year) %>% 
  summarise(total = sum(number)) %>% 
  arrange(total) %>% print(n = Inf)
options(scipen = 999) #library(scales)
# PLOTS
p1 <- amazon %>% dplyr::group_by(state) %>% 
  summarise(total = sum(number)) %>% 
  ggplot(aes(x = state, y = total)) + 
  geom_col() + theme_bw() + labs(
    caption = "https://github.com/akhapwoyaco", 
    y = "Total Fire Cases")+
  geom_text(aes(label = total), vjust = -0.3, size = 3.5) +
  theme(
    panel.grid.major.x =  element_blank(), axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 28, hjust = 1, vjust = 1,
                               colour = "gray5")) + 
  annotate ("text", x = 5, y = Inf ,
            label = "cases by state 1998 - 2017", vjust = 1.5 , size = 8)

p2 <- amazon %>% dplyr::group_by(month) %>% 
  summarise(total = sum(number)) %>% 
  ggplot(aes(x = month, y = total)) + 
  geom_col() + theme_bw() + labs( y = "Total Fire Cases")+
  geom_text(aes(label = total), vjust = -0.3, size = 3) +
  theme(
    panel.grid.major.x =  element_blank(), axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 28, hjust = 1, vjust = 1,
                               colour = "gray5")) + 
  annotate ("text", x = 5, y = Inf ,
            label = "cases by months 1998 - 2017", vjust = 1.5 , size = 8)


p3 <- amazon %>% dplyr::group_by(year) %>% 
  summarise(total = sum(number)) %>% 
  ggplot(aes(x = year, y = total)) + 
  geom_col() + theme_bw() + 
  labs(y = "Total Fire Cases")+
  geom_text(aes(x = year, label = total), vjust = -0.3, size = 3) +
  theme(
    panel.grid.major.x =  element_blank(), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      angle = 28, hjust = 1, vjust = 1,
      colour = "gray5")) 
p3
k = gridExtra::grid.arrange(p1, p2, p3, nrow = 2)
# k <- ggarrange(p1, ggarrange(p2, p3,  ncol=2), nrow = 2)
k
ggsave("plot/AmazonFires.png", plot = k, dpi = 700, units = "cm",
       width = 80, height = 65, limitsize = FALSE)

###############################################################
# 
# library(ggplot2)
# #ALL GRAPHICS
# 
# p <- ggplot(data = filter(amazon, year == 2017),
#             mapping = aes(x = month, y = number, fill = state))
# p1 <- p + geom_col(position = "dodge2") + 
#   facet_grid( ~year, scales = "free") +
#   coord_flip() + 
#   theme_minimal() +
#   theme(legend.position = "bottom", 
#         axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
#   labs(fill = NULL) + 
#   scale_fill_discrete(guide = guide_legend(nrow = 2, ncol = 12, byrow = TRUE))
# p1
# ggsave("p1.png", plot = p1, dpi = 700, units = "cm",
#        width = 80, height = 65, limitsize = FALSE)
# #
