
# load packages
library(readxl)
library(httr)
library(janitor)
library(dplyr)
library(ggplot2)


# Download the data from Tableau.com>Products>TableauPublic>Resources> 
# into the R Environment.
url1 <- 'https://public.tableau.com/app/sample-data/tour_de_france.xlsx'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
tour_de_france <- read_excel(tf) |> clean_names()

# save to CSV data 
write.csv(x = tour_de_france, file = 'data/get_tour_de_france.csv', 
          row.names = F)

# Check missing values in the data
colSums(is.na(tour_de_france)) |> 
  as.data.frame() |> knitr::kable(caption = 'Missing Data Count')
# 
str(tour_de_france)

# Exploratory Data Analysis (EDA)
head(tour_de_france)
# Variables
# winning_time  = total_distance_km / winners_avg_speed
# Finishing rate = finishers/entrants
# duration_in_hours = end_date - start_date
# above average speeds = above/below
# 
tour_de_france_EDA <- tour_de_france |> 
  select(-contains('latitude'), -contains('longitude')) |> 
  mutate(
    # winning_time  = total_distance_km / winners_avg_speed
    winning_duration_time = total_distance_km / winners_avg_speed,
    # Finishing rate = finishers/entrants
    competition_finishing_rate = finishers / entrants,
    # above average speeds = above/below
    winners_avg_speed_above_below_all = ifelse(
      winners_avg_speed > mean(winners_avg_speed), 'above', 'below'
    ),
    # deviation from the overall winning time mean
    winners_avg_speed_deviation = winners_avg_speed - mean(winners_avg_speed)
  ) |> 
  mutate(
    winning_duration = case_when(
      winning_duration_time < 100 ~ '<100',
      winning_duration_time >= 100 & winning_duration_time < 200 ~ '>=100 to <200',
      winning_duration_time >= 200 ~ '>=200'
    )
  )
#
winners_average_speed_deviation <- tour_de_france_EDA |> 
  ggplot(mapping = aes(x = year, y = winners_avg_speed_deviation)) + 
  geom_line() + 
  geom_area() +
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  labs(title = 'Tour de France Statistics Since 1903',
       subtitle = "Winner's Average Speed Deviation",
       y = 'Average Speed Deviation',
       fill = 'Deviation from Average Speed',
       caption = paste("https://github.com/akhapwoyaco", "  |   ", url1)
       ) + 
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.3, 0.8), 
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5), 
    legend.background = element_blank()
  )
winners_average_speed_deviation
#
ggsave(plot = winners_average_speed_deviation,
       filename = 'plots/winners_average_speed_deviation.jpeg', 
       width = 25, height = 20, units = 'cm', dpi = 700)
#
competition_finish_rate <- tour_de_france_EDA |> 
  ggplot(mapping = aes(x = year, y = competition_finishing_rate)) + 
  geom_point(aes(color = winning_duration))+ 
  theme_bw() + 
  labs(title = 'Tour de France Statistics Since 1903',
       subtitle = "Competition Finish Rate: finishers / entrants",
       y = 'Finishing Rate: finishers / entrants',
       col = 'Winning Duration',
       caption = paste("https://github.com/akhapwoyaco", "  |   ", url1)
         ) + 
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.85, 0.2), 
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5), 
    legend.background = element_blank()
  )
competition_finish_rate
#
ggsave(plot = competition_finish_rate,
       filename = 'plots/competition_finish_rate.jpeg', 
       width = 25, height = 20, units = 'cm', dpi = 700)
#



