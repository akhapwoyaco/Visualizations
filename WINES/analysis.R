# The data on wine constituents is obtained from the [UC Irvine Machine Learning Repository](https://archive.ics.uci.edu/dataset/109/wine). 
# The data details the the quantitive level of 13 constituents in three types of wine.

# get data
col_names <- c(
  "Cultivar",
  "Alcohol", "Malic_acid", "Ash", "Alcalinity_of_ash", 
  "Magnesium", "Total_phenols", "Flavanoids", 
  "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity", 
  "Hue", "OD280_OD315_of_diluted_wines", "Proline"
)
#
wine <- read.csv("wine/wine.data", header=FALSE)
names(wine) <- col_names
wine$Cultivar <- factor(
  wine$Cultivar,levels = c(1:3), 
  ordered = FALSE)

# The data contains `r ncol(wine)` variables, and `r nrow(wine)` number of observations.

## Summary Statistics

table(wine$Cultivar) |> 
  knitr::kable(
    col.names = c('Cultivar', "Frequency"))

## Exploratory Data Analysis

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

wine_long_fomart <- wine |> 
  # add a column to state the index of observation
  mutate(ID = row_number(), .before = 'Cultivar') |> 
  pivot_longer(
    cols = Alcohol:Proline,
    names_to = 'Feature'
  )

## Distribution of wine constituents by Cultivar
density_plots <- wine_long_fomart |> 
  ggplot(aes(fill = Cultivar, x = value)) +
  geom_density(alpha=0.4) + 
  facet_wrap(Feature~., scales = 'free') + 
  labs(
    y = 'Density', 
    title = 'Density Plots', caption = "https://github.com/akhapwoyaco",
    subtitle = 'Quantities of 13 constituents found in each of the three types of wines') +
  theme_bw()  + 
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.7, 0.15),
    legend.direction = 'horizontal', 
    axis.title.x = element_blank(), 
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5)
  )
density_plots
ggsave( 
  density_plots, height = 20, units = "cm", 
  filename = "density_plots.png", width = 20)
#

wine_box_plots = wine_long_fomart |> 
  ggplot(aes(fill = Cultivar, y = Cultivar, x = value)) +
  geom_boxplot() +
  facet_wrap(Feature~., scales = 'free') + 
  labs(y = ' ', 
       title = 'Boxplot', caption = "https://github.com/akhapwoyaco",
       subtitle = 'Quantities of 13 constituents found in each of the three types of wines') +
  theme_bw()  + 
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.7, 0.15),
    legend.direction = 'horizontal', 
    axis.title.x = element_blank(), 
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5)
  ) 
wine_box_plots
ggsave( 
  wine_box_plots, height = 20, units = "cm", 
  filename = "wine_box_plots.png", width = 20)
#

## Correlation Analysis

# correlation
wine_correlation_plot <- split(x = wine, f = wine$Cultivar) |> purrr::map(select, -Cultivar) |>
  purrr::map(cor) |> 
  reshape2::melt() |> rename(Cultivar = L1) |> 
  mutate(
    value = round(value, 2)
  ) |>
  ggplot(
    aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = value), color = 'black', size = 2.5) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  facet_wrap(~Cultivar,nrow = 1) + 
  theme_bw() + 
  labs(caption = "https://github.com/akhapwoyaco") +
  theme(
    axis.text.x = element_text(angle = 90), 
    legend.position =  "none", axis.title = element_blank()
  )
wine_correlation_plot
ggsave(
  wine_correlation_plot, height = 20, units = "cm",
        filename = "wine_correlation_plot.png", width = 30)
#