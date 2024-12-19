#
salary <- read.csv("Salary.csv", stringsAsFactors=TRUE)
head(salary)
#
library(ggplot2)
salary_hist <- salary |>
  ggplot(aes(x = Salary)) +
  geom_histogram(binwidth = 30, color = "black") +
  theme_bw() + 
  labs(
    title = "Salary Distribution", 
    caption = "https://github.com/akhapwoyaco") +
  theme(plot.title = element_text(hjust = 0.5))
salary_hist
#
ggsave(
  "salary_hist.png",plot = salary_hist, 
  width = 15, height = 15, unit = "cm", dpi = 400)
##

