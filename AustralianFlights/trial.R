
#
library(ggplot2)
library(tidyverse)
library(magrittr)
library(gridExtra)
#
flights = readr::read_csv(
  "flights.csv"
)
flights$Date <- as.Date(paste(flights$Year, flights$Month_num,"01", sep = "-")) 
#
#Create Year Month DayCol
flights$Date <- as.Date(paste(flights$Year, flights$Month_num,"01", sep = "-"))#, 
flights$Date

##InOut-----------------------------
in_out_flights_data <- flights %>% 
  select(In_Out, All_Flights, Date) %>% 
  group_by(In_Out, Date) %>% 
  summarise(sum = sum(All_Flights))
head(in_out_flights_data)

#Plot of Inbound and OutBoubd Flights Over Course of Time
plot_in_out_flight <- in_out_flights_data %>% 
  ggplot(aes(x = Date, y = sum, color = In_Out)) + 
  geom_line() + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  geom_point() +
  theme_bw() + labs(title = "Total In and OutBound Flights") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "top",
        plot.title = element_text(hjust = 0.5)) + 
  scale_color_discrete(
    limits = c("I","O"),
    labels = c("Inbound", "OutBound")
  ) +
  guides(color = guide_legend(title = NULL))
plot_in_out_flight
#

## City Traffic ---------------------

in_out_city_flights_data <- flights %>% 
  select(In_Out, Australian_City, All_Flights, Date) %>% 
  group_by(In_Out, Australian_City, Date) %>% 
  summarise(sum = sum(All_Flights)) %>% 
  mutate(Bound = recode(In_Out, "I" = "In Bound", "O" = "Out Bound"))
head(in_out_city_flights_data)

#Plot of Inbound and OutBoubd Flights Over Course of Time
plot_in_out_city_flight <- in_out_city_flights_data %>% 
  filter(Australian_City %in% c("Brisbane", "Melbourne", "Perth", "Sydney")) %>% 
  ggplot(aes(x = Date, y = sum, color = Australian_City)) + 
  geom_line() + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  geom_point() +
  theme_bw() + labs(title = "Top Cities With Total In and OutBound Flights") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold", size = rel(1.5))) + 
  scale_y_continuous(limits = c(0, max(in_out_city_flights_data$sum)))+
  guides(color = guide_legend(nrow=1,byrow=TRUE,title = NULL)) + 
  facet_grid(Bound ~.)

plot_in_out_city_flight
#
## Airline Traffic ---------------------

in_out_air_flights_data <- flights %>% 
  select(In_Out, Airline, All_Flights, Date) %>% 
  group_by(In_Out, Airline, Date) %>% 
  summarise(sum = sum(All_Flights)) %>% 
  mutate(Bound = recode(In_Out, "I" = "In Bound", "O" = "Out Bound"))
head(in_out_air_flights_data)

#Plot of Inbound and OutBound Flights Over Course of Time
plot_in_out_air_flight <- in_out_air_flights_data %>% 
  filter(Airline %in% c(
    "Air New Zealand", "Emirates", "Jetstar", "Qantas Airways",
    "Singapore Airlines", "Virgin Australia","Cathay Pacific Airways")) %>% 
  #filter(sum>250) %>% 
  ggplot(aes(x = Date, y = sum, color = Airline)) + 
  geom_line() + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  geom_point() +
  theme_bw() + labs(title = "Top Airlines With Total In and OutBound Flights") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold", size = rel(1.5))) + 
  scale_y_continuous(limits = c(0, max(in_out_air_flights_data$sum)))+
  guides(color = guide_legend(nrow=1,byrow=TRUE,title = NULL)) + 
  facet_grid(Bound ~.)

plot_in_out_air_flight
#
## Route Traffic ---------------------

in_out_route_flights_data <- flights %>% 
  select(In_Out, Route, All_Flights, Date) %>% 
  group_by(In_Out, Route, Date) %>% 
  summarise(sum = sum(All_Flights)) %>% 
  mutate(Bound = recode(In_Out, "I" = "In Bound", "O" = "Out Bound"))
head(in_out_route_flights_data)

#Plot of Inbound and OutBound Flights Over Course of Time
plot_in_out_route_flight <- in_out_route_flights_data %>% 
  filter(Route %in% c("AKL-SYD", "SYD-AKL")) %>% 
  #filter(sum>250) %>% 
  ggplot(aes(x = Date, y = sum, color = Route)) + 
  geom_line() + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  geom_point() +
  theme_bw() + labs(title = "Top Airlines With Total In and OutBound Flights") + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold", size = rel(1.5))) + 
  #scale_y_continuous(limits = c(0, max(in_out_route_flights_data$sum)))+
  guides(color = guide_legend(nrow=1,byrow=TRUE,title = NULL)) + 
  facet_grid(Bound ~.)

plot_in_out_route_flight

#Count of in and outbound flights------------
In_Out_Plot <- flights %>% 
  mutate(Bound = recode(In_Out, "I" = "In Bound", "O" = "Out Bound")) %>% 
  ggplot(aes(x = Bound)) + 
  geom_bar() + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") + 
  theme_light() + ggtitle("(a) In_Out Flights") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
In_Out_Plot



#count -------------------
aus_cities_plot <- flights %>% 
  ggplot(aes(y = Australian_City)) + 
  geom_bar() + ggtitle("(b) Australian Cities") +
  geom_text(aes(label = ..count..), stat = "count",hjust = -.2 ,colour = "black") +
  theme_light() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) 
aus_cities_plot
#
int_cities_plot <- flights %>% 
  ggplot(aes(x = International_City)) + 
  geom_bar() + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -1.5, colour = "black") +
  theme_light() + ggtitle("(c) International Cities") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90))
int_cities_plot
#
airline_plot <- flights %>% 
  ggplot(aes(x = Airline)) + 
  geom_bar() + ggtitle("(d) Airline Plots") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -1.5, 
            colour = "black") +
  theme_light() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
airline_plot

#
route_plot <- flights %>% ####Top
  ggplot(aes(x = Route)) + 
  geom_bar() + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  theme_light() + ggtitle("(e) ") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
route_plot

#Join Ports Data
port_plot <- flights %>% na.omit() %>% 
  ggplot(aes(x = Port_Country)) + 
  geom_bar() + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  theme_light() + ggtitle("(f) ") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
port_plot
#
port_reg_plot <- flights %>% 
  ggplot(aes(x = Port_Region)) + 
  geom_bar() + ggtitle("(g) Port Reg") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  theme_light() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
port_reg_plot
#
serv_country_plot <- flights %>% 
  ggplot(aes(x = Service_Country)) + 
  geom_bar() + ggtitle("(g) Port Reg") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  theme_light() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
serv_country_plot
#
serv_reg_plot <- flights %>% 
  ggplot(aes(x = Service_Region)) + 
  geom_bar() + ggtitle("(g) Port Reg") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  theme_light() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
serv_reg_plot
#
stops_plot <- flights %>% 
  ggplot(aes(x = Stops)) + 
  geom_bar() + ggtitle("(g) Port Reg") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  theme_light() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
stops_plot
#
all_flight_plot <- flights %>% 
  ggplot(aes(x = All_Flights)) + 
  geom_bar() + ggtitle("(g) Port Reg") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  theme_light() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())
all_flight_plot

#
count_plots <-
  grid.arrange(mature,
               premie,
               marital,
               lowbirthweight,
               gender,
               habit,
               whitemom)
count_plots

