library("dplyr")
library("ggplot2")
library("GGally")
library("mgcv")
library("gratia")
library("gridExtra")

## Utils
red = "#e60049"
blue = "#0bb4ff"
green = "#50e991"
yellow = "#e6d800"
purple = "#9b19f5"
orange = "#ffa300"
pink = "#dc0ab4"
babyBlue = "#b3d4ff"
algaeGreen ="#00bfa0"

my.colors = c(red,blue,green,yellow,
              purple,
              orange,
              pink,
              babyBlue,
              algaeGreen)

my_theme <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text = element_text(size = rel(0.70), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = "NA"),
      
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}

get.bike.data = function(){
  url =  "https://raw.githubusercontent.com/christophM/interpretable-ml-book/master/data/bike.csv"
  file = download.file(url, "bikes.csv")
  
  bike = read.csv('bikes.csv', stringsAsFactors = T) %>% data.frame()
}

# Relevant variables
variables.of.interest = c('season','holiday', 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', 'days_since_2011', "cnt", "weekday")

# Read data and extract variables of interest
bikes = get.bike.data() %>% dplyr::select(variables.of.interest)

# Summarise data
summary(bikes)

# Create plot
ggpairs(bikes %>% select(c(temp, season, workingday, weathersit, weekday, cnt))) +
  labs(subtitle = "Numeric variable exploration") +
  my_theme()

# Data exploration
ggplot(bikes, aes(x = season, cnt, fill = weathersit)) +
  geom_boxplot() +
  labs(y = "Number or rented bikes", x = "Season", fill = "Weather situation",
       subtitle = "Effect of season and weather")+
  scale_fill_manual(values = c(orange, yellow, blue)) + my_theme()

p1 = ggplot(bikes, aes(weathersit, cnt, color = temp)) + geom_jitter() +
  geom_boxplot(alpha = 0.5) + scale_color_gradient(high = "red", low = "yellow") +
  labs(y = "Number of rented bikes", x = "Weather situation", subtitle = "Effect of temperature and weather") + my_theme() +
  theme(legend.position = "none")

p3 = ggplot(bikes, aes(temp, cnt, color = temp)) + geom_point() +
  labs(y = NULL, x = "Temperature", color = "Temp [C]", subtitle = "")+
  facet_wrap("season")+
  geom_smooth( method = "lm") + scale_color_gradient(high = "red", low = "yellow") +
  my_theme() 

grid.arrange(p1, p3, ncol= 2,
             widths = c(1,1.5))

# Fit model
M = gam(cnt ~ season + weathersit  + s(days_since_2011, bs ="cr", k = 70) +
          s(temp, bs = "cr", by = season, k = 15), data = bikes, )

# Summarize model
summary(M)

# Checking k-value and edf
k.check(M)

# Plot smooth and parametric effects
draw(M, parametric = TRUE)
