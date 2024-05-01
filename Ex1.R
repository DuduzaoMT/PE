#install.packages("ggrepel")
#install.packages("ggplot2")

library(ggplot2)
library(ggrepel)
theme_set(theme_bw())

url <- 'C:/Users/Utilizador/OneDrive - Universidade de Lisboa/universidade/PE/Paises_PIB_ICH.csv'
dados <- read.csv(url, check.names = FALSE)
continentes <- c("Europe", "Americas")
paises <- c("Lithuania","Iceland","United States", "Saint Lucia")

dados |>
  subset(Continent %in% continentes) |>
  ggplot(aes(`HCI`, `GDP`,color = Continent)) +
  geom_point(size=3) +
  scale_x_log10() +
  ylim(0,150000)+
  scale_colour_brewer(palette = "Set2")+
  geom_text(data = subset(dados, Country %in% paises), aes(label = Country),vjust = -0.5,color="black")+
  geom_segment(data = subset(dados, Country %in% paises), 
               aes(x = HCI, y = GDP, xend = HCI, yend = GDP + 5000),
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  ggtitle("Human Capital Index (HCI) Vs GDP per capita (GDP)\nbetween Americas and Europe\n") +
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", colour = "Black"))+
  labs(
       x = "Human Capital Index (HCI)",
       y = "GDP per capita (GDP)",
       color = "Continent")+
  theme(axis.title.x = element_text(
    size = 12, face = "italic", colour = "black"))+
  theme(axis.title.y = element_text(
    size = 12, face = "italic", colour = "black"))