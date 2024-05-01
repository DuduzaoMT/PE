#install.packages("ggplot2")

library(ggplot2)
theme_set(theme_bw())

url <- 'C:/Users/Utilizador/OneDrive - Universidade de Lisboa/universidade/PE/master.csv'
dados <- read.csv(url, check.names = FALSE)

dados |>
  subset(year == 1986 & age == "25-34 years") |>
  ggplot(aes(x=`sex`,y=`suicides/100k pop`,fill = sex))+
  ylim(0,100)+
  geom_boxplot()+
  scale_x_discrete(labels = c("Female","Male"))+
  scale_fill_discrete(labels = c("Female", "Male"))+
  ggtitle("Suicide rates in 1986 for people aged 25-34") +
  theme(plot.title = element_text(
    size = rel(1.4), face = "bold.italic", colour = "black"))+
  labs(
    x = "Gender",
    y = "Suicides per 100k population",
    fill = "Gender")+
  theme(axis.title.x = element_text(
    size = 11, face = "italic", colour = "black"))+
  theme(axis.title.y = element_text(
    size = 11, face = "italic", colour = "black"))