#install.packages("ggplot2")
#install.packages("readxl")

library(dplyr)
library(readxl)
library(ggplot2)
theme_set(theme_light())

url <- 'C:/Users/Utilizador/OneDrive - Universidade de Lisboa/universidade/PE/electricity.xlsx'
#dados <- readxl::read_xlsx(url)
dados$TIME <- as.Date(paste(dados$YEAR,dados$MONTH, "01",sep='-'))
paises <- ("")

colmy <- c("Latvia"="darkred", "Italy" = "steelblue","IEA Total" = "orange")
country <- c("Latvia","Italy","IEA Total")
#grafico
dados |>
  subset(TIME > as.Date("2015-01-01") & PRODUCT == "Renewables" & COUNTRY %in% country ) |>
  ggplot(aes(x=TIME,y = as.numeric(share)*100,color = COUNTRY))+
  geom_line()+
  geom_point()+
  theme_bw()+
  ylim(0,100)+
  scale_color_manual(values = colmy)+
  scale_x_date(date_labels = "%Y %b",date_breaks = "6 months")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(title = "Percentagem de Energia Renovável ao Longo do Tempo", x = "Tempo", y = "Percentagem de Energia Renovável",color = "País")