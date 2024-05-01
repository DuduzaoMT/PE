#install.packages("ggplot2")
#install.packages("readxl")

library(dplyr)
library(readxl)
library(ggplot2)
theme_set(theme_light())

url <- 'C:/Users/Utilizador/OneDrive - Universidade de Lisboa/universidade/PE/electricity.xlsx'
#dados <- readxl::read_xlsx(url)
dados$TIME <- as.Date(paste(dados$YEAR,dados$MONTH, "01",sep='-'))

# Plotar o gráfico de linha
dados_nonrenewables <- dados |> subset(TIME >= as.Date("2015-01-01") & COUNTRY == "IEA Total" & PRODUCT == "Non-renewables") |>
  group_by(COUNTRY,TIME) |> summarise(NONRENEWABLES = sum(as.numeric(VALUE)))


dados_renewables <- dados |> subset(TIME >= as.Date("2015-01-01") & COUNTRY == "IEA Total" & PRODUCT == "Renewables") |>
  group_by(COUNTRY,TIME) |> summarise(RENEWABLES = sum(as.numeric(VALUE)))

dados_final <- cbind(dados_nonrenewables,dados_renewables["RENEWABLES"])

italy_non <- dados |> subset(TIME >= as.Date("2015-01-01") & COUNTRY == "Italy" & PRODUCT == "Non-renewables") |>
  group_by(COUNTRY,TIME) |> summarise(NONRENEWABLES = sum(as.numeric(VALUE)))


italy_renewables <- dados |> subset(TIME >= as.Date("2015-01-01") & COUNTRY == "Italy" & PRODUCT == "Renewables") |>
  group_by(COUNTRY,TIME) |> summarise(RENEWABLES = sum(as.numeric(VALUE)))

italy_final <- cbind(italy_non,italy_renewables["RENEWABLES"])

latvia_non <- dados |> subset(TIME >= as.Date("2015-01-01") & COUNTRY == "Latvia" & PRODUCT == "Non-renewables") |>
  group_by(COUNTRY,TIME) |> summarise(NONRENEWABLES = sum(as.numeric(VALUE)))


latvia_renewables <- dados |> subset(TIME >= as.Date("2015-01-01") & COUNTRY == "Latvia" & PRODUCT == "Renewables") |>
  group_by(COUNTRY,TIME) |> summarise(RENEWABLES = sum(as.numeric(VALUE)))

latvia_final <- cbind(latvia_non,latvia_renewables["RENEWABLES"])

dados_tabel <- rbind(dados_final,italy_final,latvia_final)


#grafico
dados_tabel |>
  subset(TIME > as.Date("2015-01-01")) |>
  ggplot(aes(x=TIME,y = (as.numeric(RENEWABLES)/(as.numeric(NONRENEWABLES)+as.numeric(RENEWABLES)))*100,color = COUNTRY))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ylim(0,100)+
  scale_x_date(date_labels = "%Y %b",date_breaks = "3 months")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(title = "Porcentagem de Energia Renovável ao Longo do Tempo", x = "Time", y = "Porcentagem de Energia Renovável",color = "País")