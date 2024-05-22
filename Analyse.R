library(tidyverse)
library(tidycensus)
library(modelsummary)
library(kableExtra)
library(stargazer)
library(AER)
library(scales)
library(readxl)


kunden_2016 <- read_excel("Kunden 2016.xlsx") %>%
  mutate(Jahr = as.numeric(2016))
kunden_2017 <- read_excel("Kunden 2017.xlsx") %>%
  mutate(Jahr = as.numeric(2016))
kunden_2018 <- read_excel("Kunden 2018.xlsx") %>%
  mutate(Jahr = as.numeric(2016))
kunden_2019 <- read_excel("Kunden 2019.xlsx") %>%
  mutate(Jahr = as.numeric(2016))
plz <- read_excel("Postleitzahlen.xlsx")

names(plz)[3] = "PLZ"
plz$PLZ = as.double(plz$PLZ)
dplz <- plz %>% distinct(PLZ, .keep_all = TRUE)

kunden <- bind_rows(kunden_2016, kunden_2017, kunden_2018, kunden_2019)
ges <- left_join(kunden, dplz, "PLZ")

kd <- ges[-c(1,11,12)]
kd$Kundenklasse <- kd$Kundenklasse == "A"
kd$Kundenklasse <- as.numeric(kd$Kundenklasse)

"plot(kd$Breitengrad, kd$Längengrad)"

reg <- lm(Kundenklasse ~ Konzernmarken + Umsatz,data = kd)


