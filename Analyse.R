library(tidyverse)
library(tidycensus)
library(modelsummary)
library(kableExtra)
library(stargazer)
library(AER)
library(scales)
library(readxl)
library(caTools)
library(rpart)
library(rpart.plot)


kunden_2016 <- read_excel("Kunden 2016.xlsx") %>%
  mutate(Jahr = as.numeric(2016))

kunden_2017 <- read_excel("Kunden 2017.xlsx") %>%
  mutate(Jahr = as.numeric(2017))
prog2017 <- kunden_2017[c(2,9)]
names(prog2017) <- c("Unternehmen","Prognose")
kunden_2016 <- left_join(kunden_2016,prog2017,"Unternehmen")
 
kunden_2018 <- read_excel("Kunden 2018.xlsx") %>%
  mutate(Jahr = as.numeric(2018))
prog2018 <- kunden_2018[c(2,9)]
names(prog2018) <- c("Unternehmen","Prognose")
kunden_2017 <- left_join(kunden_2017,prog2018,"Unternehmen")


kunden_2019 <- read_excel("Kunden 2019.xlsx") %>%
  mutate(Jahr = as.numeric(2019))
prog2019 <- kunden_2019[c(2,9)]
names(prog2019) <- c("Unternehmen","Prognose")
kunden_2018 <- left_join(kunden_2018,prog2019,"Unternehmen")

plz <- read_excel("Postleitzahlen.xlsx")

names(plz)[3] = "PLZ"
plz$PLZ = as.double(plz$PLZ)
dplz <- plz %>% distinct(PLZ, .keep_all = TRUE)

kunden <- bind_rows(kunden_2016, kunden_2017, kunden_2018) 
"Kunden2019n"
ges <- left_join(kunden, dplz, "PLZ")
names(ges)[7] =  "NettoDB"

ges <- ges %>%
  mutate(Quad = ifelse(Breitengrad <= 50 & Längengrad <= 10, "<50.<10", 
                ifelse(Breitengrad > 50 & Längengrad <= 10 & Breitengrad <= 52, "<52.<10",
                ifelse(Breitengrad > 52 & Längengrad <= 10, ">52.<10",
                ifelse(Breitengrad <= 50 & Längengrad > 10, "<50.>10", 
                ifelse(Breitengrad > 50 & Längengrad > 10 & Breitengrad <= 52, "<52.>10",
                ifelse(Breitengrad > 52 & Längengrad > 10, ">52.>10", NA)))))))

ges <- ges %>%
  mutate(Umsatz = ifelse(Umsatz <= 20000, "<20000",
                  ifelse(Umsatz > 20000 & Umsatz <= 40000, "20000-40000",
                  ifelse(Umsatz > 40000 & Umsatz <= 60000, "40000-60000",
                  ifelse(Umsatz > 60000 & Umsatz <= 80000, "60000-80000",
                  ifelse(Umsatz > 80000, ">80000",NA))))))
    
    
    
kd <- ges[-c(1,3,4,13,12,14,15)]

kdA <- subset(ges, Prognose == "A")
kdNA <- subset(ges, Prognose == "NA")
"Ort Visualisieren:"
plot(kdA$Breitengrad, kdA$Längengrad)
plot(kdNA$Breitengrad, kdNA$Längengrad)







table(as.factor(kd$Kundenklasse))

AMarke <- c( sum(kd$Konzernmarken == 1 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 2 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 3 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 4 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 5 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 6 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 7 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 8 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 9 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 10 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 11 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 12 & kd$Prognose == "A"),
             sum(kd$Konzernmarken == 13 & kd$Prognose == "A"))  
NAMarke <- c(sum(kd$Konzernmarken == 1 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 2 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 3 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 4 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 5 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 6 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 7 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 8 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 9 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 10 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 11 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 12 & kd$Prognose == "NA"),
             sum(kd$Konzernmarken == 13& kd$Prognose == "NA"))
Marke <- data.frame(AMarke,NAMarke)
Marke


table(kd$Konzernmarken, kd$Prognose)
table(kd$Kundenklasse , kd$Prognose)
table(kd$Jahr , kd$Prognose)
table(kd$Quad , kd$Prognose)


