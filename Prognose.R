library(tidyverse)
library(tidycensus)
library(modelsummary)
library(kableExtra)
library(stargazer)
library(AER)
library(scales)
library(readxl)
library(caTools)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(Boruta)
library(cvms)
library(dplyr)
library(arulesCBA)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

#Daten importieren und für merge vorbereiten


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

#Daten mergen

kunden <- bind_rows(kunden_2016, kunden_2017, kunden_2018,kunden_2019) 
"Kunden2019n"
ges <- left_join(kunden, dplz, "PLZ")
names(ges)[7] =  "NettoDB"

ges <- discretizeDF(ges, methods = list(
  Umsatz = list(method = "frequency", breaks = 2,labels = c("Hoher Umsatz","Niedriger Umsatz")),
  NettoDB = list(method = "frequency", breaks = 3,labels = c("Hoher Netto-DB","Mittlerer Netto-DB","Niedriger Netto-DB")),
  Preisumsetzung = list(method = "frequency", breaks = 3,labels = c("Hohe Pr.Um.","Mittlere Pr.Um.","Niedrige Pr.Um."))),
  default = list(method = "none"))


Breitgr <- (discretize(ges$Breitengrad, breaks = 2, onlycuts = TRUE))[2]
Längengr <- (discretize(ges$Längengrad, breaks = 2, onlycuts = TRUE))[2]

ges <- ges %>%
  mutate(Quad = ifelse(Breitengrad <= Breitgr & Längengrad <= Längengr, "<50,2724/<9,7901", 
                       ifelse(Breitengrad > Breitgr & Längengrad <= Längengr, ">50,2724/<9,7901",
                              ifelse(Breitengrad <= Breitgr & Längengrad > Längengr, "50,2724/>9,7901", 
                                     ifelse(Breitengrad > Breitgr & Längengrad > Längengr, ">50,2724/>9,7901", NA)))))


ges <- ges %>%
  filter(Jahr == 2019)

progdat <- ges %>%
  select("Unternehmen","Prognose","Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad")%>%
  mutate(Kundenklasse = ifelse(Kundenklasse != "A","notA","A"))

decTree <- read.csv("Decisiontree_fin.csv")


prognose <- function(erg, dictree){
  
  lu <- c("","","Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad")
  
  for(u in erg$Unternehmen){
    
    unt <- erg %>%
      filter(Unternehmen == u)
    
    branch <- dictree
    
    i <- which(lu == branch$step1[1])
    wert <- as.character(unt[i])
    
    print(i)
    
    print(wert)
    
    branch <- branch %>%
      filter(Aup1 == wert)
    
    
    i <- which(lu == branch$step2[1])
    wert <- as.character(unt[i,])
    
    branch <- branch %>%
      filter(Aup2 == wert)
    
    
    i <- which(lu == branch$step3[1])
    wert <- as.character(unt[i,])
    
    branch <- branch %>%
      filter(Aup3 == wert)
    
    i <- which(lu == branch$step4[1])
    wert <- as.character(unt[i,])
    
    branch <- branch %>%
      filter(Aup4 == wert)
    
    i <- which(lu == branch$step5[1])
    wert <- as.character(unt[i,])
    
    branch <- branch %>%
      filter(Aup5 == wert)
    
    i <- which(lu == branch$step6[1])
    wert <- as.character(unt[i,])
    
    
    branch <- branch %>%
      filter(Aup6 == wert)
    
    
  }
}

finito <- prognose(progdat, decTree)
