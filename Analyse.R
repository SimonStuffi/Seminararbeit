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

kunden <- bind_rows(kunden_2016, kunden_2017, kunden_2018) 
"Kunden2019n"
ges <- left_join(kunden, dplz, "PLZ")
names(ges)[7] =  "NettoDB"

#Daten diskreter gestalten

ges <- ges %>%
  mutate(Quad = ifelse(Breitengrad <= 50 & Längengrad <= 10, "<50.<10", 
                ifelse(Breitengrad > 50 & Längengrad <= 10 & Breitengrad <= 52, "<52.<10",
                ifelse(Breitengrad > 52 & Längengrad <= 10, ">52.<10",
                ifelse(Breitengrad <= 50 & Längengrad > 10, "<50.>10", 
                ifelse(Breitengrad > 50 & Längengrad > 10 & Breitengrad <= 52, "<52.>10",
                ifelse(Breitengrad > 52 & Längengrad > 10, ">52.>10", NA)))))))

ges <- ges %>%
  mutate(Umsatz = ifelse(Umsatz <= 20000, "0-20000",
                  ifelse(Umsatz > 20000 & Umsatz <= 40000, "20000-40000",
                  ifelse(Umsatz > 40000 & Umsatz <= 60000, "40000-60000",
                  ifelse(Umsatz > 60000 & Umsatz <= 80000, "60000-80000",
                  ifelse(Umsatz > 80000, "80000-100000",NA)))))) %>%
  mutate(NettoDB = ifelse(NettoDB > 75, "75-100%",
                   ifelse(NettoDB > 50 & NettoDB <= 75, "50-75%",
                   ifelse(NettoDB > 25 & NettoDB <= 50, "25-50%",
                   ifelse(NettoDB <= 25, "0-25%", NA))))) %>%
  mutate(Preisumsetzung = ifelse(Preisumsetzung < 0, "<0",
                          ifelse(Preisumsetzung >= 0 & Preisumsetzung <= 1, "0-1",
                          ifelse(Preisumsetzung > 1 & Preisumsetzung <= 2.5, "1-2,5",
                          ifelse(Preisumsetzung > 2.5 & Preisumsetzung <= 5, "2,5-5",
                          ifelse(Preisumsetzung > 5, ">5", NA))))))
    
    
kd <- ges[-c(1,2,3,4,13,12,14,15)]

kdA <- subset(ges, Prognose == "A")
kdNA <- subset(ges, Prognose == "NA")
"Ort Visualisieren:"
plot(kdA$Breitengrad, kdA$Längengrad)
plot(kdNA$Breitengrad, kdNA$Längengrad)


#Hier werden die Daten für die Entropieanalyse 1 vorbereitet:



Ent <- function(x,y){
    return((-1)*x*log(x,2)-y*log(y,2))
}


EntKonzernmarken1 <- as.data.frame.matrix(table(kd$Konzernmarken, kd$Prognose)) %>%
  mutate(Typ = "Konzernmarken")
EntKundenklasse1 <- as.data.frame.matrix(table(kd$Kundenklasse , kd$Prognose)) %>%
  mutate(Typ = "Kundenklasse")
EntQuad1 <- as.data.frame.matrix(table(kd$Quad , kd$Prognose)) %>%
  mutate(Typ = "Quad")
EntUmsatz1 <- as.data.frame.matrix(table(kd$Umsatz , kd$Prognose)) %>%
  mutate(Typ = "Umsatz")
EntNettoDb1 <- as.data.frame.matrix(table(kd$NettoDB , kd$Prognose)) %>%
  mutate(Typ = "NettoDB")
EntPreisumsetzung1 <- as.data.frame.matrix(table(kd$Preisumsetzung , kd$Prognose)) %>%
  mutate(Typ = "Preisumsetzung")


Entropiedata1 <- bind_rows(EntKundenklasse1,EntKonzernmarken1,EntNettoDb1,
                          EntPreisumsetzung1,EntQuad1,EntUmsatz1)
names(Entropiedata1)[2] =  "notA"

#Hier werden Entropiedaten Stufe 1 ausgerechnet:

Entropiedata1 <- Entropiedata1 %>%
  mutate(Anteil = (A+notA)) %>%
  mutate(p1 = A/(A + notA)) %>%
  mutate(p2 = 1-p1) %>%
  mutate(Entropie = Ent(p1,p2)) %>%
  group_by(Typ) %>%
  mutate(gesamt = (sum(A)+sum(notA))) %>%
  ungroup() %>%
  mutate(Entropieanteil = Anteil/gesamt*Entropie)

Entropietest1 <- Entropiedata1 %>%
  group_by(Typ) %>%
  summarise(Gesamtentropie = sum(Entropieanteil),
            Entropiegewinn = Ent(sum(A)/(sum(A)+sum(notA)),
                                 sum(notA)/(sum(A)+sum(notA))) - sum(Entropieanteil)) %>%
  arrange(desc(Entropiegewinn))

#Hier wird Stufe 2 vorbereitet unter der verwendung von Konzernmarken als Stufe 1
#funktion erstellen

entData2 <- function(daten,name){
  
  EntKundenklasse <- as.data.frame.matrix(table(daten$Kundenklasse , daten$Prognose)) %>%
    mutate(Typ = "Kundenklasse")
  EntQuad <- as.data.frame.matrix(table(daten$Quad , daten$Prognose)) %>%
    mutate(Typ = "Quad")
  EntUmsatz <- as.data.frame.matrix(table(daten$Umsatz , daten$Prognose)) %>%
    mutate(Typ = "Umsatz")
  EntNettoDb <- as.data.frame.matrix(table(daten$NettoDB , daten$Prognose)) %>%
    mutate(Typ = "NettoDB")
  EntPreisumsetzung <- as.data.frame.matrix(table(daten$Preisumsetzung , daten$Prognose)) %>%
    mutate(Typ = "Preisumsetzung")
  
  
  Entropiedata <- bind_rows(EntKundenklasse,EntNettoDb,
                            EntPreisumsetzung,EntQuad,EntUmsatz)
  names(Entropiedata)[2] =  "notA"
  
  return(Entropiedata)
  
}
entropieanalyse <- function(daten){

  Entropiedata <- daten %>%
    mutate(Anteil = (A+notA)) %>%
    mutate(p1 = A/(A + notA)) %>%
    mutate(p2 = 1-p1) %>%
    mutate(Entropie = Ent(p1,p2)) %>%
    group_by(Typ) %>%
    mutate(gesamt = (sum(A)+sum(notA))) %>%
    ungroup()
  Entropiedata$Entropie[is.nan(Entropiedata$Entropie)] <- 0
    Entropiedata <- Entropiedata %>%
    mutate(Entropieanteil = (Anteil/gesamt)*Entropie)

  Entropietest <- Entropiedata %>%
    group_by(Typ) %>%
    summarise(Gesamtentropie = sum(Entropieanteil),
            Entropiegewinn = Ent(sum(A)/(sum(A)+sum(notA)),
                                 sum(notA)/(sum(A)+sum(notA))) - sum(Entropieanteil)) %>%
    arrange(desc(Entropiegewinn))

  return(Entropietest)
}
  
#anwednung der funktion


kd2_1 <- subset(kd,Konzernmarken == 1)
kd2_2 <- subset(kd,Konzernmarken == 2)
kd2_3 <- subset(kd,Konzernmarken == 3)
kd2_4 <- subset(kd,Konzernmarken == 4)
kd2_5 <- subset(kd,Konzernmarken == 5)
kd2_6 <- subset(kd,Konzernmarken == 6)
kd2_7 <- subset(kd,Konzernmarken == 7)
kd2_8 <- subset(kd,Konzernmarken == 8)
kd2_9 <- subset(kd,Konzernmarken == 9)
kd2_10 <- subset(kd,Konzernmarken == 10)
kd2_11 <- subset(kd,Konzernmarken == 11)
kd2_12 <- subset(kd,Konzernmarken == 12)
kd2_13 <- subset(kd,Konzernmarken == 13)


Entropiedata2_1 <- entData2(kd2_1,"1")
Entropiedata2_2 <- entData2(kd2_2,"2")
Entropiedata2_3 <- entData2(kd2_3,"3")
Entropiedata2_4 <- entData2(kd2_4,"4")
Entropiedata2_5 <- entData2(kd2_5,"5")
Entropiedata2_6 <- entData2(kd2_6,"6")
Entropiedata2_7 <- entData2(kd2_7,"7")
Entropiedata2_8 <- entData2(kd2_8,"8")
Entropiedata2_9 <- entData2(kd2_9,"9")
Entropiedata2_10 <- entData2(kd2_10,"10")
Entropiedata2_11 <- entData2(kd2_11,"11")
Entropiedata2_12 <- entData2(kd2_12,"12")
Entropiedata2_13 <- entData2(kd2_13,"13")


  
Entropietest2_1 <- entropieanalyse(Entropiedata2_1)
Entropietest2_2 <- entropieanalyse(Entropiedata2_2 )
Entropietest2_3 <- entropieanalyse(Entropiedata2_3 )
Entropietest2_4 <- entropieanalyse(Entropiedata2_4 )
Entropietest2_5 <- entropieanalyse(Entropiedata2_5 )
Entropietest2_6 <- entropieanalyse(Entropiedata2_6 )
Entropietest2_7 <- entropieanalyse(Entropiedata2_7)
Entropietest2_8 <- entropieanalyse(Entropiedata2_8 )
Entropietest2_9 <- entropieanalyse(Entropiedata2_9 )
Entropietest2_10 <- entropieanalyse(Entropiedata2_10 )
Entropietest2_11 <- entropieanalyse(Entropiedata2_11 )
Entropietest2_12 <- entropieanalyse(Entropiedata2_12 )
Entropietest2_13 <- entropieanalyse(Entropiedata2_13 )

ErgebnisStufe2 <- cbind(c(1:13),
           c(Entropietest2_1[1,1],
             Entropietest2_2[1,1],
             Entropietest2_3[1,1],
             Entropietest2_4[1,1],
             Entropietest2_5[1,1],
             Entropietest2_6[1,1],
             Entropietest2_7[1,1],
             Entropietest2_8[1,1],
             Entropietest2_9[1,1],
             Entropietest2_10[1,1],
             Entropietest2_11[1,1],
             Entropietest2_12[1,1],
             Entropietest2_13[1,1]
             ),
           c(Entropietest2_1[1,3],
             Entropietest2_2[1,3],
             Entropietest2_3[1,3],
             Entropietest2_4[1,3],
             Entropietest2_5[1,3],
             Entropietest2_6[1,3],
             Entropietest2_7[1,3],
             Entropietest2_8[1,3],
             Entropietest2_9[1,3],
             Entropietest2_10[1,3],
             Entropietest2_11[1,3],
             Entropietest2_12[1,3],
             Entropietest2_13[1,3]
              ))
