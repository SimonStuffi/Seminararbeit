# Achtung: viele libraries werden nicht verwendet
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
library(rsample)

#NAN zähler

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



#                               Klassifikation:



Klassifikation <- rpart(Kundenklasse ~ Umsatz, data = ges)
rpart.plot(Klassifikation)

#Es ist klar, dass der Umsatz ausreicht um die momentane Kundenklasse zu klassifizieren


#                           Decision tree learner:


#Daten Diskret gestalten:

gesA <- ges %>%
  filter(Prognose == "A")
gesNA <- ges %>%
  filter(Prognose == "NA")

#                                   Umsatz

Erg <- data.frame(breaks = c(2:13),
                  differenz1 = rep(0,12))
                 
x <- 2

for (x in 2:13) {
    
  
    Diskret_test <- data.frame(dataA = discretize(gesA$Umsatz, method = "cluster", breaks = x,onlycuts = TRUE),
                       dataNA = discretize(gesNA$Umsatz, method = "cluster", breaks = x,onlycuts = TRUE),
                       Index = x) %>%
      filter(dataA != max(dataA) & dataA != min(dataA)) %>%
      mutate(differenz = dataNA - dataA) 
    Erg[x-1,2] = mean(abs(Diskret_test$differenz))
    print(Diskret_test)
  }

plot(Erg$differenz1, type = "l")


Erg <- data.frame(breaks = c(2:13),
                  differenz1 = rep(0,12))
x <- 2
for (x in 2:13) {
    
    
    Diskret_test <- data.frame(dataA = discretize(gesA$Umsatz, breaks = x,onlycuts = TRUE),
                               dataNA = discretize(gesNA$Umsatz, breaks = x,onlycuts = TRUE),
                               Index = x) %>%
      filter(dataA != max(dataA) & dataA != min(dataA)) %>%
      mutate(differenz = dataNA - dataA) 
    Erg[x-1,2] = mean(abs(Diskret_test$differenz))
}
plot(Erg$breaks,Erg$differenz1, type = "l")


hist(ges$Umsatz, breaks = 20, main = "Frequency")
abline(v = discretize(gesA$Umsatz, breaks = 5, 
                      onlycuts = TRUE), col = "red")
abline(v = discretize(gesNA$Umsatz, breaks = 5, 
                      onlycuts = TRUE), col = "blue")
abline(v = discretize(ges$Umsatz, breaks = 5, 
                      onlycuts = TRUE), col = "green")


#                         NettoDB


ErgNettoDb <- data.frame(breaks = c(2:13),
                  differenz1 = rep(0,12))
x <- 2
for (x in 2:13) {
  
  Diskret_test <- data.frame(dataA = discretize(gesA$NettoDB, breaks = x,onlycuts = TRUE),
                             dataNA = discretize(gesNA$NettoDB, breaks = x,onlycuts = TRUE),
                             Index = x) %>%
    filter(dataA != max(dataA) & dataA != min(dataA)) %>%
    mutate(differenz = dataNA - dataA) 
  ErgNettoDb[x-1,2] = mean(abs(Diskret_test$differenz))
}
plot(Erg$breaks,ErgNettoDb$differenz1, type = "l")


Diskret_testNettoDB3 <- data.frame(dataA = discretize(gesA$NettoDB, breaks = 3,onlycuts = TRUE),
                                   dataNA = discretize(gesNA$NettoDB, breaks = 3,onlycuts = TRUE))%>%
  filter(dataA != max(dataA) & dataA != min(dataA)) %>%
  mutate(differenz = dataNA - dataA)
Diskret_testNettoDB5 <- data.frame(dataA = discretize(gesA$NettoDB, breaks = 5,onlycuts = TRUE),
                                   dataNA = discretize(gesNA$NettoDB, breaks = 5,onlycuts = TRUE))%>%
  filter(dataA != max(dataA) & dataA != min(dataA)) %>%
  mutate(differenz = dataNA - dataA)
Diskret_testNettoDB10 <- data.frame(dataA = discretize(gesA$NettoDB, breaks = 10,onlycuts = TRUE),
                                    dataNA = discretize(gesNA$NettoDB, breaks = 10,onlycuts = TRUE))%>%
  filter(dataA != max(dataA) & dataA != min(dataA)) %>%
  mutate(differenz = dataNA - dataA)

#                         Preisumsetzung

hist(gesA$Preisumsetzung, breaks = 20, main = "Preisumsetzung A")
hist(gesNA$Preisumsetzung, breaks = 20, main = "Preisumsetzung NA")
abline(v = discretize(gesA$Preisumsetzung, breaks = 6, 
                      onlycuts = TRUE), col = "red")
abline(v = discretize(gesNA$Preisumsetzung, breaks = 6, 
                      onlycuts = TRUE), col = "blue")
abline(v = discretize(ges$Preisumsetzung, breaks = 6, 
                      onlycuts = TRUE), col = "green")


ErgPreisumsetzung <- data.frame(breaks = c(2:13),
                         differenz1 = rep(0,12))
x <- 2
for (x in 2:13) {
  
  Diskret_test <- data.frame(dataA = discretize(gesA$Preisumsetzung, breaks = x,onlycuts = TRUE),
                             dataNA = discretize(gesNA$Preisumsetzung, breaks = x,onlycuts = TRUE),
                             Index = x) %>%
    filter(dataA != max(dataA) & dataA != min(dataA)) %>%
    mutate(differenz = dataNA - dataA) 
  ErgPreisumsetzung[x-1,2] = mean(abs(Diskret_test$differenz))
}
plot(ErgPreisumsetzung$breaks,ErgPreisumsetzung$differenz1, type = "l")

Diskret_testPreis1 <- data.frame(dataA = discretize(gesA$Preisumsetzung, breaks = 3,onlycuts = TRUE),
                                   dataNA = discretize(gesNA$Preisumsetzung, breaks = 3,onlycuts = TRUE))%>%
  filter(dataA != max(dataA) & dataA != min(dataA)) %>%
  mutate(differenz = dataNA - dataA)
Diskret_testPreis2 <- data.frame(dataA = discretize(gesA$Preisumsetzung, breaks = 6,onlycuts = TRUE),
                                dataNA = discretize(gesNA$Preisumsetzung, breaks = 6,onlycuts = TRUE))%>%
  filter(dataA != max(dataA) & dataA != min(dataA)) %>%
  mutate(differenz = dataNA - dataA)

#                             Koordinaten




ErgBreitengrade <- data.frame(breaks = c(2:13),
                                differenz1 = rep(0,12))
x <- 2
for (x in 2:13) {
  
  Diskret_test <- data.frame(dataA = discretize(gesA$Breitengrad, breaks = x,onlycuts = TRUE),
                             dataNA = discretize(gesNA$Breitengrad, breaks = x,onlycuts = TRUE),
                             Index = x) %>%
    filter(dataA != max(dataA) & dataA != min(dataA)) %>%
    mutate(differenz = dataNA - dataA) 
  ErgBreitengrade[x-1,2] = mean(abs(Diskret_test$differenz))
}
plot(ErgBreitengrade$breaks,ErgBreitengrade$differenz1, type = "l")

ErgLänge <- data.frame(breaks = c(2:13),
                              differenz1 = rep(0,12))
x <- 2
for (x in 2:13) {
  
  Diskret_test <- data.frame(dataA = discretize(gesA$Längengrad, breaks = x,onlycuts = TRUE),
                             dataNA = discretize(gesNA$Längengrad, breaks = x,onlycuts = TRUE),
                             Index = x) %>%
    filter(dataA != max(dataA) & dataA != min(dataA)) %>%
    mutate(differenz = dataNA - dataA) 
  ErgLänge[x-1,2] = mean(abs(Diskret_test$differenz))
}
plot(ErgLänge$breaks,ErgLänge$differenz1, type = "l")


z <- 2
plot(gesA$Breitengrad, gesA$Längengrad, breaks = 20, main = "Breitengrade")
abline(v = discretize(gesA$Breitengrad, breaks = z, 
                      onlycuts = TRUE), col = "red")
abline(v = discretize(gesNA$Breitengrad, breaks = z, 
                      onlycuts = TRUE), col = "blue")
abline(h = discretize(gesA$Längengrad, breaks = z, 
                      onlycuts = TRUE), col = "red")
abline(h = discretize(gesNA$Längengrad, breaks = z, 
                      onlycuts = TRUE), col = "blue")
abline(v = discretize(ges$Breitengrad, breaks = z, 
                      onlycuts = TRUE), col = "green")
abline(h = discretize(ges$Längengrad, breaks = z, 
                      onlycuts = TRUE), col = "green")

Breitgr <- (discretize(ges$Breitengrad, breaks = 2, onlycuts = TRUE))[2]
Längengr <- (discretize(ges$Längengrad, breaks = 2,onlycuts = TRUE))[2]


#                             Konzernmarken

ErgKOnz <- data.frame(breaks = c(2:13),
                       differenz1 = rep(0,12))

x <- 2
for (x in 2:13) {
  
  Diskret_test <- data.frame(dataA = discretize(gesA$Konzernmarken, breaks = x,onlycuts = TRUE),
                             dataNA = discretize(gesNA$Konzernmarken, breaks = x,onlycuts = TRUE),
                             Index = x) %>%
    filter(dataA != max(dataA) & dataA != min(dataA)) %>%
    mutate(differenz = dataNA - dataA) 
  ErgKOnz[x-1,2] = mean(abs(Diskret_test$differenz))
}
plot(ErgKOnz$breaks,ErgKOnz$differenz1, type = "l")



#                       Diskretisieren abschließen

gesDisc <- discretizeDF(ges, methods = list(
                      Umsatz = list(method = "frequency", breaks = 2,labels = c("Hoher Umsatz","Niedriger Umsatz")),
                      NettoDB = list(method = "frequency", breaks = 3,labels = c("Hoher Netto-DB","Mittlerer Netto-DB","Niedriger Netto-DB")),
                      Preisumsetzung = list(method = "frequency", breaks = 3,labels = c("Hohe Pr.Um.","Mittlere Pr.Um.","Niedrige Pr.Um."))),
                      default = list(method = "none"))




gesDisc <- gesDisc %>%
  mutate(Quad = ifelse(Breitengrad <= Breitgr & Längengrad <= Längengr, "<50,2724/<9,7901", 
                ifelse(Breitengrad > Breitgr & Längengrad <= Längengr, ">50,2724/<9,7901",
                ifelse(Breitengrad <= Breitgr & Längengrad > Längengr, "<50,2724/>9,7901", 
                ifelse(Breitengrad > Breitgr & Längengrad > Längengr, ">50,2724/>9,7901", NA)))))


gesDisc <- gesDisc %>%
  filter(Jahr != 2019)



split <- initial_split(gesDisc, prop = 3/4)

gestrain <- training(split)
gestest <- testing(split)

    
kddisc <- gestrain %>%
  select("Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad", "Prognose", "Jahr") %>%
  mutate(Kundenklasse = ifelse(Kundenklasse != "A","notA","A"))

kdtest <- gestest %>%
  select("Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad", "Prognose", "Jahr") %>%
  mutate(Kundenklasse = ifelse(Kundenklasse != "A","notA","A"))


################################################################################
###################    Full recursive decision tree learner  ###################
################################################################################

######### Eröffnungungsfunktion:

start_rec_ent <- function(daten){
  
  cdat <- daten
  
  EntKonzernmarkenA <- as.data.frame.matrix(table(cdat$Konzernmarken, cdat$Prognose)) %>%
    mutate(Typ = "Konzernmarken")%>%
    mutate(Auspragung = row.names(table(cdat$Konzernmarken, cdat$Prognose)))
  EntKundenklasseA <- as.data.frame.matrix(table(cdat$Kundenklasse , cdat$Prognose)) %>%
    mutate(Typ = "Kundenklasse") %>%
    mutate(Auspragung = row.names(table(cdat$Kundenklasse , cdat$Prognose)))
  EntQuadA <- as.data.frame.matrix(table(cdat$Quad , cdat$Prognose)) %>%
    mutate(Typ = "Quad") %>%
    mutate(Auspragung = row.names(table(cdat$Quad , cdat$Prognose)))
  EntUmsatzA <- as.data.frame.matrix(table(cdat$Umsatz , cdat$Prognose)) %>%
    mutate(Typ = "Umsatz") %>%
    mutate(Auspragung = row.names(table(cdat$Umsatz , cdat$Prognose)))
  EntNettoDbA <- as.data.frame.matrix(table(cdat$NettoDB , cdat$Prognose)) %>%
    mutate(Typ = "NettoDB") %>%
    mutate(Auspragung = row.names(table(cdat$NettoDB , cdat$Prognose)))
  EntPreisumsetzungA <- as.data.frame.matrix(table(cdat$Preisumsetzung , cdat$Prognose)) %>%
    mutate(Typ = "Preisumsetzung") %>%
    mutate(Auspragung = row.names(table(cdat$Preisumsetzung , cdat$Prognose)))
  
  
  EntropiedataA <- bind_rows(EntKundenklasseA,EntKonzernmarkenA,EntNettoDbA,
                             EntPreisumsetzungA,EntQuadA,EntUmsatzA)
  names(EntropiedataA)[2] =  "notA"
  
  EntropiedataA <- EntropiedataA%>%
    mutate(Vorher = "Anfang")%>%
    mutate(teil = (A+notA)) %>%
    mutate(p1 = A/(A + notA)) %>%
    mutate(p2 = 1-p1) %>%
    mutate(Entropie = Ent(p1,p2)) %>%
    group_by(Typ) %>%
    mutate(gesamt = (sum(A)+sum(notA))) %>%
    ungroup()
  EntropiedataA$Entropie[is.nan(EntropiedataA$Entropie)] <- 0
  EntropiedataA <- EntropiedataA %>%
    mutate(Anteil = teil/gesamt) %>%
    mutate(Entropieanteil = Anteil*Entropie)
  
  EntropiesumA <- EntropiedataA %>%
    group_by(Typ) %>%
    summarise(Gesamtentropie = sum(Entropieanteil),
              Entropiegewinn = Ent(sum(A)/(sum(A)+sum(notA)),
                                   sum(notA)/(sum(A)+sum(notA))) - sum(Entropieanteil))  %>%
    arrange(desc(Entropiegewinn))
  
  best <- as.character(EntropiesumA[1,1])
  
  erg0 <- EntropiedataA %>%
    filter(Typ == best) %>%
    select("Typ","p1","A","notA","gesamt","Entropie") %>%
    mutate(step1 = best) %>%
    mutate(Aup1 = "-") %>%
    mutate(step2 = "-") %>%
    mutate(Aup2 = "-") %>%
    mutate(step3 = "-") %>%
    mutate(Aup3 = "-") %>%
    mutate(step4 = "-") %>%
    mutate(Aup4 = "-") %>%
    mutate(step5 = "-") %>%
    mutate(Aup5 = "-") %>%
    mutate(step6 = "-") %>%
    mutate(Aup6 = "-") %>%
    mutate(ending = "ZS")
  
  ERG <- erg0
  lu <- c("Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad")
  Index <- which(lu == best)
  
  
  Ergoo <- RecEntr(ERG,cdat,Index, best0 = best)
  return(Ergoo)
}

################# Rekursive Funktion:

RecEntr <- function(recERG,cdat,Index,
                    best0 = "-",best1 = "-",best2 = "-",best3 = "-",best4 = "-",best5 = "-",
                    Aup1 = "-",Aup2 = "-",Aup3 = "-",Aup4 = "-",Aup5 = "-",Aup6 = "-"){
  
  
  lu <- c("Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad")
  
  
  varlist <- data.frame(unique(cdat[Index]))
  
  y <- as.numeric(count(varlist))
  cdat <- cdat %>%
    mutate("spalte" = cdat[Index])
  
  zbest1 <- best1
  zbest2 <- best2
  zbest3 <- best3
  zbest4 <- best4
  zbest5 <- best5
  
  zAup1 <- Aup1
  zAup2 <- Aup2
  zAup3 <- Aup3
  zAup4 <- Aup4
  zAup5 <- Aup5
  zAup6 <- Aup6
  
  
  if(!("A" %in% cdat$Prognose)){
    print("no A")
    return(recERG)
  }
  
  
  if(!("NA" %in% cdat$Prognose)){
    print("no NA")
    return(recERG)
  }
  
  if(best5 != "-"){
    print("dead end")
    return(recERG)
  }
  
  
  i <- 1
  
  for(i in 1:y) {
    
    
    X <- as.character(varlist[i,])
    Zdat <- subset(cdat,cdat$spalte == X)
    
    
    ifelse(best1 == "-",zAup1 <- X,
           ifelse(best2 == "-",zAup2 <- X,
                  ifelse(best3 == "-",zAup3 <- X,
                         ifelse(best4 == "-",zAup4 <- X,
                                ifelse(best5 == "-",zAup5 <- X,NA)
                         ))))
    
    
    EntKonzernmarken <- as.data.frame.matrix(table(Zdat$Konzernmarken, Zdat$Prognose)) %>%
      mutate(Typ = "Konzernmarken")%>%
      mutate(Auspragung = row.names(table(Zdat$Konzernmarken, Zdat$Prognose)))
    EntKundenklasse <- as.data.frame.matrix(table(Zdat$Kundenklasse , Zdat$Prognose)) %>%
      mutate(Typ = "Kundenklasse") %>%
      mutate(Auspragung = row.names(table(Zdat$Kundenklasse , Zdat$Prognose)))
    EntQuad <- as.data.frame.matrix(table(Zdat$Quad , Zdat$Prognose)) %>%
      mutate(Typ = "Quad") %>%
      mutate(Auspragung = row.names(table(Zdat$Quad , Zdat$Prognose)))
    EntUmsatz <- as.data.frame.matrix(table(Zdat$Umsatz , Zdat$Prognose)) %>%
      mutate(Typ = "Umsatz") %>%
      mutate(Auspragung = row.names(table(Zdat$Umsatz , Zdat$Prognose)))
    EntNettoDb <- as.data.frame.matrix(table(Zdat$NettoDB , Zdat$Prognose)) %>%
      mutate(Typ = "NettoDB") %>%
      mutate(Auspragung = row.names(table(Zdat$NettoDB , Zdat$Prognose)))
    EntPreisumsetzung <- as.data.frame.matrix(table(Zdat$Preisumsetzung , Zdat$Prognose)) %>%
      mutate(Typ = "Preisumsetzung") %>%
      mutate(Auspragung = row.names(table(Zdat$Preisumsetzung , Zdat$Prognose)))
    
    
    Entropiedata <- bind_rows(EntKundenklasse,EntKonzernmarken,EntNettoDb,
                              EntPreisumsetzung,EntQuad,EntUmsatz)
    
    
    
    
    if(!("A" %in% Zdat$Prognose)){
      Entropiedata <- Entropiedata %>%
        mutate(A = 0) %>%
        select("A","NA","Typ","Auspragung")
    }
    
    
    if(!("NA" %in% Zdat$Prognose)){
      Entropiedata <- Entropiedata %>%
        mutate(notA = 0) %>%
        select("A","notA","Typ","Auspragung")
      
    }
    
    
    names(Entropiedata)[2] =  "notA"
    
    
    
    Entropiedata <- Entropiedata %>%
      mutate(Vorher = X)%>%
      mutate(teil = (A+notA)) %>%
      mutate(p1 = A/(A + notA)) %>%
      mutate(p2 = 1-p1) %>%
      mutate(Entropie = Ent(p1,p2)) %>%
      group_by(Typ) %>%
      mutate(gesamt = (sum(A)+sum(notA))) %>%
      ungroup()
    Entropiedata$Entropie[is.nan(Entropiedata$Entropie)] <- 0
    Entropiedata <- Entropiedata %>%
      mutate(Anteil = teil/gesamt) %>%
      mutate(Entropieanteil = Anteil*Entropie)
    
    
    
    Entropiesum <- Entropiedata %>%
      group_by(Typ) %>%
      summarise(Gesamtentropie = sum(Entropieanteil),
                Entropiegewinn = Ent(sum(A)/(sum(A)+sum(notA)),
                                     sum(notA)/(sum(A)+sum(notA))) - sum(Entropieanteil))  %>%
      arrange(desc(Entropiegewinn))%>%
      filter(Typ !=  best0 &
               Typ != best1 &
               Typ != best2 &
               Typ != best3 &
               Typ != best4)
    
    
    
    
    zbest <- as.character(Entropiesum[1,1]) 

    
    
    
    zerg <- Entropiedata %>%
      filter(Typ == zbest) %>%
      mutate(solution = A + notA)%>%
      filter(solution != 0)%>%
      select("Typ","p1","A","notA","gesamt","Entropie","Auspragung")
    
    Ausp <- zerg %>%
      select(Auspragung)
    
    ifelse(best1 == "-",zAup2 <- Ausp$Auspragung,
           ifelse(best2 == "-",zAup3 <- Ausp$Auspragung,
                  ifelse(best3 == "-",zAup4 <- Ausp$Auspragung,
                         ifelse(best4 == "-",zAup5 <- Ausp$Auspragung,
                                ifelse(best5 == "-",zAup6 <- Ausp$Auspragung,NA)
                         ))))
    
    
    ifelse(best1 == "-",zbest1 <- zbest,
           ifelse(best2 == "-",zbest2 <- zbest,
                  ifelse(best3 == "-",zbest3 <- zbest,
                         ifelse(best4 == "-",zbest4 <- zbest,
                                ifelse(best5 == "-",zbest5 <- zbest,NA)
                         ))))
    
    
    
    
    
    
    
    zerg <- zerg %>%
      select("Typ","p1","A","notA","gesamt","Entropie")%>%
      mutate(step1 = best0) %>%
      mutate(Aup1 = zAup1) %>%
      mutate(step2 = zbest1) %>%
      mutate(Aup2 = zAup2) %>%
      mutate(step3 = zbest2) %>%
      mutate(Aup3 = zAup3) %>%
      mutate(step4 = zbest3) %>%
      mutate(Aup4 = zAup4) %>%
      mutate(step5 = zbest4) %>%
      mutate(Aup5 = zAup5) %>%
      mutate(step6 = zbest5) %>%
      mutate(Aup6 = zAup6) %>%
      mutate(ending = ifelse(A == 0, "Alle NA",
                             ifelse(notA == 0, "Alle A",
                                    ifelse(step6 != "-", "Undiskret", "ZS"))))
      
    
    ZIndex <- which(lu == zbest)   
    
    
    recERG <- bind_rows(recERG, zerg)
    
    
    recERG <- RecEntr(recERG,Zdat,ZIndex,
                      best0,zbest1,zbest2,zbest3,zbest4,zbest5,
                      zAup1,zAup2,zAup3,zAup4,zAup5,zAup6)
  }
  
  return(recERG)
  
}

############################### Abschließen und abspeichern

decTree <- start_rec_ent(kddisc)

decTree_Onlypaths <- decTree %>%
  filter(ending != "ZS")

write.csv(decTree, file = 'Decisiontree_fin.csv',row.names = FALSE)
write.csv(kdtest, file = 'testdata.csv',row.names = FALSE)

