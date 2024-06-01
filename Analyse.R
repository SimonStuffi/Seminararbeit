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

kunden <- bind_rows(kunden_2016, kunden_2017, kunden_2018) 
"Kunden2019n"
ges <- left_join(kunden, dplz, "PLZ")
names(ges)[7] =  "NettoDB"



#                               Klassifikation:



Klassifikation <- rpart(Kundenklasse ~ Umsatz, data = ges)
rpart.plot(Klassifikation)

#Es ist klar, dass der Umsatz ausreicht um die momentane Kundenklasse zu klassifizieren


#                           Decision tree learner:


#Daten Diskret gestalten:
############################# Das große Ausprobieren ###########################

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
                      Umsatz = list(method = "frequency", breaks = 2),
                      NettoDB = list(method = "frequency", breaks = 3),
                      Preisumsetzung = list(method = "frequency", breaks = 3)),
                      default = list(method = "none"))

#,Konzernmarken = list(method = "frequency", breaks = 2)


gesDisc <- gesDisc %>%
  mutate(Quad = ifelse(Breitengrad <= Breitgr & Längengrad <= Längengr, "<50,2724/<9,7901", 
                ifelse(Breitengrad > Breitgr & Längengrad <= Längengr, ">50,2724/<9,7901",
                ifelse(Breitengrad <= Breitgr & Längengrad > Längengr, "50,2724/>9,7901", 
                ifelse(Breitengrad > Breitgr & Längengrad > Längengr, ">50,2724/>9,7901", NA)))))

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
    
    
kddisc <- gesDisc %>%
  select("Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad", "Prognose", "Jahr")

kd <- ges %>%
  select("Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad", "Prognose", "Jahr")


#                             Stufenlearner:

#DIE FUNKTION

#"Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad"
#==============================================================================
#       1             2        3             4                 5           6

Ent <- function(x,y){
  return((-1)*x*log(x,2)-y*log(y,2))  #Entropieformel
}

empirietest <- function(daten,spalte){
  
  if(spalte > 6){
    print("Fehler bei Spalteneingabe")&
    return("Fehler bei Spalteneingabe")}
  
  #test ob Typ Null=Initialvorgang
  
  ifelse(spalte > 0,{
    
    #######################   Typ 1-6 = Teilrechner   ##########################
    
    #bearbeitungsready machen
    
    Varlist <- unique(daten[spalte])
    j <- as.numeric(count(Varlist))
    daten <- daten %>%
      mutate("spaltenname" = daten[spalte])
    
    #daten aufgrund der richtigen Spalte aufteilen
    
    for (i in 1:j) {
    
      x <- as.character(Varlist[i,])
    
     partdata <- subset(daten,daten$spaltenname == x)
     
      EntKonzernmarken <- as.data.frame.matrix(table(partdata$Konzernmarken, partdata$Prognose)) %>%
       mutate(Typ = "Konzernmarken")
     EntKundenklasse <- as.data.frame.matrix(table(partdata$Kundenklasse , partdata$Prognose)) %>%
       mutate(Typ = "Kundenklasse")
     EntQuad <- as.data.frame.matrix(table(partdata$Quad , partdata$Prognose)) %>%
       mutate(Typ = "Quad")
     EntUmsatz <- as.data.frame.matrix(table(partdata$Umsatz , partdata$Prognose)) %>%
        mutate(Typ = "Umsatz")
     EntNettoDb <- as.data.frame.matrix(table(partdata$NettoDB , partdata$Prognose)) %>%
        mutate(Typ = "NettoDB")
      EntPreisumsetzung <- as.data.frame.matrix(table(partdata$Preisumsetzung , partdata$Prognose)) %>%
        mutate(Typ = "Preisumsetzung")
    
    
     Entropiepardata <- bind_rows(EntKundenklasse,EntKonzernmarken,EntNettoDb,
                              EntPreisumsetzung,EntQuad,EntUmsatz) %>%
        mutate(splitfactor = x)
    
      names(Entropiepardata)[2] =  "notA"
    
      
      # Daten Analysieren
    
      
      Entropiepardata <- Entropiepardata %>%
       mutate(Anteil = (A+notA)) %>%
        mutate(p1 = A/(A + notA)) %>%
        mutate(p2 = 1-p1) %>%
       mutate(Entropie = Ent(p1,p2)) %>%
        group_by(Typ) %>%
        mutate(gesamt = (sum(A)+sum(notA))) %>%
        ungroup()
      Entropiepardata$Entropie[is.nan(Entropiepardata$Entropie)] <- 0
      Entropiepardata <- Entropiepardata %>%
        mutate(Entropieanteil = (Anteil/gesamt)*Entropie)
    
      Entropietest <- Entropiepardata %>%
        group_by(Typ) %>%
        summarise(Gesamtentropie = sum(Entropieanteil),
                Entropiegewinn = Ent(sum(A)/(sum(A)+sum(notA)),
                                     sum(notA)/(sum(A)+sum(notA))) - sum(Entropieanteil)) %>%
        arrange(desc(Entropiegewinn)) %>%
        mutate(Auspragung = x)
      
    
    ifelse(i == 1,
           fuldata <- Entropietest,
           fuldata <- bind_rows(fuldata,Entropietest))
  }
    return(fuldata)
    },{
      
      
      
      #########################     Typ 0 = Initialrechner   ###########################
      
      EntKonzernmarken1 <- as.data.frame.matrix(table(daten$Konzernmarken, daten$Prognose)) %>%
        mutate(Typ = "Konzernmarken")
      EntKundenklasse1 <- as.data.frame.matrix(table(daten$Kundenklasse , daten$Prognose)) %>%
        mutate(Typ = "Kundenklasse")
      EntQuad1 <- as.data.frame.matrix(table(daten$Quad , daten$Prognose)) %>%
        mutate(Typ = "Quad")
      EntUmsatz1 <- as.data.frame.matrix(table(daten$Umsatz , daten$Prognose)) %>%
        mutate(Typ = "Umsatz")
      EntNettoDb1 <- as.data.frame.matrix(table(daten$NettoDB , daten$Prognose)) %>%
        mutate(Typ = "NettoDB")
      EntPreisumsetzung1 <- as.data.frame.matrix(table(daten$Preisumsetzung , daten$Prognose)) %>%
        mutate(Typ = "Preisumsetzung")
      
      
      Entropiedata1 <- bind_rows(EntKundenklasse1,EntKonzernmarken1,EntNettoDb1,
                                 EntPreisumsetzung1,EntQuad1,EntUmsatz1)
      names(Entropiedata1)[2] =  "notA"
      
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
      
      return(Entropietest1)
      
    })

}



Empirietest_0 <- empirietest(kd,0)



Empirietest_1 <- empirietest(kd,1)


################################################################################
########################    final learner model    #############################
################################################################################




dectreelearner <- function(daten){
  
  lu <- c("Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad") 
  
  cdat <- daten
  
  u <- 1
  v <- 1
  w <- 1
  x <- 1
  y <- 1
  z <- 1
  a <- 0
  b <- 0
  c <- 0
  d <- 0
  e <- 0
  f <- 0
  
  #                          Stufe 0
  
  
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
  
  best0 <- as.character(EntropiesumA[1,1])
  
  erg0 <- EntropiedataA %>%
    filter(Typ == best0) %>%
    select("Typ","Auspragung","Vorher","p1","Anteil","Entropie") %>%
    mutate(Stufe = 0) %>%
    mutate(aupreg = 1) %>% 
    mutate(ID = paste(f,e,d,c,b,a))
  
  ERG <- erg0
  
  Index <- which(lu == best0)
  
  
  varlist <- data.frame(unique(cdat[Index]))

  
  
  u <- as.numeric(count(varlist))
  cdat <- cdat %>%
    mutate("spalte" = cdat[Index])
  
 
 
  
  
  
  #                           Stufe 1
  
  
  for(a in 1:u) {  
    
    b <- 0
    c <- 0
    d <- 0
    e <- 0
    f <- 0
    
    K <- as.character(varlist[a,])
    Acdat <- subset(cdat,cdat$spalte == K)
    
    
    if(!("A" %in% Acdat$Prognose)){
      ergA <- data.frame(Typ = best0,
                         Auspragung = "Fertig",
                         Vorher = K,
                         p1 = 0.0,
                         Anteil= 1.0,
                         Stufe = 1,
                         aupreg = a,
                         ID= paste(f,e,d,c,b,a),
                         Entropie = 0)
      ERG <- bind_rows(ERG, ergA)
      next}
    if(!("NA" %in% Acdat$Prognose)){
      ergA <- data.frame(Typ = best0,
                         Auspragung = "Fertig",
                         Vorher = K,
                         p1 = 1.0,
                         Anteil= 1.0,
                         Stufe = 1,
                         aupreg = a,
                         ID= paste(f,e,d,c,b,a),
                         Entropie = 0)
      ERG <- bind_rows(ERG, ergA)
      next
    }
    
    EntKonzernmarkenA <- as.data.frame.matrix(table(Acdat$Konzernmarken, Acdat$Prognose)) %>%
      mutate(Typ = "Konzernmarken")%>%
      mutate(Auspragung = row.names(table(Acdat$Konzernmarken, Acdat$Prognose)))
    EntKundenklasseA <- as.data.frame.matrix(table(Acdat$Kundenklasse , Acdat$Prognose)) %>%
      mutate(Typ = "Kundenklasse") %>%
      mutate(Auspragung = row.names(table(Acdat$Kundenklasse , Acdat$Prognose)))
    EntQuadA <- as.data.frame.matrix(table(Acdat$Quad , Acdat$Prognose)) %>%
      mutate(Typ = "Quad") %>%
      mutate(Auspragung = row.names(table(Acdat$Quad , Acdat$Prognose)))
    EntUmsatzA <- as.data.frame.matrix(table(Acdat$Umsatz , Acdat$Prognose)) %>%
      mutate(Typ = "Umsatz") %>%
      mutate(Auspragung = row.names(table(Acdat$Umsatz , Acdat$Prognose)))
    EntNettoDbA <- as.data.frame.matrix(table(Acdat$NettoDB , Acdat$Prognose)) %>%
      mutate(Typ = "NettoDB") %>%
      mutate(Auspragung = row.names(table(Acdat$NettoDB , Acdat$Prognose)))
    EntPreisumsetzungA <- as.data.frame.matrix(table(Acdat$Preisumsetzung , Acdat$Prognose)) %>%
      mutate(Typ = "Preisumsetzung") %>%
      mutate(Auspragung = row.names(table(Acdat$Preisumsetzung , Acdat$Prognose)))
    
    
    EntropiedataA <- bind_rows(EntKundenklasseA,EntKonzernmarkenA,EntNettoDbA,
                               EntPreisumsetzungA,EntQuadA,EntUmsatzA)
    names(EntropiedataA)[2] =  "notA"
    
    
    
    
    EntropiedataA <- EntropiedataA%>%
      mutate(Vorher = K)%>%
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
      arrange(desc(Entropiegewinn))%>%
      filter(Typ !=  best0)
    
    
    bestA <- as.character(EntropiesumA[1,1])
    
    ergA <- EntropiedataA %>%
      filter(Typ == bestA) %>%
      select("Typ","Auspragung","Vorher","p1","Anteil","Entropie") %>%
      mutate(Stufe = 1) %>%
      mutate(aupreg = a) %>% 
      mutate(ID = paste(f,e,d,c,b,a))
    
    IndexA <- which(lu == bestA)
    
    
    varlistA <- data.frame(unique(Acdat[IndexA]))
    
    
    
    v <- as.numeric(count(varlistA))
    Acdat <- Acdat %>%
      mutate("spalte" = Acdat[IndexA])
    
    ERG <- bind_rows(ERG, ergA)
    
    
    #                                Stufe 2
    
    for(b in 1:v) {
      
      
      c <- 0
      d <- 0
      e <- 0
      f <- 0
      
      B <- as.character(varlistA[b,])
      Bcdat <- subset(Acdat,Acdat$spalte == B)
      
      
      if(!("A" %in% Bcdat$Prognose)){
        ergB <- data.frame(Typ = bestA,
                           Auspragung = "Fertig",
                           Vorher = B,
                           p1 = 0.0,
                           Anteil= 1.0,
                           Stufe = 2,
                           aupreg = b + u*(a-1),
                           ID= paste(f,e,d,c,b,a),
                           Entropie = 0)
        ERG <- bind_rows(ERG, ergB)
        next}
      if(!("NA" %in% Bcdat$Prognose)){
        ergB <- data.frame(Typ = bestA,
                           Auspragung = "Fertig",
                           Vorher = B,
                           p1 = 1.0,
                           Anteil= 1.0,
                           Stufe = 2,
                           aupreg = b + u*(a-1),
                           ID= paste(f,e,d,c,b,a),
                           Entropie = 0)
          ERG <- bind_rows(ERG, ergB)
          next
      }
      
      
      
      
      EntKonzernmarkenA <- as.data.frame.matrix(table(Bcdat$Konzernmarken, Bcdat$Prognose)) %>%
        mutate(Typ = "Konzernmarken")%>%
        mutate(Auspragung = row.names(table(Bcdat$Konzernmarken, Bcdat$Prognose)))
      EntKundenklasseA <- as.data.frame.matrix(table(Bcdat$Kundenklasse , Bcdat$Prognose)) %>%
        mutate(Typ = "Kundenklasse") %>%
        mutate(Auspragung = row.names(table(Bcdat$Kundenklasse , Bcdat$Prognose)))
      EntQuadA <- as.data.frame.matrix(table(Bcdat$Quad , Bcdat$Prognose)) %>%
        mutate(Typ = "Quad") %>%
        mutate(Auspragung = row.names(table(Bcdat$Quad , Bcdat$Prognose)))
      EntUmsatzA <- as.data.frame.matrix(table(Bcdat$Umsatz , Bcdat$Prognose)) %>%
        mutate(Typ = "Umsatz") %>%
        mutate(Auspragung = row.names(table(Bcdat$Umsatz , Bcdat$Prognose)))
      EntNettoDbA <- as.data.frame.matrix(table(Bcdat$NettoDB , Bcdat$Prognose)) %>%
        mutate(Typ = "NettoDB") %>%
        mutate(Auspragung = row.names(table(Bcdat$NettoDB , Bcdat$Prognose)))
      EntPreisumsetzungA <- as.data.frame.matrix(table(Bcdat$Preisumsetzung , Bcdat$Prognose)) %>%
        mutate(Typ = "Preisumsetzung") %>%
        mutate(Auspragung = row.names(table(Bcdat$Preisumsetzung , Bcdat$Prognose)))
      
      
      EntropiedataA <- bind_rows(EntKundenklasseA,EntKonzernmarkenA,EntNettoDbA,
                                 EntPreisumsetzungA,EntQuadA,EntUmsatzA)
      names(EntropiedataA)[2] =  "notA"
      
      
      EntropiedataA <- EntropiedataA %>%
        mutate(Vorher = B)%>%
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
        arrange(desc(Entropiegewinn))%>%
        filter(Typ !=  best0 &
                 Typ != bestA)
      
      
      bestB <- as.character(EntropiesumA[1,1])
      
      ergB <- EntropiedataA %>%
        filter(Typ == bestB) %>%
        select("Typ","Auspragung","Vorher","p1","Anteil","Entropie") %>%
        mutate(Stufe = 2) %>%
        mutate(aupreg = b + u*(a-1)) %>% 
        mutate(ID = paste(f,e,d,c,b,a))
      
      IndexB <- which(lu == bestB)
      
      
      varlistB <- data.frame(unique(Bcdat[IndexB]))
      
      
      w <- as.numeric(count(varlistB))
      Bcdat <- Bcdat %>%
        mutate("spalte" = Bcdat[IndexB])
      
      ERG <- bind_rows(ERG, ergB)
      
      #                           Stufe 3
      
      for(c in 1:w) {
        
        d <- 0
        e <- 0
        f <- 0
        
        C <- as.character(varlistB[c,])
        Ccdat <- subset(Bcdat,Bcdat$spalte == C)
        
        if(!("A" %in% Ccdat$Prognose)){
          ergC <- data.frame(Typ = bestB,
                             Auspragung = "Fertig",
                             Vorher = C,
                             p1 = 0.0,
                             Anteil= 1.0,
                             Stufe = 3,
                             aupreg = c + v*(b-1),
                             ID= paste(f,e,d,c,b,a),
                             Entropie = 0)
          ERG <- bind_rows(ERG, ergC)
          next}
        if(!("NA" %in% Ccdat$Prognose)){
          ergC <- data.frame(Typ = bestB,
                             Auspragung = "Fertig",
                             Vorher = C,
                             p1 = 1.0,
                             Anteil= 1.0,
                             Stufe = 3,
                             aupreg = c + v*(b-1),
                             ID= paste(f,e,d,c,b,a),
                             Entropie = 0)
          ERG <- bind_rows(ERG, ergC)
          next
        }
        
        
        
        
        EntKonzernmarkenA <- as.data.frame.matrix(table(Ccdat$Konzernmarken, Ccdat$Prognose)) %>%
          mutate(Typ = "Konzernmarken")%>%
          mutate(Auspragung = row.names(table(Ccdat$Konzernmarken, Ccdat$Prognose)))
        EntKundenklasseA <- as.data.frame.matrix(table(Ccdat$Kundenklasse , Ccdat$Prognose)) %>%
          mutate(Typ = "Kundenklasse") %>%
          mutate(Auspragung = row.names(table(Ccdat$Kundenklasse , Ccdat$Prognose)))
        EntQuadA <- as.data.frame.matrix(table(Ccdat$Quad , Ccdat$Prognose)) %>%
          mutate(Typ = "Quad") %>%
          mutate(Auspragung = row.names(table(Ccdat$Quad , Ccdat$Prognose)))
        EntUmsatzA <- as.data.frame.matrix(table(Ccdat$Umsatz , Ccdat$Prognose)) %>%
          mutate(Typ = "Umsatz") %>%
          mutate(Auspragung = row.names(table(Ccdat$Umsatz , Ccdat$Prognose)))
        EntNettoDbA <- as.data.frame.matrix(table(Ccdat$NettoDB , Ccdat$Prognose)) %>%
          mutate(Typ = "NettoDB") %>%
          mutate(Auspragung = row.names(table(Ccdat$NettoDB , Ccdat$Prognose)))
        EntPreisumsetzungA <- as.data.frame.matrix(table(Ccdat$Preisumsetzung , Ccdat$Prognose)) %>%
          mutate(Typ = "Preisumsetzung") %>%
          mutate(Auspragung = row.names(table(Ccdat$Preisumsetzung , Ccdat$Prognose)))
        
        
        EntropiedataA <- bind_rows(EntKundenklasseA,EntKonzernmarkenA,EntNettoDbA,
                                   EntPreisumsetzungA,EntQuadA,EntUmsatzA)
        names(EntropiedataA)[2] =  "notA"
        
        
        EntropiedataA <- EntropiedataA %>%
          mutate(Vorher = C)%>%
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
          arrange(desc(Entropiegewinn))%>%
          filter(Typ !=  best0 &
                  Typ != bestA &
                  Typ != bestB)
        
        
        bestC <- as.character(EntropiesumA[1,1])
        
        ergC <- EntropiedataA %>%
          filter(Typ == bestC) %>%
          select("Typ","Auspragung","Vorher","p1","Anteil","Entropie") %>%
          mutate(Stufe = 3) %>%
          mutate(aupreg = c + v*(b-1)) %>% 
          mutate(ID = paste(f,e,d,c,b,a))
        
        IndexC <- which(lu == bestC)
        
        
        varlistC <- data.frame(unique(Bcdat[IndexC]))
        
        
        
        x <- as.numeric(count(varlistC))
        Ccdat <- Ccdat %>%
          mutate("spalte" = Ccdat[IndexC])
        
        ERG <- bind_rows(ERG, ergC)
        
        #                           Stufe 4
        
        for(d in 1:x) {
          
          e <- 0
          f <- 0
          
          D <- as.character(varlistC[d,])
          Dcdat <- subset(Ccdat,Ccdat$spalte == D)
          
          
          if(!("A" %in% Dcdat$Prognose)){
            ergD <- data.frame(Typ = bestC,
                               Auspragung = "Fertig",
                               Vorher = D,
                               p1 = 0.0,
                               Anteil= 1.0,
                               Stufe = 4,
                               aupreg = d + w*(c-1),
                               ID= paste(f,e,d,c,b,a),
                               Entropie = 0)
            ERG <- bind_rows(ERG, ergD)
            next}
          if(!("NA" %in% Dcdat$Prognose)){
            ergD <- data.frame(Typ = bestC,
                               Auspragung = "Fertig",
                               Vorher = D,
                               p1 = 1.0,
                               Anteil= 1.0,
                               Stufe = 4,
                               aupreg = d + w*(c-1),
                               ID= paste(f,e,d,c,b,a),
                               Entropie = 0)
            ERG <- bind_rows(ERG, ergD)
            next
          }
          
          
          
          
          EntKonzernmarkenA <- as.data.frame.matrix(table(Dcdat$Konzernmarken, Dcdat$Prognose)) %>%
            mutate(Typ = "Konzernmarken")%>%
            mutate(Auspragung = row.names(table(Dcdat$Konzernmarken, Dcdat$Prognose)))
          EntKundenklasseA <- as.data.frame.matrix(table(Dcdat$Kundenklasse , Dcdat$Prognose)) %>%
            mutate(Typ = "Kundenklasse") %>%
            mutate(Auspragung = row.names(table(Dcdat$Kundenklasse , Dcdat$Prognose)))
          EntQuadA <- as.data.frame.matrix(table(Dcdat$Quad , Dcdat$Prognose)) %>%
            mutate(Typ = "Quad") %>%
            mutate(Auspragung = row.names(table(Dcdat$Quad , Dcdat$Prognose)))
          EntUmsatzA <- as.data.frame.matrix(table(Dcdat$Umsatz , Dcdat$Prognose)) %>%
            mutate(Typ = "Umsatz") %>%
            mutate(Auspragung = row.names(table(Dcdat$Umsatz , Dcdat$Prognose)))
          EntNettoDbA <- as.data.frame.matrix(table(Dcdat$NettoDB , Dcdat$Prognose)) %>%
            mutate(Typ = "NettoDB") %>%
            mutate(Auspragung = row.names(table(Dcdat$NettoDB , Dcdat$Prognose)))
          EntPreisumsetzungA <- as.data.frame.matrix(table(Dcdat$Preisumsetzung , Dcdat$Prognose)) %>%
            mutate(Typ = "Preisumsetzung") %>%
            mutate(Auspragung = row.names(table(Dcdat$Preisumsetzung , Dcdat$Prognose)))
          
          
          EntropiedataA <- bind_rows(EntKundenklasseA,EntKonzernmarkenA,EntNettoDbA,
                                     EntPreisumsetzungA,EntQuadA,EntUmsatzA)
          names(EntropiedataA)[2] =  "notA"
          
          
          EntropiedataA <- EntropiedataA %>%
            mutate(Vorher = D)%>%
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
            arrange(desc(Entropiegewinn))%>%
            filter(Typ !=  best0 &
                     Typ != bestA &
                     Typ != bestB &
                     Typ != bestC)
          
          
          bestD <- as.character(EntropiesumA[1,1])
          
          ergD <- EntropiedataA %>%
            filter(Typ == bestD) %>%
            select("Typ","Auspragung","Vorher","p1","Anteil","Entropie") %>%
            mutate(Stufe = 4) %>%
            mutate(aupreg = d + w*(c-1)) %>% 
            mutate(ID = paste(f,e,d,c,b,a))
          
          IndexD <- which(lu == bestD)
          
          
          varlistD <- data.frame(unique(Bcdat[IndexD]))
          
          
          
          y <- as.numeric(count(varlistD))
          Dcdat <- Dcdat %>%
            mutate("spalte" = Dcdat[IndexD])
          
          ERG <- bind_rows(ERG, ergD)
          
          #                           Stufe 5
          
          for(e in 1:y) {
          
            
            f <- 0
            
            E <- as.character(varlistD[e,])
            Ecdat <- subset(Dcdat,Dcdat$spalte == E)
            
            print(Ecdat$Prognose)
            
            
            if(!("A" %in% Ecdat$Prognose)){
              ergE <- data.frame(Typ = bestD,
                                 Auspragung = "Fertig",
                                 Vorher = E,
                                 p1 = 0.0,
                                 Anteil= 1.0,
                                 Stufe = 5,
                                 aupreg = e + x*(d-1),
                                 ID= paste(f,e,d,c,b,a),
                                 Entropie = 0)
              ERG <- bind_rows(ERG, ergE)
              next}
            if(!("NA" %in% Ecdat$Prognose)){
              ergE <- data.frame(Typ = bestD,
                                 Auspragung = "Fertig",
                                 Vorher = E,
                                 p1 = 1.0,
                                 Anteil= 1.0,
                                 Stufe = 5,
                                 aupreg = e + x*(d-1),
                                 ID= paste(f,e,d,c,b,a),
                                 Entropie = 0)
              ERG <- bind_rows(ERG, ergE)
              next
            }
            
            print("continues")
            
            
            EntKonzernmarkenA <- as.data.frame.matrix(table(Ecdat$Konzernmarken, Ecdat$Prognose)) %>%
              mutate(Typ = "Konzernmarken")%>%
              mutate(Auspragung = row.names(table(Ecdat$Konzernmarken, Ecdat$Prognose)))
            EntKundenklasseA <- as.data.frame.matrix(table(Ecdat$Kundenklasse , Ecdat$Prognose)) %>%
              mutate(Typ = "Kundenklasse") %>%
              mutate(Auspragung = row.names(table(Ecdat$Kundenklasse , Ecdat$Prognose)))
            EntQuadA <- as.data.frame.matrix(table(Ecdat$Quad , Ecdat$Prognose)) %>%
              mutate(Typ = "Quad") %>%
              mutate(Auspragung = row.names(table(Ecdat$Quad , Ecdat$Prognose)))
            EntUmsatzA <- as.data.frame.matrix(table(Ecdat$Umsatz , Ecdat$Prognose)) %>%
              mutate(Typ = "Umsatz") %>%
              mutate(Auspragung = row.names(table(Ecdat$Umsatz , Ecdat$Prognose)))
            EntNettoDbA <- as.data.frame.matrix(table(Ecdat$NettoDB , Ecdat$Prognose)) %>%
              mutate(Typ = "NettoDB") %>%
              mutate(Auspragung = row.names(table(Ecdat$NettoDB , Ecdat$Prognose)))
            EntPreisumsetzungA <- as.data.frame.matrix(table(Ecdat$Preisumsetzung , Ecdat$Prognose)) %>%
              mutate(Typ = "Preisumsetzung") %>%
              mutate(Auspragung = row.names(table(Ecdat$Preisumsetzung , Ecdat$Prognose)))
            
            
            EntropiedataA <- bind_rows(EntKundenklasseA,EntKonzernmarkenA,EntNettoDbA,
                                       EntPreisumsetzungA,EntQuadA,EntUmsatzA)
            names(EntropiedataA)[2] =  "notA"
            
            
            EntropiedataA <- EntropiedataA %>%
              mutate(Vorher = E)%>%
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
              arrange(desc(Entropiegewinn))%>%
              filter(Typ !=  best0 &
                       Typ != bestA &
                       Typ != bestB &
                       Typ != bestC &
                       Typ != bestD)
            
            
            bestE <- as.character(EntropiesumA[1,1])
            
            ergE <- EntropiedataA %>%
              filter(Typ == bestE) %>%
              select("Typ","Auspragung","Vorher","p1","Anteil","Entropie") %>%
              mutate(Stufe = 5) %>%
              mutate(aupreg = e + x*(d-1)) %>% 
              mutate(ID = paste(f,e,d,c,b,a))
            
            IndexE <- which(lu == bestE)
            
            
            varlistE <- data.frame(unique(Bcdat[IndexE]))
            
            
            
            z <- as.numeric(count(varlistE))
            Ecdat <- Ecdat %>%
              mutate("spalte" = Ecdat[IndexE])
            
            ERG <- bind_rows(ERG, ergE)
            
            #                         Stufe 6
            
            for(g in 1:z) {
              
              G <- as.character(varlistE[g,])
              Gcdat <- subset(Dcdat,Dcdat$spalte == G)
              
              
              if(!("A" %in% Gcdat$Prognose)){
                ergG <- data.frame(Typ = bestE,
                                   Auspragung = "Fertig",
                                   Vorher = G,
                                   p1 = 0.0,
                                   Anteil= 1.0,
                                   Stufe = 6,
                                   aupreg = g + y*(e-1),
                                   ID= paste(g,e,d,c,b,a),
                                   Entropie = 0)
                ERG <- bind_rows(ERG, ergG)
                next}
              if(!("NA" %in% Gcdat$Prognose)){
                ergG <- data.frame(Typ = bestE,
                                   Auspragung = "Fertig",
                                   Vorher = G,
                                   p1 = 1.0,
                                   Anteil= 1.0,
                                   Stufe = 6,
                                   aupreg = g + y*(e-1),
                                   ID= paste(g,e,d,c,b,a),
                                   Entropie = 0)
                ERG <- bind_rows(ERG, ergG)
                next
              }
            }
          }
        } 
      }   
    }     
  }
  
  return(ERG)
}


Ergebnis <- dectreelearner(kd)
Ergebnisdisc <- dectreelearner(kddisc)

Ergebnis_ohnne_Kopfende <- Ergebnis %>%
  filter(Auspragung != "Fertig")

Ergebnisdisc_ohnne_Kopfende <- Ergebnisdisc %>%
  filter(Auspragung != "Fertig")




#                           recursiv


RecEntr <- function(recERG,cdat,Index,best0 = "-",best1 = "-",best2 = "-",best3 = "-",best4 = "-",best5 = "-"){
  
  
  lu <- c("Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad")
  
  
  varlist <- data.frame(unique(cdat[Index]))
  
  
  y <- as.numeric(count(varlist))
  cdat <- cdat %>%
    mutate("spalte" = cdat[Index])
  
  
  for(i in 1:y) {
    
    
    f <- 0
    
    X <- as.character(varlist[i,])
    Zdat <- subset(cdat,cdat$spalte == X)
    
    
    if(!("A" %in% Zdat$Prognose)){
      Zerg <- data.frame(Typ = bestD,
                         Auspragung = "Fertig",
                         Vorher = X,
                         p1 = 0.0,
                         A = 0,
                         notA = count(Zdat$Prognose, vars= "notA"),
                         gesamt= count(Zdat$Prognose),
                         ID= paste(best0,best1,best2,best3,best4,best5),
                         Entropie = 0)
      recERG <- bind_rows(recERG, Zerg)
      next}
    if(!("NA" %in% Zdat$Prognose)){
      Zerg <- data.frame(Typ = bestD,
                         Auspragung = "Fertig",
                         Vorher = X,
                         p1 = 1.0,
                         A = count(Zdat$Prognose, vars= "A"),
                         notA = 0,
                         gesamt= count(Zdat$Prognose),
                         ID= paste(best0,best1,best2,best3,best4,best5),
                         Entropie = 0)
      recERG <- bind_rows(recERG, Zerg)
      next
    }
    
    
    
    
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
      select("Typ","Auspragung","Vorher","p1","A","notA","gesamt","Entropie") %>%
      mutate(ID = paste(best0,best1,best2,best3,best4,best5))
    
    ZIndex <- which(lu == zbest)   
    
    recERG <- bind_rows(recERG, zerg)
    
    if(best5 != "-"){
      print("End reached!")
      next
    }
    
    ifelse(best1 == "-",best1 <- zbest,
           ifelse(best2 == "-",best2 <- zbest,
                 ifelse(best3 == "-",best3 <- zbest,
                        ifelse(best4 == "-",best4 <- zbest,
                               ifelse(best5 == "-",best5 <- zbest,print("error")
                               )))))
    
    
    
    
    nexrec <- RecEntr(recERG,Zdat,ZIndex,best0,best1,best2,best3,best4,best5)
    recERG <- bind_rows(recERG, nexrec)
  }
  
  return(recERG)
  
}

cdat <- kddisc

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
  select("Typ","Auspragung","Vorher","p1","A","notA","gesamt","Entropie") %>%
  mutate(ID = paste(best,"-","-","-","-","-"))

ERG <- erg0
lu <- c("Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad")
Index <- which(lu == best)


test <- RecEntr(ERG,cdat,Index, best0 = best)

