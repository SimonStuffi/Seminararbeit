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



#                               Klassifikation:



Klassifikation <- rpart(Kundenklasse ~ Umsatz, data = ges)
rpart.plot(Klassifikation)

#Es ist klar, dass der Umsatz ausreicht um die momentane Kundenklasse zu klassifizieren


#                           Decision tree learner:


#Daten Diskret gestalten:

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
    
    
kd <- ges %>%
  select("Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad", "Prognose", "Jahr")
kdA <- subset(ges, Prognose == "A")
kdNA <- subset(ges, Prognose == "NA")
"Ort Visualisieren:"
plot(kdA$Breitengrad, kdA$Längengrad)
plot(kdNA$Breitengrad, kdNA$Längengrad)


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
  
  
  ifelse(spalte > 0,{
    
    #bearbeitungsready machen
    
    Varlist <- unique(daten[spalte])
    j <- as.numeric(count(Varlist))
    daten <- daten %>%
      mutate("spaltenname" = daten[spalte])
    
    #daten aufgrund der richtigen Spalte aufteilen
    
    for (i in 1:j) {
    
      x <- as.character(Varlist[i,])
     print(x)
    
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
    
    
      
      Entropiepardata <- Entropiepardata %>%
       mutate(Anteil = (A+notA)) %>%
        mutate(p1 = A/(A + notA)) %>%
        mutate(p2 = 1-p1) %>%
       mutate(Entropie = Ent(p1,p2)) %>%
        group_by(Typ) %>%
        mutate(gesamt = (sum(A)+sum(notA))) %>%
        ungroup()
      print(Entropiepardata)
      Entropiepardata$Entropie[is.nan(Entropiepardata$Entropie)] <- 0
      Entropiepardata <- Entropiepardata %>%
        mutate(Entropieanteil = (Anteil/gesamt)*Entropie)
    
      Entropietest <- Entropiepardata %>%
        group_by(Typ) %>%
        summarise(Gesamtentropie = sum(Entropieanteil),
                Entropiegewinn = Ent(sum(A)/(sum(A)+sum(notA)),
                                     sum(notA)/(sum(A)+sum(notA))) - sum(Entropieanteil)) %>%
        arrange(desc(Entropiegewinn))
      names(Entropietest) = c("Typ",paste("Gesamtentropie bei", x),paste("Entropiegewinn bei", x))
    
    ifelse(i == 1,
           fuldata <- Entropietest,
           fuldata <- full_join(fuldata,Entropietest,by = "Typ"))
  }
    return(fuldata)
    },{
      
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
test <- empirietest(kd,0)






