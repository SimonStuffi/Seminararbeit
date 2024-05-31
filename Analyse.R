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
#Ort Visualisieren: 
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
    select("Typ","Auspragung","p1","Anteil") %>%
    mutate(Stufe = 0) %>%
    mutate(aupreg = 1) %>% 
    mutate(ID = paste(f,e,d,c,b,a))
  
  ERG <- erg0
  
  Index <- which(lu == best0)
  
  
  varlist <- unique(cdat[Index])
  
  
  u <- as.numeric(count(varlist))
  cdat <- cdat %>%
    mutate("spalte" = cdat[Index])
  
  
  #                           Stufe 1
  
  
  for(a in 10:u) {  ###########ACHTRun
    
    b <- 0
    c <- 0
    d <- 0
    e <- 0
    f <- 0
    
    
    K <- as.character(varlist[a,])
    Acdat <- subset(cdat,cdat$spalte == K)
    
    if(!("A" %in% Acdat$Prognose)){
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
    
    
    EntropiedataA <- bind_rows(Ground,EntKundenklasseA,EntKonzernmarkenA,EntNettoDbA,
                               EntPreisumsetzungA,EntQuadA,EntUmsatzA)
    names(EntropiedataA)[2] =  "notA"
    
    
    
    
    EntropiedataA <- EntropiedataA%>%
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
    
    
    bestA <- as.character(EntropiesumA[1,1])
    
    ergA <- EntropiedataA %>%
      filter(Typ == bestA) %>%
      select("Typ","Auspragung","p1","Anteil") %>%
      mutate(Stufe = 1) %>%
      mutate(aupreg = a) %>% 
      mutate(ID = paste(f,e,d,c,b,a))
    
    IndexA <- which(lu == bestA)
    
    
    varlistA <- unique(Acdat[IndexA])
    
    
    
    v <- as.numeric(count(varlistA))
    Acdat <- Acdat %>%
      mutate("spalte" = Acdat[IndexA])
    
    ERG <- bind_rows(ERG, ergA)
    
    
    #                                Stufe 2
    
    for(b in 6:v) { ##################################achtung
      
      
      c <- 0
      d <- 0
      e <- 0
      f <- 0
      
      B <- as.character(varlistA[b,])
      Bcdat <- subset(Acdat,Acdat$spalte == B)
      
      if(!("A" %in% Bcdat$Prognose)){
        next
      }
      
      return(Bcdat)
      
      
      
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
      
      
      bestB <- as.character(EntropiesumA[1,1])
      
      ergB <- EntropiedataA %>%
        filter(Typ == bestB) %>%
        select("Typ","Auspragung","p1","Anteil") %>%
        mutate(Stufe = 2) %>%
        mutate(aupreg = b + u*(a-1)) %>% 
        mutate(ID = paste(f,e,d,c,b,a))
      
      IndexB <- which(lu == bestB)
      
      
      varlistB <- unique(Bcdat[IndexB])
      
      
      
      w <- as.numeric(count(varlistB))
      Bcdat <- Bcdat %>%
        mutate("spalte" = Bcdat[IndexB])
      
      ERG <- bind_rows(ERG, ergB)
      
      print(bestB)
      
      for(c in 1:w) {
        
        d <- 0
        e <- 0
        f <- 0
        
        for(d in 1:x) {
          
          e <- 0
          f <- 0
          
          for(e in 1:y) {
            
          
            f <- 0
            
            for(f in 1:z) {
      
              
      
      
      
            }
          }
        } 
      }   
    }     
  }
  
  return(ERG)
}


test<- dectreelearner(kd)

EntKonzernmarkenA <- as.data.frame.matrix(table(kd$Umsatz, kd$Prognose)) %>%
  mutate(Auspragung = row.names(table(kd$Umsatz, kd$Prognose))) 


