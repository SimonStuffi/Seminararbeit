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
                              ifelse(Breitengrad <= Breitgr & Längengrad > Längengr, "<50,2724/>9,7901", 
                                     ifelse(Breitengrad > Breitgr & Längengrad > Längengr, ">50,2724/>9,7901", NA)))))


ges <- ges %>%
  filter(Jahr == 2019)

progdat <- ges %>%
  select("Unternehmen","Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad")%>%
  mutate(Kundenklasse = ifelse(Kundenklasse != "A","notA","A"))%>%
  mutate(p1 = 99.000000) %>%
  mutate(Datenmenge = 0)

decTree <- read.csv("Decisiontree_fin.csv")

testdata <- read.csv("testdata.csv")%>%
  mutate(Unternehmen = 1:length(testdata$Unternehmen))

Checktable <- testdata[c(2,11)]

testdata <- testdata %>%
  select("Unternehmen","Konzernmarken", "Umsatz","NettoDB", "Preisumsetzung", "Kundenklasse", "Quad")%>%
  mutate(p1 = 99.000000) %>%
  mutate(Datenmenge = 0)


################# Funktion für DecTree Anwendung:

prognose <- function(erg, dictree){
  
  prog <- erg %>%
    filter(Unternehmen == "vier")
  
  
  for(u in erg$Unternehmen){
    
    unt <- erg %>%
      filter(Unternehmen == u)
    
    branch <- dictree
    zbranch <- branch
    
    for (i in 7:17) {
      
      
      if(sum(zbranch[,3])==0){
        
        unt[1,8] <- 0
        unt[1,9] <- zbranch[1,5]
        break
      }
      
      if(sum(zbranch[,4])==0){
        
        unt[1,8] <- 1
        unt[1,9] <- zbranch[1,5]
        break
      }
      
      branch <- zbranch
      
      
      
      if(branch[2,i] == "Konzernmarken" ){
        
        zbranch<-branch %>%
          filter(branch[,i+1] == unt$Konzernmarken)
      }else{
        if(branch[2,i] == "Umsatz" ){
          
          zbranch<-branch %>%
            filter(branch[,i+1] == as.character(unt$Umsatz))
        }else{
          if(branch[2,i] == "NettoDB" ){
            
            zbranch<-branch %>%
              filter(branch[,i+1] == as.character(unt$NettoDB))
          }else{
            if(branch[2,i] == "Preisumsetzung" ){
              
              
              zbranch<-branch %>%
                filter(branch[,i+1] == as.character(unt$Preisumsetzung))
            }else{
              if(branch[2,i] == "Kundenklasse" ){
                
                zbranch<-branch %>%
                  filter(branch[,i+1] == unt$Kundenklasse)
              }else{
                if(branch[2,i] == "Quad" ){
                  
                  zbranch<-branch %>%
                    filter(branch[,i+1] == unt$Quad)
                }
              }
            }
          }
        }
      }
      
      i <- i+1
      
      if(length(zbranch[,1]) == 0){
        
        zbranch[1,2] <- branch[1,2]
        zbranch[1,5] <- branch[1,3] + branch[1,4]
        break
      }
      
    }
    unt[1,8] <- zbranch[1,2]
    unt[1,9] <- zbranch[1,5]
    
    prog <- bind_rows(prog,unt)
  }
  return(prog)
}

################### Anwenndung und umformung der Ergebnisse

testerg <- prognose(testdata, decTree)

realtest <- left_join(Checktable, testerg, "Unternehmen")

diffrealtest <- realtest%>%
  mutate(Prognosetest = ifelse(p1 == 0,"notA",
                               ifelse(p1 < 0.33,"wahrscheinlich notA",
                                      ifelse(p1 >= 0.33 & p1 < 0.5,"tendiert notA",
                                             ifelse(p1 == 0.5,"Gleichverteilt",
                                                    ifelse(p1 > 0.5 & p1 <= 0.66,"tendiert A",
                                                           ifelse(p1 < 1,"wahrscheinlich A",
                                                                  ifelse(p1 == 1,"A",NA))))))))
realtest <- realtest%>%
  mutate(Prognosetest = ifelse(p1 < 0.5,"notA","A"))

testergebnis <- as.data.frame.matrix(prop.table(table(realtest$Prognose,realtest$Prognosetest)))

difftestergebnis <- as.data.frame.matrix(prop.table(table(diffrealtest$Prognose,diffrealtest$Prognosetest),margin = 2))
  

names(testergebnis) = c("prog. A", "prog. notA")

kable(testergebnis)%>%
  kable_styling()

difftestergebnis <- difftestergebnis %>%
  select("A","wahrscheinlich A","tendiert A","Gleichverteilt","tendiert notA","wahrscheinlich notA","notA")

kable(difftestergebnis)%>%
  kable_styling()


finito <- prognose(progdat, decTree)

finito <- finito%>%
  mutate(Prognose = ifelse(p1 == 0,"notA",
                           ifelse(p1 < 0.33,"wahrscheinlich notA",
                                  ifelse(p1 >= 0.33 & p1 < 0.5,"tendiert notA",
                                         ifelse(p1 == 0.5,"Gleichverteilt",
                                                ifelse(p1 > 0.5 & p1 <= 0.66,"tendiert A",
                                                       ifelse(p1 < 1,"wahrscheinlich A",
                                                              ifelse(p1 == 1,"A",NA))))))))



zsmfsung <- finito%>%
  group_by(Prognose)%>%
  summarise(anzahl = length(Unternehmen),
           anteil = length(Unternehmen)/length(finito$Unternehmen),
           avg_Datenmenge = mean(Datenmenge))
         
sum(zsmfsung$anzahl)

