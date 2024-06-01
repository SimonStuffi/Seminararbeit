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
    
    
    
    zerg <- EntropiedataA %>%
      filter(Typ == bestE) %>%
      select("Typ","Auspragung","Vorher","p1","A","notA","gesamt","Entropie") %>%
      mutate(ID = paste(best0,best1,best2,best3,best4,best5))
    
    ZIndex <- which(lu == zbest)   
    
    ifelse(felse(best1 == "-",best1 <- zbest,
           ifelse(best2 == "-",best2 <- zbest,
           ifelse(best3 == "-",best3 <- zbest,
           ifelse(best4 == "-",best4 <- zbest,
           ifelse(best5 == "-",best5 <- zbest,print("error")
           ))))))

    recERG <- bind_rows(recERG, zerg)
    
    if(best5 != "-"){
      print("End reached!")
      next
    }
    
    nexrec <- RecEntr(recERG,Zdat,ZIndex,best0,best1,best2,best3,best4,best5)
    recERG <- bind_rows(recERG, nexrec)
}
