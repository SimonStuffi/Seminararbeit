# obsolete code:



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


}




EntKonzernmarkenA <- as.data.frame.matrix(table(Gcdat$Konzernmarken, Gcdat$Prognose)) %>%
  mutate(Typ = "Konzernmarken")%>%
  mutate(Auspragung = row.names(table(Gcdat$Konzernmarken, Gcdat$Prognose)))
EntKundenklasseA <- as.data.frame.matrix(table(Gcdat$Kundenklasse , Gcdat$Prognose)) %>%
  mutate(Typ = "Kundenklasse") %>%
  mutate(Auspragung = row.names(table(Gcdat$Kundenklasse , Gcdat$Prognose)))
EntQuadA <- as.data.frame.matrix(table(Gcdat$Quad , Gcdat$Prognose)) %>%
  mutate(Typ = "Quad") %>%
  mutate(Auspragung = row.names(table(Gcdat$Quad , Gcdat$Prognose)))
EntUmsatzA <- as.data.frame.matrix(table(Gcdat$Umsatz , Gcdat$Prognose)) %>%
  mutate(Typ = "Umsatz") %>%
  mutate(Auspragung = row.names(table(Gcdat$Umsatz , Gcdat$Prognose)))
EntNettoDbA <- as.data.frame.matrix(table(Gcdat$NettoDB , Gcdat$Prognose)) %>%
  mutate(Typ = "NettoDB") %>%
  mutate(Auspragung = row.names(table(Gcdat$NettoDB , Gcdat$Prognose)))
EntPreisumsetzungA <- as.data.frame.matrix(table(Gcdat$Preisumsetzung , Gcdat$Prognose)) %>%
  mutate(Typ = "Preisumsetzung") %>%
  mutate(Auspragung = row.names(table(Gcdat$Preisumsetzung , Gcdat$Prognose)))


EntropiedataA <- bind_rows(EntKundenklasseA,EntKonzernmarkenA,EntNettoDbA,
                           EntPreisumsetzungA,EntQuadA,EntUmsatzA)
names(EntropiedataA)[2] =  "notA"


EntropiedataA <- EntropiedataA %>%
  mutate(Vorher = G)%>%
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


bestG <- as.character(EntropiesumA[1,1])

ergG <- EntropiedataA %>%
  filter(Typ == bestG) %>%
  select("Typ","Auspragung","Vorher","p1","Anteil","Entropie") %>%
  mutate(Stufe = 6) %>%
  mutate(aupreg = g + y*(e-1)) %>% 
  mutate(ID = paste(f,e,d,c,b,a))


ERG <- bind_rows(ERG, ergG)

}


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




