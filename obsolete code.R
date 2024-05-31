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




