######################## wczytanie bibliotek ########################
rm(list=ls()) # czyszczenie zmiennych
library(bnlearn)
library(lattice)
library(gRain)
# sciezka projektu
setwd("B:/Projekty/Studia/VI_semestr/Wnioskowanie w warunkach niepewności/Projekt")

######################## wczytanie danych ########################
dane <- read.csv("Liver.csv")
str(dane)

for(i in 1:length(dane)){ # zamiana int na numeric
  dane[,i] <- as.numeric(dane[,i])
}
dane <- dane[-7]
str(dane) # sprwadzenie struktury

########################  sprawdzenie czy dane maja rozklad normalny ########################
# jesli p-value jest wieksze niz alfa=0.05 to mozna przyjac ze dane maja rozklad normalny
shapiro_t <- c()
for(i in 1:(length(dane))){
  shapiro_t[i] <- shapiro.test(dane[,i])$p
}
shapiro_t # zadna kolumna danych nie ma rozkladu normalnego - robie dyskretyzacje

######################## ciagle dane - hc ########################
# budowa sieci
siec_hc <- hc(dane)
graphviz.plot(siec_hc)
bn_hc <- bn.fit(siec_hc, dane)
bn_hc

########################  ciagle dane - pc.stable ########################
siec_pc <- pc.stable(dane)
graphviz.plot(siec_pc)
# bn_pc <- bn.fit(siec_pc, dane) # nie dziala
# bn_pc

########################  ciagle dane - gs ########################
siec_gs <- gs(dane)
graphviz.plot(siec_gs)
bn_gs <- bn.fit(siec_gs, dane)
bn_gs

########################  ciagle dane - iamb ########################
siec_iamb <- iamb(dane)
graphviz.plot(siec_iamb)
bn_iamb <- bn.fit(siec_iamb, dane)
bn_iamb

######################## dyskretyzacja danych ########################
dane1 <- discretize(dane, method="interval", breaks=c(3, 3, 4, 5, 6, 7))

######################## dane zdyskretyzowane - na wzor siec_hc ########################
# druga siec - po dyskretyzacji
siec_dc_hc <- hc(dane1)
graphviz.plot(siec_dc_hc)
bn_dc_hc <- bn.fit(siec_dc_hc, dane1)
bn_dc_hc # wychodzi niezbyt dobra siec

# budowa na wzor siec_hc
siec_dc_hc <- set.arc(siec_dc_hc, from="sgot", to="alkphos")
siec_dc_hc <- set.arc(siec_dc_hc, from="drinks", to="mcv")
siec_dc_hc <- set.arc(siec_dc_hc, from="gammagt", to="drinks")
siec_dc_hc <- set.arc(siec_dc_hc, from="sgpt", to="gammagt")
siec_dc_hc <- set.arc(siec_dc_hc, from="sgot", to="gammagt")

graphviz.plot(siec_dc_hc)
bn_dc_hc <- bn.fit(siec_dc_hc, dane1)
bn_dc_hc

######################## dane zdyskretyzowane - na wzor siec_pc ########################
# druga siec - po dyskretyzacji
siec_dc_pc <- pc.stable(dane1)
graphviz.plot(siec_dc_pc)
bn_dc_pc <- bn.fit(siec_dc_pc, dane1)
bn_dc_pc # wychodzi niezbyt dobra siec

# budowa na wzor siec_pc
siec_dc_pc <- set.arc(siec_dc_pc, from="sgot", to="drinks")
siec_dc_pc <- set.arc(siec_dc_pc, from="drinks", to="gammagt")
siec_dc_pc <- set.arc(siec_dc_pc, from="mcv", to="drinks")
siec_dc_pc <- set.arc(siec_dc_pc, from="sgpt", to="gammagt")
siec_dc_pc <- set.arc(siec_dc_pc, from="sgot", to="gammagt")
siec_dc_pc <- set.arc(siec_dc_pc, from="sgpt", to="sgot")

graphviz.plot(siec_dc_pc)
bn_dc_pc <- bn.fit(siec_dc_pc, dane1)
#bn_dc_pc

######################## dane zdyskretyzowane - na wzor siec_gs ########################
# druga siec - po dyskretyzacji
siec_dc_gs <- gs(dane1)
graphviz.plot(siec_dc_gs)
bn_dc_gs <- bn.fit(siec_dc_gs, dane1)
bn_dc_gs # wychodzi niezbyt dobra siec

# budowa na wzor siec_gs
siec_dc_gs <- set.arc(siec_dc_gs, from="gammagt", to="drinks")
siec_dc_gs <- set.arc(siec_dc_gs, from="sgpt", to="gammagt")
siec_dc_gs <- set.arc(siec_dc_gs, from="sgot", to="gammagt")
siec_dc_gs <- set.arc(siec_dc_gs, from="sgot", to="alkphos")
siec_dc_gs <- set.arc(siec_dc_gs, from="mcv", to="drinks")
siec_dc_gs <- set.arc(siec_dc_gs, from="sgpt", to="sgot")

graphviz.plot(siec_dc_gs)
bn_dc_gs <- bn.fit(siec_dc_gs, dane1)
bn_dc_gs

######################## dane zdyskretyzowane - na wzor siec_iamb ########################
# druga siec - po dyskretyzacji
siec_dc_iamb <- iamb(dane1)
graphviz.plot(siec_dc_iamb)
bn_dc_iamb <- bn.fit(siec_dc_iamb, dane1)
bn_dc_iamb # wychodzi niezbyt dobra siec

# budowa na wzor siec_gs
siec_dc_iamb <- set.arc(siec_dc_iamb, from="gammagt", to="drinks")
siec_dc_iamb <- set.arc(siec_dc_iamb, from="sgot", to="drinks")
siec_dc_iamb <- set.arc(siec_dc_iamb, from="sgpt", to="sgot")
siec_dc_iamb <- set.arc(siec_dc_iamb, from="sgot", to="gammagt")
siec_dc_iamb <- set.arc(siec_dc_iamb, from="mcv", to="drinks")
siec_dc_iamb <- set.arc(siec_dc_iamb, from="sgpt", to="gammagt")

graphviz.plot(siec_dc_iamb)
bn_dc_iamb <- bn.fit(siec_dc_iamb, dane1)
bn_dc_iamb

######################## score ########################
score(siec_hc, data=dane)
# score(siec_pc, data=dane) # graf jest tylko czesciowo skierowany
# score(siec_gs, data=dane) # graf jest tylko czesciowo skierowany
# score(siec_iamb, data=dane) # graf jest tylko czesciowo skierowany
score(siec_dc_hc, data=dane1, type="bic") # wychodzi najblizej 0
score(siec_dc_pc, data=dane1, type="bic")
score(siec_dc_gs, data=dane1, type="bic")
score(siec_dc_iamb, data=dane1, type="bic")

######################## wybranie najlepszej sieci ########################
# funkcja testowa
test <- function(max_dist = 500, a_1 = F, a_2 = F, a_3 = F, a_4 = F, a_5 = F, a_6 = F){
  w <- data.frame(c(0), c(0))
  colnames(w) <- c("distance", "break_no")
  ee <- 2
  distance <- 5000
  
  if(a_1 == TRUE){
    col_no <- 1
    while(distance > max_dist){
      dane2 <- discretize(dane, method="interval", breaks = c(ee,3,3,3,3,3))
      ee <- ee +1
      distance <- sum(dist(table(dane2[,col_no])))
    }
  } else if(a_2 == TRUE){
    col_no <- 2
    while(distance > max_dist){
      dane2 <- discretize(dane, method="interval", breaks = c(3,ee,3,3,3,3))
      ee <- ee +1
      distance <- sum(dist(table(dane2[,col_no])))
    }
  } else if(a_3 == TRUE){
    col_no <- 3
    while(distance > max_dist){
      dane2 <- discretize(dane, method="interval", breaks = c(3,3,ee,3,3,3))
      ee <- ee +1
      distance <- sum(dist(table(dane2[,col_no])))
    }
  } else if(a_4 == TRUE){
    col_no <- 4
    while(distance > max_dist){
      dane2 <- discretize(dane, method="interval", breaks = c(3,3,3,ee,3,3))
      ee <- ee +1
      distance <- sum(dist(table(dane2[,col_no])))
    }
  } else if(a_5 == TRUE){
    col_no <- 5
    while(distance > max_dist){
      dane2 <- discretize(dane, method="interval", breaks = c(3,3,3,3,ee,3))
      ee <- ee +1
      distance <- sum(dist(table(dane2[,col_no])))
    }
  } else if(a_6 == TRUE){
    col_no <- 6
    while(distance > max_dist){
      dane2 <- discretize(dane, method="interval", breaks = c(3,3,3,3,3,ee))
      ee <- ee +1
      distance <- sum(dist(table(dane2[,col_no])))
    }
  } else{
    print("ERROR")
  }
  w[,1] <- distance
  w[,2] <- ee
  return(w)
}

# widac, ze najlepiej podzielic kolumny na 3 czesci
test(max_dist = 1000, a_1 = T)
test(max_dist = 1000, a_2 = T)
test(max_dist = 1000, a_3 = T)
test(max_dist = 1000, a_4 = T)
test(max_dist = 1000, a_5 = T)
test(max_dist = 1000, a_6 = T)

# zamiana danych na lepsze
dane1 <- discretize(dane, method="interval", breaks=rep(c(3), each=6))

# najlepsza siec
siec <- hc(dane1)
siec <- set.arc(siec, from="sgot", to="alkphos")
siec <- set.arc(siec, from="drinks", to="mcv")
siec <- set.arc(siec, from="gammagt", to="drinks")
siec <- set.arc(siec, from="sgpt", to="gammagt")
siec <- set.arc(siec, from="sgot", to="gammagt")
bn <-  bn.fit(siec, dane1)
bn
graphviz.plot(siec)

# score duzo lepszy niz wczesniej
score(siec, data=dane1, type="bic")

# ile testow musial przeprowadzic algorytm zeby nauczyc sie tej sieci
ntests(siec)

# budowa sieci z opcja debug
#hc(dane, debug = TRUE)

# usuwanie dla przejrzstosci
remove(bn_dc_gs, bn_dc_hc, bn_dc_iamb, bn_dc_pc, bn_hc,
       dane, ee, ee1, i, j, shapiro_t, test,
       siec_dc_gs, siec_dc_hc, siec_dc_iamb, siec_dc_pc, siec_gs, siec_hc, siec_iamb, siec_pc)

######################## rozklady warunkowe ######################## 
bn.fit.barchart(bn$mcv)
bn.fit.barchart(bn$alkphos)
bn.fit.barchart(bn$sgpt)
bn.fit.barchart(bn$sgot)
bn.fit.barchart(bn$gammagt)
bn.fit.barchart(bn$drinks)

######################## prawdopodobienstwa warunkowe ########################
junction <- compile(as.grain(bn)) # rozklad na drzewo wezlowe

# sgot po warunkiem sgpt
for (i in 1:length(unique(dane1$sgpt))) {
  ee <- setEvidence(junction, nodes = c("sgpt"), states = c(as.character(unique(dane1$sgpt)[i])))
  cat("\n", "sgot pod warunkiem sgpt =", as.character(unique(dane1$sgpt)[i]), "\n")
  print(querygrain(ee, nodes="sgot")$sgot)
}

# alkphos po warunkiem sgot
for (i in 1:length(unique(dane1$sgpt))) {
  ee <- setEvidence(junction, nodes = c("sgot"), states = c(as.character(unique(dane1$sgot)[i])))
  cat("\n", "alkphos pod warunkiem sgot =", as.character(unique(dane1$sgpt)[i]), "\n")
  print(querygrain(ee, nodes="alkphos")$alkphos)
}

# alkphos po warunkiem sgpt i sgot
for (i in 1:length(unique(dane1$sgpt))) {
  for (j in 1:length(unique(dane1$sgot))) {
    ee1 <- setEvidence(junction, nodes = c("sgot", "sgpt"), states = c(as.character(unique(dane1$sgot))[i], as.character(unique(dane1$sgpt))[j]))
    cat("\n", "alkphos pod warunkiem sgpt i sgot =", as.character(unique(dane1$sgot)[i]), as.character(unique(dane1$sgpt)[j]), "\n")
    print(querygrain(ee1, nodes="alkphos")$alkphos)
  }
}

######################## prawdopodobienstwa warunkowe - na kartce ########################
licz_sgot_sgpt_11 <- 0; licz_sgot_sgpt_12 <- 0; licz_sgot_sgpt_13 <- 0;
licz_sgot_sgpt_21 <- 0; licz_sgot_sgpt_22 <- 0; licz_sgot_sgpt_23 <- 0;
licz_sgot_sgpt_31 <- 0; licz_sgot_sgpt_32 <- 0; licz_sgot_sgpt_33 <- 0;

for (i in 1:nrow(dane1)){
  if(dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # sgot1
    licz_sgot_sgpt_11 <- licz_sgot_sgpt_11 + 1
  } else if (dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_sgot_sgpt_12 <- licz_sgot_sgpt_12 + 1
  } else if (dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_sgot_sgpt_13 <- licz_sgot_sgpt_13 + 1
  } else if (dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # sgot2
    licz_sgot_sgpt_21 <- licz_sgot_sgpt_13 + 1
  } else if (dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_sgot_sgpt_22 <- licz_sgot_sgpt_13 + 1
  } else if (dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_sgot_sgpt_23 <- licz_sgot_sgpt_13 + 1
  } else if (dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # sgot3
    licz_sgot_sgpt_31 <- licz_sgot_sgpt_13 + 1
  } else if (dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_sgot_sgpt_32 <- licz_sgot_sgpt_13 + 1
  } else if (dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_sgot_sgpt_33 <- licz_sgot_sgpt_13 + 1
  } else{print("error")}
}

licz_alk_sgot_sgpt_111 <- 0; licz_alk_sgot_sgpt_112 <- 0; licz_alk_sgot_sgpt_113 <- 0;
licz_alk_sgot_sgpt_121 <- 0; licz_alk_sgot_sgpt_122 <- 0; licz_alk_sgot_sgpt_123 <- 0;
licz_alk_sgot_sgpt_131 <- 0; licz_alk_sgot_sgpt_132 <- 0; licz_alk_sgot_sgpt_133 <- 0;
licz_alk_sgot_sgpt_211 <- 0; licz_alk_sgot_sgpt_212 <- 0; licz_alk_sgot_sgpt_213 <- 0;
licz_alk_sgot_sgpt_221 <- 0; licz_alk_sgot_sgpt_222 <- 0; licz_alk_sgot_sgpt_223 <- 0;
licz_alk_sgot_sgpt_231 <- 0; licz_alk_sgot_sgpt_232 <- 0; licz_alk_sgot_sgpt_233 <- 0;
licz_alk_sgot_sgpt_311 <- 0; licz_alk_sgot_sgpt_312 <- 0; licz_alk_sgot_sgpt_313 <- 0;
licz_alk_sgot_sgpt_321 <- 0; licz_alk_sgot_sgpt_322 <- 0; licz_alk_sgot_sgpt_323 <- 0;
licz_alk_sgot_sgpt_331 <- 0; licz_alk_sgot_sgpt_332 <- 0; licz_alk_sgot_sgpt_333 <- 0;

for (i in 1:nrow(dane1)){
  if (dane1$alkphos[i] == unique(dane1$alkphos)[1] && dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # alkphos1 - sgot1
    licz_alk_sgot_sgpt_111 <- licz_alk_sgot_sgpt_111 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[1] && dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_alk_sgot_sgpt_112 <- licz_alk_sgot_sgpt_112 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[1] && dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_alk_sgot_sgpt_113 <- licz_alk_sgot_sgpt_113 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[1] && dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # alkphos1 - sgot2
    licz_alk_sgot_sgpt_121 <- licz_alk_sgot_sgpt_121 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[1] && dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_alk_sgot_sgpt_122 <- licz_alk_sgot_sgpt_122 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[1] && dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_alk_sgot_sgpt_123 <- licz_alk_sgot_sgpt_123 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[1] && dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # alkphos1 - sgot3
    licz_alk_sgot_sgpt_131 <- licz_alk_sgot_sgpt_131 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[1] && dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_alk_sgot_sgpt_132 <- licz_alk_sgot_sgpt_132 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[1] && dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_alk_sgot_sgpt_133 <- licz_alk_sgot_sgpt_133 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[2] && dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # alkphos2 - sgot1
    licz_alk_sgot_sgpt_211 <- licz_alk_sgot_sgpt_211 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[2] && dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_alk_sgot_sgpt_212 <- licz_alk_sgot_sgpt_212 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[2] && dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_alk_sgot_sgpt_213 <- licz_alk_sgot_sgpt_213 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[2] && dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # alkphos2 - sgot2
    licz_alk_sgot_sgpt_221 <- licz_alk_sgot_sgpt_221 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[2] && dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_alk_sgot_sgpt_222 <- licz_alk_sgot_sgpt_222 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[2] && dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_alk_sgot_sgpt_223 <- licz_alk_sgot_sgpt_223 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[2] && dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # alkphos2 - sgot3
    licz_alk_sgot_sgpt_231 <- licz_alk_sgot_sgpt_231 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[2] && dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_alk_sgot_sgpt_232 <- licz_alk_sgot_sgpt_232 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[2] && dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_alk_sgot_sgpt_233 <- licz_alk_sgot_sgpt_233 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[3] && dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # alkphos3 - sgot1
    licz_alk_sgot_sgpt_311 <- licz_alk_sgot_sgpt_311 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[3] && dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_alk_sgot_sgpt_312 <- licz_alk_sgot_sgpt_312 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[3] && dane1$sgot[i] == unique(dane1$sgot)[1] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_alk_sgot_sgpt_313 <- licz_alk_sgot_sgpt_313 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[3] && dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # alkphos3 - sgot2
    licz_alk_sgot_sgpt_321 <- licz_alk_sgot_sgpt_321 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[3] && dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_alk_sgot_sgpt_322 <- licz_alk_sgot_sgpt_322 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[3] && dane1$sgot[i] == unique(dane1$sgot)[2] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_alk_sgot_sgpt_323 <- licz_alk_sgot_sgpt_323 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[3] && dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[1]){ # alkphos3 - sgot3
    licz_alk_sgot_sgpt_331 <- licz_alk_sgot_sgpt_331 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[3] && dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[2]){
    licz_alk_sgot_sgpt_332 <- licz_alk_sgot_sgpt_332 + 1
  } else if (dane1$alkphos[i] == unique(dane1$alkphos)[3] && dane1$sgot[i] == unique(dane1$sgot)[3] && dane1$sgpt[i] == unique(dane1$sgpt)[3]){
    licz_alk_sgot_sgpt_333 <- licz_alk_sgot_sgpt_333 + 1
  } else {print("error")}
}

table(dane1$sgpt)
table(dane1$sgot)
table(dane1$alkphos)

