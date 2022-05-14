######################## wczytanie bibliotek ########################
rm(list=ls()) # czyszczenie zmiennych
library(bnlearn)
library(lattice)
library(gRain)
setwd("B:/Projekty/Studia/VI_semestr/Wnioskowanie w warunkach niepewności/Projekt")

######################## wczytanie danych ########################
dane <- read.csv("Liver.csv")

for(i in 1:length(dane)){
  dane[,i] <- as.numeric(dane[,i])
}
dane <- dane[-7]

######################## funkcja testowa ########################
test_bayes <- function(vector){
  dane1 <- discretize(dane, method="interval", breaks=vector)
  siec_dc_hc <- hc(dane1)

  siec_dc_hc <- set.arc(siec_dc_hc, from="sgot", to="alkphos")
  siec_dc_hc <- set.arc(siec_dc_hc, from="drinks", to="mcv")
  siec_dc_hc <- set.arc(siec_dc_hc, from="gammagt", to="drinks")
  siec_dc_hc <- set.arc(siec_dc_hc, from="sgpt", to="gammagt")
  siec_dc_hc <- set.arc(siec_dc_hc, from="sgot", to="gammagt")
  
  graphviz.plot(siec_dc_hc)
  bn_dc_hc <- bn.fit(siec_dc_hc, dane1)
  
  bn.fit.barchart(bn_dc_hc$mcv)
  bn.fit.barchart(bn_dc_hc$alkphos)
  bn.fit.barchart(bn_dc_hc$sgpt)
  bn.fit.barchart(bn_dc_hc$sgot)
  bn.fit.barchart(bn_dc_hc$gammagt)
  bn.fit.barchart(bn_dc_hc$drinks)
  
  print(score(siec_dc_hc, data=dane1, type="bic"))
}

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


######################## testy ########################
test(max_dist = 1000, a_1 = T)
test(max_dist = 1000, a_2 = T)
test(max_dist = 1000, a_3 = T)
test(max_dist = 1000, a_4 = T)
test(max_dist = 1000, a_5 = T)
test(max_dist = 1000, a_6 = T)

test_bayes(rep(c(3), each=6))

