library(e1071)
library(openxlsx)
library(ggplot2)
dane <- read.xlsx("dane.xlsx")
dane <- dane[, -ncol(dane)]

srednia_bezrobocie <- mean(dane$bezrobocie)
odchylenie_bezrobocie <- sd(dane$bezrobocie)
skosnosc_bezrobocie <- skewness(dane$bezrobocie)
kurtoza_bezrobocie <- kurtosis(dane$bezrobocie)
wspo_zmiennosci_bezrobocie <- odchylenie_bezrobocie/srednia_bezrobocie * 100
cat("BEZROBOCIE:
Średnia:", srednia_bezrobocie,
"Odchylenie standardowe:", odchylenie_bezrobocie,
"Skośność:", skosnosc_bezrobocie,
"Kurtoza:", kurtoza_bezrobocie,
"Współczynnik zmienności:", wspo_zmiennosci_bezrobocie)
ggplot(dane, aes(y = bezrobocie)) +
  geom_boxplot()

srednia_wynagrodzenia <- mean(dane$wynagrodzenie)
odchylenie_wynagrodzenia <- sd(dane$wynagrodzenie)
skosnosc_wynagrodzenia <- skewness(dane$wynagrodzenie)
kurtoza_wynagrodzenia <- kurtosis(dane$wynagrodzenie)
wspo_zmiennosci_wynagrodzenia <- odchylenie_wynagrodzenia/srednia_wynagrodzenia * 100
cat("WYNAGRODZENIA:
Średnia:", srednia_wynagrodzenia,
    "Odchylenie standardowe:", odchylenie_wynagrodzenia,
    "Skośność:", skosnosc_wynagrodzenia,
    "Kurtoza:", kurtoza_wynagrodzenia,
    "Współczynnik zmienności:", wspo_zmiennosci_wynagrodzenia)
ggplot(dane, aes(y = wynagrodzenie)) +
  geom_boxplot()

srednia_wsp_fem <- mean(dane$wspolczynnik_feminizacji)
odchylenie_wsp_fem <- sd(dane$wspolczynnik_feminizacji)
skosnosc_wsp_fem <- skewness(dane$wspolczynnik_feminizacji)
kurtoza_wsp_fem <- kurtosis(dane$wspolczynnik_feminizacji)
wspo_zmiennosci_wsp_fem <- odchylenie_wsp_fem/srednia_wsp_fem * 100
cat("WSPÓŁCZYNNIK FEMINIZACJI:
Średnia:", srednia_wsp_fem,
    "Odchylenie standardowe:", odchylenie_wsp_fem,
    "Skośność:", skosnosc_wsp_fem,
    "Kurtoza:", kurtoza_wsp_fem,
    "Współczynnik zmienności:", wspo_zmiennosci_wsp_fem)
ggplot(dane, aes(y = wspolczynnik_feminizacji)) +
  geom_boxplot()

srednia_wsp_urb <- mean(dane$wspolczynnik_urbanizacji)
odchylenie_wsp_urb <- sd(dane$wspolczynnik_urbanizacji)
skosnosc_wsp_urb <- skewness(dane$wspolczynnik_urbanizacji)
kurtoza_wsp_urb <- kurtosis(dane$wspolczynnik_urbanizacji)
wspo_zmiennosci_wsp_urb <- odchylenie_wsp_urb/srednia_wsp_urb * 100
cat("WSPÓŁCZYNNIK URBANIZACJI:
Średnia:", srednia_wsp_urb,
    "Odchylenie standardowe:", odchylenie_wsp_urb,
    "Skośność:", skosnosc_wsp_urb,
    "Kurtoza:", kurtoza_bezrobocie,
    "Współczynnik zmienności:", wspo_zmiennosci_wsp_urb)
ggplot(dane, aes(y = wspolczynnik_urbanizacji)) +
  geom_boxplot()

srednia_oferty <- mean(dane$oferty_pracy_na_10000)
odchylenie_oferty<- sd(dane$oferty_pracy_na_10000)
skosnosc_oferty <- skewness(dane$oferty_pracy_na_10000)
kurtoza_oferty <- kurtosis(dane$oferty_pracy_na_10000)
wspo_zmiennosci_oferty<- odchylenie_oferty/srednia_oferty * 100
cat("OFERTY PRACY:
Średnia:", srednia_oferty,
    "Odchylenie standardowe:", odchylenie_oferty,
    "Skośność:", skosnosc_oferty,
    "Kurtoza:", kurtoza_oferty,
    "Współczynnik zmienności:", wspo_zmiennosci_oferty)
ggplot(dane, aes(y = oferty_pracy_na_10000)) +
  geom_boxplot()

srednia_malzenstwa <- mean(dane$malzenstwa_na_10000)
odchylenie_malzenstwa <- sd(dane$malzenstwa_na_10000)
skosnosc_malzenstwa <- skewness(dane$malzenstwa_na_10000)
kurtoza_malzenstwa <- kurtosis(dane$malzenstwa_na_10000)
wspo_zmiennosci_malzenstwa <- odchylenie_malzenstwa/srednia_malzenstwa * 100
cat("MAŁŻEŃSTWA:
Średnia:", srednia_malzenstwa,
    "Odchylenie standardowe:", odchylenie_malzenstwa,
    "Skośność:", skosnosc_malzenstwa,
    "Kurtoza:", kurtoza_malzenstwa,
    "Współczynnik zmienności:", wspo_zmiennosci_malzenstwa)
ggplot(dane, aes(y = malzenstwa_na_10000)) +
  geom_boxplot()

dane_do_korelacji <- dane[, -1]
korelacja <- cor(dane_do_korelacji)

#metoda hellwiga
Y <- dane[, 2]
xlist <- lapply(dane[, 3:7], function(x) as.numeric(x))

# Funkcja Metoda Hellwiga, która zwraca sumę wszystkich h- indywidualnych pojemności nośników informacji
metoda_hellwiga <- function(y, podzbior_zmiennych) {
  H <- 0
  n <- length(podzbior_zmiennych)
  for (i in 1:n) {
    c1 <- (cor(podzbior_zmiennych[[i]], y))^2 # Iteruje przez każdą zmienną w podzbiorze i dla każdej zmiennej oblicza kwadrat korelacji między nią a zmienną y
    suma_mianownik <- 0
    for (j in 1:n) {
      c2 <- abs(cor(podzbior_zmiennych[[i]], podzbior_zmiennych[[j]])) # Oblicza sumę wartości bezwzględnych korelacji między tą zmienną a każdą inną zmienną w podzbiorze
      
      suma_mianownik <- suma_mianownik + c2
    }
    h <- c1 / suma_mianownik # Dzieli kwadrat korelacji przez sumę wartości bezwzględnych korelacji
    H <- H + h # Sumuje te wartości dla wszystkich zmiennych w podzbiorze
  }
  return(H) # Zwraca wartość integralnej pojemności informacyjnej dla tego podzbioru
}

# Funkcja do obliczania integralnej pojemności informacyjnej dla podzbioru zmiennych
integralna_pojemnosc <- function(y, xlist) {
  n <- length(xlist)
  H_max <- -Inf
  najlepszy_podzbior <- NULL
  
  for (i in 1:(2^n - 1)) { # Pętla iteruje po wszystkich kombinacjach podzbiorach zmiennych z listy xlist
    pozycja_binarna <- (i %/% 2^(0:(n-1))) %% 2 == 1 # zwraca wektor binarny, który określa które zmienne zostaną wybrane do podzbioru
    wybrane_zmienne <- xlist[pozycja_binarna]
    H <- metoda_hellwiga(y, wybrane_zmienne) # Oblicza wartość integralnej pojemności informacyjnej H dla aktualnego podzbioru zmiennych
    if (H > H_max) {
      H_max <- H
      najlepszy_podzbior <- which(pozycja_binarna)
    }
  }
  
  return(list(H_max, najlepszy_podzbior)) # Zostaje zwracana lista z maksymalną wartością integralnej pojemności informacyjnej oraz indeksy najlepszego podzbioru zmiennych.
}

# Wypisanie wyników dla wylosownaych danych
wynik <- integralna_pojemnosc(Y, xlist)
cat("Największa wartość integralnej pojemności informacyjnej:", wynik[[1]], "\n")
cat("Najlepszy podzbiór zmiennych:")
for (i in wynik[[2]]) {
  cat(" ", paste("X", i, sep = ""))
}

