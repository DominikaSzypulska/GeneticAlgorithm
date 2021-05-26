install.packages("compositions")
library(compositions)

znajdzMaksimum <- function (fun, x1, x2) {
  max <- 0
  for(i in seq(from=x1,to=x2, by=1.0)){
    if (fun(i) > max) max = fun(i)
  }
  return (max)
}

wylosujPopulacje <- function() {
  losowaPopulacja <- matrix(round(runif(40, 0, 1)), 10, 4)
  return (losowaPopulacja)
}

obliczFenotyp <- function(m) {
  fenotyp <- vector()
  for (i in 1:10) {
    fenotyp[i] <- unbinary(paste(m[i, 2:4], collapse = ""));
    if (m[i, 1] == 1) {
      fenotyp[i] <- fenotyp[i] * (-1);
    }
  }
  return (fenotyp)
}

obliczWycinek <- function(przystosowanie){
  return ((przystosowanie / sum(przystosowanie)) * 100.0)
}

utworzDanePopulacji <- function(populacja){
  fenotyp <- obliczFenotyp(populacja)
  przystosowanie <- fp(fenotyp)
  najlepszePrzystosowanie  <- max(przystosowanie)
  sredniePrzystosowanie <- mean(przystosowanie)
  najlepszyOsobnik <- fenotyp[match(najlepszePrzystosowanie, przystosowanie)]
  
  return (list("populacja" = populacja, "sredniePrzystosowanie" = sredniePrzystosowanie, "najlepszePrzystosowanie" = najlepszePrzystosowanie, "najlepszyOsobnik" = najlepszyOsobnik))
}

iteracjaAlgorytmuGenetycznego <- function(fp, populacja, pKrzyzowania, pMutacji){
  
  przystosowanie <- fp(obliczFenotyp(populacja))
  
  # Metoda ruletki
  wycinekKola <- round(obliczWycinek(przystosowanie) * 10000.0)
  
  przedzialy <- vector()
  for (i in 1:10) {
    if (i == 1) {
      przedzialy[i] <- wycinekKola[i] 
    } else {
      przedzialy[i] <- przedzialy[i - 1] + wycinekKola[i] 
    }
  }
  
  losoweLiczby <- round(runif(10, 0, 1000000))
  wylosowaneChrom <- vector()
  for (j in 1:10) {
    for (i in 1:10) {
      if (i == 1) {
        if (losoweLiczby[j] <= przedzialy[i]) {
          wylosowaneChrom[j] = i
        }
      } else if (losoweLiczby[j] > przedzialy[i - 1] && losoweLiczby[j] <= przedzialy[i]) {
        wylosowaneChrom[j] = i;
      }
    }
  }
  
  # Krzyzowanie
  wylosowaneChrom <- sample(wylosowaneChrom)
  chromDoKrzyz <- matrix(nrow = 10, ncol = 4)
  for(i in 1:10) {
    for(j in 1:4){
      chromDoKrzyz[i, j] = populacja[wylosowaneChrom[i], j]
    }
  }
  
  skrzyzowaneChrom <- matrix(nrow = 10, ncol = 4)
  for(i in seq(from=1,to=10, by=2.0)){
    wylosowanaLiczba <- round(runif(1, 0, 1), 2)
    if(wylosowanaLiczba <= pKrzyzowania){
      miejsceKrzyzowania <- sample(1:3, 1)
      # krzyzujemy
      for(j in 1:4){
        if(j > miejsceKrzyzowania){
          skrzyzowaneChrom[i, j] <- chromDoKrzyz[i + 1, j]
          skrzyzowaneChrom[i + 1, j] <- chromDoKrzyz[i, j]
        } else {
          skrzyzowaneChrom[i, j] <- chromDoKrzyz[i, j]
          skrzyzowaneChrom[i + 1, j] <- chromDoKrzyz[i + 1, j]
        }
      }
    } else {
      # przepisujemy
      for(j in 1:4){
        skrzyzowaneChrom[i, j] <- chromDoKrzyz[i, j]
        skrzyzowaneChrom[i + 1, j] <- chromDoKrzyz[i + 1, j]
      }
    }
  }
  
  # Mutacja
  poMutacji <- matrix(nrow = 10, ncol = 4)
  for(i in 1:10) {
    for(j in 1:4){
      losMutacja <- runif(1,0,1)
      if(losMutacja <= pMutacji){
        poMutacji[i, j] = 0
        if(skrzyzowaneChrom[i, j] == 0){
          poMutacji[i, j] = 1
        }
      } else {
        poMutacji[i, j] <- skrzyzowaneChrom[i, j]
      }
    }
  }
  return (utworzDanePopulacji(poMutacji))
}

algorytmGenetyczny <- function(fp, pKrzyzowania, pMutacji, maxIter){
  kolejneSredniePrzystosowanie <- vector()
  populacja <- wylosujPopulacje()
  
  for(i in 1:maxIter) {
    wynik <- iteracjaAlgorytmuGenetycznego(fp, populacja, pKrzyzowania, pMutacji)
    populacja <- wynik$populacja
    kolejneSredniePrzystosowanie[i] <- wynik$sredniePrzystosowanie	
    
    if (i == 1) {
      najlepszePrzystosowanie <- wynik$najlepszePrzystosowanie
      najlepszyOsobnik <- wynik$najlepszyOsobnik
    }
    else if (wynik$najlepszePrzystosowanie > najlepszePrzystosowanie) {
      najlepszePrzystosowanie <- wynik$najlepszePrzystosowanie
      najlepszyOsobnik <- wynik$najlepszyOsobnik
    }
    
    cat("GA | Nr populacji = ", i, " | Srednia = ", wynik$sredniePrzystosowanie, " | Najlepsza = ", najlepszePrzystosowanie, "\n")
    
    if(i > 1) {	
      # znalezienie najlepszego rozwiazania
      if (kolejneSredniePrzystosowanie[i] >= znajdzMaksimum(fp, -7, 7)) {
        print("Znalezienie najlepszego rozwiazania")
        break
      }
      
      # kolejne epoki nie przynosza zmian (nie ma szans utworzyc lepszej populacji)
      if (kolejneSredniePrzystosowanie[i] == kolejneSredniePrzystosowanie[i-1]) {
        print("Kolejne epoki nie przynosza zmian")
        break
      }
      
      # populacja sklada sie z tego samego chromosomu
      if (length(unique(obliczFenotyp(populacja))) == 1) {
        print("Populacja sklada sie z tego samego chromosomu")
        break
      }
    }
  }
  
  cat("\n\nLiczba iteracji: ", i)
  cat("\nWartosc funkcji fitness: ", najlepszePrzystosowanie)
  cat("\nRozwiazanie: x =", najlepszyOsobnik)
  
  # Wykres zaleznosci
  par(mar = rep(5, 4))
  x <- seq(1, length(kolejneSredniePrzystosowanie), 1)
  plot(x, kolejneSredniePrzystosowanie, xlab = "Liczba generacji", ylab = "Srednia wartosc przystosowania")
  lines(kolejneSredniePrzystosowanie, col="blue")
}

# Definicja funkcji fitness
fp <- function (x) {
  return (-0.2 * x ^ 3 - 0.1 * x ^ 2 + 8 * x + 30)
}

# Wybor parametrow
pK <- 0.8
pM <- 0.1
maxIter <- 100

# Rozpoczynamy symulacje
algorytmGenetyczny(fp, pK, pM, maxIter)
