# Project Name- GeneticAlgorithm
> Classic genetic algorithm created with R and RStudio.

## Table of contents
* [General Information](#general-information)
* [Technologies](#technologies)
* [Features](#features)
* [Screenshots](#screenshots)
* [Code Examples](#code-examples)
* [Setup](#setup)
* [Status](#status)
* [Contact](#contact)

## General Information
The program finds the maximum of functions using the classical genetic algorithm.
The project was created for the needs of the classes.

## Technologies
Project is created with:
- RStudio version: 1.4.1106
- R version: 4.1.0

## Features
- Chromosome coding: binary
- Roulette wheel selection
- Crossover with a given probability (the place of crossover is randomized)
- Mutation with a given probability
- Calculation of the average value of fitness
- Creating a graph of dependence between the number of the population (iteration) and the average value of fitness

## Screenshots
Example screenshots showing the operation of the classical genetic algorithm.

Example random population:<br>
![Random population](/images/randomPopulation.png)

Fit function graph:<br>
![Fit function graph](/images/fitFunction.png)

Graph of the relationship between the number of generations and the average value of fitness:<br>
![Graph](/images/graph.PNG)

## Code Examples
The code represents the crossover logic:
```
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
```

## Setup
To run the code, download the R language installer [here](https://cran.r-project.org/) and download and install the [RStudio](https://www.rstudio.com/products/rstudio/) graphical interface.
After a successful installation it is necessary to download the "composition" package and the "composition" library.
The final step is to download the code and run it.

## Status
Project is: *complete*.

## Contact
Created by [Dominika Szypulska](https://github.com/DominikaSzypulska).
<br>E-mail: dominikaszypulska@onet.pl -feel free to contact me!
