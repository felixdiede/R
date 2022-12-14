# Aufgabe 4.1 (covid) 
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2020/Aufgabe 4.1")

covid <- read.csv(file = "data/covid_19_daily_reports_11-28-2020.csv", header = TRUE, sep = " ", dec = ".")
covid_ts <- read.csv(file = "data/time_series_covid19_confirmed_11-28-2020.csv", header = TRUE, sep = " ", dec = ".")

# Aufgabe 4.1 b) 
covid_germany <- covid[covid$Country_Region == "Germany", ]

my_mean <- function(vector) {
    n = length(vector)
    sum = sum(vector)
    arith_mit = sum / n 
    return(arith_mit)
}

# teste eigene Funktion 
vector <- c(1:10)
if(my_mean(vector) == mean(vector)) {
    print("[System]: Function valid")
} else {
    print("[Systems]: Function invalid")
}


covid_germany_mean_confirmed <- my_mean(covid_germany$Confirmed)
covid_germany_mean_active <- my_mean(covid_germany$Active)
covid_germany_mean_deaths <- my_mean(covid_germany$Deaths)

# Aufgabe 4.1 c) 
sturges <- round(log2(nrow(covid_germany)), 0)+1 # 6
rice <- round(2*(nrow(covid_germany)^(1/3)), 0) # 5
quadrat <- round(sqrt(nrow(covid_germany)), 0) # 4
summary(covid_germany$Deaths) 

# Hier wurde die Klassengroesse einheitlich gewaehlt, d.h. die Klassen wurden in gleichgrosse Intervalle zerlegt.
hist(covid_germany$Deaths, # Vier Klassen
     freq = FALSE, probability = TRUE, 
     breaks = c(0, 1000, 2000, 3000, 4000), 
     xlab = "Number of Deaths in Germany", 
     main = "Histogram of COVID19-Deaths in Germany in November")

# Hier wurde die Klassengroesse einheitlich gewaehlt, d.h. die Klassen wurden in gleichgrosse Intervalle zerlegt.
hist(covid_germany$Deaths, # Fuenf Klassen
     freq = FALSE, probability = TRUE, 
     breaks = c(0, 800, 1600, 2400, 3200, 4000), 
     xlab = "Number of Deaths in Germany", 
     main = "Histogram of COVID19-Deaths in Germany in November")

# Hier wurden die vorderen Wertebereiche etwas differenzierter betrachtet
hist(covid_germany$Deaths,  
     freq = FALSE, 
     breaks = c(0, 200, 400, 600, 800,  1000, 2000, 3000, 4000), 
     xlab = "Number of Deaths in Germany", 
     main = "Histogram of COVID19-Deaths in Germany in November")

# Durch die Aufsplittung in deutlich kleinere Klassen werden "Luecken" ersichtlich
hist(covid_germany$Deaths,  
     freq = FALSE, probability = TRUE, 
     breaks = c(0, 200, 400, 600, 800,  1000, 1200, 1400, 1600, 1800,2000, 2200, 2400, 2600, 2800,3000, 3200, 3400, 3600, 3800, 4000), 
     xlab = "Number of Deaths in Germany", 
     main = "Histogram of COVID19-Deaths in Germany in November")

# Aufgabe 4.1 e) 
covid_ts_ger <- covid_ts[which(covid_ts$Country_Region == "Germany"), ]
covid_ts_ger <- covid_ts_ger[, c(-1,-2)]
covid_ts_ger <- sapply(covid_ts_ger, function(x) {if(is.integer(x)) as.numeric(x) else x})

install.packages("tidyverse")
library(tidyverse)

covid_ts_ger_nov <- covid_ts_ger[str_detect(names(covid_ts_ger), "X11_")]

growth.rate <- function(vector) {
    r = r_sum = 0
    i = 1
    while(i < length(vector)){
     r <- (vector[i+1] - vector[i]) / vector[i]
     r_sum = r_sum + r
     i = i +1
    }
    return(as.numeric(r_sum/ length(vector)))
}

r <- growth.rate(covid_ts_ger_nov)

# nutze nun geometrischen Mittelwert
install.packages("pracma")
library(pracma)

changes <- function(vector) {
     c = c(1)
     i = 1
     while(i < length(vector)) {
          c[i+1] <- (vector[i+1] - vector[i])/vector[i]
          i = i + 1
     }
     return(c)
}

my_geom_mean <- function(vector) {
     return(pracma::nthroot(prod(changes(vector)), length(vector)))
}

geom_mean <- my_geom_mean(covid_ts_ger_nov)

# Aufgabe 4.1 f) 
install.packages("maps")
library(maps)

severeness <- function(df){
  for(i in 1:nrow(df)){
    if(df$Confirmed[i] < 1000){
      df$Severity[i] <- "green"
      next()
    }
    else if(df$Confirmed[i] < mean(df$Confirmed)){
      df$Severity[i] <- "orange" 
      next()
    }
    else {
      df$Severity[i] <- "red" 
    }
  }
  return(df)
}

covid_sev <- severeness(covid)

map("world", fill = TRUE, col = "white", bg = "lightblue", ylim = c(-60, 90), mar = c(0,0,0,0))

points(covid_sev$Long_, covid_sev$Lat, col = covid_sev$Severity, pch = 16, cex = 0.5)
