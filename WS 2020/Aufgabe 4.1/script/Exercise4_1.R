# Aufgabe 4.1
setwd("/Users/felixdiederichs/R/WS 2020/Aufgabe 4.1")

#### Aufgabe a) ---- 

covid_cum <- read.table(file = "data/covid_19_daily_reports_11-28-2020.csv",header = TRUE, sep = " ", dec = ".")

covid_ts <- read.table(file = "data/time_series_covid19_confirmed_11-28-2020.csv", header = TRUE, sep = " ", dec = ".")

# Anschauen des Datensatzes: 
str(covid_cum)
summary(covid_cum)
str(covid_ts)
summary(covid_ts)

#### ende ---- 


#### Aufgabe b) ---- 
# Filtern nach den Werten fuer Deutschland 
covid_cum_germany <- covid_cum[covid_cum$Country_Region == "Germany",]

my_mean <- function(vector){
  return( (1/length(vector))*sum(vector) )
}

# Teste die eigene mean-Funktion
my_mean(c(1,2,3)) == mean(c(1,2,3))

# Mittelwert berechnen fuer die geforderten Werte: Durchschnittswerte pro Bundesland  
mean_conf <- my_mean(covid_cum_germany[,which(colnames(covid_cum_germany) == "Confirmed")]) #65271.81 
# oder
mean_conf2 <- my_mean(covid_cum_germany$Confirmed)
mean_death <- my_mean(covid_cum_germany$Deaths) #1011.31
mean_recov <- my_mean(covid_cum_germany$Recovered) #44912.75  
mean_active <- my_mean(covid_cum_germany$Active) #19347.75?

#### ende ---- 


#### Aufgabe c)-d) ---- 
sturges <- round(log2(nrow(covid_cum_germany)), 0)+1 # 6
rice <- round(2*(nrow(covid_cum_germany)^(1/3)), 0) # 5
quadrat <- round(sqrt(nrow(covid_cum_germany)), 0) # 4
summary(covid_cum_germany$Deaths) 

# Hier wurde die Klassengroesse einheitlich gewaehlt, d.h. die Klassen wurden in gleichgrosse Intervalle zerlegt.
hist(covid_cum_germany$Deaths, # Vier Klassen
     freq = FALSE, probability = TRUE, 
     breaks = c(0, 1000, 2000, 3000, 4000), 
     xlab = "Number of Deaths in Germany", 
     main = "Histogram of COVID19-Deaths in Germany in November")

# Hier wurde die Klassengroesse einheitlich gewaehlt, d.h. die Klassen wurden in gleichgrosse Intervalle zerlegt.
hist(covid_cum_germany$Deaths, # Fuenf Klassen
     freq = FALSE, probability = TRUE, 
     breaks = c(0, 800, 1600, 2400, 3200, 4000), 
     xlab = "Number of Deaths in Germany", 
     main = "Histogram of COVID19-Deaths in Germany in November")

# Hier wurden die vorderen Wertebereiche etwas differenzierter betrachtet
hist(covid_cum_germany$Deaths,  
     freq = FALSE, 
     breaks = c(0, 200, 400, 600, 800,  1000, 2000, 3000, 4000), 
     xlab = "Number of Deaths in Germany", 
     main = "Histogram of COVID19-Deaths in Germany in November")

# Durch die Aufsplittung in deutlich kleinere Klassen werden "Luecken" ersichtlich
hist(covid_cum_germany$Deaths,  
     freq = FALSE, probability = TRUE, 
     breaks = c(0, 200, 400, 600, 800,  1000, 1200, 1400, 1600, 1800,2000, 2200, 2400, 2600, 2800,3000, 3200, 3400, 3600, 3800, 4000), 
     xlab = "Number of Deaths in Germany", 
     main = "Histogram of COVID19-Deaths in Germany in November")

#### ende ---- 


#### Aufgabe e) ---- 
#Wir nehmen nun den anderen Datensatz zur Hand und filtern ihn nach den Eintraegen, die Deutschland betreffen.
covid_ts_ger <- covid_ts[which(covid_ts$Country_Region == "Germany"),]

#Nun loeschen wir die Spalten, die die Laenderbezeichnung innehaben und wandeln alle Typen in numeric um 
covid_ts_ger <- covid_ts_ger[,-c(1:2)]
covid_ts_ger <- sapply(covid_ts_ger,function(x){if(is.integer(x)) as.numeric(x) else x})

# Novemberzaehlungen herausfiltern
install.packages("tidyverse")
library(tidyverse)

covid_ts_ger_nov <- covid_ts_ger[str_detect(names(covid_ts_ger), "X11_")] 
#oder 
covid_ts_ger_nov <- covid_ts_ger[285:312] 

# Durchschnittliche Wachstumsrate 
growth.rate <- function(vector){
  r = r_sum = 0 
  i = 1 
  while(i < length(vector)){
    r <- (vector[i+1] - vector[i])/vector[i]
    r_sum = r_sum + r 
    i = i+1
  }
  return(as.numeric(r_sum / length(vector))) 
}
r <- growth.rate(covid_ts_ger_nov)# 0.0239

# Geometrischer Mittelwert der relativen Änderungen 
install.packages("pracma")
library(pracma)

changes <- function(vector){
  c = c(1) 
  i = 1 
  while(i < length(vector)){
    c[i+1] <- (vector[i+1] - vector[i])/vector[i]
    i = i+1
  }
  return(c) 
}

my_geom_mean <- function(vector){
  return(pracma::nthroot(prod(changes(vector)), length(vector))) } 

geom_mean1 <- my_geom_mean(covid_ts_ger_nov) # 0.0245122878 -> Durchschnittlich +2.4 % 

install.packages("EnvStats")
library(EnvStats)

geom_mean2 <- EnvStats::geoMean(changes(covid_ts_ger_nov)) 

round(geom_mean1,4) == round(geom_mean2,4)

#### ende ---- 


#### Aufgabe f) ---- 

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

covid_sev <- severeness(covid_cum)

map("world", fill=TRUE, col="white", bg="lightblue",ylim=c(-60, 90), mar=c(0,0,0,0))

points(covid_sev$Long_,covid_sev$Lat, col=covid_sev$Severity, pch=16, cex = 0.5)

#### ende ---- 

