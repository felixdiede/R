# Aufgabe 11.3 a)
covid <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2021/Aufgabe 11.3/data/covid_19_daily_reports_01-15-2022.csv", header = TRUE, sep = ";") # nolint
world <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2021/Aufgabe 11.3/data/World_Countries.csv", header = T, sep = ";")


# aggregiere Fallzahlen nach Ländern
sum.countries <- function(df) {
    countries <- unique(df$Country_Region)
    Confirmed = Deaths = 0
    countries_aggregrated = data.frame(Land = c(), 
                                       Bestätigt = c(), 
                                       Tod = c())

    for(i in 1:length(countries)) { # nolint
        countries_aggregrated[i, 1] <- countries[i]
        Confirmed <- sum(df$Confirmed[df$Country_Region == countries[i]])
        countries_aggregrated[i, 2] <- Confirmed
        Deaths <- sum(df$Deaths[df$Country_Region == countries[i]])
        countries_aggregrated[i, 3] <- Deaths
    }
    colnames(countries_aggregrated) <- c("Country", "Confirmed", "Deaths")
    return(countries_aggregrated)
}

covid_world <- sum.countries(covid)

covid_world <- merge(x = covid_world, y = world, by = "Country")

covid_world$rel.Confirmed <- covid_world$Confirmed / covid_world$Population
covid_world$rel.Deaths <- covid_world$Deaths / covid_world$Population


# Aufgabe 11.3 b)
Kontingenztafel <- data.frame(Land = c("Canada", "France", "Germany", "Italy", "Japan", "United.Kingdom", "US", "Zeilensumme"), 
                                Leicht = c(313, 259, 300, 281, 376, 268, 172, 1969), 
                                Mittel = c(63, 117, 76, 95, 0, 93, 155, 599), 
                                Schwer = c(1, 1, 1, 1, 1, 16, 50, 71), 
                                Spaltensumme = c(377, 377, 377, 377, 377, 377, 377,2639)) 

kp_stern <- function(kt){
  x <- c()
  # Durchgehen jeder Zeile im Data Frame außer der Zeilensumme 
  for(i in 1:(nrow(kt)-1)){
    # Durchgehen jeder Spalte im Dataframe außer der ersten character-Spalte mit den Ländern und der Spaltensumme 
    for(k in 2:(ncol(kt)-1)){
      # Quadrierte Einträge geteilt durch die multiplizierten Spalten- und Zeilenhäufigkeiten 
      x <- c(x, kt[i,k]^2 / (kt[nrow(kt), k] * kt[i, ncol(kt)]))
    }
  }
  # Summe der Spaltensummen und Zeilensummen 
  n = kt[nrow(kt), ncol(kt)]
  # Berechnen von Chi-Quadrat 
  chi_square <- n * (sum(x)-1)
  # Anzahl der Spalten ohne die Länder-Spalte und die Spaltensumme 
  K = ncol(kt)-2
  # Anzeil der Zeilen ohne die Zeilensumme 
  L = nrow(kt) -1
  
  kp_stern <- sqrt(chi_square  /(chi_square + n)) / sqrt( (min(K,L)-1) / min(K,L))
  return(kp_stern)
}

kp_stern(Kontingenztafel)



# Aufgabe 11.3 c)
pairs(covid_world[, c("Confirmed", "Deaths", "rel.Confirmed", "rel.Deaths")])

cor(covid_world$Confirmed, covid_world$Deaths)
cor(covid_world$rel.Confirmed, covid_world$rel.Deaths)
