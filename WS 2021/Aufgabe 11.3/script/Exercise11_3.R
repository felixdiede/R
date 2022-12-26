# Aufgabe 11.3 
setwd("/Users/felixdiederichs/R/WS 2021/Aufgabe 11.3")

# Aufgabe 11.3 a) 
covid <- read.csv(file = "data/covid_19_daily_reports_01-15-2022.csv", header = TRUE, sep = ";")
world <- read.csv(file = "data/World_Countries.csv", header = TRUE, sep = ";")

sum.countries <- function(df){
  countries <- unique(df$Country_Region) # Heraussuchen der einzelnen Länder 
  Country = Confirmed = Deaths = c()
  for(i in 1:length(countries)){
    # Die erste Spalte enthält die Länder 
    Country[i] <- countries[i]
    # Die Spalten für die Fallzahlen werden einfach aufsummiert pro Land 
    # Dies kann auf drei verschiedene Art und Weisen getan werden, die man unten sehen kann: 
    Confirmed[i] <- sum(df[df$Country_Region == countries[i], which(colnames(df) == "Confirmed")])
    Deaths[i] <- sum(subset(df, df$Country_Region == countries[i], select = "Deaths"))
  }
  # Das Ergebnis wird in einen Data Frame geschrieben 
  df_cum <- data.frame(
    Country = Country, 
    Confirmed = Confirmed, 
    Deaths = Deaths)
  return(df_cum)
}

covid_world <- sum.countries(covid)
covid_world = merge(covid_world, world, by = "Country")

covid_world$rel.Deaths <- covid_world$Deaths/covid_world$Population
covid_world$rel.Conf <- covid_world$Confirmed/covid_world$Population


# Aufgabe 11.3 b) 
Kontingenztafel <- data.frame(Land = c("Canada", "France", "Germany", "Italy", "Japan", "United.Kingdom", "US", "Zwischensumme"),
                              Leicht = c(313, 259, 300, 281, 376, 268, 172, 1969), 
                              Mittel = c(63, 117, 76, 95, 0, 93, 155, 599),
                              Schwer = c(1, 1, 1, 1, 1, 16, 50, 71), 
                              Spaltensumme = c(377, 377, 377, 377, 377, 377, 377, 2639))


# Aufgabe 11.3 c) 

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

# Aufgabe 11.3 d) 
pairs(covid_world[, c("Confirmed", "Deaths", "rel.Conf", "rel.Deaths")])

cor(covid_world$rel.Conf, covid_world$rel.Deaths)
cor(covid_world$Confirmed, covid_world$Deaths)

