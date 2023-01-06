# Aufgabe 10.1
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2020/Aufgabe 10.1")

# Aufgabe 10.1 a) 
covid <- read.csv(file = "covid_19_daily_reports_01-31-2021.csv", header = TRUE, sep = ";")
covid_ts <- read.csv(file = "time_series_covid19_confirmed_01-31-2021.csv", header = TRUE, sep = ";")
world_countries <- read.csv(file = "World_Countries_new.csv", header = TRUE, sep = ";")


# 1. 
sum.countries <- function(df){
  countries <- unique(df$Country_Region) # Heraussuchen der einzelnen Länder 
  Country = Confirmed = Deaths = Recovered = Active = c()
  for(i in 1:length(countries)){ # nolint
    # Die erste Spalte enthält die Länder 
    Country[i] <- countries[i]
    # Die Spalten für die Fallzahlen werden einfach aufsummiert pro Land 
    Confirmed[i] <- sum(df[which(df$Country_Region == countries[i]), which(colnames(df) == "Confirmed")])
    Deaths[i] <- sum(df[which(df$Country_Region == countries[i]), which(colnames(df) == "Deaths")])
    Recovered[i] <- sum(df[which(df$Country_Region == countries[i]), which(colnames(df) == "Recovered")])
    Active[i] <- sum(df[which(df$Country_Region == countries[i]), which(colnames(df) == "Active")])
  }
  # Das Ergebnis wird in einen Data Frame geschrieben 
  df_cum <- data.frame(
    Country = Country, 
    Confirmed = Confirmed, 
    Deaths = Deaths, 
    Recovered = Recovered, 
    Active = Active)
  return(df_cum)
}

covid <- sum.countries(covid)
covid <- merge(x = covid, y = world_countries, by.x = "Country", by.y = "Country")
covid$rel.Conf <- covid$Confirmed / covid$Population
covid$rel.Deaths <- covid$Deaths / covid$Population
covid$rel.Recovered <- covid$Recovered / covid$Population
covid$rel.Active <- covid$Active / covid$Population


# 2. 
covid_ts_g7 <- covid_ts[covid_ts$Country_Region %in% c("Germany", "Italy", "Canada", "France", "Japan", "United.Kingdom", "US"), ]
covid_ts_g7 <- covid_ts_g7[order(covid_ts_g7$Country_Region), ]
covid_ts_g7 <- covid_ts_g7[, -1]
covid_ts_g7 <- merge(x = covid_ts_g7, y = world_countries, by.x = "Country_Region", by.y = "Country")
covid_ts_g7_rel <- cbind(covid_ts_g7$Country_Region, covid_ts_g7[, 2:ncol(covid_ts_g7)] / covid_ts_g7$Population)
colnames(covid_ts_g7_rel)[1] <- "Country_Region"

# Aufgabe 10.1 b) 
kontingenztabelle <- function(df) {
    Leicht = Mittel = Schwer = Spaltensumme = rep(0, nrow(df))

    for(i in 1:nrow(df)) { # nolint
        for(k in 1:ncol(df)) { # nolint
            if(df[i, k] < 0.01) {
                Leicht[i] <- Leicht[i] + 1
            } else {
                if(df[i, k] < 0.05) {
                    Mittel[i] <- Mittel[i] + 1
                } else {
                    Schwer[i] <- Schwer[i] + 1
                }
                Spaltensumme[i] <- sum(Leicht[i] + Mittel[i] + Schwer[i])
            } 
        }
    }
    Spaltensumme[i + 1] <- sum(Spaltensumme)
    Leicht[i + 1] <- sum(Leicht)
    Mittel[i + 1] <- sum(Mittel)
    Schwer[i + 1] <- sum(Schwer)

    kontingenztabelle <- data.frame(
        Land = c(df$Country_Region, "Zeilensumme"),
        Leicht = Leicht, 
        Mittel = Mittel, 
        Schwer = Schwer,
        Spaltensumme = Spaltensumme
    )
    return(kontingenztabelle)
}

kontingenztafel <- kontingenztabelle(covid_ts_g7_rel)

# Aufgabe 10.1 c) 

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

# Data Frame auf Folie 
df <- data.frame(
  Abschluss = c("R", "GY", "GS", "S"), 
  Ja = c(4,20,2, 26),
  V = c(12,24,6, 42), 
  Nein = c(8, 4, 4, 16), 
  Z = c(24,48,12,84)
)  

kp_stern(df) # Es sollte 0.415139 herauskommen. 
kp_stern(kontingenztafel) # Ergebnis: 0.4754108


# Aufgabe 10.1 d) 
cor(covid$rel.Conf, covid$rel.Recovered)
cor(covid$rel.Conf, covid$rel.Deaths)

pairs(covid[, c("rel.Active", "rel.Conf", "rel.Recovered", "rel.Deaths")])
