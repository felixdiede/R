# Aufgabe 8.1 
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2020/Aufgabe 8.1")

# Aufgabe 8.1 a) 
covid <- read.csv(file = "data/covid_19_daily_reports_01-17-2021.csv", header = TRUE, sep = ";")
bundeslaender <- read.csv(file = "data/DE_Bundeslaender.csv", header = TRUE, sep = ";")
states <- read.csv(file = "data/USA_States.csv", header = TRUE, sep = ";")
countries <- read.csv(file = "data/World_Countries.csv", header = TRUE, sep = ";")

# Aufgabe 8.1 b) 
covid_us <- covid[covid$Country_Region == "US", ]
covid_germany <- covid[covid$Country_Region == "Germany", ]

# Aufsummieren der Länder im Welt-Datensatz 
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

covid_world <- sum.countries(covid) 

aggregate_states <- function(df) {
    states <- unique(df$Province_State)
    State = Confirmed = Deaths = Recovered = Active = c()

    for(i in 1:length(states)) { # nolint
        State[i] <- states[i]
        Confirmed[i] <- sum(df[which(df$Province_State == states[i]), which(colnames(df) == "Confirmed")])
        Deaths[i] <- sum(df[which(df$Province_State == states[i]), which(colnames(df) == "Deaths")])
        Recovered[i] <- sum(df[which(df$Province_State == states[i]), which(colnames(df) == "Recovered")])
        Active[i] <- sum(df[which(df$Province_State == states[i]), which(colnames(df) == "Active")])
    }
    
    df_cum <- data.frame(
        State = State, 
        Confirmed = Confirmed, 
        Deaths = Deaths, 
        Recovered = Recovered, 
        Active = Active)

    return(df_cum)
}

covid_us <- aggregate_states(covid_us)


# Aufgabe 8.1 c) 
covid_germany <- merge(x = covid_germany,y = bundeslaender, by.x = "Province_State", by.y = "Bundesland")
covid_us <- merge(x = covid_us, y = states, by.x = "State", by.y = "State")
covid_world <- merge(x = covid_world, y = countries, by.x = "Country", by.y = "Country")

calculate_relative_numbers <- function(df) {
    df$rel.Conf <- df$Confirmed / df$Population
    df$rel.Deaths <- df$Deaths / df$Population

    return(df)
}

covid_germany <- calculate_relative_numbers(covid_germany)
covid_us <- calculate_relative_numbers(covid_us)
covid_world <- calculate_relative_numbers(covid_world)


# Aufgabe 8.1 d) 
boxplot(covid_germany$rel.Conf, main = "Boxplot der relativen bestätigten Fallzahlen in Deutschland")
boxplot(covid_us$rel.Conf, main = "Boxplot der relativen bestätigten Fallzahlen in USA")
boxplot(covid_world$rel.Conf, main = "Boxplot der relativen bestätigten Fallzahlen der gesamten Welt")
par(mfrow = c(1, 3))

# Deutschland 
covid_germany <- covid_germany[order(covid_germany$rel.Conf, decreasing = TRUE), ]
covid_germany[1:2, ] # Bundesländer mit meisten Fallzahlen: Sachsen, Berlin
covid_germany[15:16, ]# Bundesländer mit geringsten Fallzahlen: Schleswig-Holstein, Mecklenburg-Vorpommern

boxplot(covid_germany$rel.Conf, plot = FALSE)
quantile(covid_germany$rel.Conf, probs = c(0.25, 0.5, 0.75))


# USA
covid_us <- covid_us[order(covid_us$rel.Conf, decreasing = TRUE), ]
covid_us[1:2, ]# Bundestaaten mit meisten Fallzahlen: North- and South Dakota
covid_us[c(nrow(covid_us):nrow(covid_us) - 1), ]# Bundestaaten mit den geringsten Fallzahlen: Hawaii

boxplot(covid_us$rel.Conf, plot = FALSE)
quantile(covid_us$rel.Conf, probs = c(0.25, 0.5, 0.75))


# World
covid_world <- covid_world[order(covid_world$rel.Conf, decreasing = TRUE), ]
covid_world[1:2, ]# Ländern mit den meisten Fallzahlen: Montenegro, Czechia, San Marino 

boxplot(covid_world$rel.Conf, plot = FALSE)
quantile(covid_us$rel.Conf, probs = c(0.25, 0.5, 0.75))


# Aufgabe 8.1 d) 
install.packages("ineq")
library(ineq)

plot(Lc(covid_germany$rel.Conf), main = "Lorenzkurve Deutschland", col = "blue")
plot(Lc(covid_us$rel.Conf), main = "Lorenzkurve USA", col = "gold")
plot(Lc(covid_world$rel.Conf), main = "Lorenzkurve Welt", col = "red")
par(mfrow = c(1,3))
