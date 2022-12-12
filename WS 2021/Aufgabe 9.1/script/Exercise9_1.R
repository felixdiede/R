# Aufgabe 9.1 (Boxplot und Konzentration)
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2021/Aufgabe 9.1")

# Aufgabe 9.1 a) 
covid <- read.csv(file  = "data/covid_19_daily_reports_12-12-2021.csv", header = TRUE, sep = ";")
bundeslaender <- read.csv(file = "data/DE_Bundeslaender.csv", header = TRUE, sep = ";")
states <- read.csv(file = "data/USA_States.csv", header = TRUE, sep = ";")
countries <- read.csv(file = "data/World_Countries.csv", header = TRUE, sep = ";")

# Aufgabe 9.1 b) 
covid_germany <- covid[covid$Country_Region == "Germany", ]
covid_us <- covid[covid$Country_Region == "US", ]

# Aufsummieren der einzelnen Regionen der Länder 
sum.countries <- function(df){
  Country = Confirmedirmed = Deaths = c()
  countries <- unique(df$Country_Region) # Heraussuchen der einzelnen Länder 
  for(i in 1:length(countries)){ # nolint
    # Die erste Spalte enthält die Länder 
    Country[i] <- countries[i]
    # Die Spalten für die Fallzahlen werden einfach aufsummiert pro Land 
    # Dies kann auf drei verschiedene Art und Weisen getan werden, die man unten sehen kann: 
    Confirmedirmed[i] <- sum(df[df$Country_Region == countries[i], which(colnames(df) == "Confirmedirmed")])
    Deaths[i] <- sum(subset(df, df$Country_Region == countries[i], select = "Deaths"))
  }
  # Das Ergebnis wird in einen Data Frame geschrieben 
  df_cum <- data.frame(
    Country = Country, 
    Confirmedirmed = Confirmedirmed, 
    Deaths = Deaths)
  return(df_cum)
}

covid_sum <- sum.countries(covid)

# Aufsummieren der Staaten der USA
sum.states <- function(df){
  states <- unique(df$Province_State) # Heraussuchen der einzelnen Länder 
  State = Confirmedirmed = Deaths = Recovered = Active = c() # nolint # nolint
  for(i in 1:length(states)){ # nolint
    # Die erste Spalte enthält die Länder 
    State[i] <- states[i]
    # Die Spalten für die Fallzahlen werden einfach aufsummiert pro Land 
    Confirmedirmed[i] <- sum(df[which(df$Province_State == states[i]), which(colnames(df) == "Confirmedirmed")])
    Deaths[i] <- sum(df[which(df$Province_State == states[i]), which(colnames(df) == "Deaths")])
  }
  # Das Ergebnis wird in einen Data Frame geschrieben 
  df_cum <- data.frame(
    State = State, 
    Confirmedirmed = Confirmedirmed, 
    Deaths = Deaths)
  return(df_cum)
}

covid_us <- sum.states(covid_us)


# Aufgabe 9.1 c)

# Germany
covid_germany <- merge(covid_germany, bundeslaender, by.x = "Province_State", by.y = "Bundesland")
covid_germany$rel.Deaths = covid_germany$Deaths / covid_germany$Population
covid_germany$rel.Confirmedirmed = covid_germany$Confirmedirmed / covid_germany$Population

# US
covid_us <- merge(covid_us, states, by = "State")
covid_us$rel.Deaths = covid_us$Deaths / covid_us$Population
covid_us$rel.Confirmedirmed = covid_us$Confirmedirmed / covid_us$Population 

# World
covid_sum <- merge(covid_sum, countries, by = "Country")
covid_sum$rel.Deaths = covid_sum$Deaths / covid_sum$Population
covid_sum$rel.Confirmedirmed = covid_sum$Confirmedirmed / covid_sum$Population


# Aufgabe 9.1 d) 
boxplot(list(covid_germany$rel.Confirmedirmed, covid_us$rel.Confirmedirmed, covid_sum$rel.Confirmedirmed), 
        xlab = c("Länder"), names = c("Deutschland", "USA", "Welt"),
        ylab = "Bestätigte COVID19-Fälle")

boxplot(list(covid_germany$rel.Deaths, covid_us$rel.Deaths, covid_sum$rel.Deaths), 
        xlab = c("Länder"), names = c("Deutschland", "USA", "Welt"),
        ylab = "Bestätigte COVID19-Todesfälle")

# Datenanalyse
# Germany
# Interpretation Deutschland 

ord_ger = covid_germany[order(covid_germany$rel.Confirmed),]
ord_ger[1:2,] # Bundesländer mit wenigstens rel. Fallzahlen: Schlesweig-Holstein und Niedersachsen (3% bzw. 5% der Bevölkerung hatten Corona)
ord_ger[nrow(ord_ger),] # Bundesland mit meisten rel. Fallzahlen: Sachsen (14,5% der Bevölkerung hatten Corona)

boxplot(covid_germany$rel.Confirmed, plot=FALSE) # Stats für Median und Whisker (Zeile 1, 3 und 5 der stats-Matrix)
quantile(x=covid_germany$rel.Confirmed, probs=c(0.25, 0.75)) # 25%:0.06277900, 75%: 0.08523146 

# Interpretation USA 

ord_usa = covid_us[order(covid_us$rel.Confirmed),]
ord_usa[1:3,] # Bundesstaaten mit wenigstens rel. Fallzahlen: Hawaii, Vermont und Oregon (6% - 9% der Bevölkerung hatten Corona )
ord_usa[nrow(ord_usa),] # Bundesstaat mit meisten rel. Fallzahlen: North Dakota (22% der Bevölkerung hatten Corona) 

boxplot(ord_usa$rel.Confirmed, plot=FALSE) # Stats für Median und Whisker (Zeile 1, 3 und 5 der stats-Matrix)
quantile(x=ord_usa$rel.Confirmed, probs=c(0.25, 0.75)) # Quantile 


# Interpretation Welt 

ord_world = covid_sum[order(covid_sum$rel.Confirmed),]
nrow(ord_world)
ord_world[172:174,] # Länder mi den meisten rel. Fallzahlen

boxplot(covid_sum$rel.Confirmed, plot=FALSE) # Stats für Median und Whisker (Zeile 1, 3 und 5 der stats-Matrix)
quantile(x=covid_sum$rel.Confirmed, probs=c(0.25, 0.75)) # Quantile 


#### ende ---- 

#### Aufgabe e) ---- 

# Nach einigem Suchen wurde das "ineq" Paket gefunden, dass eine ganz einfach Funktion "lc" hat zum Zeichnen der Lorenzkurve
# install.packages("ineq")
library(ineq)

# Wie beim Boxplot kann man entweder einen Plot machen und Linien hinzufügen oder den par()-Befehl nutzen, um die Fläche zu dritteln 

plot(Lc(covid_germany$rel.Deaths), col = "gold", main = "Lorenzkurve - relative Todesfälle Deutschland") 
lines(Lc(covid_us$rel.Deaths), col = "darkblue")
lines(Lc(covid_sum$rel.Deaths), col = "darkred")
legend("topleft", legend = c("Deutschland", "USA", "Welt"), fill = c("gold", "darkblue", "darkred"))

# Beim Welt-Plot ist die Ungleichheit am Größten. 

#### ende ---- 
