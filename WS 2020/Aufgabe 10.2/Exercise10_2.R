# Aufgabe 10.2
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2020/Aufgabe 10.2")

covid <- read.csv(file = "covid_19_daily_reports_01-31-2021.csv", header = TRUE, sep = ";" )
world_nominal <- read.csv(file = "World_nominal.csv", header = TRUE, sep  = ";")

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

# Aufgabe 10.2 b) 
distance_matrix <- dist(covid$Confirmed)

covid_cluster_single <- hclust(distance_matrix, method = "single")
plot(covid_cluster_single)

covid_cluster_complete <- hclust(distance_matrix, method = "complete")
plot(covid_cluster_complete)

covid_cluster_average <- hclust(distance_matrix, method = "average")
plot(covid_cluster_average)

covid_cluster_ward <- hclust(distance_matrix, method = "ward.D2")
plot(covid_cluster_ward)

# Im Dendogramm erkennt man 179, 24, 80 als Außenseiter
covid_outlier <- covid[-c(179, 24, 80), ]
dist_matrix_outlier <- dist(covid_outlier)
covid_cluster_single_out <- hclust(dist_matrix_outlier)
plot(covid_cluster_single_out)


# Aufgabe 10.2 c) 
# Cluster-Ergebnis wird von Funktion hclust erzeugt 
# Cluster-Ergebnis besteht aus mehreren Variablen, wie unter "Value" zu lesen ist
# $order gibt die Reihenfolge an Beobachtungen aus, wie sie im Dendogramm dargestellt sind 
covid_cluster_single$order
# $labels ist leer, da noch keine Anzahl an Clustern festgelegt wurde 
covid_cluster_single$labels


# Aufgabe 10.2 d) 
# Label ordnet jeder Beobachtung das Cluster zu, zu dem es nach dem Clusterergebnis im dendrogramm gehört 
# Da die Anzahl an Clustern nicht vom Algorithmus, sondern erst nach der Betrachtung des Dendogramms festgelegt werden kann, muss diese Variable manuell ausgefüllt werden 
# Dafür kann $order benutzt werden, da es die Reihenfolge der Beobachtungen im Dendogramm festlegt 
covid_cluster_single_out$labels[covid_cluster_single_out$order[1:171]] <- "darkblue"
covid_cluster_single_out$labels[covid_cluster_single_out$order[172:189]] <- "red"

pairs(covid_outlier[,2:5], col = covid_cluster_single_out$labels)

# Aufgabe 10.2 e) 

similarity_coefficient <- function(t) {
    return(t[1,1] / (t[1,1] + t[1,2] + t[2,1]))
}
matching_coefficient <- function(t) {
    return(t[1,1] + t[2,2] / (sum(t)))
}

kontingenz_USA <- table(nominal[which(nominal$Country == "Germany"),2:5], nominal[which(nominal$Country == "US"),2:5])
kontingenz_Frankreich <- table(nominal[which(nominal$Country == "Germany"),2:5], nominal[which(nominal$Country == "France"),2:5])
kontingenz_China <- table(nominal[which(nominal$Country == "Germany"),2:5], nominal[which(nominal$Country == "China"),2:5])

matching_coefficient(kontingenz_USA)
matching_coefficient(kontingenz_Frankreich)
matching_coefficient(kontingenz_China)

similarity_coefficient(kontingenz_USA)
similarity_coefficient(kontingenz_Frankreich)
similarity_coefficient(kontingenz_China)

# Zu Frankreich ist Deutschland am ähnlichsten