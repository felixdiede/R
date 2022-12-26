# Aufgabe 12.2
setwd("/Users/felixdiederichs/R/WS 2021/Aufgabe 12.2")

# Aufgabe 12.2 a) 
covid <- read.csv(file = "data/covid_19_daily_reports_01-23-2022.csv", header = TRUE, sep = ";")

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

covid <- sum.countries(covid)

# Aufgabe 12.2 b) 

# Distanzmatrix erstellen, hier ist eine Euklidische Distanz angebracht
## Manhatten-Distanzen sind nicht so anfällig für Ausreißer
dist_euc <- dist(covid$Confirmed, method = "euclidean")

# Erst das Clusterobjekt erstellen (hier immer mit "c..." benannt) und dann mittels der plot-Funktion das Dendrogramm automatisch zeichnen 
## Um das Dendrogramm vernünftig lesen zu können kann es nötig sein, es zu exportieren 
csingle <- hclust(dist_euc, method = "single")
plot(csingle, main = "Single Linkage Clustering", xlab = "Länder-Nummern", ylab = "Distanz")

ccomplete <- hclust(dist_euc, method = "complete")
plot(ccomplete, main = "Complete Linkage Clustering", xlab = "Länder-Nummern", ylab = "Distanz")

caverage <- hclust(dist_euc, method = "average")
plot(caverage, main = "Average Linkage Clustering", xlab = "Länder-Nummern", ylab = "Distanz")

cward <- hclust(dist_euc, method = "ward.D2")
plot(cward, main = "Ward Clustering", xlab = "Länder-Nummern", ylab = "Distanz")


# Entferne Aussenseiter
covid_outlier <- covid[-c(181, 79, 24),]
dist_euc_out <- dist(covid_outlier$Confirmed, method = "euclidean")
cward_out <- hclust(dist_euc_out, method = "ward.D2")
plot(cward_out, main = "Ward Clustering", xlab = "Länder-Nummern", ylab = "Distanz")

# Aufgabe 12.2 c) 
# Das Cluster-Objekt besteht aus mehreren Variablen, wie unter "Value" zu lesen ist
# $order gibt die Reihenfolge an Beobachtungen aus, wie sie im Dendogramm dargestellt sind 

# Aufgabe 12.2 d) 
cward_out$labels[cward_out$order[1:8]] <- "darkblue"
cward_out$lables[cward_out$order[9:157]] <- "forestgreen"
cward_out$labels[cward_out$order[158:191]] <- "red"

pairs(covid_outlier[,2:3], col = cward_out$labels)
