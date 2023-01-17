######### ====================== Loesung zum 7.Aufgabenzettel ====================== #########

#### Aufgabe 7.3 ---- 
# a)

greenhouse = read.csv("greenhouse.csv", header=T, sep=";")
meta = read.csv("meta_data.csv", header=T, sep=";")

summary(greenhouse)
str(greenhouse)

options(scipen=999)

# b) 

berechne_summe <- function(df, column){
  # Heraussuchen der einzelnen Werte 
  elemente <- unique(df[, which(colnames(df) == column)]) 
  element = c()
  summe = c()
  for(i in 1:length(elemente)){
    # Die erste Spalte enthält die Länder 
    element[i] <- elemente[i]
    # Die Spalten für die Tonnen CO2 werden einfach aufsummiert pro Land 
    summe[i] <- sum(df[which(df[, which(colnames(df) == column)] == elemente[i]), which(colnames(df) == "Tonnes.Co2e")])
  }
  # Das Ergebnis wird in einen Data Frame geschrieben 
  df_cum <- data.frame(
    Element = element, 
    Sum = summe)
  return(df_cum)
}


# c) 

# Aufsummieren über Sub-Sektoren 
greenhouse_subsector = berechne_summe(greenhouse, "Subsector")
greenhouse_subsector = greenhouse_subsector[order(greenhouse_subsector$Sum, decreasing = T),]
print(head(greenhouse_subsector))

# Aufsummieren über Sektoren 
greenhouse_sector = berechne_summe(greenhouse, "Sector")
greenhouse_sector = greenhouse_sector[order(greenhouse_sector$Sum, decreasing = T),]
print(head(greenhouse_sector))

# Aufsummieren über Länder 
greenhouse_laender = berechne_summe(greenhouse, "Country")
greenhouse_laender = greenhouse_laender[order(greenhouse_laender$Sum, decreasing = T),]
print(head(greenhouse_laender))

# Alternative: aggregate-Funktion 
aggregate(greenhouse$Tonnes.Co2e, list(greenhouse$Country), FUN="sum")

# d) 

meta_greenhouse = meta[meta$Country %in% greenhouse$Country,]
greenhouse$Rel.Tonnes.Co2e = rep(0, nrow(greenhouse))

for(i in 1:nrow(greenhouse)){
  country = greenhouse$Country[i]
  bevoelkerung = meta[which(meta$Country == country), which(colnames(meta) == "Population")]
  greenhouse$Rel.Tonnes.Co2e[i] = greenhouse$Tonnes.Co2e[i] / bevoelkerung
}

# Alternative: merge() und dann teilen 

# e)
## analog zu b) 
berechne_rel_summe <- function(df, column){
  elemente <- unique(df[, which(colnames(df) == column)]) 
  element = c()
  summe = c()
  for(i in 1:length(elemente)){
    element[i] <- elemente[i]
    summe[i] <- sum(df[which(df[, which(colnames(df) == column)] == elemente[i]), which(colnames(df) == "Rel.Tonnes.Co2e")])
  }
  df_cum <- data.frame(
    Element = element, 
    Sum = summe)
  return(df_cum)
}

# Aufsummieren über Sub-Sektoren 
greenhouse_subsector_rel = berechne_rel_summe(greenhouse, "Subsector")
greenhouse_subsector_rel = greenhouse_subsector_rel[order(greenhouse_subsector_rel$Sum, decreasing = T),]
print(head(greenhouse_subsector_rel))

# Aufsummieren über Sektoren 
greenhouse_sector_rel = berechne_rel_summe(greenhouse, "Sector")
greenhouse_sector_rel = greenhouse_sector_rel[order(greenhouse_sector_rel$Sum, decreasing = T),]
print(head(greenhouse_sector_rel))

# Aufsummieren über Länder 
greenhouse_laender_rel = berechne_rel_summe(greenhouse, "Country")
greenhouse_laender_rel = greenhouse_laender_rel[order(greenhouse_laender_rel$Sum, decreasing = T),]
print(head(greenhouse_laender_rel))

# f)

var_sec = var(greenhouse_sector$Sum)
sd_sec = sd(greenhouse_sector$Sum)

# Wie groß ist die Standardabweichung in Relation zum maximalen Wert? 
100 / max(greenhouse_sector$Sum) * sd_sec
  
# g) 

#install.packages("moments")

co2_mean <- mean(greenhouse$Tonnes.Co2e) # 8789635
co2_median <- median(greenhouse$Tonnes.Co2e) # 83249

co2_median < co2_mean
# Da med < mean ist, liegt die Vermutung einer rechtsschiefen bzw. linkssteilen Verteilung nahe. 

library(moments)
sk <- moments::skewness(greenhouse$Tonnes.Co2e) # 32.732
sk > 0
# Da sk > 0 ist, ist die Vermutung einer rechtsschiefen bzw. linkssteilen Verteilung bestätigt. 

# Zur schnellen visuellen Überprüfung: 
hist(greenhouse_laender$Sum, freq = F)
