# Aufgabe 7.3 (greenhouse)
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2022/Aufgabe 7 (greenhouse)")

# Aufgabe 7.3 a) 
greenhouse <- read.csv(file = "data/greenhouse.csv", header = TRUE, sep = ";")

# Aufgabe 7.3 b) 
berechne_summe <- function(df, column) {
    
    elemente <- unique(df[, which(colnames(df) == column)])
    element = c()
    summe = c()

    for(i in 1:length(elemente)) {
        element[i] <- elemente[i]
        summe[i] <- sum(df[which(df[, which(colnames(df) == column)] == elemente[i]), which(colnames(df) == "Tonnes.Co2e")])
    }


    df_cum <- data.frame(
        Element = element, 
        Sum = summe)
    
    return(df_cum)
}

# Aufgabe 7.3 c) 

# Aufsummieren über Sub-Sektoren
greenhouse_subsector = berechne_summe(greenhouse, "Subsector")
greenhouse_subsector = greenhouse_subsector[order(greenhouse_subsector$Sum, decreasing = TRUE), ]

# Aufsummieren über Sektoren
greenhouse_sector = berechne_summe(greenhouse, "Sector")
greenhouse_sector = greenhouse_sector[order(greenhouse_sector$Sum, decreasing = TRUE), ]

# Aufsummieren über Länder 
greenhouse_country = berechne_summe(greenhouse, "Country")
greenhouse_country = greenhouse_country[order(greenhouse_country$Sum, decreasing = TRUE), ]

aggregate(greenhouse$Tonnes.Co2e, list(greenhouse$Country), FUN = "sum")


# Aufgabe 7.3 d) 
meta <- read.csv(file = "data/meta_data.csv", header = TRUE, sep = ";")

meta_greenhouse = meta[meta$Country %in% greenhouse$Country,]
greenhouse$Rel.Tonnes.Co2e = rep(0, nrow(greenhouse))

for(i in 1:nrow(greenhouse)){
  country = greenhouse$Country[i]
  bevoelkerung = meta[which(meta$Country == country), which(colnames(meta) == "Population")]
  greenhouse$Rel.Tonnes.Co2e[i] = greenhouse$Tonnes.Co2e[i] / bevoelkerung
}

# Aufgabe 7.3 e) 
berechne_rel_summe <- function(df, column) {
    
    elemente <- unique(df[, which(colnames(df) == column)])
    element = c()
    summe = c()

    for(i in 1:length(elemente)) {
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

# Aufgabe 7.3 f) 
var_sec = var(greenhouse_sector$Sum)
sd_sec = sd(greenhouse_sector$Sum)

co2_mean <- mean(greenhouse$Tonnes.Co2e)
co2_median <- median(greenhouse$Tonnes.Co2e)

install.packages("moments")
library(moments)

sk <-moments::skewness(greenhouse$Tonnes.Co2e)
# sk > 0 -> rechtsschief bzw linkssteil

hist(greenhouse_country$Sum, freq = F)

