# Aufgabe 7.3 (greenhouse) 
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2022/Aufgabe 7.3 (greenhouse)")


# Aufgabe 7.3 b)

berechne.summe <- function(df, column) {
    elemente <- unique(df[, colnames(df) == column])
    element <- c()
    summe <- c()

    for(i in 1:length(elemente)) { # nolint
        element[i] <- elemente[i]
        summe[i] <- sum(df[which(df[, which(colnames(df) == column)] == elemente[i]), which(colnames(df) == "Tonnes.Co2e")])
    }

    df_cum <- data.frame(
        Element = element, 
        Sum = summe
    )

    return(df_cum)
}

# Aufgabe 7.3 c) 
greenhouse_sector <- berechne.summe(greenhouse, "Sector")
greenhouse_sector <- greenhouse_sector[order(greenhouse_sector$Sum, decreasing = TRUE), ]

greenhouse_subsector <- berechne.summe(greenhouse, "Subsector")
greenhouse_subsector <- greenhouse_subsector[order(greenhouse_subsector$Sum, decreasing = TRUE), ]

greenhouse_country <- berechne.summe(greenhouse, "Country")
greenhouse_country <- greenhouse_country[order(greenhouse_country$Sum, decreasing = TRUE), ]

# Aufgabe 7.3 d) 
meta <- read.csv(file = "data/meta_data.csv", header = TRUE, sep = ";")
for(i in 1:nrow(greenhouse)) { # nolint
    country = greenhouse$Country[i]
    bevoelkerung = meta[which(meta$Country == country), which(colnames(meta) == "Population")]
    greenhouse$Rel.Tonnes.Co2e[i] = greenhouse$Tonnes.Co2e / bevoelkerung
}


berechne_rel_summe <- function(df, column) {
    elemente <- unique(df[, colnames(df) == column])
    element <- c()
    summe <- c()

    for(i in 1:length(elemente)) { # nolint
        element[i] <- elemente[i]
        summe[i] <- sum(df[which(df[, which(colnames(df) == column)] == elemente[i]), which(colnames(df) == "Rel.Tonnes.Co2e")])
    }

}

greenhouse_subsector_rel = berechne_rel_summe(greenhouse, "Subsector")
greenhouse_subsector_rel = greenhouse_subsector_rel[order(greenhouse_subsector_rel$Sum, decreasing = T),]
print(head(greenhouse_subsector_rel))

# Aufsummieren über Sektoren 
greenhouse_sector_rel = berechne_rel_summe(greenhouse, "Sector")
greenhouse_sector_rel = greenhouse_sector_rel[order(greenhouse_sector_rel$Sum, decreasing = T),]
print(head(greenhouse_sector_rel))
