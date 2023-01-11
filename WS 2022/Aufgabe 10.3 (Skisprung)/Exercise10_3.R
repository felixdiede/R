# Aufgabe 10.3 
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2022/Aufgabe 10.3 (Skisprung)")

# Aufgabe 10.3 a) 
skiresults <- read.csv(file = "skiresults.csv", header = TRUE, sep = ",")
meta <- read.csv(file = "all_comps.csv", header = TRUE, sep = ",")

# Aufgabe 10.3 b) 
meta_extraction <- meta[, c(1, 8, 10, 11)]

skijump <- merge(x = skiresults, y = meta_extraction, by.x = "id", by.y = "id")


# Aufgabe 10.3 c) 

# 1. An welchen Orten finden die Wettbewerbe statt? 
orte <- unique(skijump$place)


# 2. Welcher Mann und welche Frau ist in 2021 absolute am weitesten gesprunge? 
skijump_men <- skijump[skijump$gender == "Men", ]
skijump_men <- skijump_men[order(skijump_men$dist, decreasing = TRUE), ]
View(skijump_men$name[1]) # Pavlovcic, Bor

skijump_women <- skijump[skijump$gender == "Women", ]
skijump_women <- skijump_women[order(skijump_women$dist, decreasing = TRUE), ]
View(skijump_women$name[1]) # Kramer, Martina


# 3. Welcher Mann und welche Frau ist in 2021 durchschnittlich am weitesten gesprungen? 
skijump_men_agg <- aggregate(skijump_men$dist, by = list(skijump_men$name), FUN = mean)
skijump_men_agg <- skijump_men_agg[order(skijump_men_agg$x, decreasing = TRUE), ]
View(skijump_men_agg$Group.1[1]) # Prevc, Domen


skijump_women_agg <- aggregate(skijump_women$dist, by = list(skijump_women$name), FUN = mean)
skijump_women_agg <- skijump_women_agg[order(skijump_women_agg$x, decreasing = TRUE), ]
View(skijump_women_agg$Group.1[1]) # Wuerth, Svenja

# Aufgabe 10.3 d) 
View(unique(skijump$gender))
# Men, Woman, Mix

skijump_mix <- skijump[skijump$gender == "Mix", ]

par(mfrow = c(1,3))
boxplot(skijump_men$dist, main = "Boxplot Men")
boxplot(skijump_women$dist, main = "Boxplot Women")
boxplot(skijump_mix$dist, main = "Boxplot Mix")


# Aufgabe 10.3 e) 
pairs(skijump[, c(6, 7, 9, 10)])

# Aufgabe 10.3 f) 
install.packages("ineq")
library(ineq)

skijump_quali <- skijump[skijump$round == "qualification", ]
skijump_final <- skijump[skijump$round == "final round", ]

quali <- Lc(skijump_quali$points, plot = TRUE)
final <- Lc(skijump_final$points, plot = TRUE)
