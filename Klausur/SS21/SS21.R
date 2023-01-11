# Klausuraufgabe 4 aus SS21

# Aufgabe 4 a) 
fussball <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/Klausur/SS21/fussball.csv", header = TRUE, sep = ";", dec = ",")
str(fussball)
summary(fussball)


# Aufgabe 4 b) 

# Trefferquote der Torschüsse -> Torschuesse / Tore
fussball$Trefferquote <- fussball$Tore / fussball$Torschuesse


# Erfolgsquote der Zweikämpfe -> gewonnen zweikämpfe / gewonnene + verlorene 
fussball$Erfolgsquote_Zweikämpfe <- fussball$ZweikaempfeGewonnen / (fussball$ZweikaempfeGewonnen + fussball$ZweikaempfeVerloren)


# Erfolgsquote der Pässe -> PaesseErfolg / (PaesseErflog + Fehlpaesse) 
fussball$Erfolgsquote_Pässe <- fussball$PaesseErfolg / (fussball$PaesseErfolg + fussball$Fehlpaesse) 


# Aufgabe 4 c) 
fussball <- fussball[, -c(2, 5)]

rangkorrelation <- function(df) {
    cor(df, method = "spearman")
}

korrelation_matrix <- rangkorrelation(fussball)





# Aufgabe 4 d) 
#