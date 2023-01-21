# Aufgabe 11.3 

# a) 
fussball <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2022/Aufgabe 11.3 (Fussball)/fussball.csv", header = TRUE, sep = ";")

# b) 
fussball$Trefferquote <- fussball$Tore / fussball$Torschuesse * 100
fussball$Erfolgsquote_Zweikämpfe <- fussball$ZweikaempfeGewonnen / (fussball$ZweikaempfeGewonnen + fussball$ZweikaempfeVerloren) * 100
fussball$Erfogsquote_Pässe <- fussball$PaesseErfolg / (fussball$PaesseErfolg + fussball$Fehlpaesse) * 100


# c)
fussball <- fussball[,-(2:5)]
fussball$Ballkontakte.pro.Spiel <- as.integer(sub(",", ".", fussball$Ballkontakte.pro.Spiel))
fussball$KmProSpiel <- as.integer(sub(",", ".", fussball$KmProSpiel))
fussball$Durchschnittsalter <- as.integer(sub(",", ".", fussball$Durchschnittsalter))

calculate_correlation <- function(df) {
    names <- colnames(df)
    correlation_df <- data.frame(names)

    for(i in 2:length(names)) {
        correlation_df[i, 2] <- cor(df$Tabellenplatz, df[, i], method = "spearman")
    }

    return(correlation_df)
}

correlation_df <- calculate_correlation(fussball)


# d) 
# einige der Variablen sind nicht kardinal, sodass der Rangkorrelationskoeffizient genutzt werden muss um die nichtlinearen Zusammenhänge zu berechnen

# e) 
# Function to copy values greater than 0.7 and variable names


# f) 
pairs(correlation_df)
