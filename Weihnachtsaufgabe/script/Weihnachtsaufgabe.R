# Daten und Wahrscheinlichkeiten Weihnachtsaufgabe

setwd("/Users/felixdiederichs/R/Weihnachtsaufgabe")

# Aufgabe: stelle die Daten schlimmer da als die Realität

# Teil 1: einlesen der Daten
####################################################################################################################################################################################################
energie_timeline <- read.csv(file = "data/energie_timeline.csv", header = TRUE, sep = ";")

haushalte <- read.csv(file = "data/haushalte.csv", header = TRUE, sep = ";")

# Teil 2: Bereinigung und Vorbereitung der Daten
####################################################################################################################################################################################################

# Um die Datumseingaben zu plotten, müssen diese von der Klasse "char" in die Klasse "date" umgewandelt werden
energie_timeline$Datum <- as.Date(energie_timeline$Datum, format = "%d.%m.%y")

# Um die Daten zu plotte, müssen diese von der Klasse "char" in die Klasse "numeric" umgewandelt werden und die Kommaschreibweise in eine Punktschreibweise konvertiert werden
energie_timeline$Super_E5 <- as.numeric(gsub(",", ".", energie_timeline$Super_E5))
energie_timeline$Super_E10 <- as.numeric(gsub(",", ".", energie_timeline$Super_E10))
energie_timeline$Diesel <- as.numeric(gsub(",", ".", energie_timeline$Diesel))
energie_timeline$Verbraucherpreise <- as.numeric(gsub(",", ".", energie_timeline$Verbraucherpreise))
energie_timeline$Einfuhrpreis_Erdgas <- as.numeric(gsub(",", ".", energie_timeline$Einfuhrpreis_Erdgas))
energie_timeline$Erzeugerpreis_Leichtes_Heizöl <- as.numeric(gsub(",", ".", energie_timeline$Erzeugerpreis_Leichtes_Heizöl))
energie_timeline$VerbraucherpreisErdgas <- as.numeric(gsub(",", ".", energie_timeline$VerbraucherpreisErdgas))
energie_timeline$Strompreis <- as.numeric(gsub(",", ".", energie_timeline$Strompreis))
energie_timeline$Erdgasspeicherstand <- as.numeric(gsub(",", ".", energie_timeline$Erdgasspeicherstand))



par(mfrow = c(3,2))
# Teil 3: Plotte die Preisentwicklungen der Kraftstoffe
####################################################################################################################################################################################################

# erstelle den Plot
plot(energie_timeline$Datum, energie_timeline$Super_E5, type = "b", 
    main = "Entwicklung der Kraftstoffpreise in Deutschland", 
    xlab = "Monate",
    ylab = "Preis / Liter [Cent]",
    ylim = c(1,3),
    xaxt = "n",
    col = "red")

# bearbeite die x-Achse, sodass die monatliche Zeitspanne angegeben wird
axis(1, at = energie_timeline$Datum, labels = format(energie_timeline$Datum, "%y-%m-%d"), las = 1)

# füge die Graphen für Super E10 und Diesel hinzu
points(energie_timeline$Datum, energie_timeline$Super_E10, type = "b", col = "green")
points(energie_timeline$Datum, energie_timeline$Diesel, type = "b", col = "blue")

# erstelle eine Legende
legend("topleft", legend = c("Super E5", "Super E10", "Diesel"), fill = c("red", "green", "blue"))


# Teil 4: Plotte die Preisentwicklung der Verbraucherpreise
####################################################################################################################################################################################################

# erstelle den Plot
plot(energie_timeline$Datum, energie_timeline$Verbraucherpreise, type = "b", 
    main = "Preisentwicklung der Verbraucherpreise in Deutschland", 
    xlab = "Monate", 
    ylab = "Verbraucherpreis", 
    ylim = c(90, 200), 
    xaxt = "n", 
    col = "red"
    )

points(energie_timeline$Datum, energie_timeline$VerbraucherpreisErdgas, type = "b", col = "blue")

# bearbeite die x-Achse, sodass die monatliche Zeitspanne angegeben wird
axis(1, at = energie_timeline$Datum, labels = format(energie_timeline$Datum, "%y-%m-%d"), las = 1)

# füge eine Legende hinzu 
legend("topleft", c("Verbrauchepreise", "Verbraucherpreise für Erdgas"), fill = c("red", "blue"))


# Teil 5: Plotte die Einfuhrpreise für Erdgas 
####################################################################################################################################################################################################
plot(energie_timeline$Datum, energie_timeline$Einfuhrpreis_Erdgas, type = "b", 
    main = "Preisentwicklung der Einfuhrpreise für Erdgas", 
    xlab = "Monate", 
    ylab = "Einfuhrpreise für Erdgas", 
    ylim = c(80, 675), 
    xaxt = "n", 
    col = "red")

# bearbeite die x-Achse, sodass das die monatliche Zeitspanne angegeben wird
axis(1, at = energie_timeline$Datum, labels = format(energie_timeline$Datum, "%y-%m-%d"), las = 1)

# füge eine Legende hinzu
legend("topleft", c("Einfuhrpreise für Erdgas"), fill = c("red"))


# Teil 6: Plotte die Erzeugungskosten für Heizöl 
####################################################################################################################################################################################################
plot(energie_timeline$Datum, energie_timeline$Erzeugerpreis_Leichtes_Heizöl, type = "b",
    main = "Entwicklung der Erzeugungskosten für Heizöl", 
    xlab = "Monate", 
    ylab = "Erzeugungskosten für Heizöl", 
    ylim = c(90, 300), 
    xaxt = "n", 
    col = "red")

# bearbeite die x-Achse, sodass die monatliche Zeitspanne angegeben wird
axis(1, at = energie_timeline$Datum, labels = format(energie_timeline$Datum, "%y-%m-%d"), las = 1)

# füge eine Legende hinzu 
legend("topleft", c("Erzeugungskosten für Heizöl"), fill = c("red"))


# Teil 7: Plotte die Strompreise 
####################################################################################################################################################################################################
plot(energie_timeline$Datum, energie_timeline$Strompreis, type = "b", 
    main = "Entwicklung der Strompreise", 
    xlab = "Monate", 
    ylab = "Strompreise", 
    ylim = c(0.2, 0.6), 
    xaxt = "n", 
    col = "red")

# bearbeite die x-Achse, sodass die monatliche Zeitspanne angegeben wird
axis(1, at = energie_timeline$Datum, labels = format(energie_timeline$Datum, "%y-%m-%d"), las = 1)

# füge eine Legende hinzu 
legend("topleft", c("Strompreise"), fill = c("red"))

# Teil 8: Plotte die Entwicklung des Erdgasspeicherstandes
####################################################################################################################################################################################################
plot(energie_timeline$Datum, energie_timeline$Erdgasspeicherstand, type = "b", 
    main = "Entwicklung des Erdgasspeichersstandes", 
    xlab = "Monate", 
    ylab = "Erdgasspeicherstand [Prozent]", 
    ylim = c(0, 100), 
    xaxt = "n", 
    col = "red")

# bearbeite die x-Achse, sodass die monatliche Zeitspanne angegeben wird
axis(1, at = energie_timeline$Datum, labels = format(energie_timeline$Datum, "%y-%m-%d"), las = 1)

# füge eine Legende hinzu 
legend("topleft", c("Erdgasspeicherstand"), fill = c("red"))

















