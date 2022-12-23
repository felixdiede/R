# Daten und Wahrscheinlichkeiten Weihnachtsaufgabe

setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/Weihnachtsaufgabe")

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



plot(energie_timeline$Datum, energie_timeline$Super_E5, type = "b", 
    main = "Entwicklung der Super E5 Preise in Deutschland", 
    xlab = "Monate",
    ylab = "Super E5 Preise [in Cent/ Liter]",
    ylim = c(0,3),
    xaxt = "n")

axis(1, at = energie_timeline$Datum, labels = format(energie_timeline$Datum, "%y-%m-%d"), las = 1)














