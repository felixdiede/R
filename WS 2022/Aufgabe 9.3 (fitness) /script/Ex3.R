# Aufgabe 9.3 (fitness)
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2022/Aufgabe 9.3 (fitness) /")

# Aufgabe 9.3 a) 
fitness <- read.csv(file = "data/fitness_data.csv", header = TRUE, sep = ",")

# Aufgabe 9.3 b) 

# 1 Wieviele Teilnehmer sind im Datensatz enthalten? 
fitness_number_of_participants <- length(unique(fitness$Id))
fitness_participants <- list(unique(fitness$Id))

# 2 Wer ist der aktivste Teilnehmer in Bezug auf Schrittzahl, Distanz, aktive Minuten und verbrannte Kalorien? 

