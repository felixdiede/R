# Aufgabe 9.3 (fitness)
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2022/Aufgabe 9.3 (fitness) /")

# Aufgabe 9.3 a) 
fitness <- read.csv(file = "data/fitness_data.csv", header = TRUE, sep = ",")

# Aufgabe 9.3 b) 

# 1 Wieviele Teilnehmer sind im Datensatz enthalten? 
fitness_number_of_participants <- length(unique(fitness$Id))
fitness_participants <- list(unique(fitness$Id))

# 2 Wer ist der aktivste Teilnehmer in Bezug auf Schrittzahl, Distanz, aktive Minuten und verbrannte Kalorien? 
fitness_filter_df <- fitness[order(fitness$TotalSteps, fitness$TotalDistance, fitness$TotalActiveMinutes, fitness$Calories, decreasing = TRUE), ]
fitness_most_active_participant <- fitness_filter_df$Id[1]

# 3 Über welchen Zeitraum wurden die Daten gesammelt? 
fitness_max_date <- as.Date(max(fitness$ActivityDate))
fitness_min_date <- as.Date(min(fitness$ActivityDate))

fitness_period <- fitness_max_date - fitness_min_date # 146 Tage 

# 4 Welcher Tag war der durchschnittlich sportlichste in Bezug auf die aktiven Minuten? 
fitness_aggregate <- aggregate(fitness$Id, by = list(unique(fitness$ActivityDate)), FUN = "sum")


# 5 Wie lange ist der durchschnittliche Schritt der Teilnehmer? 
fitness_average_step <- mean(fitness$TotalSteps)

# Aufgabe 9.3 c) 
fitness_histogramm_steps <- hist(fitness$TotalSteps, 
                                xlab = "Anzahl der Schritte", 
                                ylab = "Häufigkeit der Schritte")

fitness_boxplot_steps <- boxplot(fitness$TotalSteps)

par(mfrow = c(1,2))

fitness_histogramm_distance <- hist(fitness$TotalDistance, 
                                xlab = "Distanzen", 
                                ylab = "Häufigkeit der Distanzen")

fitness_boxplot_distance <- boxplot(fitness$TotalDistance)

par(mfrow = c(1,2))


#....

# Aufgabe 9.3 d) 
install.packages("ineq")
library(ineq)

Lc(fitness$TotalSteps, plot = TRUE) 
Lc(fitness$TotalDistance, plot = TRUE) 
Lc(fitness$TotalActiveMinutes, plot = TRUE) 
Lc(fitness$SedentaryMinutes, plot = TRUE) 
Lc(fitness$Calories, plot = TRUE) 
par(mfrow = c(1,7))


concentrations <- data.frame(concentrations = c(conc(fitness$TotalSteps), 
                    conc(fitness$TotalDistance), 
                    conc(fitness$TotalActiveMinutes), 
                    conc(fitness$SedentaryMinutes), 
                    conc(fitness$Calories)), variable = c("TotalSteps", "TotalDistance", "TotalActiveMinutes", "SedentaryMinutes", "Calories"))

concentrations <- concentrations[order(concentrations$concentrations, decreasing = TRUE), ]
concentration_max <- concentrations$variable[1] # TotalDistance hat größte Ungleichheit 
concentration_min <- concentrations$variable[5] # SedentaryMinutes hat die geringste Ungleichheit 
