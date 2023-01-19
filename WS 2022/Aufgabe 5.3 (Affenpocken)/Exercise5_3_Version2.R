# Aufgabe 5.3 
monkeypox <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2022/Aufgabe 5.3 (Affenpocken)/2022-11-09_monkeypox.csv", header = TRUE, sep = ";")
meta <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2022/Aufgabe 5.3 (Affenpocken)/meta_data.csv", header = TRUE, sep = ";")

# Aufgabe b) 

# 1. 
monkeypox_most_cases <- monkeypox[order(monkeypox$Confirmed_Cases, decreasing = TRUE), ]
head(monkeypox_most_cases)


# 2. 
monkeypox$hospitalized_rate <- monkeypox$Hospitalized / monkeypox$Confirmed_Cases * 100


# 3. 
plot(monkeypox$Confirmed_Cases, monkeypox$Confirmed_Cases, 
    main = "Fallzahlen in Relation zu Reiseaktivitäten", 
    xlab = "Fallzahlen", 
    ylab = "Reiseaktivität")


# c) 
sturges <- round(1+log2(nrow(monkeypox)), 0) # 8
rice <- round(2*(nrow(monkeypox)^(1/3)), 0) # 9
quadrat <- round(sqrt(nrow(monkeypox)), 0) # 10

hist(monkeypox$Confirmed_Cases, freq = FALSE, prob = TRUE, 
    breaks = sturges, 
    main = "Fallzahlen von Affenpocken [Sturges]", 
    xlab = "absolute Fallzahlen", 
    ylab = "empirische Dichte der Fallzahlen")


# d) 
monkeypox <- merge(x = monkeypox, y = meta, by.x = "Country", by.y = "Country")


# e) 
monkeypox$rel.Cases <- monkeypox$Confirmed_Cases / monkeypox$Population

plot(monkeypox$Confirmed_Cases, main = "absolute Fallzahlen")
plot(monkeypox$rel.Cases, main = "relative Fallzahlen")
