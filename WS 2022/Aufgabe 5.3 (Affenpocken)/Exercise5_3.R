# Aufgabe 5.3 
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2022/Aufgabe 5.3 (Affenpocken)")

moneypox <- read.csv(file = "2022-11-09_monkeypox.csv", header = TRUE, sep = ";")
meta <- read.csv(file = "meta_data.csv", header = TRUE, sep = ";")

summary(moneypox)
summary(meta)
str(moneypox)
str(meta)

# Aufgabe 5.3 b) 

# 1. Welche Länder haben die meisten Fälle von Affenpocken?
moneypox <- moneypox[order(moneypox$Confirmed_Cases, decreasing = TRUE), ]
moneypox$Country[1:3] # Brazil, Spain, France

# 2. Wie viel Prozent der bestätigten Fälle landen im Krankenhaus? 
moneypox$hospitalized_rate <- moneypox$Hospitalized / moneypox$Confirmed_Cases * 100


# 3. Sind die Fallzahlen in den Ländern höher, in denen die vielen Erkrankten vorher gereist sind? 
plot(moneypox$Confirmed_Cases, moneypox$Travel_History_Yes)


# Aufgabe 5.3 c) 
sturges <- round(1+log2(nrow(moneypox))) # 8 -> 1000 Daten pro Klasse

hist(moneypox$Confirmed_Cases, freq = FALSE, breaks = 8)


# Aufgabe 5.3 d) 
moneypox <- merge(x = moneypox, y = meta, by.x = "Country", by.y = "Country")


# Aufgabe 5.3 e) 
moneypox$rel.Confirmed <- moneypox$Confirmed_Cases / moneypox$Population 
moneypox <- moneypox[order(moneypox$rel.Confirmed, decreasing = TRUE), ]

plot(moneypox$Confirmed_Cases)
plot(moneypox$rel.Confirmed)
par(mfrow = c(1:2))
