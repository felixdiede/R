# Aufgabe a) 
#skijump <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/Klausur/WS21/skijump.csv", header = TRUE, sep = ",")
#summary(skijump)
#str(skijump)

skiresults <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/Klausur/WS21/skiresults.csv", header = TRUE, sep = ",")
summary(skiresults)
str(skiresults)

comps <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/Klausur/WS21/all_comps.csv", header = TRUE, sep = ",")
summary(comps)
str(comps)

# Aufgabe b) 
new_data <- comps[, c("id", "season", "place", "gender")]
skijump <- merge(x = skiresults, y = new_data, by.x = "id", by.y = "id")


# Aufgabe c) 
gender <- unique(skijump$gender)
# Es wird zwischen "Men", "Woman" und "Mix" unterschieden

# Für eine bessere Vergleichbarkeit, wähle gleiche Skalen (ylim)
boxplot(skijump$dist[skijump$gender == "Men"], ylim = c(0, max(skijump$dist)), ylab = "Sprungdistanz der Männer", main = "Boxplot Männer", plot = TRUE)
boxplot(skijump$dist[skijump$gender == "Women"], ylim = c(0, max(skijump$dist)), ylab = "Sprungdistanz der Frauen", main = "Boxplot Frauen", plot = TRUE)
boxplot(skijump$dist[skijump$gender == "Mix"], ylim = c(0, max(skijump$dist)), ylab = "Sprungdistanz von Mix", main = "Boxplot Mix", plot = TRUE)

par(mfrow = c(1, 3))

# Auffälligkeiten: 
# Männer haben extrem viele Ausreißer in Relation zu Frauen und Mix 
# Box (50% der Merkmale) der Männer ist wesentlich größer (damit mehr gestreut), als bei Frauen und Mix -> Distanzen der Männer streuen deutlich mehr 

# Aufgabe d) 
pairs(skijump[c("dist_points", "note_points", "wind_comp", "gate_points", "points")])
# visuell sieht es so aus, als ob die "dist_points" die größte Korrelation mit den "points" haben
# dist_points steigen sehr linear mit den points
# dist_points haben am meisten Anteil an den points
# nächsthöchste Korrelation scheint mit note_points zu sein

# Aufgabe e) 
corr <- function(df) {
    cor_points_gate_points = cor(df$points, df$gate_points)
    cor_points_wind_comp = cor(df$points, df$wind_comp)
    cor_points_note_points = cor(df$points, df$note_points)
    cor_points_dist_points = cor(df$points, df$dist_points)
    
    result = data.frame(Variable_One = c("points", "points", "points", "points"), 
                        Variable_Two = c("gate_points", "wind_comp", "note_points", "dist_points"), 
                        Korrelationen = c(cor_points_gate_points, cor_points_wind_comp, cor_points_note_points, cor_points_dist_points))

    return(result)
}

korrelationen <- corr(skijump)
# BP: Daten sind quantitativ und kardinal; Zusammenhang muss linear sein


# Aufgabe f)
qualifikation <- skijump[skijump$round == "qualification", ]
final <- skijump[skijump$round == "final round", ]

install.packages("ineq")
library(ineq)

dev.off()

plot(Lc(qualifikation$points))
lines(Lc(final$points))
# Die Lorenzkurve der finalen Runde liegt näher an der Diagonalen
# Die Ungleichheit der finalen Runden ist somit geringer als im Qualifying