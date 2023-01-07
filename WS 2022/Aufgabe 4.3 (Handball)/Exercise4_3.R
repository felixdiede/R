######### ====================== Loesung zum 4.Aufgabenzettel ====================== #########

#### Aufgabe 4.3 ---- 
# a)
handball = read.table("handball.csv", header=T, sep=";")
summary(handball)

# b) 

## 1. 
saisons = unique(handball$saison)

## 2. 

for(s in saisons){
  relevante_daten = handball[handball$saison == s,]
  spieltage = max(unique(relevante_daten$spieltag)) 
  print(paste("Saison:", paste(s, (paste("Spieltage:", spieltage, sep = " ")), sep = ", "), sep = " "))
}

## 3. 

n = nrow(handball)
zaehler = 0 

for(i in 1:n){
  if(handball$tore_mannschaft1[i] == handball$tore_mannschaft2[i]){
    zaehler = zaehler + 1
  }
}

ws = 100 / n * zaehler 

print(paste("Die empirische Wahrscheinlichkeit, dass ein Spiel unentschieden ausgeht, beträgt", paste(round(ws, 2), "%.", sep = ""), sep = " "))

## 4. 

tore_pro_spieltag = c()

for(i in 1:38){
  relevante_daten = handball[handball$spieltag == i,]
  n_rel = nrow(relevante_daten)
  tore_pro_spieltag = c(tore_pro_spieltag, sum(relevante_daten$tore_mannschaft1 + relevante_daten$tore_mannschaft2)/n_rel)
}

barplot(tore_pro_spieltag, 
        main = "Anzahl Tore pro Spieltag", 
        xlab = "Spieltag", 
        ylab = "Durchschnittliche Anzahl Tore", 
        names.arg = 1:38, 
        col = "white", 
        ylim = c(0,60))

# c) 

handball$tore = handball$tore_mannschaft1 + handball$tore_mannschaft2

# d) 


ws_t = c()
tore_sort = sort(unique(handball$tore))
for(t in 1:length(tore_sort)){ # nolint
  relevante_daten = handball[handball$tore == tore_sort[t],]
  ws_t[t] = nrow(relevante_daten)/nrow(handball) 
}

barplot(ws_t, 
        main = "Anzahl Tore pro Spiel", 
        xlab = "Tore", 
        ylab = "Wahrscheinlichkeit", 
        names.arg = tore_sort, 
        col = c("#e4685a"))

legend("topright", legend=c("Empirisch"), fill = c("#e4685a"))

# e) 

p_t = dpois(tore_sort, 55)

data = data.frame(empirisch = ws_t, 
                  poisson = dpois(tore_sort, 55)) 

barplot(t(as.matrix(data)),
        beside=TRUE, 
        main = "Anzahl Tore pro Spiel", 
        xlab = "Tore", 
        ylab = "Wahrscheinlichkeit", 
        names.arg = tore_sort, 
        col = c("#e4685a", "#847a7d"))

legend("topright", legend=c("Empirisch", "Theoretisch"), fill = c("#e4685a", "#847a7d"))

#f) 

plot(ppois(tore_sort, 55), 
     type = "s", 
     main = "Verteilungsfunktion der Poissonverteilung mit Lambda = 55",
     xlab = "Tore", 
     ylab = "F(x)"
     )

weniger_als_54 = ppois(54, 55)
mehr_als_44 = 1 - ppois(44, 55)

zwischen_44_54 = mehr_als_44 - weniger_als_54


#g) 

set.seed(1)
gesamtzahl_tore = rpois(9, lambda = 55)

#### ende ---- 

