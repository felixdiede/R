######### ====================== Loesung zum 3.Aufgabenzettel ====================== #########

#### Aufgabe 3.4 ---- 
# a)
marvel = read.table("mcu.csv", header=T, sep=";")

str(marvel)
summary(marvel)

# b) 
marvel$success = rep(0, nrow(marvel))
for(m in 1:nrow(marvel)){ # nolint
  marvel$success[m] = 100/marvel$production_budget[m] * marvel$worldwide_box_office[m]
}

marvel_success = marvel[order(marvel$success, decreasing = T),]
marvel_failure = marvel[order(marvel$success, decreasing = F),]

# c) 

plot(x=marvel$tomato_meter, y=marvel$audience_score, 
     xlab = "Expertenmeinungen", 
     ylab = "Zuschauermeinungen", 
     main = "Kritiken für MCU Filme", 
     xlim = c(0,100), 
     ylim = c(0,100), 
     col = marvel$colour
     )

#### Pareto-Front ----



#### ende ---- 

# d) 

?grepl
filmreihen = c("Iron Man", "Hulk", "Thor", "Captain America", "Avengers", 
               "Guardians of the Galaxy", "Ant-Man", "Doctor Strange", 
               "Spider-Man", "Black Panther", "Captain Marvel", 
               "Black Widow", "Shang-Chi", "Eternals")

beste_filmreihen = function(marvel, filmreihen){
  reihe = c()
  durchschnittsbewertung = c()
  durchschnittseinkommen = c()
  
  for(film in filmreihen){
    mcu_filme = marvel[grepl(film, marvel$movie_title),]
    reihe = c(reihe, film)
    durchschnittsbewertung = c(durchschnittsbewertung, sum(mcu_filme$audience_score) / nrow(mcu_filme) )
    durchschnittseinkommen = c(durchschnittseinkommen, sum(mcu_filme$worldwide_box_office) / nrow(mcu_filme) ) 
  }
  
  resultat = data.frame(
    filmreihe = reihe, 
    durchschnittsbewertung = durchschnittsbewertung, 
    durchschnittseinkommen = durchschnittseinkommen
  )
  
  resultat_bewertung = resultat[order(resultat$durchschnittsbewertung, decreasing = T),]
  resultat_einkommen = resultat[order(resultat$durchschnittseinkommen, decreasing = T),]
  
  return (
    intersect(resultat_bewertung$filmreihe[1:3], resultat_einkommen$filmreihe[1:3]) 
    )
}

ergebnis = beste_filmreihen(marvel, filmreihen)
print(ergebnis)

# e) 
?sample

set.seed(55)

marvel$movie_title[sample(1:27,2, replace=F)]

## Anzahl der Möglichkeiten der Kombination ohne Zurücklegen bestimmen: Binomialkoeffizient 

choose(27,2)

#### ende ---- 

