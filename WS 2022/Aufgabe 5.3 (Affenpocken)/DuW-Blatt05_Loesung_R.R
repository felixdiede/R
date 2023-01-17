######### ====================== Loesung zum 5.Aufgabenzettel ====================== #########

#### Aufgabe 4.3 ---- 
# a)

affenpocken = read.table("2022-11-09_monkeypox.csv", header=T, sep=";")
meta = read.table("meta_data_monkeypox.csv", sep =";", header=T)


# b) 

## 1) 
betroffene_laender = affenpocken[order(affenpocken$Confirmed_Cases, decreasing = T),1]
print(betroffene_laender[1:10])

## 2) 

affenpocken$percentage_hospitalized = rep(0, nrow(affenpocken))

for(i in 1:nrow(affenpocken)){ # nolint
  if(affenpocken$Confirmed_Cases[i] > 0 && affenpocken$Hospitalized[i] > 0){
    affenpocken$percentage_hospitalized[i] = 100 / affenpocken$Confirmed_Cases[i] * affenpocken$Hospitalized[i]
  }
  else{
    affenpocken$percentage_hospitalized[i] = 0 
  }
}

summary(affenpocken$percentage_hospitalized)
affenpocken[affenpocken$percentage_hospitalized>0,1]

## 3) 

plot(x = affenpocken$Confirmed_Cases, y = affenpocken$Travel_History_Yes, 
     main = "Zusammenhang zwischen bestätigten Fallzahlen und Reisetätigkeit", 
     xlab = "Bestätigte Fallzahlen", 
     ylab = "Anzahl der verreisten Patienten")

# c) 

sturges <- round(1+log2(nrow(affenpocken)), 0) # 8
rice <- round(2*(nrow(affenpocken)^(1/3)), 0) # 9
quadrat <- round(sqrt(nrow(affenpocken)), 0) # 10
summary(affenpocken$Confirmed_Cases) 

hist(affenpocken$Confirmed_Cases, 
     freq = TRUE, #probability = TRUE, 
     breaks = sturges,
     xlab = "Bestätigte Fallzahlen", 
     main = "Histogramm der bestätigten Affenpocken Fallzahlen") 

# d) 

all(affenpocken$Country == meta$Country)

meta = meta[meta$Country %in% affenpocken$Country,]

am = merge(affenpocken, meta, by = "Country")


# e) 

affenpocken_sorted = affenpocken[order(affenpocken$Confirmed_Cases, decreasing = T),]

plot(affenpocken_sorted$Confirmed_Cases[1:10], 
     xaxt="n",
     xlab = "Land", 
     ylab = "Relative Bestätigte Fallzahlen", 
     type = "p", pch = 16)

axis(1,at = 1:10, labels=affenpocken_sorted$Country[1:10])


am$Rel_Confirmed = am$Confirmed_Cases/am$Population
am_rel = am[order(am$Rel_Confirmed, decreasing = T),]

plot(am_rel$Rel_Confirmed[1:10], 
     xaxt="n",
     xlab = "Land", 
     ylab = "Relative Bestätigte Fallzahlen", 
     type = "p", pch = 16)

axis(1,at = 1:10, labels=am_rel$Country[1:10])

