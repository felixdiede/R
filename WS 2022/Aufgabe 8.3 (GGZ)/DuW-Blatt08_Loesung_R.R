######### ====================== Loesung zum 8.Aufgabenzettel ====================== #########

#### Aufgabe 8.3 ---- 
# a)
library(extraDistr)

n = 10

set.seed(55)

gluehbirne = rnorm(n, mean=8000, sd=100)

pam = rexp(n, rate=1/20)

radunfaelle = rpois(n, lambda=14)

schlafen = rbinom(n, size=150, prob = 0.1)

schweine = rdunif(n, 1, 5)

#b) 
## (Starkes) Gesetz der großen Zahlen 
m_gluehbirne = c()
m_pam = c()
m_rad = c()
m_schlafen = c()
m_schweine = c()

set.seed(55)
for(n in 1:1000){
  m_gluehbirne[n] = mean(rnorm(n, mean = 8000, sd = 100))
  m_pam[n] = mean(rexp(n, rate = 1/20))
  m_rad[n] = mean(rpois(n, lambda = 14))
  m_schlafen[n] = mean(rbinom(n, 150, 0.1))
  m_schweine[n] = mean(rdunif(n, 1, 5))
}

plot(m_gluehbirne, type=('l'), xlab="Versuchswiederholungen", ylab="Mittelwert", main="Glühbirnen")
abline(h=8000, col="red")

plot(m_pam, type=('l'), xlab="Versuchswiederholungen", ylab="Mittelwert", main="Wartezeit beim PAM")
abline(h=20, col="red")

plot(m_rad, type=('l'), xlab="Versuchswiederholungen", ylab="Mittelwert", main="Radfahrer")
abline(h=14, col="red")

plot(m_schlafen, type=('l'), xlab="Versuchswiederholungen", ylab="Mittelwert", main="Schlafende Studenten")
abline(h=15, col="red")

plot(m_schweine, type=('l'), xlab="Versuchswiederholungen", ylab="Mittelwert", main="Schweinewürfeln")
abline(h=3, col="red")

#c) 
# Hauptsatz der Statistik / Glivenko-Cantelli 
par(mfrow = c(1, 3))
set.seed(55)

for(n in c(10, 100, 1000)){
  gluehbirne = rnorm(n, mean = 8000, sd = 100)
  plot(ecdf(gluehbirne),pch=".") 
  curve(pnorm(x, mean = 8000, sd = 100),add=TRUE,col="blue") 
}

for(n in c(10, 1000, 10000)){
  pam = rexp(n, rate = 1/20)
  plot(ecdf(pam),pch=".") 
  curve(pexp(x, rate = 1/20),add=TRUE,col="blue") 
}

for(n in c(10, 1000, 10000)){
  radunfaelle = rpois(n, lambda = 14)
  plot(ecdf(radunfaelle),pch=".") 
  curve(ppois(x, lambda = 14),add=TRUE,col="blue") 
}

for(n in c(10, 1000, 10000)){
  schlafen = rbinom(n, 150, 0.1)
  plot(ecdf(schlafen),pch=".") 
  curve(pbinom(x, 150, 0.1),add=TRUE,col="blue") 
}

for(n in c(10, 1000, 10000)){
  schweine = rdunif(n,1,5)
  plot(ecdf(schweine),pch=".") 
  curve(pdunif(x, 1,5),add=TRUE,col="blue") 
}

# d) 
## Zentraler Grenzwertsatz (statt sum() kann auch mean() verwendet werden)
z_gluehbirne = data.frame()
z_pam = data.frame()
z_rad = data.frame()
z_schlafen = data.frame()
z_schweine = data.frame()

set.seed(55)
for(k in 1:10000){
  versuche = c(1,2,6)
  for(m in 1:3){
    z_gluehbirne[k,m] = sum(rnorm(versuche[m], mean = 8000, sd = 100))
    z_pam[k,m] = sum(rexp(versuche[m], rate = 1/20))
    z_rad[k,m] = sum(rpois(versuche[m], lambda = 14))
    z_schlafen[k,m] = sum(rbinom(versuche[m], 150, 0.1))
    z_schweine[k,m] = sum(rdunif(versuche[m],1,5)) 
  }
}

par(mfrow = c(1, 3))
hist(z_gluehbirne$V1, 
     xlab = "Lebensdauer einer Glühbirne", 
     ylab = "Häufigkeit (nach dem Testen von 10.000 Birnen)", 
     main = "Glühbirne")
hist(z_gluehbirne$V2, 
     xlab = "Summe Lebensdauer zweier Glühbirnen", 
     ylab = "Häufigkeit (nach dem Testen von 10.000 Birnen)", 
     main = "Glühbirne")
hist(z_gluehbirne$V3, 
     xlab = "Summe Lebensdauer von sechs Glühbirnen", 
     ylab = "Häufigkeit (nach dem Testen von 10.000 Birnen)", 
     main = "Glühbirne")

par(mfrow = c(1, 3))
hist(z_pam$V1, 
     xlab = "Wartezeit eines Studenten", 
     ylab = "Häufigkeit (nach dem Messen der Wartezeit von 10.000 Studenten)", 
     main = "Wartezeit beim PAM")
hist(z_pam$V2, 
     xlab = "Summe der Wartezeit zweier Studenten", 
     ylab = "Häufigkeit (nach dem Messen der Wartezeit von 10.000 Studenten)",  
     main = "Wartezeit beim PAM")
hist(z_pam$V3, 
     xlab = "Summe der Wartezeit von sechs Studenten", 
     ylab = "Häufigkeit (nach dem Messen der Wartezeit von 10.000 Studenten)", 
     main = "Wartezeit beim PAM")


par(mfrow = c(1, 3))
hist(z_rad$V1, 
     xlab = "Betrachten einer Woche", 
     ylab = "Häufigkeit (nach 10.000 Wochen)", 
     main = "Radunfälle")
hist(z_rad$V2, 
     xlab = "Betrachten zweier Wochen", 
     ylab = "Häufigkeit (nach 10.000 Wochen)", 
     main = "Radunfälle")
hist(z_rad$V3, 
     xlab = "Betrachten von sechs Wochen", 
     ylab = "Häufigkeit (nach 10.000 Wochen)", 
     main = "Radunfälle")


par(mfrow = c(1, 3))
hist(z_schlafen$V1, 
     xlab = "Schlafende Studierende nach einer Vorlesung", 
     ylab = "Häufigkeit (nach 10.000 Vorlesungen)",
     main = "Schlafende Studierende")
hist(z_schlafen$V2, 
     xlab = "Schlafende Studierende nach zwei Vorlesungen", 
     ylab = "Häufigkeit (nach 10.000 Vorlesungen)", 
     main = "Schlafende Studierende")
hist(z_schlafen$V3, 
     xlab = "Schlafende Studierende nach sechs Vorlesungen", 
     ylab = "Häufigkeit (nach 10.000 Vorlesungen)", 
     main = "Schlafende Studierende")

par(mfrow = c(1, 3))
hist(z_schweine$V1, 
     xlab = "Schweinewurf mit einem Schwein", 
     ylab = "Häufigkeit (nach 10.000 Würfen)", 
     main = "Schweinewurf")

hist(z_schweine$V2, 
     xlab = "Schweinewurf mit zwei Schweinen", 
     ylab = "Häufigkeit (nach 10.000 Würfen)", 
     main = "Schweinewurf")

hist(z_schweine$V3, 
     xlab = "Schweinewurf mit sechs Schweinen", 
     ylab = "Häufigkeit (nach 10.000 Würfen)", 
     main = "Schweinewurf")



