######### ====================== Loesung zum 1. Aufgabenzettel ====================== #########

#### Aufgabe a) ---- 

salary <- read.table(file = "data_science_salaries.csv",header = TRUE, 
                     sep = ",", dec = ".")

#### Aufgabe b) ----
# Anschauen des Datensatzes: 
str(salary)
summary(salary)

# Welches Skalenniveau gibt es?  
# work_year: kardinal, diskret 
# experience_level: ordinal 
# job_title: nominal
# salary_in_usd: kardinal, diskret
# employee_residence: nominal
# remote_ratio: nominal
# company_location: nominal
# company_size: ordinal

#### ende ---- 

#### Aufgabe c) ---- 

# Wechselkurz (26.10.2022): 1 USD = EUR 1,0031 Euro
salary$salary_in_euro = round(salary$salary_in_usd * 1.0031, 0)

# Alternativer Lösungsweg: 
salary_in_euro= round(salary$salary_in_usd * 1.0031, 0)
salary = cbind(salary, salary_in_euro)

#### ende ---- 

options(scipen=999)

#### Aufgabe d) ---- 
sturges <- round(1+log2(nrow(salary)), 0) # 10
rice <- round(2*(nrow(salary)^(1/3)), 0) # 17
quadrat <- round(sqrt(nrow(salary)), 0) # 25
summary(salary$salary_in_euro) 

# Hier wurde die Klassengroesse einheitlich gewaehlt, d.h. die Klassen wurden in gleichgrosse Intervalle zerlegt.
hist(salary$salary_in_euro, 
     freq = FALSE, probability = TRUE, 
     breaks = sturges,
     xlab = "Gehalt in Euro", 
     main = "Histogramm der Gehälter von Data Scientists") 

hist(salary$salary_in_euro, 
     freq = FALSE, probability = TRUE, 
     breaks = rice,
     xlab = "Gehalt in Euro", 
     main = "Histogramm der Gehälter von Data Scientists") 

hist(salary$salary_in_euro,
     freq = FALSE, probability = TRUE, 
     breaks = quadrat,
     xlab = "Gehalt in Euro", 
     main = "Histogramm der Gehälter von Data Scientists") 


# Etwas differenziertere Betrachtung der Klassen durch ungleichmäßige Intervallgrenzen:  
hist(salary$salary_in_euro, 
     freq = FALSE, probability = TRUE, 
     breaks = c(0,30000, 60000, 90000, 120000, 150000, 180000, 210000, 240000, 270000, 700000),
     xlab = "Gehalt in Euro", 
     main = "Histogramm der Gehälter von Data Scientists") 

# Wissenschaftliche Schreibweise mit e^i vermeiden: options(scipen=9999)

#### ende ---- 


#### Aufgabe e) ---- 

# 1. 
# Heraussuchen der einzelnen Länder 
einzelne_laender = unique(salary$employee_residence)

laender = c()
durchschnitts_gehalt = c()

# Filtern des Datensatzes nach und nach für jedes Land 
for(e in einzelne_laender){
  salary_relevant = salary[salary$employee_residence == e,]
  laender = c(laender, e)
  # Durchschnitt = Summe / Anzahl, deswgeen hier beides bestimmen 
  summe = sum(salary_relevant$salary_in_euro)
  anzahl = nrow(salary_relevant)
  durchschnitts_gehalt = c(durchschnitts_gehalt, summe/anzahl)
}

mean(salary$salary_in_euro)

beste_wohnorte = data.frame(land = laender, gehalt = durchschnitts_gehalt)

# Sortieren des Data Frames absteigend nach Gehalt ! 
beste_wohnorte = beste_wohnorte[order(beste_wohnorte$gehalt, decreasing = TRUE),]
beste_wohnorte = beste_wohnorte[1:10,]

?barplot

wohnorte = barplot(beste_wohnorte$gehalt, names = beste_wohnorte$land, 
                   xlab="Land", ylab="Durchschnittsgehalt", 
                   ylim = c(0, 200000))

# 2. 

# Analog zu 1., nur mit "company_location"
einzelne_laender = unique(salary$company_location)

laender = c()
durchschnitts_gehalt = c()

for(e in einzelne_laender){
  salary_relevant = salary[salary$company_location == e,]
  laender = c(laender, e)
  summe = sum(salary_relevant$salary_in_euro)
  anzahl = nrow(salary_relevant)
  durchschnitts_gehalt = c(durchschnitts_gehalt, summe/anzahl)
}

bester_arbeitsort = data.frame(land = laender, gehalt = durchschnitts_gehalt)

bester_arbeitsort = bester_arbeitsort[order(bester_arbeitsort$gehalt, decreasing = TRUE),]
bester_arbeitsort = bester_arbeitsort[1:10,]

?barplot

arbeitsort = barplot(bester_arbeitsort$gehalt, names = bester_arbeitsort$land, 
                   xlab="Land", ylab="Durchschnittsgehalt", 
                   ylim = c(0, 200000)
)

# 3. 
?ecdf 

# Verteilungsfunktion zeichnen 
plot(ecdf(salary$salary_in_eu), xlab = "Gehalt", xlim = c(0, 600000), main = "Verteilungsfunktion")
abline(v=100000, col="blue")
abline(h=0.485, col="blue")


# 4. 
# Wieder filtern des Datensatzes nach den Arbeitsmodi, danach kann das in einem Barplot dargestellt werden 
full_remote = salary[salary$remote_ratio == 100,]
gehalt_full_remote = sum(full_remote$salary_in_euro) / nrow(full_remote)

half_remote = salary[salary$remote_ratio == 50,]
gehalt_half_remote = sum(half_remote$salary_in_euro) / nrow(half_remote)

full_office = salary[salary$remote_ratio == 0,]
gehalt_full_office = sum(full_office$salary_in_euro) / nrow(full_office)

df = data.frame(gehalt = c(gehalt_full_remote, gehalt_half_remote, gehalt_full_office), 
                modus = c("Nur Remote", "Halb Remote", "Nur Office"))

gehalt_full_remote < gehalt_full_office
gehalt_full_office < gehalt_half_remote


barplot (df$gehalt, names=df$modus)

# 5. 
t = table(salary$remote_ratio)
pie(t, main = "Verteilung der Remote-Office Data Scientists")

# 6. 
## Analog zu 5. 

small = salary[salary$company_size == "S",]
gehalt_small = sum(small$salary_in_euro) / nrow(small)

medium = salary[salary$company_size == "M",]
gehalt_medium = sum(medium$salary_in_euro) / nrow(medium)

large = salary[salary$company_size == "L",]
gehalt_large = sum(large$salary_in_euro) / nrow(large)

df = data.frame(gehalt = c(gehalt_small, gehalt_medium, gehalt_large), 
                groesse = c("Klein", "Mittel", "Groß"))

barplot (df$gehalt, names=df$groesse, 
         xlab = "Unternehmensgröße", ylab = "Durchschnittsgehalt", 
         ylim = c(0, 130000))


#### ende ---- 
