# Aufgabe 1.3 (Data Science salaries)

setwd("/Users/felixdiederichs/R/WS 2022/Aufgabe 1.3 (Data science salaries)")

# Aufgabe 1.3 a) 
salaries <- read.csv(file = "data/data_science_salaries.csv", header = TRUE, sep = ",", dec = ".")

# Aufgabe 1.3 b) 
summary(salaries)
str(salaries) # -> Variablen: work_year, experience_level, job_title, salary_in_usd, employee_residence,
              #              remote_ratio, company_location, company_size
              # work_year: kardinal, diskret
              # experience: ordinal
              # job_title: nominal
              # salary_in_usd: kardinal, diskret
              # employee_residence: nominal
              # remote_ratio: nominal
              # company_location: nominal
              # company_size: ordinal


# Aufgabe 1.3 c) 
salaries$salary_in_euros <- round(salaries$salary_in_usd * 0.94)


# Aufgabe 1.3 d) 
sturges <- round(1+log2(nrow(salaries))) # 10 
rice <- round(2*nrow(salaries)^(1/3)) # 17
quadrat <- round(sqrt(nrow(salaries))) # 25

sturges_histogramm <- hist(salaries$salary_in_euros, freq = FALSE, breaks = "sturges")
rice_histogramm <- hist(salaries$salary_in_usd, freq = FALSE, breaks = 17)
quadrat_histogramm <- hist(salaries$salary_in_usd, freq = FALSE, breaks = 25)


# Aufgabe 1.3 e) 

# Welches sind die zehn Länder, in denen Data Scientiests mit dem höchsten Durchschnittsgehalt wohnen? 
einzelne_laender = unique(salaries$employee_residence)
laender = c()
durchschnitts_gehalt = c()

for(e in einzelne_laender){
  salary_relevant = salaries[salaries$employee_residence == e,]
  laender = c(laender, e)
  summe = sum(salary_relevant$salary_in_euro)
  anzahl = nrow(salary_relevant)
  durchschnitts_gehalt = c(durchschnitts_gehalt, summe/anzahl)
}

mean(salaries$salary_in_euros)

beste_wohnort = data.frame(land = laender, gehalt = durchschnitts_gehalt)

beste_wohnort <- beste_wohnort[order(beste_wohnort$gehalt, decreasing = TRUE), ]

barplot(beste_wohnort$gehalt, names = beste_wohnort$land, ylab = c(200000, 0))

# Welches sind die zehn Länder, in denen Data Scientiests mit dem höchsten Durchschnittsgehalt arbeiten? 
einzelne_laender = unique(salaries$company_location)
laender = c()
durchschnitts_gehalt = c()

for(e in einzelne_laender){
  salary_relevant = salaries[salaries$company_location == e,]
  laender = c(laender, e)
  summe = sum(salary_relevant$salary_in_euro)
  anzahl = nrow(salary_relevant)
  durchschnitts_gehalt = c(durchschnitts_gehalt, summe/anzahl)
}

mean(salaries$salary_in_euros)

beste_arbeitsorte = data.frame(land = laender, gehalt = durchschnitts_gehalt)

beste_arbeitsorte <- beste_wohnort[order(beste_wohnort$gehalt, decreasing = TRUE), ]

barplot(beste_arbeitsorte$gehalt, names = beste_arbeitsorte$land, ylab = c(200000, 0))


# Wie hoch ist der Anteil der Data Scientiests, die ein Jahregehalt von mehr als 100.000 Euro erhalten? 

anteil_salaries_gt_100 <- nrow(salaries[salaries$salary_in_euros > 100000, ]) / nrow(salaries) * 100


# In welchem Arbeitsmodus verdienen Data Scientiests am meisten Geld, full remote, half remote oder full office? 
full_office <- salaries[salaries$remote_ratio == 0, ]
full_remote_salaries <- mean(full_office$salary_in_usd) # 106355

half_remote <- salaries[salaries$remote_ratio == 50, ]
half_remote_salaries <- mean(half_remote$salary_in_usd) # 80823

full_remote <- salaries[salaries$remote_ratio == 100, ]
full_remote_salaries <- mean(full_remote$salary_in_usd) # 122457


# Was ist der Anteil der Arbeitsmodi im Datensatz? 
t = table(salaries$remote_ratio)
pie(t)


# Welche Unternehmensgröße zahlt durchschnittlich am meisten? 
company_size_large <- salaries[salaries$company_size == "L", ]
salary_company_size_large <- mean(company_size_large$salary_in_usd) # 119243

company_size_medium <- salaries[salaries$company_size == "M", ]
salary_company_size_medium <- mean(company_size_medium$salary_in_usd) # 116905

company_size_small <- salaries[salaries$company_size == "S", ]
salary_company_size_small <- mean(company_size_small$salary_in_usd) # 77633
