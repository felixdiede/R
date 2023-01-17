######### ====================== Loesung zum 6.Aufgabenzettel ====================== #########

#### Aufgabe 6.3 ---- 
# a)

load("time_series_covid19_confirmed_11-16-2022.rda")

summary(corona)
str(corona)

# b) 

mittelwert <- function(vector){
  
  m = (1/length(vector))*sum(vector)
  
  return(m)
}

# Teste die eigene mean-Funktion
mean(c(1,2,3)) == mittelwert(c(1,2,3))

mw_22_1_2020 = mittelwert(corona[corona$Date == "2020-01-22",1:(ncol(corona)-1)])
mw_22_1_2021 = mittelwert(corona[corona$Date == "2021-01-22",1:(ncol(corona)-1)])
mw_22_1_2022 = mittelwert(corona[corona$Date == "2022-01-22",1:(ncol(corona)-1)])


# c) 

mein_median <- function(df){
  vector = as.vector(t(df))
  sorted <- sort(vector)
  n = length(vector)
  # Checken, ob n gerade ist mit dem Modulo-Operator 
  if(n %% 2 == 0) {
    my_median = 1/2*(sorted[n/2] + sorted[n/2 + 1])
  } else {
    my_median = sorted[(n+1)/2]
  }
  return(my_median)
}

md_22_1_2020 = mein_median(corona[corona$Date == "2020-01-22",1:(ncol(corona)-1)])
md_22_1_2021 = mein_median(corona[corona$Date == "2021-01-22",1:(ncol(corona)-1)])
md_22_1_2022 = mein_median(corona[corona$Date == "2022-01-22",1:(ncol(corona)-1)])

#d) 
corona_de_jan_21 = corona$Germany[grep("2021-01", corona$Date)]
corona_de_jan_22 = corona$Germany[grep("2022-01", corona$Date)]

wachstums_faktor <- function(vector){
  w = 1
  i = 1 
  while(i < length(vector)){
    w[i+1] <- 1/vector[i] * vector[i+1]  
    i = i+1
  }
  return(w[2:length(w)]) 
}

library(pracma)

geo_mittelwert <- function(vector){
  geo_mean = pracma::nthroot(prod(wachstums_faktor(vector)), n=length(vector))
  return(geo_mean) 
} 

gm_21 = geo_mittelwert(corona_de_jan_21) 
gm_22 = geo_mittelwert(corona_de_jan_22) 

library(EnvStats)

gm_21_2 <- EnvStats::geoMean(wachstums_faktor(corona_de_jan_21)) 
gm_22_2 <- EnvStats::geoMean(wachstums_faktor(corona_de_jan_22)) 


# e) 

corona$Date = as.POSIXct(corona$Date, format="%Y-%m-%d")

# Die wissenschaftliche Schreibweise mit e^x unterdrücken
options(scipen=999)

plot(Germany~Date, corona, xaxt = "n", type = "l", 
     ylim = c(0, 100000000), 
     xlab = "Datum", 
     ylab = "Bestätigte Fallzahlen", 
     col = "black")

lines(China~Date, corona, col = "red")
lines(United.Kingdom~Date, corona, col = "blue")
lines(US~Date, corona, col = "lightblue")
lines(Sweden~Date, corona, col = "orange")

axis.POSIXct(1, format="%m-%Y", cex.axis = .7, 
     at=seq(min(corona$Date), max(corona$Date), by="months"))

legend("topleft", legend = c("Deutschland", "China", "Vereinigtes Königreich", "Vereinigte Staaten", "Schweden"), 
       fill = c("black", "red", "blue", "lightblue", "orange"))




