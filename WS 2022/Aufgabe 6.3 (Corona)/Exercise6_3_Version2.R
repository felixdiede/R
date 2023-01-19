# Aufgabe 6.3
load("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2022/Aufgabe 6.3 (Corona)/time_series_covid19_confirmed_11-16-2022.rda")
str(corona)
summary(corona)

# b) 
my_mean <- function(vector) {
    n = length(vector)
    for(i in 1:n) {
        sum = sum(vector)
        mean = sum / n
    }
    return(mean)
}

corona_mean_22_jan_2020 <- my_mean(corona[corona$Date == "2020-01-22", (1:ncol(corona) - 1)]) #nolint
corona_mean_22_jan_2021 <- my_mean(corona[corona$Date == "2021-01-22", (1:ncol(corona) - 1)]) #nolint
corona_mean_22_jan_2022 <- my_mean(corona[corona$Date == "2022-01-22", (1:ncol(corona) - 1)]) #nolint

# c) 
my_median <- function(df) {
    vector = as.vector(t(df))
    n = length(vector)
    sorted_vector = sort(vector)

    if(n%%2 != 0) {
        median = sorted_vector[(n + 1)/ 2]
    } else {
        median = 1/2 * (sorted_vector[n / 2] + sorted_vector[n / 2 + 1])
    }
    return(median)
}

corona_median_22_jan_2020 <- my_median(corona[corona$Date == "2020-01-22", (1:ncol(corona) - 1)])
corona_median_22_jan_2021 <- my_median(corona[corona$Date == "2021-01-22", (1:ncol(corona) - 1)])
corona_median_22_jan_2022 <- my_median(corona[corona$Date == "2022-01-22", (1:ncol(corona) - 1)])


# d)
corona_feb_2021_germany <- corona$Germany[grepl("2021-02", corona$Date)] 
corona_feb_2022_germany <- corona$Germany[grepl("2022-02", corona$Date)]


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

gm_21_2 <- EnvStats::geoMean(wachstums_faktor(corona_feb_2021_germany)) 
gm_22_2 <- EnvStats::geoMean(wachstums_faktor(corona_feb_2022_germany)) 


# e) 
corona$Date <- as.POSIXct(corona$Date, format = "%Y-%m-%d %H:%M:%S")

corona_germany <- corona$Germany

plot(corona_germany, main = "Corona Fallzahlen Deutschland")
