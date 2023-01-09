# Aufgabe 6.3  # nolint
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2022/Aufgabe 6.3 (Corona)") # nolint

load("time_series_covid19_confirmed_11-16-2022.rda")

# Aufgabe 6.3 b) 
my_mean <- function(vector) {
    return(sum(vector) / length(vector))
}

mean_jan_22_2020 <- my_mean(corona[corona$Date == "2020-01-20", 1:ncol(corona) - 1]) # nolint
mean_jan_22_2021 <- my_mean(corona[corona$Date == "2020-01-21", 1:ncol(corona) - 1]) # nolint
mean_jan_22_2022 <- my_mean(corona[corona$Date == "2020-01-22", 1:ncol(corona) - 1]) # nolint

# Aufgabe 6.3 c) 
my_median <- function(df) {
    vector <- as.vector(t(df))
    n <- length(vector)
    vector <- sort(vector)
    
    median = 0

    if((n%%2) != 0) {
        median <- vector[n / 2 + 1]
    } else {
        median <- 0.5 * (vector[n / 2] + vector[(n/2) + 1])
    }
    return(median)
}

median_jan_22_2020 <- my_median(corona[corona$Date == "2020-01-20", 1:ncol(corona) - 1]) # nolint
median_jan_22_2021 <- my_median(corona[corona$Date == "2021-01-20", 1:ncol(corona) - 1]) # nolint
median_jan_22_2022 <- my_median(corona[corona$Date == "2022-01-20", 1:ncol(corona) - 1]) # nolint


# Aufgabe 6.3 d) 
corona_de_feb_21 = corona$Germany[grep("2021-02", corona$Date)]
corona_de_feb_22 = corona$Germany[grep("2022-02", corona$Date)]

wachstums_faktor <- function(vector) {
  w = c(1)
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


gm_21 = geo_mittelwert(corona_de_feb_21) 
gm_22 = geo_mittelwert(corona_de_feb_22) 


# Aufgabe 6.3 e) 
corona$Date <- as.POSIXct(corona$Date, format = "%Y-%m-%d %H:%M:%S")
plot(Germany~Date, corona)
