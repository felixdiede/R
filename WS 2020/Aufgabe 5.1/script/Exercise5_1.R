# Aufgabe 5.1 
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2020/Aufgabe 5.1")

covid <- read.csv(file = "data/covid_19_daily_reports_12-06-2020.csv", header = TRUE, sep = " ")

# Aufgabe 5.1 a) 
aggregate_cases <- function(df) {
    df_agg <- aggregate(df[, c("Confirmed", "Deaths", "Recovered", "Active")],
                by = list(df$Country_Region), 
                FUN = sum)

    colnames(df_agg) <- c("Country_Region", "Confirmed", "Deaths", "Recovered", "Active")

    return(df_agg)
}

covid_sum <- aggregate_cases(covid)


# Aufgabe 5.1 b)
my_median <- function(vector){
  sorted <- sort(vector)
  my_median = (1/2)*(sorted[floor((length(sorted)+1)/2)] + sorted[ceiling((length(sorted)+1)/2)])
  return(my_median)
}

my_mean <- function(vector){
  return((1/length(vector) * sum(vector)))
}

my_median_deviation <- function(vector){
  return( sum( abs(vector-my_median(vector)) ) / length(vector) ) 
}

my_var <- function(vector){
  return(sum( (vector - my_mean(vector))^2 ) / (length(vector)-1) ) 
}

my_sd <- function(vector){
  return( sqrt(sum( (vector - my_mean(vector))^2 ) / (length(vector)-1) ))
}

my_md_dev <-  my_median_deviation(covid_sum$Confirmed) #374107.3
my_variance <- my_var(covid_sum$Confirmed) # 2118098527509.39
# round(my_variance, 2) == round(var(covid_sum$Confirmed),2)
my_sdev <- my_sd(covid_sum$Confirmed) # 1455369
# my_sdev == sd(covid_sum$Confirmed) # 


# Aufgabe 5.1 c) 
confirmed_mean <- my_mean(covid_sum$Confirmed) # 382716.43
# round(my_mean(covid_count$Confirmed),2) == round(mean(covid_count$Confirmed),2)

confirmed_median <- my_median(covid_sum$Confirmed) # 41468
# my_median(covid_count$Confirmed) == median(covid_count$Confirmed)
# med < mean -> rechtsschief/ linkssteile Verteilung 
hist(covid_sum$Confirmed, freq = FALSE)
