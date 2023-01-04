# Aufgabe 7.1
setwd("/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2020/Aufgabe 7.1")

# Aufgabe 7.1 a) 
covid <- read.csv(file = "data/covid_19_daily_reports_01-10-2021.csv", header = TRUE, sep = " ")

aggregate_countries <- function(df) {
    agg_df <- aggregate(df[, c("Confirmed", "Deaths", "Recovered", "Active")], 
                        by = list(df$Country_Region), 
                        FUN = sum)
    colnames(agg_df) <- c("Country_Region", "Confirmed", "Deaths", "Recovered", "Active")
    
    return(agg_df)

}

covid_sum <- aggregate_countries(covid)

# Aufgabe 7.1 b) 
install.packages("animation")
library(animation)

?saveGIF

# Aufgabe 7.1 c) 
?rnorm

norm <- rnorm(nrow(covid_sum), mean = mean(covid_sum$Confirmed), sd = sd(covid_sum$Confirmed))

# Aufgabe 7.1 d) 
amazing_animation <- function(vec, steps, name){
  subset = seq(5,length(vec),steps)
  x <- (seq(-4, 4, length = length(vec)) - mean(vec))/sd(vec)
  saveGIF(expr = (
    for(i in 1:(length(subset))) {
      hist(vec[1:subset[i]], freq = FALSE, xlab = name)
      curve(dnorm(x, mean(vec), sd(vec)), add=TRUE, col="red")
    }
    ), movie.name = paste(name, ".gif", sep=""), convert="magick") 
}

amazing_animation(vec=norm, steps=10, name="norm")
