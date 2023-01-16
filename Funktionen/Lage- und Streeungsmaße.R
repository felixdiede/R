# Testvektor
vec <- c(120,125,1528,56,658,5742, 258,857,5689, 25)


# arithmetisches Mittel
my_arithmetic_mean <- function(vector) {
    n <- length(vector) 
    arithmetic_mean <- sum(vector) / n

    return(arithmetic_mean)
}

# Teste die Funktion arithmetisches Mittel 
if(my_arithmetic_mean(vec) == mean(vec)) {
    print("Funktion ist korrekt implementiert")
}


# median
my_median <- function(vector) {
    n <- length(vector)
    sorted <- sort(vector)

    if((n%%2) == 0) { # n ungerade
        median <- 0.5 * (sorted[n / 2] + sorted[n / 2 + 1])
    } else {
        median <- sorted[n / 2 + 1]
    }
    return(median)
}

# Teste die Funktion median
my_median(vec) == median(vec)


# Medianabweichung 
my_mad <- function(vector) {
    return( sum( abs(vector-median(vector)) ) / length(vector) ) 
}

# Teste die Funktion mittlere absolute Medianabweichung
my_mad(vec) == mad(vec)


