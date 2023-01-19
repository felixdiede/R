# Kovarianz
my_cov <- function(x, y) {
    s <- 0
    for(i in 1:length(x)) {
        s = s + ((x[i] - mean(x)) * (y[i] - mean(y)))
    }
    return(s /length(x))
}

x <- c(0,1,2,3,6,5,8)
y <- c(0,5,6,9,8,5,6)

my_cov(x,y)
cov(x,y)
