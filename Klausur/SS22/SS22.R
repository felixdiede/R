# Klausur SS22

# Aufgabe a)
laptops <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/Klausur/SS22/laptops.csv", sep = ",")

summary(laptops)
str(laptops)

# Aufgabe b)

# discount_price = latest_price * 1 - discount
laptops$discount_price <- laptops$latest_price * (1- (laptops$discount / 100)) 


# Aufgabe c) 

# spearman-korrelation: cor(x,y, method = "spearman")
star_rating <- laptops$star_rating
discount <- laptops$discount

correlation <- cor(star_rating, discount, method = "spearman")
# es herrscht ein sehr geringer linearer Zusammenhang zwischen den Star_ratings und den Discounts

# Aufgabe d)


# Aufgabe e) 
assign_colors <- function(df) {
    df$col = "green"
    
    for(i in 1:ncol(df)) {
        if(df$ram_gb[i] == "4 GB GB") {
            df$col[i] = "green"
        } 
        
        if(df$ram_gb[i] == "8 GB GB") {
            df$col[i] = "orange"
        }

        if(df$ram_gb[i] == "16 GB GB") {
            df$col[i] = "red"
        }

        if(df$ram_gb[i] == "32 GB GB") {
            df$col[i] = "black"
        }
    }
    return(df)

}

laptops <- assign_colors(laptops)

plot(laptops$old_price, laptops$discount_price, 
    xlab = "alte Preise", 
    ylab = "neue Preise inkl. Rabatt", 
    xlim = c(0, 380000), 
    ylim = c(0, 500000), 
    col = c(laptops$col))


# Aufgabe f) 

# 1.
laptops_apple <- laptops[laptops$brand == "APPLE", ]

# 2.
distanzmatrix <- dist(laptops$latest_price, laptops$old_price, method = "euclidean")

# 3. 
ward_clust <- hclust(distanzmatrix, method = "ward.D2") 
single_clust <- hclust(distanzmatrix, method = "single")

plot(single_clust)
plot(ward_clust)
