data <- c(200, 300, 600, 1300, 2000, 4000)

my_lorenzkurve <- function(vector) {
    sorted_v = sort(vector)
    sum_all = sum(sorted_v)

    n = length(sorted_v)
    u = seq(0, 1, (1/n))

    v = c(0)
    for(i in 1:n) {
        sum_i = sum(sorted_v[1:i])
        v = c(v, sum_i/ sum_all)
    }

    plot(u, v, type = "b")
    
}

my_lorenzkurve(data)
