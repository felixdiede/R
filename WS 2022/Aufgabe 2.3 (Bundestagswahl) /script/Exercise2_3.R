# Aufgabe 2.3 (Bundestagswahl)
setwd("/Users/felixdiederichs/R/WS 2022/Aufgabe 2.3 (Bundestagswahl) ")

# Aufgabe 2.3 a)
load("data/Tweets.rda")

tw1 <- tw[tw$discussion == 1, ]
tw2 <- tw[tw$discussion == 2, ]
tw3 <- tw[tw$discussion == 3, ]
tw4 <- tw[tw$discussion == 4, ]

# Aufgabe 2.3 b)
discussion <- read.csv(file = "data/Diskussionen.csv", header = TRUE, sep = ";")
discussion$dates <- as.POSIXct(discussion$dates, format = "%Y-%m-%d %H:%M:%S")


plot(discussion$dates, discussion$discussion_1, ylim = c(0, 175),
    xlab = "Zeit", ylab = "durchschnittliche Tweet-Aktivität",
    col = "blue", type = "b")
lines(discussion$dates, discussion$discussion_2, col = "green", type = "b")
lines(discussion$dates, discussion$discussion_3, col = "red", type = "b")
lines(discussion$dates, discussion$discussion_4, col = "gold", type = "b")
legend("topleft", c("Diskussion 1", "Diskussion 2", "Diskussion 3", "Diskussion 4"), fill = c("blue", "green", "red", "gold")) # nolint


# Aufgabe 2.3 c)
user <- read.csv(file = "data/Users.csv", heade = TRUE, sep = ";")

# 1. Wieviel Prozent der Twitter User ist zertifiziert?
user_d1 <- user[user$user %in% tw1$user, ]
user_d2 <- user[user$user %in% tw2$user, ]
user_d3 <- user[user$user %in% tw3$user, ]
user_d4 <- user[user$user %in% tw4$user, ]

user_d1_verified <- nrow(user_d1[which(user_d1$verified == TRUE), ]) / nrow(user_d1) * 100  #0.04 # nolint
user_d2_verifiied <- nrow(user_d2[which(user_d2$verified == TRUE), ]) / nrow(user_d2) * 100 #0.075 # nolint
user_d3_verifiied <- nrow(user_d3[which(user_d3$verified == TRUE), ]) / nrow(user_d3) * 100 #0.03 # nolint
user_d4_verifiied <- nrow(user_d4[which(user_d4$verified == TRUE), ]) / nrow(user_d4) * 100 #0.06 # nolint





# 2.
