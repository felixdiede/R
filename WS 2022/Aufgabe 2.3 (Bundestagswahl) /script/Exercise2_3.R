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
user_d1 <- user[user$user %in% tw1$user, ]
user_d2 <- user[user$user %in% tw2$user, ]
user_d3 <- user[user$user %in% tw3$user, ]
user_d4 <- user[user$user %in% tw4$user, ]

# 1. Wieviel Prozent der Twitter User ist zertifiziert?
user_d1_verified <- nrow(user_d1[which(user_d1$verified == TRUE), ]) / nrow(user_d1) * 100  #0.04 # nolint
user_d2_verified <- nrow(user_d2[which(user_d2$verified == TRUE), ]) / nrow(user_d2) * 100 #0.075 # nolint
user_d3_verified <- nrow(user_d3[which(user_d3$verified == TRUE), ]) / nrow(user_d3) * 100 #0.03 # nolint
user_d4_verified <- nrow(user_d4[which(user_d4$verified == TRUE), ]) / nrow(user_d4) * 100 #0.06 # nolint

# 2. Wieviel Prozent der User Nutzen das Standard-Profilbild?
user_d1_image <- nrow(user_d1[which(user_d1$default_profile_image == TRUE), ]) / nrow(user_d1) * 100 #0
user_d2_image <- nrow(user_d2[which(user_d2$default_profile_image == TRUE), ]) / nrow(user_d2) * 100 #0
user_d3_image <- nrow(user_d3[which(user_d3$default_profile_image == TRUE), ]) / nrow(user_d3) * 100 #0
user_d4_image <- nrow(user_d4[which(user_d4$default_profile_image == TRUE), ]) / nrow(user_d4) * 100 #0

# 3. Wie hoch ist der Friends/ Follower-Ratio pro Account? 
user_d1$friends_follower_rate <- user_d1$friends_count / user_d1$followers_count
user_d2$friends_follower_rate <- user_d2$friends_count / user_d2$followers_count
user_d3$friends_follower_rate <- user_d3$friends_count / user_d3$followers_count
user_d4$friends_follower_rate <- user_d4$friends_count / user_d4$followers_count


# 4. Welcher User mit Standard-Profilbild und einem nicht verifizierten Account haben eine Quote über 50?
user_diskussion <- list(user_d1, user_d2, user_d3, user_d4)

for (u in user_diskussion) {
  u <- as.data.frame(u)
  relevante_user <- subset(u, u$verified == FALSE)
  relevante_user <- subset(relevante_user, default_profile_image == TRUE)
  relevante_user <- subset(relevante_user, friends_follower_ratio >= 50, select = user)
}
