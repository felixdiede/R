######### ====================== Loesung zum 2.Aufgabenzettel ====================== #########

#### Aufgabe a) ---- 

load("Tweets.rda")

str(tw)
summary(tw)

# Filtern der Tweets nach Diskussion 
tw1 = tw[tw$discussion == 1,]
tw2 = tw[tw$discussion == 2,]
tw3 = tw[tw$discussion == 3,]
tw4 = tw[tw$discussion == 4,]

# ALternative: 
split(tw, tw$discussion)

# Daten anschauen: 
# Diskussion 1: AfD 
# Diskussion 2: FDP
# Diskussion 3: CDU 
# Diskussion 4: Die Grünen 

View(tw1)

#### ende ---- 


#### Aufgabe b) ---- 

discussion <- read.table(file = "Diskussionen.csv", header = TRUE, sep = ";", dec = ".")
str(discussion)
summary(discussion)

# 1. 
# Umwandeln, sodass R das Datum auch als solches erkennt 
discussion$dates = as.POSIXct(discussion$dates, format="%Y-%m-%d %H:%M:%S")

# 2. 
# Plotten einer Diskussion 
plot(x = discussion$dates, y = discussion$discussion_4,
     type = "l", col = "blue", frame.plot = F,
     xlab = "Zeit", ylab = "Tweet Aktivität", 
     ylim = c(0, 200))

# Hinzufügen der anderen als Linie 
lines(x = discussion$dates, y = discussion$discussion_3, col="red")
lines(x = discussion$dates, y = discussion$discussion_2, col="forestgreen")
lines(x = discussion$dates, y = discussion$discussion_1, col="gold")

# Legende hinzufügen um die Lesbarkeit zu verbessern 
legend("topleft", pch = 4, title = "Diskussionen", text.font = 1, 
       legend = c("Thema 1","Thema 2","Thema 3", "Thema 4"),
       col = c("gold", "forestgreen", "red", "blue"))

#### ende ---- 

#### Aufgabe c) ---- 

users <- read.table(file = "Users.csv", header = TRUE, sep = ";", dec = ".")

user_d1 <- users[users$user %in% tw1$user,]
user_d2 <- users[users$user %in% tw2$user,]
user_d3 <- users[users$user %in% tw3$user,]
user_d4 <- users[users$user %in% tw4$user,]


## 1.  

percentage_verified1 = length(user_d1$verified[user_d1$verified == TRUE]) / nrow(user_d1) # 0.04
percentage_verified2 = length(user_d2$verified[user_d2$verified == TRUE]) / nrow(user_d2) # 0.075
percentage_verified3 = length(user_d3$verified[user_d3$verified == TRUE]) / nrow(user_d3) # 0.03
percentage_verified4 = length(user_d4$verified[user_d4$verified == TRUE]) / nrow(user_d4) # 0.06

# Alternative, die Anzahl an Zeilen herauszukriegen: 
nrow(user_d1[user_d1$verified == TRUE,])

## 2. 

percentage_default_image1 = length(user_d1$default_profile_image[user_d1$default_profile_image == TRUE]) / nrow(user_d1)
percentage_default_image2 = length(user_d2$default_profile_image[user_d2$default_profile_image == TRUE]) / nrow(user_d2)
percentage_default_image3 = length(user_d3$default_profile_image[user_d3$default_profile_image == TRUE]) / nrow(user_d3)
percentage_default_image4 = length(user_d4$default_profile_image[user_d4$default_profile_image == TRUE]) / nrow(user_d4)


## 3.  

user_d1$friends_follower_ratio = user_d1$friends_count / user_d1$followers_count
user_d2$friends_follower_ratio = user_d2$friends_count / user_d2$followers_count
user_d3$friends_follower_ratio = user_d3$friends_count / user_d3$followers_count
user_d4$friends_follower_ratio = user_d4$friends_count / user_d4$followers_count

user_d4[which(user_d4$friends_follower_ratio > 50),]


## 4.

users_diskussion = list(user_d1, user_d2, user_d3, user_d4) 

for(u in users_diskussion){
  u = as.data.frame(u)
  relevante_user = subset(u, u$verified == FALSE)
  relevante_user = subset(relevante_user, default_profile_image == TRUE)
  relevante_user = subset(relevante_user, friends_follower_ratio >= 50, select = user)
}

print(relevante_user)

## 5) 

subset(tw, user == relevante_user[[1]], select = tweets)

#### ende ---- 


