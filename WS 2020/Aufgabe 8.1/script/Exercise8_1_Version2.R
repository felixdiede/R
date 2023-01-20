# Aufgabe 8.1 Version 2
covid <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2020/Aufgabe 8.1/data/covid_19_daily_reports_01-17-2021.csv", header = TRUE, sep = ";")
states <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2020/Aufgabe 8.1/data/USA_States.csv", header = TRUE, sep = ";")
bundeslaender <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2020/Aufgabe 8.1/data/DE_Bundeslaender.csv", header = TRUE, sep = ";")
world <- read.csv(file = "/Users/felixdiederichs/Desktop/Wirtschaftsinformatik/5. Semester/Daten und Wahrscheinlichkeiten/R/WS 2020/Aufgabe 8.1/data/World_Countries.csv", header = TRUE, sep = ";")

# b) 
covid_germany <- covid[covid$Country_Region == "Germany", ]
covid_us <- covid[covid$Country_Region == "US", ]


aggregate_states <- function(df) {
    bundesstaaten <- unique(df$Province_State)
    Confirmed = Deaths = Recovered = Active = c()

    aggregate_df <- data.frame()
    
    for(i in 1:length(bundesstaaten)) {
        aggregate_df[i, 1] = bundesstaaten[i]
        
        Confirmed = sum(df$Confirmed[df$Province_State == bundesstaaten[i]])
        aggregate_df[i, 2] = Confirmed

        Deaths = sum(df$Deaths[df$Province_State == bundesstaaten[i]])
        aggregate_df[i, 3] = Deaths

        Recovered = sum(df$Recovered[df$Province_State == bundesstaaten[i]])
        aggregate_df[i, 4] = Recovered

        Active = sum(df$Active[df$Province_State == bundesstaaten[i]])
        aggregate_df[i, 5] = Active
    }

    colnames(aggregate_df) <- c("Bundesstaat", "Confirmed", "Deaths", "Recovered", "Active")
    
    return(aggregate_df)
}

covid_us <- aggregate_states(covid_us)


sum.countries <- function(df) {
    countries = unique(df$Country_Region)
    Confirmed = Deaths = Recovered = Active = c()
    
    aggregate_df <- data.frame()

    for(i in 1:length(countries)) {
        aggregate_df[i, 1] = countries[i]

        Confirmed = sum(df$Confirmed[df$Country_Region == countries[i]])
        aggregate_df[i, 2] = Confirmed

        Deaths = sum(df$Deaths[df$Country_Region == countries[i]])
        aggregate_df[i, 3] = Deaths 

        Recovered = sum(df$Recovered[df$Country_Region == countries[i]])
        aggregate_df[i, 4] = Recovered 

        Active = sum(df$Active[df$Country_Region == countries[i]])
        aggregate_df[i, 5] = Active 
    }

    colnames(aggregate_df) <- c("Country", "Confirmed", "Deaths", "Recovered", "Active")
    return(aggregate_df)
}

covid <- sum.countries(covid)

# c) 
covid_us <- merge(x = covid_us, y = states, by.x = "Bundesstaat", by.y = "State")
covid_us$rel.Confirmed <- covid_us$Confirmed / covid_us$Population
covid_us$rel.Deaths <- covid_us$Deaths / covid_us$Population

covid <- merge(x = covid, y = world, by.x = "Country", by.y = "Country")
covid$rel.Confirmed <- covid$Confirmed / covid$Population
covid$rel.Deaths <- covid$Deaths / covid$Population

covid_germany <- merge(x = covid_germany, y = bundeslaender, by.x = "Province_State", by.y = "Bundesland")
covid_germany$rel.Confirmed <- covid_germany$Confirmed / covid_germany$Population
covid_germany$rel.Deaths <- covid_germany$Deaths / covid_germany$Population


# d) 
boxplot(covid_us$rel.Confirmed, main = "relative bestätigte Fallzahlen USA")
boxplot(covid$rel.Confirmed, main = "relative bestätigte Fallzahlen Welt")
boxplot(covid_germany$rel.Confirmed, main = "relative bestätigte Fallzahlen Deutschland")
par(mfrow = c(1,3))

boxplot(covid_us$rel.Deaths, main = "relative Todeszahlen USA")
boxplot(covid$rel.Deaths, main = "relative Todeszahlen Welt")
boxplot(covid_germany$rel.Deaths, main = "relative Todeszahlen Deutschland")
par(mfrow = c(1,3))

# e) 
install.packages("ineq")
library(ineq)

dev.off()

plot(Lc(covid_us$rel.Deaths), col = "red")
lines(Lc(covid$rel.Deaths), col = "green")
lines(Lc(covid_germany$rel.Deaths), col = "blue")

legend("topleft", c("Todeszahlen USA", "Todeszahlen Welt", "Todeszahlen Deutschland"), fill = c("red", "green", "blue"))
