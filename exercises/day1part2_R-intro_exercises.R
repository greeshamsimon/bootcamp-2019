library(readr)
library(tidyverse)
install.packages("here")
library(here)
gapminder <- read.csv(here::here("data", "gapminder5.csv"))
head(gapminder)

#convert factor columns to character 
str(gapminder)
gapminder$country <- as.character(gapminder$country)
gapminder$continent <- as.character(gapminder$continent)

# computing the mean life expectancy for Afghanistan and Albania

mean(gapminder$lifeExp[gapminder$country == "Afghanistan"])
mean(gapminder$lifeExp[gapminder$country == "Albania"])

# create a vector of values that you want to repeat the function for
obs <- 1:nrow(gapminder)

# initialize the for loop with `for (i in vector)` 
for (i in obs) { # the function to repeat is enclosed in braces {}
    gapminder[i, "gdp"] <- gapminder[i, "pop"] * gapminder[i, "gdpPercap"]
}

#create columns of log_gdpPerap and log_pop
for (i in obs) { # the function to repeat is enclosed in braces {}
    gapminder[i, "log_gdpPercap"] <- log(gapminder[i,"gdpPercap"])
    gapminder[i, "log_pop"] <- log(gapminder[i,"pop"])
}

#use vector formula to calculate log
gapminder$vec_log_gdpPercap<-log(gapminder$gdpPercap)
all(gapminder$vec_log_gdpPercap == gapminder$log_gdpPercap)
#TRUE

#calc the mean life expectancy by year
years <- unique(gapminder$year)

for (i in years) {
    mean_le <- mean(gapminder$lifeExp[gapminder$year == i], 
                    na.rm = T)
    print(paste0(i, ": ", mean_le))
}


#calc the mean life expectancy by continent
continents <- unique(gapminder$continent)

for (i in continents) {
    mean_le <- mean(gapminder$lifeExp[gapminder$continent == i], 
                    na.rm = T)
    print(paste0(i, ": ", mean_le))
}

#calc the mean life expectancy by continent and by year using nested for loops

for (i in continents) {
    print(paste0("Continent: ", i))
          for (j in years){
              mean_le <- mean(gapminder$lifeExp[gapminder$continent == i
                              & gapminder$year == j], na.rm = T)
              print(paste0(j, ": ", mean_le))
          }
}

#calc the standard deviation of life expectancy by continent and by year 
#using nested for loops

for (i in continents) {
    print(paste0("Continent: ", i))
    for (j in years){
        sd_le <- sd(gapminder$lifeExp[gapminder$continent == i
                                          & gapminder$year == j], na.rm = T)
        print(paste0(j, ": ", sd_le))
    }
}

# learning how to use apply
vars <- gapminder[, c("lifeExp", "pop", "gdpPercap")]
apply(vars, 2, mean)
 #alternative using for loop
for (i in vars) {
    print(mean(i))
}

#applying lappy and sapply to the data set

lapply(gapminder, mean)
sapply(gapminder, mean)

sapply(years, function(x) mean(gapminder$lifeExp[gapminder$year == x]))


# practice using while loop

#prints standard deviation of life expectancy data by year starting on 1952 every 5 years until 1987
i <-  1952 # define the interator

while (i < 1987) {
    sd_lf <- sd(gapminder$lifeExp[gapminder$year == i])
    print(paste0(i, ": ", sd_le)
    )
    i <- i + 5 # increase the iterator by the interval between years
}

i <-  1987 # define the interator

#what is the standard deviation for life expectancy 
#for each 5 years between 1987 and 2002 (inclusive)
while (i <= 2002) {
    sd_lf <- sd(gapminder$lifeExp[gapminder$year == i])
    print(paste0(i, ": ", sd_le)
    )
    i <- i + 5 # increase the iterator by the interval between years
}


##IF/ELSE statements

random_year <- sample(years, 1)

random_year

set.seed(10)
random_year <- sample(years, 1)

if (random_year > 1977) {
    print(random_year)
}

#using else statements

random_year <- sample(years, 1)

if (random_year > 1977) {
    print(paste0(random_year, ": ", 
                 mean(gapminder$lifeExp[gapminder$year == random_year]))
    )
} else { 
    print("sorry, random year is less than 1977")
}



#combining for loops with if/else statements
#
threshold <- 70

for (i in unique(gapminder$continent)) {
    tmp <- mean(gapminder$lifeExp[gapminder$continent==i])
    
    if (tmp < threshold) {
        print(paste("Mean Life Expectancy in", i, "is less than", threshold))
    } else {
        print(paste("Mean Life Expectancy in", i, "is greater than", threshold))
    }
}


#prints mean population for years greater 1987

for (i in years) {
    if (i >= 1987) {
        mean_pop <- mean(gapminder$pop[gapminder$year == i])
        print(paste0(i, ": ", mean_pop))
    } else {
        print("Sorry, year is less than 1987")
    }
}

##FUNCTIONS

#write a simple function that prints the value of a selected variable 
#in the gapminder datase

get_values<-
    function(df, variable = "continent"){
        vals <- unique(df[[variable]])
        print(paste0(variable, ": ", vals))
    }
get_values(gapminder)

#write a function that prints the mean and standard deviation 
#for life expentancy for a given country in the gapminder dataset
report_mean_sd <- 
    function(df, variable, country) {
        var <- df[[variable]][gapminder$country == country]
        m_le <- mean(var)
        sd_le <- sd(var)
        cat("Country:", country, 
            "\nMean Life Expectancy:", m_le,
            "\nSD Life Expectancy:", sd_le)
    }

report_mean_sd(gapminder, "lifeExp", "Bulgaria")

#a function that reports the mean, median, minimum, 
#and maximum for life expectancy for a continent in gapminder
report_continent_stats <- 
    function(df, variable, continent) {
        var <- df[[variable]][gapminder$continent == continent]
        m_le <- mean(var)
        med_le <- median(var)
        max_le <- max(var)
        min_le <- min(var)
        cat("Continent:", continent, 
            "\nMean Life Expectancy:", m_le,
            "\nMedian Life Expectancy:", med_le,
            "\nMax Life Expectancy:", max_le,
            "\nMin Life Expectancy:", min_le)
    }
report_continent_stats(gapminder, "lifeExp", "Africa")
