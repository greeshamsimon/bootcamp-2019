library(here)
library(readr)
generation <- read_csv(here::here("data","ca_energy_generation.csv"))
imports <- read_csv(here::here("data","ca_energy_imports.csv"))


#use lubridate library to get as_datetime function to convert char to datetime

#
head(generation)
library(reshape2)
long_gen <- melt(generation, id.vars = "datetime",
                 variable.name = "source",
                 value.name = "usage")

head(long_gen[order(long_gen$datetime), ])

#Merging sources of data using the merge function
merged_energy <- merge(generation, imports, by = "datetime")
head(merged_energy)


#Create long form of merged data sets
long_merged_energy <- melt(merged_energy, id.vars = "datetime",
                           variable.name = "source",
                           value.name = "usage")
head (long_merged_energy)

library(tidyverse)

#select certain variables
tmp <- select(merged_energy, biogas, biomass, geothermal, solar)
names(tmp)

# select variables except ones preceded with "-"
tmp <- select(merged_energy, -biogas, -biomass, -geothermal, -solar)
names(tmp)

#select has helper functions to parse through columns
# you can use regEx with match help function
tmp <- select(merged_energy, contains("hydro"), starts_with("bio"))
names(tmp)


# filters data based on certain conditions
tmp <- filter(merged_energy, imports > 7000)
nrow(tmp)

tmp <- filter(merged_energy, imports > 7000, natural_gas < 7000)
nrow(tmp)


# mutate creates new variables
tmp <- mutate(long_merged_energy, log_usage = log(usage))
head(tmp)

# you can create multiple variables at the same time
tmp <- mutate(long_merged_energy, log_usage = log(usage), usage2 = usage^2, usage3 = usage^3)
head(tmp)


#summamrize--summarize reduces observations to a single value based 
#on functions - mean, sum, sd, min, max, etc.

# total energy consumption and the mean
summarize(long_merged_energy, total = sum(usage, na.rm = T),mean_cons = mean(usage, na.rm = T))

# you can use pipe (%>% -- cmd+shift+m) to apply multiple "verbs" together
# take df then filter it then select these variables
# you do not need to repeat the name of the dataframe!
long_merged_energy %>% 
    filter(source == "geothermal") %>% 
    select(-datetime) %>% 
    mutate(log_usage = log(usage)) %>% 
    summarize(mean_log_usage = mean(log_usage, na.rm = T))

#To refer to the manipulated dataframe, use .
merged_energy %>% 
    select(-datetime) %>% 
    mutate(total_usage = rowSums(., na.rm = T)) %>% 
    summarize(total_usage = sum(total_usage, na.rm = T))


merged_energy %>% 
    select(contains("hydro"))  %>% 
    mutate(total_hydro = rowSums(., na.rm = T) ) %>% 
    summarize(mean_total_hydro = mean(total_hydro,na.rm=T))

#using group by to apply a "verb to certain groups
long_merged_energy %>% 
    group_by(source) %>% 
    summarize(sum_usage = sum(usage, na.rm = T))


#example and you can do multiple groups
gapminder <- read.csv(here::here("data/gapminder5.csv"))

gapminder %>% 
    group_by(year, continent) %>% 
    summarize(mean_le = mean(lifeExp, na.rm = T),
              sd_lf = sd(lifeExp, na.rm = T))

long_merged_energy %>% 
    filter(source %in% c("small_hydro", "large_hydro", "biogas", "biomass")) %>% 
    group_by(source) %>% 
    summarize(mean_usage = mean(usage, na.rm=T))



################ DATA TABLES ###################

# import data.table library
library(data.table)

data_file <- here::here("data", "ca_energy_generation.csv")

# read in two versions of data, one as a data.frame and one as a data.table
generation_df <- read_csv(data_file)

generation_dt <- fread(data_file)

#dat tables inherits from data frame
class (generation_df)
class(generation_dt)

View(generation_df)
View(generation_dt)
generation_df
generation_dt
str(generation_df)
str(generation_dt)

#DT[row ops,col ops, group ops]
generation_dt[wind > 4400]
#filters on the 7th
generation_dt[wind > 4400 & mday(datetime) == 7]


generation_dt[natural_gas<=5000 & large_hydro >2000]
#select rows for which coal generation is greater
#than 10 MW and solar generation is greater than the 
#median value of solar generation
generation_dt[coal>10 & solar > median(solar)]

#perform operation in columns
generation_dt[,wind + solar]

generation_dt[,newcol := 3*wind + solar*biogas/2]
generation_dt[,newcol := NULL]



generation_dt[,.(newcol= 3*wind + solar*biogas/2)]


#:= in place operation modifying data table
#. creates list
generation_dt[,total_hydro:=small_hydro+large_hydro]  
generation_dt[,.(mean(nuclear),mean(biogas))]
# or, with column names
# generation_dt[,.(mean_nuc = mean(nuclear), mean_biogas = mean(biogas))]
generation_dt[solar==0, .(datetime, total_thermal = natural_gas +coal)]


generation_dt[,mean(nuclear), by = mday(datetime)]

generation_dt[,.(mean_nuc = mean(nuclear), mean_wind = mean(wind)), 
              by = mday(datetime)]

generation_dt[hour(datetime) > 19,
              .(mean_nuc = mean(nuclear), mean_wind = mean(wind)), 
              by = mday(datetime)]

generation_dt[,.(med_solar=median(solar)), by = hour(datetime)]
generation_dt[solar>0,max(natural_gas), by = mday(datetime)]

library(lubridate)

long_merged_energy <- long_merged_energy %>%
    mutate(day = as_date(datetime),
           log_output = log(output)) %>%
    group_by(day) %>%
    mutate(total_daily_output = sum(output, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(per_output = output/total_daily_output)


all_generation_long[,day := as_date(datetime)]
all_generation_long[,log_output := log(value)]
all_generation_long[,per_output := value/sum(value), by = day]


generation_dt[,.N]

generation_dt[,.N, by = day(datetime)]

# for .I: more advanced syntax
generation_dt[,.I]
key(generation_dt)

# check the current key
key(generation_dt)

# set key
setkey(generation_dt, datetime)
key(generation_dt)

imports_dt <- fread(here::here("data", "ca_energy_imports.csv"))

imports_dt
# set key or specify on 
imports_dt[generation_dt, on = "datetime"]

