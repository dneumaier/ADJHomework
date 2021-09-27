library(tidyverse)
library(janitor)

# site: wonder.cdc.gov
# downloaded it on Sept 20
deaths <- read_delim("Data/Deaths2010-2019StateYearAgeRaceEth.txt", delim="\t")

# remove notes at the bottom to a separate file
notes <- deaths %>% filter(!is.na(Notes)) %>% select(Notes)

deaths <- deaths %>% filter(is.na(Notes)) %>%
  select(-Notes)

# clean names
deaths <- deaths %>% clean_names()

# What year had the most deaths? 
deaths %>% group_by(year) %>% summarise(deaths = sum(deaths))
deaths %>% group_by(year) %>% summarise(death_rate = sum(deaths)/sum(population))

# What's wrong with population, why is it a character?
deaths %>% count(population) %>% arrange(desc(population))
deaths %>% filter(population != "Not Applicable") %>%
  group_by(year) %>% summarise(death_rate = sum(deaths)/sum(as.numeric(population))*100000)

# change from write.csv to write_csv
# make sure to save deaths as .csv rather than .txt 
# (although it doesn't really matter, you can feed .txt into read_csv and it will work, 
# the formatting is what matters)

write_csv(notes, "Data/deaths-notes.csv", na="")

write_csv(deaths, "Data/clean1Deaths2010-2019StateYearAgeRaceEth.csv", na="")

deaths <- read_csv("Data/clean1Deaths2010-2019StateYearAgeRaceEth.csv")

deaths %>% filter(population == "Not Applicable")

deaths <- deaths %>% mutate(new_pop = as.numeric(population))

# intro to ggplot2: plot death_rate by year

death_by_year <- deaths %>% filter(!is.na(new_pop)) %>%
  group_by(year) %>% summarise(death_rate = sum(deaths)/sum(new_pop)*100000)

?ggplot

ggplot(data=death_by_year, aes(x=year, y=death_rate, group=1)) +
  geom_line()+
  geom_point()

# plot death_rate by year for each age group: 

age_by_year <- deaths %>% filter(!is.na(new_pop)) %>%
  group_by(year, ten_year_age_groups) %>% summarise(death_rate = sum(deaths)/sum(new_pop)*100000)

ggplot(data=age_by_year, aes(x=year, y=death_rate, group=ten_year_age_groups, color=ten_year_age_groups)) +
  geom_line()+
  geom_point()

