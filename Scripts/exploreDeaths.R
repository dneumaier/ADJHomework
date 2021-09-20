#loading libraries
library(tidyverse)
library(janitor)

# site: https://wonder.cdc.gov/
# downloaded on Sept 20
deaths <- read_delim("Data/Deaths2010-2019StateYearAgeRaceEth.txt", delim = "\t")

#Filtering out the notes
notes <- deaths %>% filter(!is.na(Notes)) %>% select(Notes)

#creating readable data
deaths <- deaths %>% filter(is.na(Notes)) %>%
  select(-Notes)

#cleaning column names
deaths <- deaths %>% clean_names()

#What year had the most deaths?
deaths %>% group_by(year) %>% summarize(deaths = sum(deaths)) %>%
  arrange(desc(deaths)) %>%
  select(year, deaths)

#Isn't working because pop is a character
deaths %>% group_by(year) %>% summarize(deathRate = sum(deaths)/sum(population)) %>%
  arrange(desc(deathRate)) %>%
  select(year, deathRate)

#Why is population a character?
deaths %>% count(population) %>% arrange(desc(population))

#filtering Not Applicable populations and deaths/population*100000
deaths %>% filter(population != "Not Applicable") %>% group_by(year) %>%
  summarize(deathRate = sum(deaths)/sum(as.numeric(population))*100000) %>%
  arrange(desc(deathRate)) %>%
  select(year, deathRate)

write.csv(notes, "Data/deaths-notes.csv", na="")
write.csv(deaths, "Data/clean1Deaths2010-2019StateYearAgeRaceEth.csv", na="")
