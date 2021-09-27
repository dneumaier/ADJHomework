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

write_csv(notes, "Data/deaths-notes.csv", na="")
write_csv(deaths, "Data/clean1Deaths2010-2019StateYearAgeRaceEth.csv", na="")

#importing deaths to be the clean data
deaths <- read_csv("Data/clean1Deaths2010-2019StateYearAgeRaceEth.csv")

#Introducing new_pop column (numeric population)
deaths <- deaths %>% mutate(new_pop = as.numeric(population))

#creating an object that has death_by_year using the new_pop
death_by_year <- deaths %>% filter(!is.na(new_pop)) %>%
  group_by(year) %>% summarise(death_rate = sum(deaths)/sum(new_pop)*100000)

write_csv(deaths, "Data/clean1Deaths2010-2019StateYearAgeRaceEth.csv", na="")


# intro to ggplot2: plot death_rate by year
ggplot(data=death_by_year, aes(x=year, y=death_rate, group=1)) +
  geom_line()+
  geom_point()

# plot death_rate by year for each age group: 

age_by_year <- deaths %>% filter(!is.na(new_pop)) %>%
  group_by(year, ten_year_age_groups) %>% summarise(death_rate = sum(deaths)/sum(new_pop)*100000)

ggplot(data=age_by_year, aes(x=year, y=death_rate, group=ten_year_age_groups, color=ten_year_age_groups)) +
  geom_line()+
  geom_point()
#& for "and" | for "or", %in% is a faster solution to an "or" statement compare 2010, 2019 death rates by state
state_year <- deaths %>% filter(!is.na(new_pop) & year %in% c("2010", "2019")) %>%
  group_by(state, year) %>% summarize(death_rate = sum(deaths)/sum(new_pop)*100000)

#essentially rotating the table to be wider than it is tall, adding percent change
state_year %>% pivot_wider(names_from = year, values_from = death_rate) %>%
  mutate(pct_chg = (`2019`-`2010`)/`2010`*100) %>%
  arrange(pct_chg)

#creating ageYear to compare 2010, 2019 death rates by age group
ageYear <- deaths %>% filter(!is.na(new_pop) & year %in% c("2010", "2019")) %>%
  group_by(ten_year_age_groups, year) %>% summarize(death_rate = sum(deaths)/sum(new_pop)*100000)

#adding in percent change
ageYear %>% pivot_wider(names_from = year, values_from = death_rate) %>%
  mutate(pct_chg = (`2019`-`2010`)/`2010`*100) %>%
  arrange(desc(pct_chg))

#changes in death rates over all years by age groups
ageYearAll <- deaths %>% filter(!is.na(new_pop)) %>%
  group_by(ten_year_age_groups, year) %>% summarize(death_rate = sum(deaths)/sum(new_pop)*100000)

#pivoting it to be wider
ageYearAllPivot <- ageYearAll %>% pivot_wider(names_from = year, values_from = death_rate) %>%
  mutate(pct_chg = (`2019`-`2010`)/`2010`*100) %>%
  arrange(desc(pct_chg))


#creating a line chart for highest increasing rates

ageYearPair <- deaths %>% filter(!is.na(new_pop) & ten_year_age_groups %in% c("25-34 years", "35-44 years")) %>%
  group_by(ten_year_age_groups, year) %>% summarize(death_rate = sum(deaths)/sum(new_pop)*100000)

ggplot(data=ageYearPair, aes(x=year, y=death_rate, group=ten_year_age_groups, color=ten_year_age_groups)) +
  geom_line()+
  geom_point()
