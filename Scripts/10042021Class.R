library(tidyverse)
library(janitor)

covidDeaths <- read_csv("Data/Provisional_COVID-19_Deaths_by_Race_and_Hispanic_Origin__and_Age.csv")
covidDeaths <- clean_names(covidDeaths)

stateDeaths <- covidDeaths %>% group_by(state) %>% filter(age_group == "65-74 years") %>%
  filter(!is.na(total_deaths)) %>%
  summarize(countT = sum(total_deaths)) %>%
  select(state, countT) %>% arrange(desc(countT))

stateInfluzenza <- covidDeaths %>% group_by(state) %>%
  filter(age_group == "65-74 years") %>%
  filter(!is.na(influenza_deaths)) %>%
  summarize(countI = sum(influenza_deaths)) %>%
  select(state, countI) %>% arrange(desc(countI))

stateCoivd <- covidDeaths %>% group_by(state) %>%
  filter(age_group == "65-74 years") %>%
  filter(!is.na(covid_19_deaths)) %>%
  summarize(countC = sum(covid_19_deaths)) %>%
  select(state, countC) %>% arrange(desc(countC))

joinedDI <- full_join(stateDeaths, stateInfluzenza)
joinedDCI <- full_join(joinedDI, stateCoivd)

NewMexico <- covidDeaths %>% filter(state == "New Mexico")

NMAges <- NewMexico %>% group_by(age_group) %>% filter(!is.na(total_deaths)) %>%
  summarize(countT = sum(total_deaths)) %>%
  arrange(desc(countT))

NMAgeRace <- NewMexico %>% filter(age_group == "30-49 years") %>% group_by(race_and_hispanic_origin_group)

deathsNHB <- covidDeaths %>% filter(race_and_hispanic_origin_group == "Non-Hispanic Black")

covidDeaths %>% group_by(age_group) %>% filter(!is.na(covid_19_deaths) & !is.na(total_deaths)) %>%
  summarize(DPer = (sum(covid_19_deaths)/sum(total_deaths)))
                                                                                                             