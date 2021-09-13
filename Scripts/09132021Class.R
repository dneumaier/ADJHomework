library(tidyverse) #bringing in tidyverse
library(janitor) #bringing in janitor to clean up this dirty, dirty data

crimes <- read_csv("Data/Offenses_Known_to_Law_Enforcement_by_State_by_City_2019.csv")

#CLEANING
crimes <- crimes %>% clean_names()

#REVIEW
crimes %>% select(state, city, population) %>%
  arrange(desc(population))

crimes %>% filter(state == "NEW YORK") %>%
  select(state, city, population, violent_crime) %>%
  arrange(desc(violent_crime))

crimes %>% group_by(state) %>%
  summarize(sum_pop = sum(population)) %>%
  arrange(desc(sum_pop))

crimes %>% filter(population > 100000) %>%
  mutate(murder_rate = murder_and_nonnegligent_manslaughter/population*100000) %>%
  select(state, city, murder_and_nonnegligent_manslaughter, population, murder_rate) %>%
  arrange(desc(murder_rate))

#Which city has the higheset rate in violent_crime?
crimes %>% filter(population > 100000) %>%
  mutate(vcrime_rate = violent_crime/population*100000) %>%
  select(state, city, vcrime_rate) %>%
  arrange(desc(vcrime_rate))

#How many motor vehicle thefts have there been in total?
crimes %>% filter(!is.na(motor_vehicle_theft)) %>%
  summarize(sum_mvt = sum(motor_vehicle_theft)) #not done

#How many cities have more than 50 murders
crimes %>% filter(murder_and_nonnegligent_manslaughter > 50) %>%
  summarize(count = n())
