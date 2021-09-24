library(tidyverse)
library(janitor) #bringing in the libraries

#creating a view called drAlDeaths (for drugAlcoholDeaths)
drAlDeaths <- read_delim("Data/drug&alcohol_deaths_by_state-age-year.txt", delim = "\t")

#separating the notes out
notes <- drAlDeaths %>% filter(!is.na(Notes)) %>% select(Notes)

#reassigning the view so that it no longer includes the notes
drAlDeaths <- drAlDeaths %>% filter(is.na(Notes)) %>%
  select(-Notes)

#cleaning names
drAlDeaths <- drAlDeaths %>% clean_names()

#words like unreliable and suppressed and Not Applicable are kind of useless to me for now.
#I am interested in, however, how many of these groups of deaths are suppressed, 
#1)so how many suppressions of deaths are there?

drAlDeaths %>% group_by(deaths) %>% filter(deaths == "Suppressed") %>% summarize(count = n())
#So 14035 instances of suppressed data

#2)How many of these suppressions are for minors? (But we will pull the whole thing because that's just a little easier)
drAlDeaths %>% group_by(ten_year_age_groups_code) %>%
  filter(deaths == "Suppressed") %>%
  summarize(count =n())
#1785 for 1, 1810 for 1-4, 1798 for 5-14, 1048 for 15-24. Notably NS (not stated) is the highest at 2022. 

#3)Which cause has the highest number of unsuppressed deaths?
drAlDeaths %>% group_by(mcd_drug_alcohol_induced_cause) %>%
  filter(deaths != "Suppressed") %>%
  summarize(death = sum(as.numeric(deaths))) %>%
  arrange(desc(death)) %>%
  select(mcd_drug_alcohol_induced_cause, death)
#Well I suppose I should have seen that coming that "All other..." would be at the top... 
#But Drug poisonings (overdoes) unintentional is the highest of the non-"all other" 
#categories by 220,000, so that's interesting

#4) Which year had the highest number of unsupressed Unintentional Drug overdoses deaths? 
#(the thing with total 299,431 from the last question)
drAlDeaths %>% group_by(year) %>%
  filter(deaths != "Suppressed") %>%
  filter(mcd_drug_alcohol_induced_cause_code == "D1") %>% #found the code for this from the data because it is too long to type out
  summarize(death = sum(as.numeric(deaths))) %>%
  arrange(desc(death)) %>%
  select(year, death)
#Looks like 2019 was a bad year for people. About a thousand more non-suppressed ODs than 2017

#5) This may come accross wrong, but what is the druggy state (most deaths by state)?
#putting into a set so I can actually read the whole thing if I wanted.
stateDeaths <- drAlDeaths %>% group_by(state) %>%
  filter(deaths != "Suppressed") %>%
  summarize(death = sum(as.numeric(deaths))) %>%
  arrange(desc(death)) %>%
  select(state, death)
#Answer is California, by a lot. But that makes sense since they have one of the largest pops. 
#6)Following up the last for rationality's sake, death rate by drugs?
stateDR <- drAlDeaths %>% group_by(state) %>%
  filter(deaths != "Suppressed") %>% filter(!is.na(population)) %>%
  summarize(dR = sum(as.numeric(deaths))/sum(as.numeric(population))*100000) %>%
  arrange(desc(dR)) %>%
  select(state, dR)
#Vermont has the highest (known) death rate overall using only known numbers.
#Interestingly enough, the greater the Urban population, the lower the drug death rate... More (stereotypical) isolation in the higher states


#Surprise surprise everyone, California has the highest number of known drug related deaths, but Vermont, yes the land of maple syrup, has the highest drug related death rate. 
#Data from the CDC seems to show a trend of urban areas having more drug related deaths but lower drug related death rates.

#Unintentional overdose is the leading cause of known drug related deaths in the United States. 
#Following after combinations of "All other causes", unintentional overdose tops the charts by a landslide 
#of about 220,000 deaths between it and the next leading cause, Alcohol poisonings.

#Last one because I thought of a morbidly funny lede, but not much else.
#Did 2019 really suck that bad? According to data for drug related deaths from the CDC, it really did.