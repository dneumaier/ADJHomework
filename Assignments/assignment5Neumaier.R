library(tidyverse)
library(janitor)

# Import the Provisional Deaths and the Population files
# Change the paths if needed
pop <- read_csv("Data/pop-by-race-eth-age.csv")
deaths <- read_csv("Data/Provisional_COVID-19_Deaths_by_Race_and_Hispanic_Origin__and_Age.csv")

# Clean up the data: alter the pop file (as we discussed in class)
# Clean up the column names in the deaths data
pop <- pop %>% mutate(race_eth = str_replace(race_eth, "Non Hispanic", "Non-Hispanic"))
deaths <- deaths %>% clean_names()

# Join the tables
combined <- deaths %>% inner_join(pop, by=c("state"="name","age_group","race_and_hispanic_origin_group"="race_eth"))

#### First off, thank you for all this free code. 

# Your job is to analyze the data and come up with the most compelling story idea to pitch to your editor. 
# I expect some thorough analysis here: explore several possible ideas. Walk me through those ideas 
# in your code. I should be able to run the code below and check your work and verify the numbers in your pitch. 
# Include a short pitch (a few sentences) with numbers from your analysis that outlines your story idea.
# You will be graded on the code you write, the level of your analysis, and the strength of your pitch 
# (don't pitch a story idea that simply states the death rate for one group, for example. Think in terms of comparisons.)


##Getting Confirmed Covid Death Rates for different race groups
combined %>% filter(!is.na(covid_19_deaths)) %>% filter(!is.na(pop2019)) %>% #adding that pop2019 line to be safe, didn't see any N/As
  group_by(race_and_hispanic_origin_group) %>%
  summarize(DRs = (sum(covid_19_deaths)/sum(pop2019)*100000)) %>% arrange(desc(DRs))
#So evidently, this is not my story idea or my pitch, but does get me a head start.
#It does actually seem that Death rates are higher for Non-Hispanic Black Americans(?) @ 285 per 100K, Next highest is NHW @ 242  
#Than others, but (code doesn't show cuz it didn't work how I wanted, I am interested in breakdown of that by state too)

#going to try a different methodology here
stateDR <- combined %>% filter(!is.na(covid_19_deaths)) %>% filter(!is.na(pop2019)) %>%
  group_by(state) %>%
  summarize(DRs = (sum(covid_19_deaths)/sum(pop2019)*100000)) %>% arrange(desc(DRs))
#North Dakota had the highest DR as a state, BUT IT IS STILL NOT WHAT I AM GOING FOR. I am wanting State in the first column, race in the second, DR in the third
#Yes, I recognize the above code does not even attempt that, but I was removing lines that weren't doing anything anyways.

stateRaceDR <- combined %>% filter(!is.na(covid_19_deaths)) %>% filter(!is.na(pop2019)) %>%
  select(state, race_and_hispanic_origin_group, covid_19_deaths, pop2019) %>%
  group_by(state, race_and_hispanic_origin_group) %>%
  summarize(DRs = (sum(covid_19_deaths)/sum(pop2019)*100000)) %>% arrange(desc(DRs))
#OKAY, FINALLY. THAT TABLE IS WHACK, GO LOOK AT IT. New Mexico, Arizona make sense, they have high pops
#of Native Americans, but did like all of the Pacific Islander population in Arkansas of all places just die off with COVID?
#That death rate there is too high, but this supplies me with a lot of jumping off points. 

#The first of which, I want to run the above code again, but only for the District of Columbia, 
#they had a few high death rates for multipe races
combined %>% filter(!is.na(covid_19_deaths)) %>% filter(!is.na(pop2019)) %>% filter(state == "District of Columbia") %>%
  select(state, race_and_hispanic_origin_group, covid_19_deaths, pop2019) %>%
  group_by(state, race_and_hispanic_origin_group) %>%
  summarize(DRs = (sum(covid_19_deaths)/sum(pop2019)*100000)) %>% arrange(desc(DRs))
#So comparing this with the code above of just DRs for different Race groups, we can see that in D.C. Hispanics and Asians are disproportionately dying from COVID-19

#but this reminds me, I want a count of suppressed rows by race as well. 
combined %>% filter(footnote == "One or more data cells have counts between 1-9 and have been suppressed in accordance with NCHS confidentiality standards.") %>%
  group_by(race_and_hispanic_origin_group) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
#So from my understanding of what the footnote means, it means that we may have totals of up to 9 in multiple
#of theses groups, so the more suppressions the more small totals that can add up.
#What makes this particularly interesting again is that with how high of a population of NHBs there are in America
#And with how high their death rate already is, I would have expected more NHAsians to be suppressed, granted it is a difference of one,
#but still, those two numbers should not be that close. So the DRs we have seen for Blacks are likely significantly higher than already represented
#As are those for American Indians and Pacific Islanders, but with how low they are already, that is expected that they would have higher suppressions
#To avoid identification, but the NHB population should not have that high of DR and that high of suppression count.

#pop of Hispanics in DC
pop %>% filter(name == "District of Columbia") %>% filter(race_eth == "Hispanic")

pop %>% filter(name == "District of Columbia") %>% filter(race_eth == "Hispanic") %>% summarize(sum(pop2019))
#getting distribution of the other highest in DC

pop %>% filter(name == "District of Columbia") %>% filter(race_eth == "Non-Hispanic Black")

#adding sum of Covid-19 deaths to end here
combined %>% filter(!is.na(covid_19_deaths)) %>% filter(!is.na(pop2019)) %>% filter(state == "District of Columbia") %>%
  select(state, race_and_hispanic_origin_group, covid_19_deaths, pop2019) %>%
  group_by(state, race_and_hispanic_origin_group) %>%
  summarize(DRs = (sum(covid_19_deaths)/sum(pop2019)*100000), deaths = sum(covid_19_deaths)) %>% arrange(desc(DRs))





