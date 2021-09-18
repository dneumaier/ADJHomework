## LOAD TIDYVERSE AND THE DATA FILE
library(tidyverse)
baltimoreSals <- read_csv("Data/Baltimore_City_Employee_Salaries.csv")


## INTEGRITY CHECKS

# Is any individual in here twice? Why?
baltimoreSals %>%
  group_by(Name) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
#many are listed in here multiple times (this just gets us the 
#count of each number of individual)

#using Thomas,Stacey as an example as he is the most frequent
baltimoreSals %>%
  filter(Name == "Thomas,Stacey")

#It appears the same person appears for different years in addition to different
#jobs


# How many years of data do we have?
baltimoreSals %>% summarize(years = range(FiscalYear))

#From 2011 to 2020, so inclusively 10 years

#I would also like to note I attempted this at first with HireDate before realizing 
#that was irrelevant even for people who were hired for multiple jobs, 
#it was keeping track of Salaries over the years

# What's the min and max annual salary given out in 2020? 

baltimoreSals %>% filter(FiscalYear == "FY2020") %>%
  summarize(minMax = range(AnnualSalary))

#Minimum was $0, Maximum was $275,000

# What jobs get paid $0?
baltimoreSals %>% filter(AnnualSalary == "0") %>%
  select(JobTitle) %>%
  group_by(JobTitle) %>%
  summarize(count = n())

#Election Judges Regular, (both with and w/o parentheses), AIDE BLUE CHIP, 
#Community Health Nurse School Health Aide, Recreation Leader II


# How clean are the job titles? Are there a lot of duplicates?

#Just going off what I saw in my output above, not clean. Duplicates with slight 
#differences, I also remember seeing some lowercase some uppercase when just looking
#at the data, some also included things like "10 months" in the title. OpenRefine would have its
#hayday with this dataset


# Clean up the JobTitles by making everything lowercase 
# (hint: use mutate to overwrite the current JobTitle field, using the function str_to_lower())
baltimoreSals <- baltimoreSals %>% mutate(JobTitle = (str_to_lower(JobTitle)))

# Take a look at agency names; how clean are they? 

#By perusing the data myself, they look pretty consistent, but here's a group_by view

baltimoreAgencies <- baltimoreSals %>% select(AgencyName) %>%
  group_by(AgencyName) %>%
  summarize(count = n())

view(baltimoreAgencies)
#But looking at this there are a lot of typing errors like incomplete names 
# (like Enfor instead of Enforce), there is an instance of BPD for each district
#it looks like, A lot of separate numbers after each agency


## QUESTIONS OF THE DATA

# Who's the highest paid employee in FY2020?

baltimoreSals %>% filter(FiscalYear == "FY2020") %>%
  select(Name, AnnualSalary) %>%
  arrange(desc(AnnualSalary))

#The highest paid (individual) employee in FY2020 is Michael S Harrison

# Which job title has the highest average salary in 2020? (hint: use mean() )
# Any potential problems with citing these results? 

baltimoreSals %>% filter(FiscalYear == "FY2020") %>%
  group_by(JobTitle) %>%
  summarize(avg = mean(AnnualSalary)) %>%
  arrange(desc(avg))

#Highest average is the same as the highest paid (just one position). Potential
#issues with outliers of few/singular individuals being in 
#charge of a high paying job.


# How many people work in the police department in 2020?
baltimoreSals %>% filter(FiscalYear == "FY2020") %>%
  filter(str_detect(AgencyName, "Police Department")) %>%
  summarize(count = n())

#or, to test the method you all used in class, which seems like the same thing really

baltimoreSals %>% filter(FiscalYear == "FY2020") %>%
  filter(grepl("Police Department", AgencyName)) %>%
  summarize(count = n())

#3208 people worked in the police department in 2020

# How many are "police officers"? 
baltimoreSals %>% filter(FiscalYear == "FY2020") %>%
  filter(str_detect(AgencyName, "Police Department")) %>%
  filter(str_detect(JobTitle, "police officer")) %>%
  summarize(count = n())

#2058 of those who worked at the police department in 2020 are police officers
#I am reading the question as a follow up on the previous one. Remove the filters
#for FY2020 and the first str_detect to apply it generally (assuming there even 
#are those outside of the police department listed as police officers)

# What was their total salary?
baltimoreSals %>% filter(FiscalYear == "FY2020") %>%
  filter(str_detect(AgencyName, "Police Department")) %>%
  filter(str_detect(JobTitle, "police officer")) %>%
  summarize(sumSal  = sum(AnnualSalary))
#Their combined total salary was $146,697,758