              #### ASSIGNMENT 2 ####

# Answer the following questions using the dispatch.csv file 
# and the tidyverse functions we've learned 

library(tidyverse)
              
setwd("~/Documents/GitHub/ADJHomework/Data")
#setting the directory to my own. If you want to follow along, just change the directory to where the dispatch.csv
#file is located and the rest should fall into place
Dis <- read_csv("dispatch.csv")
# Question 1: What's the timeframe that this dataset covers?  

# Your code: 

Dis %>% select(CallDateTime) %>% arrange(desc(CallDateTime)) #view the end point
Dis %>% select(CallDateTime) %>% arrange(CallDateTime) #view start point

# The answer

#From 8/23/2021 until 8/29/2021 It is hard to say the start time because AM/PM
#is not taken into consideration when the data is ordered


# Question 2: Which day of the week had the most incidents? 

# Your code: 

Dis %>% group_by(DOW) %>% summarize(count = n()) %>% arrange(desc(count))
#grouping by the day of the week and counting how many values of each day of the
#week are present. Then arranging them by highest to lowest

# The answer: 

#Tuesday it seems. I guess people really do be going to church on Sunday with
#all those crime drops. Or more crime is happening but no one calls it in o_@


# Question 3:  Which hour of the day had the most incidents?

# Your code: 
Dis %>% group_by(Hour) %>% summarize(count = n()) %>% arrange(desc(count))
#grouping the data by the hour since the data table simply has it as a 24 hour
#format, counting the number of occurrences, then arranging by highest to lowest
#count


# The answer: 
#Hour 13 (otherwise known as 1 p.m.)



# Question 4:  Which police district had the most traffic stops?

# Your code: 
Dis %>% filter(ExtNatureDisplayName == "TRAFFIC STOP") %>% group_by(PolArea) %>% summarize(count = n()) %>% arrange(desc(count))
#filtering the data for Traffic stops, grouping the data by the policing 
#area/disctrict (PolArea) then counting the number of times each one shows up 
#then arranging it by highest to lowest number of occurrences



# The answer:
#Area 50E with 27 traffic stops




# Question 5:  What's the most common reason police are dispatched to the airport? (11300 S AIRPORT DR)

# Your code: 

Dis %>% filter(Address == "11300 S AIRPORT DR") %>% group_by(ExtNatureDisplayName) %>% summarize(count = n()) %>% arrange(desc(count))
#Filtering for the airport address, grouping by reason name, counting the 
#occurrences of each sorting high to low by count

# The answer:

#Simply to Check Area, though I guess at an Airport that could be a little more 
#severe


