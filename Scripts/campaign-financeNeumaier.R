library(tidyverse)

# fix the paths below as needed:
contrib <- read_csv("Data/mo_contributions.csv")
cands <- read_csv("Data/mo_candidates.csv")
comms <- read_csv("Data/mo_committees.csv")

#changes
contrib <- contrib %>% mutate(new_date = as.Date(transaction_dt, "%m%d%Y"), .after=transaction_dt)


contrib %>% count(transaction_tp)


#type   description
#-------------------
# 15   | contrib from an individual
# 15C  | candidates contributing to themselves
# 15E  | earmarked from an intermediary
# 22Y  | refund to an individual
# 24K  | donation from a PAC to a candidate

### when transaction_tp == "24K", the filer (cmte_id) is a PAC giving to a candidate's principal campaign committee (other_id)
### for everything else, the filer (cmte_id) is the candidate's principal campaign committee, receiving individual contributions


contrib %>% summarize(total = sum(transaction_amt))
#total amount contributed, 7,945,192

#total given by individual contributions
contrib %>% filter(transaction_tp %in% c("15", "15E", "22Y")) %>% 
  summarise(total = sum(transaction_amt))

#total given by PACs
contrib %>% filter(transaction_tp == "24K") %>% 
  summarise(total = sum(transaction_amt))

#total given to self
contrib %>% filter(transaction_tp == "15C") %>% 
  summarise(total = sum(transaction_amt))

#most individual contributions
contrib %>% inner_join(cands, by=c("cmte_id"="pcc")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y")) %>%
  group_by(cand_name) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))

#most PAC money, because we want the name, we have to join to the cands name
contrib %>% inner_join(cands, by=c("other_id"="pcc")) %>%
  filter(transaction_tp == "24K") %>%
  group_by(cand_name) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))

#which cand is giving most to self
contrib %>% inner_join(cands, by=c("cmte_id"="pcc")) %>%
  filter(transaction_tp == "15C") %>%
  group_by(cand_name) %>% 
  summarise(total = sum(transaction_amt)) %>%
  arrange(desc(total))

#which PAC is giving the most money, we want to know the name of the PAC we have to join the comms table
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(transaction_tp == "24K") %>%
  group_by(cmte_nm) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))

#Which cand is getting the most money from WINRED
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(cmte_nm == "WINRED") %>% 
  group_by(name) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))
#We wanted to start with this, but then noticed our local rep who is running for house (and is very Republican) was not receiving any money from WINRED for her race for the Senate

#Vicky is not receiving support from WINRED, interesting, still a lot going to Josh, which PACs are supporting her the most?
vickyPAC <- contrib %>%  inner_join(comms, by=c("cmte_id")) %>%
  filter(name == "VICKY HARTZLER FOR CONGRESS" & transaction_tp == "24K") %>%
  group_by(cmte_nm) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))
#The top 3 are all giving her $10K EMPLOYEES OF NORTHROP GRUMMAN CORPORATION PAC, L3HARRIS TECHNOLOGIES, INC. PAC and POET PAC, two of which are weapons companies


#this ended up not working how I wanted, but doesn't look like it matters
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(cmte_nm == "EMPLOYEES OF NORTHROP GRUMMAN CORPORATION PAC" | cmte_nm == "L3HARRIS TECHNOLOGIES, INC. PAC" | cmte_nm == "POET PAC") %>%
  group_by(name) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))
#Who else are those three giving to? Not many others. Graves and Jason Smith are the other two. 

#This is what I wanted, but yeah, not much changed
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(cmte_nm == "EMPLOYEES OF NORTHROP GRUMMAN CORPORATION PAC" | cmte_nm == "L3HARRIS TECHNOLOGIES, INC. PAC" | cmte_nm == "POET PAC") %>%
  group_by(cmte_nm, name) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))
#And it turns out that only Employees of Northrop grumman corporation PAC are giving to other reps (of those three). And it was here that it occured to me, I was doing this all for the house
#And not for her running for Senate

vickySEN <- contrib %>%  inner_join(comms, by=c("cmte_id")) %>%
  filter(name == "VICKY HARTZLER FOR SENATE" & transaction_tp == "24K") %>%
  group_by(cmte_nm) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))
#when it comes to Senate, a lot more actually, now I want to know about her top 2 donators for Senate, who else are they giving to?
#And a quick note, nebraska giving to a MO candidate, is kind of whack to me.
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(cmte_nm == "FAMILY RESEARCH COUNCIL ACTION POLITICAL ACTION COMMITTEE" | cmte_nm == "NEBRASKA SANDHILLS PAC") %>%
  group_by(cmte_nm, name) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))
#But it looked like they were only giving to Vicky

#Then some other questions
#Which individual gave the most?
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y")) %>%
  group_by(tres_nm) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))
#Purpura, Salvatore gave the most over all and considering that he is on Hawley's team (as someone else found out) kinda makes sense as seen above that Hawley has a major lead finance wise

#No onne contributed to other candidates
contrib %>% inner_join(cands, by=c("cmte_id" = "pcc")) %>%
  filter(transaction_tp == "24K")
  
#Outta state contribs from individuals
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y") & state != "MO") %>%
  group_by(cmte_nm) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))
#VICKY is 5th among those running for senate, 12th over all for out of state contributions

#In state contribs from individuals
contrib %>% inner_join(comms, by=c("cmte_id")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y") & state == "MO") %>%
  group_by(cmte_nm) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))
#And here she is also 5th among those running for Senate, but 8th over all for in-state

#In short, based on everything we have seen, at least from a financial standpoint (as of right now) our local rep may soon just loose her position in the house and not make it to the Senate.
#She simply is not getting the support she will need to really compete in the race. She has been the rep for 10 years at this point, so with her not running for congress anymore, what does that mean for 
#the future of central Missouri.

#At minimum we can report just on the numbers and at maximum, with interviews with some of the other contestants running for her spot in our district, we can see (through the political speech) what may be in store
#for central Missouri. Alternatively, we could ask her what she thinks her chances are for winning the election to the Senate spot (and no offense to her, but she is not well liked by a lot of people here in Como, and I think she knows that)

#Possible headline: Vicky Hartzler set to (unofficially) retire from congress. 

#kinda true, kinda just a burn ^

#I recognize I have more code than those my peers may have had in my group, but I just noticed some things much later than I should have. 

#woopsie doodle this is better for where does Hartzler Rank in PAC giving first for Senate Candidates
contrib %>% inner_join(cands, by=c("other_id" = "pcc")) %>%
  filter(transaction_tp == "24K") %>%
  group_by(cand_name, office, other_id) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))


#better for individual contribs
contrib %>% inner_join(cands, by=c("cmte_id" = "pcc")) %>%
  filter(transaction_tp %in% c("15", "15E", "22Y")) %>%
  group_by(cand_name, office, cmte_id) %>%
  summarize(total = sum(transaction_amt)) %>%
  arrange(desc(total))

