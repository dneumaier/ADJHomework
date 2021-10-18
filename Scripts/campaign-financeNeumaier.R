library(tidyverse)

# fix the paths below as needed:
contrib <- read_csv("Data/mo_contributions.csv")
cands <- read_csv("Data/mo_candidates.csv")
comms <- read_csv("Data/mo_committees.csv")

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
