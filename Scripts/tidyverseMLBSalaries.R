library(tidyverse) #bringing in the library

MLB <- read_csv("/Data/MLB_Salaries_1.csv) #setting the data to be readable

MLB$Salary

MLB %>% arrange(Salary)

MLB %>% select(Name, Salary) %>% arrange(Salary)