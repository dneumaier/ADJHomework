#Introduction to vectors
name <- "David" 
#created variable for my name
id <- "****0956" 
#created variable for my student id name
class(name) 
#checking the class of my variable name
names <- c("David", "John", "Neumaier", 3) 
#creating a variable for all the names (and including a number for shiggles) I go by

print(names) 
#printing the variable for all my names
names[2]
#getting what my second name (or middle name is)

lista <- list("hello", "goodbye", 1, 5) #creating a list
length(lista) #checking the length of the script

listb <- list(c("hello", 5), c("apple", 21, "blueberry")) #creating a compound list
length(listb) #checking the length of the compound list
listb[1] #pulling the first value of the compound list

students <- data.frame(student_id = c(1:5), name = c("David","Bailey","Grace","Andrea","Roshae")) 
#A list of students in the class
print(students) 
#printing the small dataframe I made for my classmates
students$student_id #recognizing the value in the dataframe
