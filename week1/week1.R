# install.packages("swirl")
library(swirl)

setwd("week1")

# 4. If I execute the expression x <- 4 in R, what is the class of the object `x' as determined by the `class()' function?
x <- 4
class(x)

# 5. What is the class of the object defined by the expression x <- c(4, "a", TRUE)?
x <- c(4, "a", TRUE)
class(x) 

# 6. If I have two vectors x <- c(1,3, 5) and y <- c(3, 2, 10), what is produced by the expression rbind(x, y)?
x <- c(1,3, 5)
y <- c(3, 2, 10)
rbind(x,y)

# 8. Suppose I have a list defined as x <- list(2, "a", "b", TRUE). What does x[[2]] give me? Select all that apply.
x <- list(2, "a", "b", TRUE)
x[[2]]

# 9. Suppose I have a vector x <- 1:4 and a vector y <- 2. What is produced by the expression x + y?
x <- 1:4
y <- 2
x + y

# 10. Suppose I have a vector x <- c(3, 5, 1, 10, 12, 6) and I want to set all elements of this vector that are less than 6 to be equal to zero. What R code achieves this? Select all that apply.
x <- c(3, 5, 1, 10, 12, 6)
x[x<6] <- 0
x[x %in% 1:5] <- 0
x[x<=5] <- 0

## Use the Week 1 Quiz Data Set to answer questions 11-20.
table <- read.csv("hw1_data.csv")

# 11. In the dataset provided for this Quiz, what are the column names of the dataset? 
colnames(table)

# 12. Extract the first 2 rows of the data frame and print them to the console. What does the output look like?
table[1:2,]

# 13. How many observations (i.e. rows) are in this data frame?
dim(table)

# 14. Extract the last 2 rows of the data frame and print them to the console. What does the output look like?
table[152:153,]

# 15. What is the value of Ozone in the 47th row?
table[47, "Ozone"]

# 16. How many missing values are in the Ozone column of this data frame?
sum(is.na(table$Ozone))

# 17. What is the mean of the Ozone column in this dataset? Exclude missing values (coded as NA) from this calculation.
mean(table$Ozone, na.rm = TRUE)

# 18. Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?
ozone <- table$Ozone > 31
temp <- table$Temp > 90
mean(table[ozone & temp, ]$Solar.R, na.rm = TRUE)
table2 <- table[ozone & temp, ]

# 19. What is the mean of "Temp" when "Month" is equal to 6? 
june <- table$Month == 6
mean(table$Temp[june], na.rm = TRUE)

# 20. What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?
may <- table$Month == 5
max(table$Ozone[may], na.rm = TRUE)
