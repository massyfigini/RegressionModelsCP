# REGRESSION MODEL COURSE PROJECT

# 1. obiettivo del progetto
# 2. descrizione del db (?mtcars)

mtcars
summary(mtcars)

#? Is an automatic or manual transmission better for MPG
# column am = transmission (0=automatic, 1=manual)

# exploratory graph 1
# require(graphics)
pairs(mtcars, main = "mtcars data")

# exploratory graph 2
boxplot(mpg ~ am, data = mtcars,
         col  = c("salmon", "dark green"),
         xlab = "Transmission",
         ylab = "Miles per Gallon",
         main = "MPG by Transmission Type",
         names= c("automatic","manual"),
         horizontal= T)   

# manual transimission is better seeing the data

# t-test
auto=subset(mtcars,select=mpg,am==0)
manual=subset(mtcars,select=mpg,am==1)
t.test(auto,manual)

# null hypotesis (mean mpg for automatic and manual are similar) rejected

# regression
regSIM <- lm(mpg~am,mtcars) 
summary(regSIM)     # seems that manual is better (average 7.245 miles plus then automatic)

regTOT <- lm(mpg~.,mtcars)
summary(regTOT)     # now manual is better but not like before (only 2.52 plus then automatic)
# R squared shown that model explains 86.9% of the variance.
# The problem is that all the coefficients aren't significative

# Try to use the stepwise regression method for choice the best variables
regBEST=step(regTOT,trace=0)
summary(regBEST) 
# This model explains 84.97% of the variance and have all the coefficients significative at 5%

