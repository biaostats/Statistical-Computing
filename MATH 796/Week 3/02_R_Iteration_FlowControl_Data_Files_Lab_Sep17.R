
###'
###' Flow control
###'
###' 1. (2 points) Use if else statements to create a script that returns TRUE if the elements of a vector x, with length 3, are strictly increasing, and FALSE otherwise.
###'  Test your script using x = c(1,2,3) and then x=c(1,3,2)
is.increasing = function(x) {
  if (length(x) != 3)
    stop("The length of input vector should be 3 !")
  
  # if (x[1] < x[2] && x[2] < x[3])
  #   return(TRUE)
  # else
  #   return(FALSE)
  
  return(x[1] < x[2] && x[2] < x[3])
}
###' Comments: I think the above way will be more efficient than using if-else statement. The origional if-else statement is commented.
is.increasing(c(1,2,3))
is.increasing(c(1,3,2))
try(is.increasing(c(1,2)))
###'    
###' 2. (2 points )R function print() print out the input as a string, e.g., print('x') print out 'x', print(x) print out the value of x.
###' Please use if else statements to crate a script that print "Positive number!" if a variable x>0, "Negative number!" if x<0, and "Zero!" if x=0
###' Test your script using x=3,-3 and 0 respectively.
sign.number = function(x) {
  if (x > 0)
    print("Positive number!")
  else if (x < 0)
    print("Negative number!")
  else
    print("Zero!")
}
sign.number(3)
sign.number(-3)
sign.number(0)
###'



###'
###' Iteration
###'
###' 3. (2 points each part)mtcars is a data frame that was extracted from the 1974 Motor Trend US magazine,
###' and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973-74 models).
###' You may need a function unique(x), which returns a vector of the unique values of a vector x.
###' Answer each of the question using no more than three lines of code.
###' 
###' a)  Write a for loop to compute the average mpg for the cars with the same number of cylinders (cyl). Let the output vector to be mpg.by.cyl.for
###' b) Use function split the column mpg by cyl, and call the output list as mpg.list.
###' Then use sapply to calculate the same thing. Let the output vector to be mpg.by.cyl.sapply
###' c) Calculate the same thing using tapply. Let the output vector to be mtcars.by.cyl.tapply. Are your three outputs the same
###' d) Using any of the three above approaches, calculate the average mpg of the cars with the same number of cylinders (cyl) and transmission type (am).
###' Depending on how you calculate it, your output can be a matrix or a vector.
###' 
###' Solution:
###' a.
n.row = nrow(mtcars)
type.cyl = sort(unique(mtcars$cyl))
sum.mpg.by.cyl.for = rep(0, length(type.cyl))
count.type.cyl = rep(0, length(type.cyl))
names(sum.mpg.by.cyl.for) = type.cyl
for (i in 1:n.row) {
  for (j in 1:length(type.cyl)) {
    if (mtcars$cyl[i] == type.cyl[j]) {
      count.type.cyl[j] = count.type.cyl[j] + 1
      sum.mpg.by.cyl.for[j] = mtcars$mpg[i] + sum.mpg.by.cyl.for[j]
    }
  }
}
mpg.by.cyl.for = sum.mpg.by.cyl.for/count.type.cyl
mpg.by.cyl.for
###' b)
mpg.list = split(mtcars$mpg, f = mtcars$cyl)
mpg.by.cyl.sapply = sapply(mpg.list, mean)
mpg.by.cyl.sapply
###' c)
mpg.by.cyl.tapply = tapply(mtcars$mpg, INDEX = mtcars$cyl, mean)
mpg.by.cyl.tapply
###' d) 
mpg.tm.cyl.list = split(mtcars$mpg, list(mtcars$cyl, mtcars$am), drop = T)
mpg.by.tm.cyl.sapply = sapply(mpg.tm.cyl.list, mean)
mpg.by.tm.cyl.sapply

###' 4. (2 points each part, except 4 points for g)Rio Olympics data set
###'  Now we're going to examine data from the 2016 Summer Olympics in Rio de Janeiro, taken from https://github.com/flother/rio2016 (itself put together by scraping the official Summer Olympics website for information about the athletes).
rio = read.csv("https://raw.githubusercontent.com/flother/rio2016/master/athletes.csv")
###' You may need to use R functions table and unique.
###' 
###' a) What kind of object is rio? What are its dimensions and columns names of `rio`? What does each row represent? Is there any missing data?
class(rio)
dim(rio)
colnames(rio)
head(rownames(rio))
head(rio)
###' Comments: According to the above results, each row represnets an anthlete. The rownames of this data do not make sense.
any(is.na(rio))
###' b) How many athletes competed in the 2016 Summer Olympics? How many countries were represented?
length(unique(rio$id))
length(unique(rio$nationality))
###' c) Create a column called `total` which adds the number of gold, silver, and bronze medals for each athlete, and add this column to `rio`.
###' Which athlete had the most number of medals and how many was this? In the case of ties, here, display all the relevant athletes.
rio$total = rio$gold + rio$silver + rio$bronze
(rio[rio$total == max(rio$total),])$name
max(rio$total)
###' d) Using `tapply()`, calculate the total medal count for each sport. Save the result as `total.by.sport`, and print it to the console.
###'  Which sport had the most number of medals, and how many was this?
total.by.sport = tapply(rio$total, INDEX = rio$sport, sum)
total.by.sport
names(total.by.sport[total.by.sport == max(total.by.sport)])
max(total.by.sport)
###' e) The variable `date_of_birth` contains strings of the date of birth of each athlete. Use the `substr()` function to extract the year of birth for each athlete,
###' and then create a new numeric variable called `age`, equal to 2016 - (the year of birth). As always, you can also look at the help file for `substr()` for more details.
###' Add the `age` variable to the `rio` data frame. Who is the oldest athlete, and how old is he/she? In the case of ties, here, display all the relevant athletes.
age = 2016 - as.numeric(substr(rio$date_of_birth,1,4))
rio$age = age
(rio[rio$age == max(rio$age),])$name
max(rio$age)
###' f) Answer the same questions as in the last part, but now only among athletes who won a medal.
rio.medal = rio[rio$total != 0,]
(rio.medal[rio.medal$age == max(rio.medal$age),])$name
max(rio.medal$age)
###' g) Create a new data frame called `sports`, that include the following columns.
###' sport: the names of the sporting events in alphabetical order.
###' n_participant: the number of participants in each sport
###' n_gold: the number of gold metal
###' n_participant_metal: the number of participants that won a metal in each sport.
###' age_youngst: the age of the youngest athlete participating in each sport.
###' age_oldest_male: the age of the oldest male athlete participating in each sport.
###' age_oldest_female: the age of the oldest female athlete participating in each sport.
###' ave_height: the average height of the atheletes in each sport.
###' Display your output matrix in the console. If you see NA's in any of the columns, please revise how you revise that column so that meaningful numbers will be displayed, instead of NA.
sport.name = sort(unique(rio$sport))
n.par = tapply(rio$id, INDEX = rio$sport, length)
n.gold = tapply(rio$gold, INDEX = rio$sport, sum)
n.par.metal = tapply(rio$total, INDEX = rio$sport, function(x) {
  length(x[x != 0])
})
age.youngest = tapply(rio$age, INDEX = rio$sport, min)
rio.male = rio[rio$sex == "male",]
age.oldest.m = tapply(rio.male$age, INDEX = rio.male$sport, max)
rio.female = rio[rio$sex == "female",]
age.oldest.f = tapply(rio.female$age, INDEX = rio.female$sport, max)
mean.height = tapply(rio$height, INDEX = rio$sport, function(x) {
  mean(x[which(!is.na(x))])
})
sports = data.frame(sport = sport.name, n_participant = n.par, n_gold = n.gold, n_participant_metal = n.par.metal,
                    age_youngest = age.youngest, age_oldest_male = age.oldest.m, age_oldest_female = age.oldest.f,
                    avg_height = mean.height)
any(is.na(sports))
sports
