#' ---
#' title: "R Basics Part 1"
#' author: "Qi Zhang (including materials from Dr. Ryan Tibshirani's class at CMU)"
#' date: "August 30, 2021"
#' ---
#' 
#'  Objectives: this lecture note introduces the basic data types and operator in R
#' 
#' 
###' "Everything that exists is an object.
###' 
###' Everything that happens is a function call."
###' 
###' --- John McKinley Chambers
###' 
###' 
###' Two basic types of things/objects: data and functions
###' 
###' * Data: things like 42, "forty-two", 42.000, and $$\begin{array}{ccc} 1 & 2 & 3\\ 4 & 5 & 6 \end{array}$$
###' 
###' * Functions: things like log, +, < , %%, and median. 
###' 
###' A function turns the input object/arguments, into an output object, according to a defnite rule.
###' 
###' Programming is writing functions and apply to transform inputs into outputs.
###' 
###' * Good programming ensures the transformation is done easily and correctly.
###' 
###' * The trick to good programming is to take a big transformation and break it down into smaller ones, and then break those down, 
###' until you come to tasks which are easy (e.g., using built-in functions)
###' 
###' #### Data
###' 
###' At base level, all data can represented in binary format, by bits (i.e., TRUE/FALSE, YES/NO, 1/0). The following are the basic data types:
###' 
###' * Booleans/Logical: Direct binary values: TRUE or FALSE in R
###' * Integers: whole numbers (positive, negative or zero), represented by a fixed-length block of bits
###' * Characters: fixed-length blocks of bits, with special coding; strings: sequences of characters
###' * Floating point numbers: an integer times a positive integer to the power of an integer, as in $3\times 10^6$ or $1\times 10^{3}$. When it is too large, it is Inf or -Inf.
###' * Missing or ill-defined values: NA, NaN, etc.
###' 
###' #### Operators
###' 
###' * Unary: take just one argument. E.g., - for arithmetic negation, ! for Boolean negation
###' * Binary: take two arguments. E.g., +, -, *, and /. Also, %% (for mod), and ^

-42
42 + 5
42 - 5
41 * 5
4 ^ 2
42 / 5
42 %% 5

###' The operators can be snacked. What are the values of the following?
4-2*5
4+2%%5

###' Comparison operators
###'
###'These are also binary operators; they take two objects, and give back a Boolean

4 > 5
4 < 5
5 >= 4
4 <= 5
4 == 5
4 != 5

###' Warning: == is a comparison operator, = is not!
###' 
###' Logical operators
###'   These basic ones are & (and) and | (or)

(5 > 8) & (6 * 7 == 42)
(5 > 8) | (6 * 7 == 42)
(5 > 8) | (6 * 7 == 42) & (0 != 0)
(5 > 8) | (6 * 7 == 42) & (0 != 0) | (10 - 8 >= 0)

###' Note: The double forms && and || have different meanings. We'll see them later
###' 
###' "#" is for comments. Whatever after # symbol is a line will not run. Usually we use it to add annotations to our code, 
###' or use it for old code that we do not want to use, but do not want to delete completely now.

4+5 # 4 plus 5

# 4+5 this code will not run because I "comment" it away.


###' #### Data type
###' 
###' The typeof() function returns the data type
###' 
###' is.foo() functions return Booleans for whether the argument is of type foo
###' 
###' as.foo() (tries to) "cast" its argument to type foo, to translate it sensibly into such a value
###' When it is not sensible to translate, it returns NA.
###' 
###' class() function returns the class of the object, it returns what "type" of an object is from the point of view of object-oriented programming in R (e.g., a specific R package). 
###' 
###' typeof() returns the type of data from the point of view of R itself.
typeof(4)
is.numeric(4)
is.na(4)
is.na(4/0)
is.na(0/0)
is.character(4)
is.character("4")
is.character("four")
is.na("four")
as.numeric('four')
as.numeric('4')

as.character(5/4)
as.numeric(as.character(5/4))
4 * as.numeric(as.character(5/4))
5/4 == as.numeric(as.character(5/4))

###' #### Variables
###' Data can have names. We can assign the data objects to names, which give us variables. Some variables are built-in:

pi
###' Variables can be arguments to functions or operators, just like constants:

pi * 10
cos(pi)
###' We create variables with the assignment operator, <- or =

approx.pi <- 22/7
approx.pi
radius = 10
radius
approx.pi * radius^2

###' The assignment operator also changes values:

area <- approx.pi * radius^2
area
area = 30
area

###' In most of cases, "<-" and "=" works the same as assignment operator. 
###' But there are actually some subtle differences.
###' We will discuss some of these differences when it matters in practice.
###' Some people argue that "<-" should be preferred over "="
###' At least, we should use the same type of assignment operator throughout one R script.
###' 
###' You will write code using many variables.
###' It will make your life much easier if the names of your variables are descriptive, 
###' easier to design, easier to debug, easier to improve, and easier for others to read.
###' Please try to limit use of fixed constants in code; instead use named variables
###' Named variables are a first step towards abstraction
###' 
###' The following is one example of such abstraction
###' This code check whether the area of a circle is big

area = 30
(area>10)

###' Here the criterion of a big circle is whether its area is bigger than 10.
###' If we decide to change the cutoff to 40. We have to do the following
(area>40)

###' An alternative is to define a variable for the cutoff
cutoff.big = 10
(area>cutoff.big)

###' Then when we want to change the cutoff, we only need to change of value of the cutoff, and do not need to change the code for comparison.
cutoff.big = 40
(area>cutoff.big)




###' #### Practice 
###' 
###' Please write R programs to answer the following questions.
###' 
###' (1) Is 379 plus 83 times 48 lager than 38 times 94 minus 61?
###' 
###' (2) What is the data type of pi? What about 'pi'? We know that pi is a built-in constant, can you convert 'pi' to numeric?
###' 



###' #### Useful commands in R workspace
###' ls() command shows you What have been defined in the environment.
ls()
###' Getting rid of variables:
rm("area")
ls()
rm(list=ls()) # Be warned! This erases everything
ls()
###' #### R packages
###' The functions that we have been using so far are "built-in" functions that we can use directly when we open R.
###' 
###' R is well-known for large repository of R packages that contains various functions for general and customized tasks.
###' 
###' An R package is just a bunch of R functions written and compiled by someone.
###' R/RStudio provides no guarantee on the quality of these packages. Only the authors do, if they want to. 
###' 
###' These functions cannot be used directly. You need to install these packages first, and then load it to the R session that you are currently working on.
###' If the name of a package that you have installed is called "magicHat", then run the line library(magicHat) will load the functions in this packge to the R session.
###' 
###' Depending on the R you are using (Rstudio or not), and where the pacakge is hosted, there are many ways of installing packages. 
###' 
###' * The most common way is to use command install.packages, install.packages(c('magicHat','magicGloves','magicbowtie')). It installs R packages named magicHat, magicGloves, and magicbowtie hosted on CRAN https://cran.r-project.org/
###' This is the main repository of R packages. Each package has a webpage on CRAN where you can also find the manual, and some other useful information.
###' * If you are using Rstudio, you can also click "Tools--> install packages", and select the packages to be installed from the menu.
###' Either way, you may be asked to choose a mirror of CRAN where these files will be downloaded. Usually I choose one that is close to me geographically. 
###' * Another significant source of R packages is Bioconductor https://bioconductor.org/. This is extremely useful especially if you are interested in analyzing biological data.
###' But some of the packages actually can be applied to other data, and rather general. Bioconductor packages are usually better ducumented, and may contain a detailed tutorial with examples.
###' To install a Bioconductor package, you need to install the Bioconductor (installer) first, and then run the commands that install a specific package. Please refer to Bioconductor website for more details.  
###' * More and more people start to host R packages on their on Github page. For some of them, you may be able to install them directly.
###' There is an install_github function to install R packages hosted on GitHub in the devtools package. But it requests developer's name.
###' install_github("DeveloperName/PackageName")
###' 
###' In this course, we will only use R packages from CRAN.
###' 
###' #### R markdown and knitr package.
###' kinitr package allows you to write documents that blend text and code seamlessly.
###' Then you just need to knit (compile) it into a document. 
###' 
###' There are various ways of creating such a document. We will only cover one way in this course, because it is how you will create your submission for lab.
###' 
###' In a R script, all lines starting with " #' " will be recognized as text in R markdown file.
###' 
###' You may choose to compile the document as pdf or html, and I prefer html. 
###' 
###' #### The most important operator in R
###' ?
###' 
###' Type ?function_name in the console will bring you the help page of that function, if it exists.
###' 
###' Usually this help page contains the meaning of each input argument of that function, output data type, default inputs, and some additional details, and a usage example.
###' 
###' If you still have doubt about a function's usage after reading the help page, sometimes it may be helpful to test the function. 
###' 
###' That is, consider some hypothetical input of the function, predict the output in your head, then feed the input to the function to see whether it is consistent with your prediction.
###' If not, try to revise the prediction model in your head based on the actual output you see.
###' 
###' #### Practice
###' 
###' 1. Install R packages ggplot2
###' 
###' 2. Knit this lecture note.
###' 
###' 
###' #### Basic data structure
###' 
###' A data structure is a grouping of related data values into an object
###' 
###' Vector: A vector is a sequence of values, all of the same type
###' 
###' #### Creating vectors
###' The c() function returns a vector containing all its arguments in specified order.
x = c(3, 5, 10, 42)
x
is.vector(x)

###' vector(length=n) returns an empty vector of length n; helpful for filling things up later
solar.planets = vector(length=8)
solar.planets
solar.planets[3] = 'earth'
solar.planets

###' Creating specific vectors:
###' A sequence of consecutive integers
1:5
-1:5
###' More flexible way of generating a sequence 
seq(from=1,to=5)
seq(from=1,to=5,by=0.5)
seq(from=1,to=5,by=0.7)
seq(from=1,to=5,length.out=6)

###' Replications of the same value (sequences)
rep(2,3)
rep('earth',3)
rep(c(1,3),3)
rep(c(1,3),each=3)

###' #### Vector arithmetic
###' Arithmetic operator apply to vectors in a "component-wise" fashion

y = c(-3, -5, -10, -42)
x + y
x * y
###' Recycling:
###' When the vectors have different lengths, the elements in shorter vector are repeated (recycled) when the shorter vector runs out.

x + c(-7,-8)
x^c(1,0,-1,0.5)

###' Single numbers are vectors of length 1 for purposes of recycling:
2 * x

###' Can do component-wise comparisons with vectors:

x > 9
###' Logical operators also work elementwise:
(x > 9) & (x < 20)

###' To compare whole vectors, best to use identical() or all.equal():

x == -y
identical(x, -y)
identical(c(0.5-0.3,0.3-0.1), c(0.3-0.1,0.5-0.3))
all.equal(c(0.5-0.3,0.3-0.1), c(0.3-0.1,0.5-0.3)) # ignore the differences.

###' What?!
###' 
###' Reason: numbers in computer (floating point numbers) are not exactly the same as the numbers on the paper (real numbers in math).
0.5-0.3
0.3-0.1
(0.5-0.3)-(0.3-0.1)

###' This may have practical implications in flow control and indexing if your conditionals in flow control and indeces when indexing depends on the equality of two variables.
###' We will discuss this when we encounter it.
###' 
###' 
###' #### Indexing vectors
###' Vector of indices:
x = c(3, 5, 10, 42)
y = c(-3, -5, -10, -42)
x[c(2,4)]
###' Vector of negative indices (everything but .. ):
x[-c(1,3)]
###' Boolean vector (everything such that):
x[x > 9]
y[x > 9] # Interesting !
###' which() gives the elements of a Boolean vector that are TRUE:
indices <- which(x > 9)
indices
y[indices]

###' #### Named components
###' We can give names to elements/components of vectors, and index vectors accordingly

names(x) = c("v1","v2","jane","john")
names(x)
x[c("jane","v1")]
###' For a named vector, R prints the names. What you saw were not additional components of x
###' 
###' names() returns another vector (of characters):
names.x <- names(x)
names.x
names(y) <- names(x)
which(names(x) == "jane")

###' #### Functions on vectors
###' Many functions can take vectors as arguments:

###'   mean(), median(), sd(), var(), max(), min(), length(), and sum() return single numbers
###' sort() returns a new vector
sort(x)
sort(names.x)
###' hist() takes a vector of numbers and produces a histogram, a highly structured object, with the side effect of making a plot
###' 
###' ecdf() similarly produces a cumulative-density-function object
###' 
###' summary() gives a five-number summary of numerical vectors
###' 
###' any() and all() are useful on Boolean vectors
###' 

###' #### Practice 
###' Please write R program to answer each of the following questions.
###' 
###' We will use an R built-in dataset LakeHuron. You can get the dataset using command data(LakeHuron). 
###' This dataset record Annual measurements of the level, in feet, of Lake Huron 1875--1972. 
###' 
###' a) Display the first 10 elements of this vector
###' 
###' b) create a variable named year whose values are the corresponding years of LakeHuron.
###' 
###' c) Name the elements of LakeHuron by year, and display the first 10 elements of this vector again.
###' 
###' d) Run identical(year,names(LakeHuron)). Does the output make sense? Why or why not?
###' 
###' e) Extract the sub-vector corresponding to 1912-1919
###' 
###' f) Find the year with the highest level during 1940-1960
###' 
###' g) Find the year with the second lowest level during 1940-1960
###' 
###' h) look at the help page of functions any and all. Use one of them to determine if the average level before 1900 has been ever reached after 1955. 
###' 

data(LakeHuron)
# a)
LakeHuron[c(1:10)]
# b)
year = as.numeric(time(LakeHuron)) 
# c)
names(LakeHuron) = time(LakeHuron)
LakeHuron[c(1:10)]
# d)
identical(year, names(LakeHuron))
## Char vector is compared with the Num vector.
# e)
LakeHuron[year >= 1912 & year <= 1919]
# f)
sub.indcies = as.character(c(1940:1960))
M = max(LakeHuron[sub.indcies])
ind = which(LakeHuron[sub.indcies] == M)
names((LakeHuron[sub.indcies])[ind])
# Ans:
sort(LakeHuron[year >= 1940 & year <= 1960], decreasing = T)[1]
# g)
L.sub = LakeHuron[sub.indcies]
L = min(L.sub)
Low = which(L.sub == L)
m = min(L.sub[-Low])
names(L.sub[L.sub == m])
# Ans:
sort(LakeHuron[year >= 1940 & year <= 1960], decreasing = F)[2]
# h)
any(LakeHuron[year>1955] >= mean(LakeHuron[year<1900]))
