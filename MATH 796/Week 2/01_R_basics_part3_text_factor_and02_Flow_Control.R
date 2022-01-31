#' ---
#' title: "R Basic Part 3- text manipulation, Flow control"
#' author: "Qi Zhang (including materials from Dr. Ryan Tibshirani's class at CMU)"
#' date: "September 1, 2021"
#' ---
#'
#'
###'
###' #### Text manipulation
###'
###' Text are in character class in R. A string is just a sequence of characters bound together.
###'
###' Why do we need to learn how to handle text?
###'
###' * A lot of interesting questions can be addressed using text data
###' * Data cleaning and data fusion may need text manipulation. For example, you may need to merge text data from different sources with slightly different format
###'  (e.g., full names with full middle name, no middle name or only initial of the middle name), and how can you match them (semi-)automatically.
###' * Text manipulation is useful in automating data analysis that requires input/output path and file names and plot titles.

###' Define a string.
###' " " is for space, "\t" is for tab, and "\n" is for new line.
my.str <- 'The answer is \n \n \t 42'
my.str
print(my.str)

###' Use cat() to print strings to the console, displaying whitespaces properly

cat(my.str)

###' The character is a basic data type in R, and we can define vectors or matrices of characters/strings.

my.str.vec <- c('The answer', 'to', 'life, the universe and everything','is', '\n\n', '42')
my.str.mat <- matrix(my.str.vec,3,2)
my.str.vec
my.str.mat
my.str.mat[1,]

###' You can convert other data types to characters, and charcters to others when it is possible.

as.character(42)
as.numeric('42')
as.character(0.5e-3)
as.numeric('0.5e-3')
as.numeric('Hello, world!')
as.logical('T')
as.logical('TR')


###' The following are some of the most frequently used commands for text manipulation.
###'
###' * number of characters in each element of the input string vector.
nchar(my.str.vec)
###' * substr() can be used to extract substrings or substitute the substring
###'

substr(my.str.vec,2,3)

my.str.vec2 <- my.str.vec
substr(my.str.vec2,2,3) <- 'OK'
my.str.vec2


###' * strsplit() split each element of the string vector based on a pattern. the output is a list, even when there is only one input string
my.str.list <- strsplit(my.str.vec,split = 'n')
my.str.list
class(strsplit(my.str.vec[1],split = 'n'))



###' * paste() combine strings, very useful in generating file path and names and plot titles

my.str.vec.towel <- c('I','really','know','where','my','towel', 'is')

paste(my.str.vec, my.str.vec.towel,sep = '------')

###' paste0(...) is identical to paste(...,sep='')
paste0(my.str.vec,my.str.vec.towel)
paste(my.str.vec,my.str.vec.towel,sep='')


###' paste also condenses a vector of strings as a single string
paste(my.str.vec,collapse = ' ')


###' If collapse and sep arguments are used jointly, the two string vectors are combined as a single vector fist, and then condensed as a single string.
paste(my.str.vec,my.str.vec.towel, collapse = ' ',sep = '-----')


#' * grep(pattern, x) returns the indecies or the value of the elements of string vector x that contains pattern.
#' grepl returns a logical vector of the same length, indicating whether it contains the pattern.
grep('n',my.str.vec,value = F,fixed=T)
grep('n',my.str.vec,value = T,fixed=T)
grepl('n',my.str.vec,fixed=T)

#' * gsub(pattern, replacement,x) replace the substring pattern found in x by the string replacement
#' This is very useful in data cleaning and merging text data.
gsub('n','MMMMM',my.str.vec,fixed = T)


###' #### Practice:
###' state.name is a vector that include the names of the 50 states in US. Define a vector my.state.name = state.name
###' Answer each of the question using one line of code.
###'
###' 1. Count the number of characters in each state
###'
###' 2. Count the number of characters in each state, excluding the space.
###'
###' 3. Find all the state names with letter b.
###'
###' 4. Paste all state names together as a single string,separated by "|".
###'
###' 5. Calculate the frequency of the initial letters of the state names. You need a R function named table
###'
my.state.name = state.name
#1.
nchar(my.state.name)
#2.
my.state.name.nosp = gsub(' ', '', my.state.name, fixed = T)
nchar(my.state.name.nosp)
#3.
grep('b', my.state.name, value = T, fixed = T)
#4.
paste(my.state.name, collapse = '|')
#5.
my.state.init = substr(my.state.name, 1, 1)
table(my.state.init)


###'
###' Apply text manipulation in data preprocessing and pipeline building
###'
###' While text data itself is very important, we will not get into text data analysis in this course.
###' Instead, we will focus on using text manipulation to manipulate data frame, matrix or files.
###' It will facilitate data cleaning, and building more automated data analysis pipelines.
###' Let us consider the example of the state data that we have considered before.
state.df = data.frame(state.x77, Region=state.region, Division=state.division)
#' Previously, you have learned how to extract the sub-data.frame based on the value of a certain variable.
#' For example, if we want to create a data frame for New England, we can do the following
state.df.ne.2 = state.df[state.df$Division == "New England", ]
#' The variable Division has nine distinct values, representing different geographical regions.
unique(as.character(state.df$Division))
#' Some of these regions may have similar chracteristics, and we may want to create a data frame for not just one but several relevant regions.
#' For example, we may want to extract the data for all the regions in the south, defined by whether the name of the division contains "South"
#' If we know exactly what regions to be included, we can use is.element() function to create the logical vector that extracts the rows.
ix.south.1 = is.element(state.df$Division,c('East South Central','West South Central','South Atlantic'))
head(ix.south.1)
sum(ix.south.1)
state.df.south.1 = state.df[ix.south.1,]
#' This only works if you know exactly you are looking for, and may not work as expected if there is any slight issues
ix.south.2 = is.element(state.df$Division,c('East South Central','West South Central ','South Atlantic'))
head(ix.south.2)
sum(ix.south.2)
state.df.south.2 = state.df[ix.south.2,]
#' The above two data frames are different, why?
#' If the regions that you are looking for share some similar yet distinct feature in text, we can extract these rows without knowing the exact names of these regions.
ix.south.3 = grepl('South',state.df$Division)
head(ix.south.3)
sum(ix.south.3)
state.df.south.3 = state.df[ix.south.3,]
#' The advantage of this approach is that we do not need to know the exact names of the divisions to be extracted.



###' Text manipulation also helps us to manipulate files.
###'  Before that, we need to understand the directories.
###' Your working directory is where your temporary R data is saved at.
###' When saving or reading a file, if a path is not specified, R will only focus on this directory.
###'
getwd()
###' You can set your working directory using setwd()
dir.data = "D:\\Dropbox\\courses\\stat_computing\\dataexample\\"
setwd(dir.data)

###' You can create a directory if its parent directory exists.
###'
dir.new = "D:\\Dropbox\\courses\\stat_computing\\dataexample\\nonsense\\"
dir.create(dir.new)

###' You can list the files in any specific directory using function list.files. It output a string vector of these files.
filenames = list.files(path = "D:\\Dropbox\\courses\\stat_computing\\dataexample\\")
filenames = list.files(path = "H:\\TimeSeriesTest") # pattern for subsetting.
head(filenames)

###' Sometimes we need to process data stored in many files, and we only want to analyze a subset of them.
###' So we need to identify these files.
###' For example, in the folder dataexample, there are many files for state level Covid data, including pdf and csv files.
###' If we only want to analyze the csv file for Montana, we can identify the file name by
file.to.be.analyzed = filenames[grepl('Convergence',filenames)&grepl('.pdf',filenames)]
file.to.be.analyzed
#' We may want to customize the name of the output file so that it is tied to the input file.
#' For example, we can do the following.
file.output = gsub('.pdf','_output.txt',file.to.be.analyzed)
file.output
#' to make sure that the output file is in the right directory, you may want to paste them together using R function paste0.
#'
dir.new = "H:\\TimeSeriesTest"
paste0(dir.new,file.output)

###'
###' #### Practice
###' 1. Consider the state.df data frame.
###' (a) Extract the rows whose division contain "North"
###' (b) Calculate the average high school graduation rate (HS.Grad) for the states whose names contains letter "c"
###' (c) Calculate the average Income for the states whose names start with letter "N"
###'
my.state.df = state.df
#1. a)
my.state.df[grepl('North', my.state.df$Division),] 
# b)
mean(my.state.df[grepl('C', row.names(my.state.df)),"HS.Grad"])
# c)
mean(my.state.df[grepl('N', substr(row.names(my.state.df),1,1)),"Income"])

###' 2. Consider the downloaded folder dataexample.
###' (a) Download the .zip file from CANVAS, and unzip
###' (b) Change the working directory to this folder
###' (c) Display the names of all the files for Maine
###' (d) Display the names of all pdf files for the states whose names start with letter "C".
#2. b)
setwd("H:\\MATH796\\Week 2\\dataexample")
# c)
file.names = list.files(getwd())
file.names[grepl('Maine', file.names)]
# d)
file.names[grepl('.pdf', file.names)&grepl('C', substr(file.names,1,1))]

###'
###' #### Factor
###'Factors are used to represent categorical data. They look like characters, but are actually integers under the hood.
###'
###' These integers are their positions in the sequence of the unique values of the charaters in a specific order.
###' Factors can be ordered (ordinal data) or unordered.
###' Factors are an important class for statistical analysis and for plotting
solar.planets = c('mercury','venus','earth','mars','jupiter','saturn','uranus','neptune')
typeof(solar.planets)
solar.planets.data = c('mercury','jupiter','saturn','venus','earth','mars','mercury','venus','jupiter','saturn','mercury','venus','uranus','neptune')
typeof(solar.planets.data)
solar.planets.data.factor = factor(solar.planets.data)
solar.planets.data.factor
as.numeric(solar.planets.data.factor)
as.numeric(solar.planets.data)
###' As you have seen, a factor can be converted to numeric, because they are integers under the hood, but a character cannot.
###'
levels(solar.planets.data.factor)
###' "Levels" of a factor is the ordered sequence of its unique values. The integer values depends on the order. What is the order by default?
###' You can change the order by adding an argument specifying the levels when defining the factor.
###' Let us try to assign the levels based on their closeness to the sun.
solar.planets.data.factor.ordered = factor(solar.planets.data,levels = solar.planets)
solar.planets.data.factor.ordered
as.numeric(solar.planets.data.factor.ordered)


###' ## Control flow
###'Summary of the control flow tools in R:
###'
###'  if(), else if(), else: standard conditionals
###'
###'ifelse(): conditional function that vectorizes nicely
###'
###'switch(): handy for deciding between several options
###'

###' #### if() and else
###'  Use if() and else to decide whether to evaluate one block of code or another, depending on a condition.
###'
###'Condition in if() needs to give one TRUE or FALSE value

x = 0.5

if (x >= 0) {
  x
} else {
  -x
}
###'Note that the else statement is optional

if (x < 0) {
  x
}

###'Single line actions don't need braces, i.e., could shorten above to if (x >= 0) x else -x
###'We can use else if() arbitrarily many times following an if() statement

x = -2

if (x^2 < 1) {
  x^2
} else if (x >= 1) {
  2*x-1
} else {
  -2*x-1
}

###'Each else if() only gets considered if the conditions above it were not TRUE.
###'
###'The else statement gets evaluated if none of the above conditions were TRUE
###'
###'Note again that the else statement is optional

###' #### ifelse() for quick decision making.
###'In the ifelse() function we specify a condition, then a value if the condition holds, and a value if the condition fails

ifelse(x > 0, x, -x)

###'One advantage of ifelse() is that it vectorizes nicely
x = c(1,-2,-3)
ifelse(x > 0, x, -x)

if (x >= 0) {
  x
} else {
  -x
}

###' #### swich() for deciding between many options
###'Instead of an if() statement followed by many elseif() statements (and perhaps a final else), we can use switch().
###'We pass a variable to select on, then a value for each option
x.vec = runif(20)
type.of.summary = "median"

switch(type.of.summary,
       mean=mean(x.vec),
       median=median(x.vec),
       histogram=hist(x.vec),
       "I don't understand")

###'Here we are expecting type.of.summary to be a string, either "mean", "median", or "histogram"; we specify what to do for each.
###'
###'The last passed argument has no name, and it serves as the else clause
###'
###' You do not need to name each option.

type.of.summary = 2

switch(type.of.summary,
       mean(x.vec),
       median(x.vec),
       hist(x.vec),
       "I don't understand")


###' #### Flow control and Boolean operators
###'Remember our standard Boolean operators, & and |. These combine terms elementwise

u.vec = 0.1*(-2:3)
u.vec
-0.1 <= u.vec & u.vec <= 0.1

###'In contrast to the standard Boolean operators, && and || give just a single Boolean. They evaluate left to right examining only the first element of each vector.
###'
###' They are more appropriate for programming control-flow and typically preferred in if clauses where a single Boolean is required.

-0.1 <= u.vec && u.vec <= 0.1

(0 > 0) && all(matrix(0,2,2) == matrix(0,3,3))
(0 > 0) && (ThisVariableIsNotDefined == 0)

###'Note R never evaluates the expression on the right in each line (each would throw an error)
###'
###'In control flow, we typically just want one Boolean
###'
###'Rule of thumb: use & and | for indexing or subsetting, and && and || for programming control-flow such as if
###'


###'
###' #### Practice:
###'
###' 1. Smoothly Clipped Absolute Deviation (SCAD) Penalty is a popular penalty function used in high dimensional variable selection.
###' For fixed parameters $\lambda>0$ and $a>2$, and input $x\geq 0$, it returns value lambda if x is no larger than lambda,
###'  $(a\lambda-x)/(a-1)$ if x is between $\lambda$ and $a\lambda$, and 0 if x is larger than $a\lambda$.
###'  When x<0, it calculate the penalty for the absolute value of x. Write a if statement that calculate SCAD penalty, and test it using $\lambda=2$,$a=3.7$ and $x=5$.
SCAD = function(lambda, a, x){
  if (lambda <= 0 || a <= 2)
    stop("Input values are not in valid range.")
  
  if (x < 0)
    return(abs(x))
  
  if (x <= lambda)
    return(lambda)
  else if (x > lambda && x <= a*lambda)
    return((a*lambda - x)/(a - 1))

  return(0)
}
SCAD(2, 3.7, 5)
###'

