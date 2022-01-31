#' ---
#' title: "R Basics Part 2"
#' author: "Qi Zhang (including materials from Dr. Ryan Tibshirani's class at CMU)"
#' date: "September 1, 2021"
#' ---
#' 
#' Objectives: this lecture introduces arrays, data frame, and text manipulation. 
#' 

###' #### Arrays
###' An array is a multi-dimensional generalization of vectors

x = 1:12
x.arr = array(x, dim=c(2,3))
x.arr
###' dim says how many rows and columns; filled by columns
###' 
###' Can have 3d arrays, 4d arrays, etc.; dim is vector of arbitrary length
###' 
x.arr.3d = array(x, dim=c(2,3,2))
x.arr.3d

###' Some properties of our array:

dim(x.arr)
is.vector(x.arr)
is.array(x.arr)
typeof(x.arr)
###' The underlying vector of an array is by column 

as.vector(x.arr)
as.vector(x.arr.3d)

###' #### Indexing arrays
###' Can access the elements of an arry by dimensional indices or by the underlying vector (column-major order):

x.arr.3d[1,2,1]
x.arr.3d[3] # ? ? ? Why ? ? ?

###' Omitting an index (but not the comma!) means "all of it":

x.arr.3d[c(1,2),2,1]
x.arr.3d[,2,1]
x.arr.3d[,2,]
x.arr.3d[,2,1,drop=FALSE] #Keep the data stucture

###' Note: the optional last argument drop=FALSE ensures that the result is still an array, not a vector

###' #### Functions on arrays
###' Many functions applied to an array will just boil things down to the underlying vector (Attention !):
###' 
which(x.arr.3d > 9)
###' This happens unless the function is set up to handle arrays specifically
###' 

###' Arithmetic and logical operator and some other functions work component-wise and preserve array structure.

y.arr = array(-x, dim=c(2,3))
y.arr + x.arr

###' But no recycling
y.arr.2 = array(-x, dim=c(2,2))
try( y.arr.2+x.arr)

###' A matrix is a specialization of a 2d array

z = c(40,1,60,3)
z.mat = matrix(z, nrow=2)
z.mat
is.array(z.mat)
is.matrix(z.mat) # Both are true
###' Could also specify ncol for the number of columns.
###' 
###' To fill by rows, use byrow=TRUE

z.mat.byrow = matrix(z,nrow=2,byrow = T)
z.mat.byrow

###' Defining a matrix filled by row does not change how the underlying vector is defined. 
###' The underlying vector is always by column. Attention !
###' 
z
as.vector(z.mat)
as.vector(z.mat.byrow)

###' Usual arithmetic and comparison operators works component-wise (e.g., z.mat/3)

###' #### Matrix multiplication
###' Matrices have its own special multiplication operator, written %*%:
###' 
###' Are you familiar with matrix multiplications?

mat.2by3 = matrix(2, ncol=3,nrow = 2)
mat.2by3
z.mat %*% mat.2by3 # [2x2] * [2x3]

#Can also multiply a matrix and a vector
z.mat%*%c(1,1)
c(1,1)%*%z.mat
#An interesting result !

#Row/column manipulations
#Row/column sums, or row/column means:

rowSums(z.mat)
colSums(z.mat)
rowMeans(z.mat)
colMeans(z.mat)
###' Matrix diagonal
###' 
###' The diag() function can be used to extract the diagonal entries of a matrix:

diag(z.mat)
###' It can also be used to change the diagonal:

diag(z.mat) = c(35,4)
z.mat
###' Creating a diagonal matrix.
###' Finally, diag() can be used to create a diagonal matrix:

diag(c(3,4))
diag(2)

###' #### Other matrix operators
###' Transpose:

t(z.mat)
###' Determinant:

det(z.mat)
###' Inverse:

solve(z.mat)
z.mat %*% solve(z.mat)
###' #### Names in matrices
###' We can name either rows or columns or both, with rownames() and colnames()
###' These names are character vectors, and we use them just like we do names() for vectors
###' Names help us understand what we're working with

rownames(z.mat) = c('r1','r2')
colnames(z.mat) = c('c1','c2')
z.mat['r1','c2']



###' #### Lists
###' A list is sequence of values, but not necessarily all of the same type
###' Recall that a vector is a sequence of values of the same type


my.sim = list("exponential", 7, matrix(1,2,2))
my.sim
###' Most of (non-numeric) vector operations can be applied to lists
###' 
###' #### Accessing parts of lists
###' Can use [ ] as with vectors
###' 
###' Or use [[ ]], but only with a single index [[ ]] drops names and structures, [ ] does not
my.sim[2]
my.sim[[2]]
my.sim[[2]]^2
try(my.sim[2]^2)


###' Pluck out all but one piece of a list (also works with vectors):

my.sim[-2]

###' We can name some or all of the elements of a list:

names(my.sim) = c("family","mean","data")
my.sim
my.sim[["family"]]
my.sim["family"]
###' Lists have a special shortcut way of using names, with $:

my.sim[["family"]]
my.sim$family

###' Creating a list with names:

my.sim2 = list(mean=7, sd=1, family="gaussian",data=matrix(rnorm(6,3,2)))

###' Adding named elements:

my.sim$random.seed = 1
my.sim[["last.updated"]] = "2021-07-01"

###' Removing a named list element, by assigning it the value NULL:

my.sim$ranom.seed = NULL

###' Combine lists with c() (also works with vectors):
my.sim.combined = c(my.sim,my.sim2)

###' Chop off the end of a list by setting the length to something smaller (also works with vectors):

length(my.sim)
length(my.sim) = 2
my.sim  

###' Adding unnamed elements 
my.sim[[4]] = 1
my.sim[5] = 2
my.sim
###' Note that the third element is NULL because it is not defined. But this element has to be listed because the 4th element is defined.


###' #### Structures of structures
###' List elements can be other data structures, e.g., vectors and matrices, even other lists:

my.list = list(z.mat=z.mat, x.arr.3d=x.arr.3d, my.sim=my.sim)
my.list


###' #### Key-value pairs
###' Lists give us a natural way to store and look up data by name, rather than by position.
###' 
###' This is a really useful programming concept that may come with many names: key-value pairs, dictionaries, or associative arrays
###' 
###' For example, we can compare the "family" element of my.sim and my.sim2 by looking that up by name, without caring where it is (in what position it lies) in the list
###' 
my.sim[['family']]
my.sim2[['family']]

###' My personal take-away when I learned this: always name the elements of your list and vector when natural names exist, and always index these elements with names when they are named.
###' 

###' #### Practice
###' 1. Define a variable x.vec to contain 100 random samples from N(0,1). You may want to read the help page of R function rnorm. 
###' Report the length, and the data type being stored in x.vec, and the average of all its elements.
###' 
x.vec = rnorm(100)
length(x.vec); typeof(x.vec); mean(x.vec)
###' 2. Convert x.vec into a matrix with 20 rows and 5 columns, and sotre it as x.mat. Check the dimensions and the data type of x.mat
###' Compute the means of each of the 5 columns of x.mat using a build-in R function. 
###' 
x.mat = matrix(x.vec,nrow = 20,ncol = 5)
dim(x.mat); typeof(x.mat); colMeans(x.mat)
###' 3. Extract and display the rows 3, 12, 19 of x.mat with a single line of code. 
x.mat[c(3,12,19),]
###' Answer the following questions, each with a single line of code: 
###' 
###' a) What is the proportion of the elements in column 3 of x.mat that are larger than 1.96?
###' 
length(which(x.mat[,3] > 1.96))/nrow(x.mat)
###' b) How many elements in rows 12 to 18 that are between 1.5 and 2.5?
###' 
length(which(x.mat[c(12,18),] >= 1.5 & x.mat[c(12,18),] <= 2.5))
###' 4. Modify x.vec so that all elements between -1 to 1 are replaced by 0. Print out the result to the console.
###' 
x.vec[x.vec >= -1 & x.vec <= 1] = 0
x.vec
###' 5. Create a list x.list with three elements. The first element is x.vec, the second element is the english lower case letters 
###' (The built-in R vector named "letters" is exactly that.), and the third element is sample(c(TRUE,FALSE),size=4,replace=TRUE).
###' The name of these three elements should be "x.vec", "letters", "random_logical", respectively.Then complete the following tasks, each with a single line of code:
###' 
x.list = list(x.vec = x.vec, letters = letters, random_logical = sample(c(TRUE,FALSE),size=4,replace=TRUE))
###' a) Extract all but the first element of x.list
###'
x.list[-1] 
###' b) Extract only the third element of x.list. The output should not be a list.
###' 
x.list[[3]]
###' c) Extract the first 5 elements of the second element of x.list
head(x.list[[2]],5)
x.list$letters[1:5]

###' #### Data frames
###' The format for the "classic" data table in statistics: data frame. 
###' 
###' * Think of each row as an observation/case
###' * Think of each as a variable/feature
###' * Not just a matrix because variables can have different types
###' * Both rows and columns can be assigned names
###' You can think of a data frame to be a matrix whose columns can have different data types, 
###' or a list whose elements are all length n vectors with potentially different data types.

###' Creating a data frame:
###' Use data.frame(), similar to how we create lists

my.df = data.frame(nums=seq(0.1,0.6,by=0.1), chars=letters[1:6], 
                   bools=sample(c(TRUE,FALSE), 6, replace=TRUE))
my.df
###' Note, a list can have different lengths for different elements.
my.list = list(nums=seq(0.1,0.6,by=0.1), chars=letters[1:12], 
               bools=sample(c(TRUE,FALSE), 6, replace=TRUE))
my.list
###' Indexing a data frame
###' 
###' * By rows/columns: similar to how we index matrices
###' * By columns only: similar to how we index lists
my.df[,1] # Also works for a matrix 

my.df[,"nums"] # Also works for a matrix

my.df$nums # Doesn't work for a matrix, but works for a list

my.df$chars # Note: this one has been converted into a factor data type Attention !

as.character(my.df$chars) # Converting it back to a character data type


###' Adding rows and columns:
###' We can add rows or columns to an array or data frame with rbind() and cbind(), but be careful about forced type conversions. (They also work for matrices)

rbind(my.df,list(nums=-3,chars='B+',bools=T))
rbind(my.df,c(-3,4,6))
rbind(my.df,c(3,4,6))[7,2]


###' Creating a data frame from a matrix:
###' Sometimes you are given a matrix. You can then make it a data frame, and even add additional columns to it. 
###' 

class(state.x77) # Built-in matrix of states data, 50 states x 8 variables
head(state.x77) 
class(state.region) # Factor of regions for the 50 states
head(state.region)
class(state.division) # Factor of divisions for the 50 states
head(state.division) 
state.df = data.frame(state.x77)
state.df$Region=state.region
state.df$Division=state.division
state.df$order = 1:nrow(state.df)
state.df$order_reverse = nrow(state.df):1
class(state.df)
head(state.df) # Note that the first 8 columns name carried over from state.x77

###' You can convert a matrix to a data frame and add additional columns in one single step
###' 
# state.df = data.frame(state.x77, Region=state.region, Division=state.division)

###' Deleting columns from a data frame.
###' To delete columns: we can either use negative integer indexing, or set a column to NULL

state.df = state.df[,-ncol(state.df)]
head(state.df, 3)

state.df$order = NULL
head(state.df, 3)

###' With matrices or data frames, we often want to access a subset of the rows corresponding to some condition. 
###' You already know how to do this, with Boolean indexing

# Compare the averages of the Frost column for states in New England and for Pacific divisions
mean(state.df[state.df$Division == "New England", "Frost"]) 
mean(state.df[state.df$Division == "Pacific", "Frost"]) 

###' subset(): Attention ! ! !
###' The subset() function provides a convenient alternative way of accessing rows for data frames
###' 
###' Using subset(), we can just use the column names directly (i.e., no need for using $, or write state.df twice)

state.df.ne.1 = subset(state.df, Division == "New England")
###' Get same thing by extracting the appropriate rows manually
state.df.ne.2 = state.df[state.df$Division == "New England", ]
all(state.df.ne.1 == state.df.ne.2)

###' Same calculation using subset()
mean(subset(state.df, Division == "New England")$Frost)
mean(subset(state.df, Division == "Pacific")$Frost) 


data("USArrests")
# 1.
dim(USArrests)
# 2.
USArrests$Murder = NULL
# 3.
state.crime.df = cbind(USArrests,state.df)
# 4.
state.crime.df['New Hampshire',c('HS.Grad','Murder')]
# 5.
mean(state.crime.df$Rape[state.crime.df$Region == 'Northeast'])
# 6.
sd(subset(state.crime.df,Illiteracy >= 1.5)$Assault)

###' #### Practice: 
###' R built-in dataset USArrests contains statistics, in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973. 
###' Also given is the percent of the population living in urban areas.
###' 
###' 1. Check the dimension of this dataset, and display the first four rows in the console
###' 
###' 2. Remove the column Murder from state.df (to avoid confusion in the next steps.)
###' 
###' 3. Combine state.df with USArest as a single data.frame named state.crime.df, and explain why it is possible to do so. Note that the column "Murder" is from USArest, not state.df.
###' 
###' 4. Display the high school graduation rate (HS.Grad) and Murder rate for New Hampshire
###' 
###' 5. Calculate the average Rape rate in Northeast Region without using function subset.
###' 
###' 6. Calculate the standard deviation of the Assault rates of the states whose illiteracy rate is at least 1.5(%). You must use function subset. 

###' #### Text manipulation
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
