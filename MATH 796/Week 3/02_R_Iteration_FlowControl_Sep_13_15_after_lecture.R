#' ---
#' title: "R Control Flow and Iteration"
#' author: "Qi Zhang (including materials from Dr. Ryan Tibshirani's class at CMU)"
#' date: "August 01, 2021"
#' ---

###'
###' ## Control flow
###'Summary of the control flow tools in R:
###'
###'  if(), else if(), else: standard conditionals
###'
###'ifelse(): conditional function that vectorizes nicely
###'
###'switch(): handy for deciding between several options
###'

###'
###' #### if() and else
###'  Use if() and else to decide whether to evaluate one block of code or another, depending on a condition.
###'
###'Condition in if() needs to give one TRUE or FALSE value

x = 0.5

if (x >= 0) {
  x
} else {
  -2*x
}
###'Note that the else statement is optional

if (x < 0) {
  x
}

###'Single line actions don't need braces, i.e., could shorten above to if (x >= 0) x else -x
if (x< 0) x



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

ifelse(x > 0, x, -2*x)

###'One advantage of ifelse() is that it vectorizes nicely
x = c(1,-2,-3)
ifelse(x > 0, x, -2*x)

if (x >= 0) {
  x
} else {
  -2*x
}

###'
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
median(x.vec)
###'Here we are expecting type.of.summary to be a string, either "mean", "median", or "histogram"; we specify what to do for each.
###'
###'The last passed argument has no name, and it serves as the else clause
###'
###' You do not need to name each option.

type.of.summary = 1

switch(type.of.summary,
       mean(x.vec),
       median(x.vec),
       hist(x.vec),
       "I don't understand")


###'
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
###'  $(a\lambda-x)/(a-1)$ if $x$ is between lambda and $a*\lambda$, and $0$ if $x$ is larger than $a*\lambda$.
###'  When $x<0$, it calculate the penalty for the absolute value of x. Write a if statement that calculate SCAD penalty, assuming lambda=2,a=3.7 and x=5.
###'


lambda=2
a=3.7
x=1

if(abs(x)<=lambda){
  lambda
}else if(abs(x)>=a*lambda){
  0
}else{
  (a*lambda-x)/(a-1)
}



###'
###'## Iteration
###'
###'Computers are good at applying rigid rules over and over again. Humans are not so good at this. Iteration is at the heart of programming

###'Different types of the iteration methods in R:
###'
###' * for(), while() loops: standard explicit loop constructs
###' * Vectorization: use it whenever possible! Often faster and simpler
###' * apply() family of functions: alternative to for() loop, these are built-in R functions, such as sapply, lapply, tapply
###' * **ply() family of functions: another alternative, very useful, from the plyr package (Will not be covered in this course)

###'
###'#### for()
###'  A for() loop increments a counter variable along a vector.
###'  It repeatedly runs a code block, called the body of the loop, with the counter set at its current value, until it runs through the vector

n = 10
log.vec = vector(length=n, mode="numeric")
for (i in 1:n) {
  log.vec[i] = log(i)
}
log.vec


###'Here i is the counter and the vector we are iterating over is 1:n. The body is the code in between the braces

###'Breaking from the loop:
###'We can break out of a for() loop early (before the counter has been iterated over the whole vector), using break

n = 10
log.vec = vector(length=n, mode="numeric")
for (i in 1:n) {
  if (log(i) > 2) {
    cat("I don't like numbers bigger than 2\n")
    break
  }
  log.vec[i] = log(i)
}
log.vec

###' Note that in the above the length of log.vec is predefined because this object is pre-defined as a length n numeric vector.
###' If it is desirable to trim the un-altered 0's in log.vec (all 0's except the first element), we need to know the length of the vector
###' You can get it from the current value of i
###' This reminds us that counter is a variable defined globally.
i
log.vec
log.vec.trimmed=log.vec[1:(i-1)]

###' An alternative of the above is to update the length of log.vec along the way

n = 10
log.vec = c()
for (i in 1:n) {
  if (log(i) > 2) {
    cat("I don't like numbers bigger than 2\n")
    break
  }
  log.vec = c(log.vec,log(i))
}
log.vec

###'Many different variations on standard for() are possible. Two common ones:
###'  * Non-numeric counters: counter variable always gets iterated over a vector, but it doesn't have to be numeric

names.stat.professors <- c('Qi', 'JC','Linyuan','Ernst')

for (str in names.stat.professors) {
  cat(paste(str, "is very nice\n"))
}

###' * Nested loops: body of the for() loop can contain another for() loop (or several others)
for (i in 1:4) {
  for (j in 1:i^2) {
    cat(paste(j,"<=",i^2,','))
  }
  cat("\n")
}

###'
###' #### while()
###'  A while() loop repeatedly runs a code block, again called the body, until some condition is no longer true.
###'
###'  For while loop, you have to increase the counter by yourself. It does not go up automatically.

i = 1
log.vec = c()
while (log(i) <= 2) {
  log.vec = c(log.vec, log(i))
  i = i+1
}
log.vec

###' Note that this while loop produce the same results as the previous for loop.
###'
###'for() versus while()
###' *  for() is better when the number of times to repeat (values to iterate over) is clear in advance
###' * while() is better when you can recognize when to stop once you're there, even if you can't guess it to begin with
###' * while() is more general, in that every for() could be replaced with a while() (but not vice versa)
###' * poorly designed While() loop may run forever. It is a common practice to set a maximal iteration.

# i=1
# log.vec=c()
# while(i>0){
#   log.vec=c(log.vec,log(i))
#   i=i+1
# }

###' The above runs forever, why?
###' Add a maximal iteration

i=1
iter.max = 10
log.vec=c()
while(log(i)<2&&i<=iter.max){
  log.vec=c(log.vec,log(i))
  i=i+1
}


###'
###' #### repeat()
###' repeat() repeats the body indefinitely, until something causes the flow to break. Example (try running in your console):

log.vec = c()
i=1
repeat {
  if (log(i) >2){
    break
  }
  print(i)
  log.vec = c(log.vec,log(i))
  i = i+1
}




###'
###' #### Practice:
###'
###' 1. In a Fibonacci sequence, each number is the sum of the two preceding ones, starting from 0 and 1.
###' Write a for loop to compute the next 10 Fibonacci numbers, and save your output and the initial 0 and 1 as a vector fab12 whose first two elements are 0 and 1.
###'
###' 2. How big is Inf? We know that R will recognize a big enough number as Inf. But how big is big enough? At least on my computer, 2^1000 is not big enough, but 2^1100 is.
###' Please give a better estimate of the lower bound of Inf in terms of 2^x (where x is an integer) using a while loop or a for loop with appropriate break condition. Print your final x in the console as your output.

fab12 = c(0,1)
for(i in 3:12){
  fab.i = fab12[i-1]+fab12[i-2]
  fab12 = c(fab12,fab.i)
}
fab12

x.inf = 1000
iter.max = 200
iter = 0

while(2^x.inf<Inf &&iter<iter.max){
  x.inf = x.inf+1
  iter = iter+1
}
x.inf


for(ii in 1000:1200){
  if(2^ii==Inf){
    cat(ii)
    break
  }
}


###'
###' #### Avoiding explicit iteration
###'Some people have a tendency to overuse for() and while() loops in R
###'
###'The problem with them in R is that they are usually slower than they should be (C++ does not have this problem).
###'
###'They aren't always needed. Very often vectorization can be used as alternatives (e.g., sapply, lapply, or vector/matrix operation)
###'We'll emphasize this on the lab, and try to hit upon it throughout the course
###'

###'
###' #### apply, sapply, lapply, tapply family
###'
###' R offers a family of apply functions, which allow you to apply a function across different chunks of data.
###' Offers an alternative to explicit iteration using for() loop; can be simpler and faster, though not always.
###' Summary of functions:
###'
###' * lapply(): apply a function to elements of a list or vector, output a list of the length of the input list or vector
###' * sapply(): same as the above, but simplify the output (usually to a matrix or vector, if possible)
###' * apply(): apply a function to rows or columns of a matrix or data frame, and simplify the output (usually to a matrix or vector, if possible)
###' * tapply(): apply a function to levels of a factor vector

###'lapply() apply a function to the elements of a list or vector
###'
###'The lapply() function takes inputs as in: lapply(x, FUN=my.fun), to apply my.fun() across elements of a list or vector x.
###'The output is always a list, whose elements are corresponding to the elements of the original list


my.list = list(char="exponential", nums=7, mat=matrix(1:4,2,2))

my.list
lapply(my.list, FUN=mean) # Get a warning: mean() can't be applied to chars
lapply(my.list, FUN=summary) # summary() provides five-number summary plus mean for numeric vector, and length/class/mode for non-numeric. What does it do to a matrix?

###' Recall that a data frame is also a list whose elements are the columns.
###' When lapply is used on a data frame, it apply the function to its columns, and output a list


state.df = data.frame(state.x77)
state.df$Region=state.region
state.df$Division=state.division
class(state.df)
typeof(state.df)
lapply(state.df, summary)
lapply(state.df,mean)

###'sapply() apply a function to the elements of a list or vector.
###'
###'The sapply() function works just like lapply(), but tries to simplify the return value whenever possible. E.g., most common is the conversion from a list to a vector

sapply(state.df, FUN=mean) # Simplifies the result, now a vector
sapply(state.df[,1:5], FUN=summary) # simplifies the result, now a matrix
sapply(state.df, FUN=summary) # cannot simply, still a list


###' Custom function. It is handy to use a customized function defined by ourselves with lapply (and sapply, apply and etc)
###'
###' Our custom function: trimmed mean

trimmed.mean = function(v) {
  if(is.numeric(v)){
    q1 = quantile(v, prob=0.05)
    q2 = quantile(v, prob=0.95)
    return(mean(v[q1 <= v & v <= q2]))
  } else{
    return(NA)
  }
}

sapply(state.df,FUN=trimmed.mean)

###' The custom function can be defined on the fly

sapply(state.df,FUN=function(v) {
  if(is.numeric(v)){
    q1 = quantile(v, prob=0.05)
    q2 = quantile(v, prob=0.95)
    return(mean(v[q1 <= v & v <= q2]))
  } else{
    return(NA)
  }
})

###' We will learn more about defining custom functions later.

###' The apply() function apply a function to the rows or columns of a matrix
###'
###'  apply(x, MARGIN=1, FUN=my.fun), to apply my.fun() across rows of a matrix or data frame x
###'apply(x, MARGIN=2, FUN=my.fun), to apply my.fun() across columns of a matrix or data frame x
apply(state.x77, MARGIN=1, FUN=mean) # mean of each row
apply(state.x77, MARGIN=2, FUN=summary) # Summary of each col, get back matrix!

###' You may consider apply as sapply with the row/column names as the input
###' When there is no row/column names,the row/column IDs.
###' The following sapply returns the same values as apply functions above.
sapply(rownames(state.x77), FUN = function(ii) mean(state.x77[ii,]))
sapply(colnames(state.x77), FUN = function(jj) summary(state.x77[,jj]))

# sapply(1:nrow(state.x77), FUN = function(ii) mean(state.x77[ii,]))
# sapply(1:ncol(state.x77), FUN = function(jj) summary(state.x77[,jj]))


###' When the output of apply cannot be reduced to a vector or a matrix, it returns a list just like sapply
###'
###' The following code returns the values that are larger than 10000 in each column of state.x77, which cannot be reduced to a vector or a matrix meaningfully.

apply(state.x77, MARGIN=2, FUN= function(xx) xx[xx>10000])

###' Applying a function that takes extra arguments
###'
###'Can tell apply() to pass extra arguments to the function in question.
###'E.g., can use: apply(x, MARGIN=1, FUN=my.fun, extra.arg.1, extra.arg.2), for two extra arguments extra.arg.1, extra.arg.2 to be passed to my.fun()

temp.state.x77 = apply(state.x77,2,sort)
head(temp.state.x77)

temp.state.x77.dc = apply(state.x77,2,sort,decreasing=T)
head(temp.state.x77.dc)

uvec = rnorm(50)

apply(state.x77,2,cor,y=uvec)

###'Optimized functions for special tasks
###'
###'Don't overuse the apply paradigm to reinvent the wheels!
###'There are lots of special functions optimized that are will be both simpler and faster than using apply(). E.g.,
###'
###' * rowSums(), colSums(): for computing row, column sums of a matrix
###' * rowMeans(), colMeans(): for computing row, column means of a matrix
###' * max.col(): for finding the maximum position in each row of a matrix
###'
###'Combining these functions with logical indexing and vectorized operations will enable you to do quite a lot. E.g., how to count the number of positives in each row of a matrix?

x = matrix(-4:4, 3, 3)
apply(x, MARGIN=1, function(v) { return(sum(v > 0)) })
rowSums(x > 0)

###' tapply(): apply function to a vector by the level of a factor
###'
###'The function tapply() takes inputs as in: tapply(x, INDEX=my.index, FUN=my.fun), to apply my.fun() to subsets of entries in x that share a common level in my.index
head(state.x77)
head(state.region)
###'# Compute the mean and sd of the Frost variable, within each region
tapply(state.x77[,"Frost"], INDEX=state.region, FUN=mean)
tapply(state.x77[,"Frost"], INDEX=state.region, FUN=sd)


###'split(), split by levels of a factor
###'
###'The function split() split up the rows of a data frame by levels of a factor, as in: split(x, f=my.index) to split a data frame x according to levels of my.index

###' Split up the state.x77 matrix according to region
state.by.reg = split(state.df, f=state.df$Region)
class(state.by.reg) # The result is a list
names(state.by.reg) # This has 4 elements for the 4 regions
class(state.by.reg[[1]]) # Each element is a data frame

###' For each region, display the first 3 rows of the data frame
lapply(state.by.reg, FUN=head, n=3)




###'
###' #### Practice:
###'
###' 1. Calculate the standard deviation of each column of state.x77 using apply
###'
###' 2. Function split can be used to split one vector of the same length as the factor.
###' Split Income by Region, and call the output income.by.reg.
###' Check its date type, and then calculate the mean income of each region by sapply
###'
###' 3. Use tapply to calculate the mean murder rate by the initial letter of the state names.
###'
###' 4. To appreciate vectorization and *apply family as faster alternatives to explicit iterations, we need to document computing time.
###' system.time() is the R function that evaluate the time of an R expression. In the following, we will compare the running time of the three approaches.
###' The following is the code that document the running time of creating a vector of log of integers from 1 to n by vectorization.
###' system.time() report three values. If only one CPU core is used, the first two should add to the third "elapsed" time, essentially the real time we feel.
n=100000
system.time(log.vec.vec<- log(1:n))
###' Please create log.vec.for, which is the same output vector computed using a for loop, and log.vec.sapply, which is computed using sapply.
###' What is your conclusion in the comparison of computing time?
###'
###' 5. If you substitute "<-" in system.time(log.vec.vec<- log(1:n)) with "=", what will you get? Can you guess why?

apply(state.x77,2,sd)

income.by.reg <- split(state.df$Income,f=state.df$Region)
typeof(income.by.reg)
sapply(income.by.reg,mean)

tapply(state.df$Murder,INDEX = substr(rownames(state.df),1,1),FUN=mean)

n=100000
log.vec.for <- c()
system.time(for(ii in 1:n) log.vec.for <- c(log.vec.for,log(ii)))

log.vec.for <- rep(0,n)
system.time(for(ii in 1:n) log.vec.for[ii] <- log(ii))

system.time(log.vec.sapply <- sapply(1:n, log))

