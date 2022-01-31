#' ---
#' title: "Writing R functions"
#' author: "Qi Zhang"
#' date: "August 01, 2021"
#' ---

###' "To understand computations in R, two slogans are helpful:
###' Everything that exists is an object.
###' Everything that happens is a function call."
###'
###' - John McKinley Chambers

###'   Data structures tie related values into one object
###' Functions tie related commands into one object
###' In both cases: easier to understand, easier to work with, easier to build into larger things
###'

###' Consider the following example

u.vec <- runif(100) ### simulate 100 observations from uniform(0,1)
u.vec[1] <- -10
u.vec[100] <- 100
hist(u.vec)
mean(u.vec)

###' As you can see, due to the presence of the strong outliers, the mean is outside the bulk region of the distribution (0 to 1), and not a good representation of the center of the distribution.
###' A better measure is a trimmed mean. That is, trim the extreme data points on both sides, and calculate the mean of the rest.
###' We can do so by first define the cut-off points of where to trim using quantiles of the data vector itself, trim, and then calculate the mean.

q1.u = quantile(u.vec, prob=0.05)
q2.u = quantile(u.vec, prob=0.95)
mean(u.vec[q1.u <= u.vec & u.vec <= q2.u])

###' This takes three lines of code.
###' If in addition to this u.vec, you also need to calculate the trimmed means for some other vectors, say, what.vec,
###' you need to write another three lines to do so
###' q1.what = quantile(what.vec, prob=0.05)
###' q2.what = quantile(what.vec, prob=0.95)
###' mean(what.vec[q1.what <= what.vec & what.vec <= q2.what])
###' Repeating this process 100 times needs 300 lines of code, which is too much.
###' An easier way is to write a function for trimmed mean that take the data vector as input, and output the trimmed mean.

###'  When we discuss sapply, I have written such a function, and use it in sapply().

trimmed.mean = function(v) {
  q1 = quantile(v, prob=0.05)
  q2 = quantile(v, prob=0.95)
  output = mean(v[q1 <= v & v <= q2])
  return(output)
}

###' All R functions have three parts:
###'   the body(), the code inside the function.
###'   the formals(), the list of arguments which controls how you can call the function.
###'   the environment(), the "map" of the location of the function's variables.
###'   When you print a function in R, it shows you these three important components.
###'   If the environment isn't displayed, it means that the function was created in the global environment.

trimmed.mean
body(trimmed.mean)
formals(trimmed.mean)
environment(trimmed.mean)

###' Some function returns an object to the current environment, while some others do not.
###' return() specifies the object to be returned to the environment. Without it, it returns the last object evaluated.

trimmed.mean.no.return = function(v) {
  q1 = quantile(v, prob=0.05)
  q2 = quantile(v, prob=0.95)
  output = mean(v[q1 <= v & v <= q2])
}

temp = trimmed.mean.no.return(1:100)
temp

trimmed.mean.no.return2 = function(v) {
  q1 = quantile(v, prob=0.05)
  q2 = quantile(v, prob=0.95)
  output = mean(v[q1 <= v & v <= q2])
  tmp=2
}

temp = trimmed.mean.no.return2(1:100)
temp

###' If you want the output to be printed in console without assigning it to a variable

trimmed.mean.no.return3 = function(v) {
  q1 = quantile(v, prob=0.05)
  q2 = quantile(v, prob=0.95)
  output = mean(v[q1 <= v & v <= q2])
  output
}

trimmed.mean.no.return(1:100)
trimmed.mean.no.return3(1:100)
tmp = trimmed.mean.no.return3(1:100)
tmp

###' For any chunk of code that you wrote, if the same task will be repeated in the future, you can write a function to wrap the code as shown in the above.
###' The chunk of the code is the function body, and you need to specify what are the input arguments, and what output you need.
###'
###' #### Practice
###' 1. Write an R function myhead3, whose input is a data frame, and the output is the first 3 rows of the data frame. You should NOT use R funcion head().
###' 2. Write an R function summarySub20, whose input is a data frame, and the output is a list of summaries (output of function summary()) of the columns (except the first column) of a submatrix
###'  consisting of all the rows that the value in the first column is 20.
###' Test your two functions using the insurance data insurance.df = read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv')
insurance.df = read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv')
myhead3 = function (d) {d[c(1:3),]}
summarySub20 = function (d) {summary(d[d[,1] == 20,-1])}


###'
###'  If a task is to be repeated for a large number of times using different inputs, it will not be repeated in exactly the same way due to the specific situations.
###' It is desirable to design a function that is flexible enough to accommodate the foreseeable variations of the application context,
###' and informative enough so that the user will have the necessary information to resolve potential issues, and understand what has been done exactly when calling this function.
###' These two goals can be achieved by designing the input arguments, outputs, printing messages in the console, and flow control.
###' In what follows, I will use trimmed.mean function as an example to illustrate my thought process when designing a function.
###' It is an iterative process, and there is no perfect answer.
###' The rationale that I use in this process of improving the function is helpful in practice.
###' You are not required to be able to think in the same way in this course now. (I was not able to think in this way when I was a graduate student!)
###' But you need to start trying your best now. It takes practice, and some of you may be able to write the code better than the instructor in six months.

###'
###' We start with what we have before.
###' Since we will present a sequence of revisions to this function, it is convenient to name the version at the starting point as trimmed.mean0
###' Such name violates our principle of using informative names, and may cause confusion in practice.
###' I do this here only to keep the names of the functions short.
trimmed.mean0 = function(v) {
  q1 = quantile(v, prob=0.05)
  q2 = quantile(v, prob=0.95)
  output = mean(v[q1 <= v & v <= q2])
  return(output)
}

###' First, we recognize that trimming 5% on each end is an arbitrary choice, and depending on the prevalence of the potential outliers.
###' It is desirable to allow the user to choose how much to trim, and keep the trimming symmetric.
###' While you can revise the constants (0.05 and 0.95) in the functions every time before applying the function, it is more desirable to set an input argument of the function for the trim proportion.

trimmed.mean1 = function(v,prop.trim=0.1) {
  q1 = quantile(v, prob=prop.trim/2)
  q2 = quantile(v, prob=1-prop.trim/2)
  output = mean(v[q1 <= v & v <= q2])
  return(output)
}

trimmed.mean0(u.vec)
trimmed.mean1(u.vec,prop.trim = 0.1)
trimmed.mean1(u.vec)
trimmed.mean1(u.vec,prop.trim = 0.05)

###' Here "=0.1" in trimmed.mean1 = function(v,prop.trim=0.1) shows that the default value of prop.trim is 0.1 (5% on each side).
###' When the function is called without providing a specific value of prop.trim, prop.trim=0.1 is used.
###' The reason why we trim in trimmed mean is to remove the outliers before calculating the mean, and the proportion to trim depends on the provalence of the potential outliers.
###' If we know the proportion of outliers, we will know how much to trim.
###' In your past statistics classes, you should have learned at two ways to identify outliers. What are they?
###'
###' We will use 1.5*IQR rule here to detect outliers and determine the value of prop.trim when it is not given in the input.

trimmed.mean2 = function(v,prop.trim=NULL) {

  if(is.null(prop.trim)){
    quartiles = quantile(v,probs = c(0.25, 0.75))## find Q1 and Q3
    iqr = quartiles[2]-quartiles[1] # define IQR
    upper.bound = quartiles[2]+1.5*iqr
    lower.bound = quartiles[1]-1.5*iqr
    p.outliers.lower = mean(v<lower.bound) # proportion of outliers with extremely small values
    p.outliers.upper = mean(v>upper.bound) # proportion of outliers with extremely large values
    prop.trim = max(p.outliers.lower,p.outliers.upper)*2 # since we want to trim all outliers on both ends, the prop.trim should be 2*max()
  }
  print(paste0('prop.trim=',prop.trim))
  q1 = quantile(v, prob=prop.trim/2)
  q2 = quantile(v, prob=1-prop.trim/2)
  output = mean(v[q1 <= v & v <= q2])
  return(output)
}

trimmed.mean2(u.vec,prop.trim = 0.1)
trimmed.mean2(u.vec)
trimmed.mean2(u.vec,prop.trim = 0.02)



###'
###' #### Practice
###' 1. Starting with your myhead3, write a new function myNrows whose input is a data frame dt, option n, and the output is n rows of it.
###' There should be another option "method" that allows the user decide whether the first n ("head"), last n ("tail") or randomly selected n rows ("random") will be returned. The default option should be randomly selected 6.
###' You should not used R functions head() or tail().
###' 2. Starting with your summarySub20, write a new function summarySubKey whose input is dt, a data frame, col.key, the name of a column, and x.key, a value that this column may have.
###' The output is a list of summaries of the columns (except the column col.key) of a submatrix consisting of all the rows that the value in the column col.key is x.key.
###' Test your two functions using the insurance data insurance.df = read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv')
###' To test summarySubKey, try col.key='children', and x.key=0, and then col.key='smoker', and x.key='yes'.
myNrows = function (dt, n = 6, method = "random") {
  
  if (method == "head") {
    id.sub = c(1:n)
  } else if (method == "tail") {
    id.sub = c((nrow(dt) - n  + 1):nrow(dt))
  } else if (method == "random") {
    id.sub = sample(1:nrow(dt), n, replace = FALSE)
  } else {
    stop("Invalid Inputs !")
  }
  
  return(dt[id.sub,])
}
myNrows(insurance.df, 3)
summarySubKey = function (dt, col.key, x.key) {
  summary(dt[dt[,col.key] == x.key,which(colnames(dt) != col.key)])
}
summarySubKey(insurance.df, col.key = 'age', x.key = 20)
summarySubKey(insurance.df, col.key = 'smoker', x.key = 'yes')


###'
###'
###' Since the prop.trim is defined automatically within the function when it is not given, we want to know what it is, so that we can evaluate whether it is appropriate.
###' Afterall, 1.5IQR is only one way to define outliers.
###' One way to get this information is to let function print a message in R console using print()

###' Another way to get this information along with other information is to make them part of the output.
###' For example, the user may want to know prop.trimmed, and the exact outliers detected to understand what is going on in the dataset.

trimmed.mean3 = function(v,prop.trim=NULL) {

  if(is.null(prop.trim)){
    quartiles = quantile(v,probs = c(0.25, 0.75))## find Q1 and Q3
    iqr = quartiles[2]-quartiles[1] # define IQR
    upper.bound = quartiles[2]+1.5*iqr
    lower.bound = quartiles[1]-1.5*iqr
    p.outliers.lower = mean(v<lower.bound) # proportion of outliers with extremely small values
    p.outliers.upper = mean(v>upper.bound) # proportion of outliers with extremely large values
    prop.trim = max(p.outliers.lower,p.outliers.upper)*2 # since we want to trim all outliers on both ends, the prop.trim should be 2*max()
  }
  print(paste0('prop.trim=',prop.trim))
  q1 = quantile(v, prob=prop.trim/2)
  q2 = quantile(v, prob=1-prop.trim/2)
  trimmed.mean = mean(v[q1 <= v & v <= q2])
  points.trimmed = v[q1>v|q2<v]
  output = list(trimmed.mean=trimmed.mean,prop.trim=prop.trim, iqr.rule=is.null(prop.trim),points.trimmed=points.trimmed)
  return(output)
}

trimmed.mean3(u.vec)
trimmed.mean3(u.vec,prop.trim = 0.1)

###' We may choose to revise the function further.
###' For example, what should we do if the input vector include NA (usually missing values)?
###' The trimmed.mean function uses R functions mean and quantile. Both functions include an input argument on whether or not to remove the missing values before calculation.
###' Their default is false. So if the input vector include NA, it will trigger error. We can do something similar


trimmed.mean4 = function(v,prop.trim=NULL,na.rm=FALSE) {
  if(na.rm){
    v = v[!is.na(v)]
  }
  if(is.null(prop.trim)){
    quartiles = quantile(v,probs = c(0.25, 0.75))## find Q1 and Q3
    iqr = quartiles[2]-quartiles[1] # define IQR
    upper.bound = quartiles[2]+1.5*iqr
    lower.bound = quartiles[1]-1.5*iqr
    p.outliers.lower = mean(v<lower.bound) # proportion of outliers with extremely small values
    p.outliers.upper = mean(v>upper.bound) # proportion of outliers with extremely large values
    iqr.rule=is.null(prop.trim)
    prop.trim = max(p.outliers.lower,p.outliers.upper)*2 # since we want to trim all outliers on both ends, the prop.trim should be 2*max()
  }
  print(paste0('prop.trim=',prop.trim))
  q1 = quantile(v, prob=prop.trim/2)
  q2 = quantile(v, prob=1-prop.trim/2)
  trimmed.mean = mean(v[q1 <= v & v <= q2])
  points.trimmed = v[q1>v|q2<v]
  output = list(trimmed.mean=trimmed.mean,prop.trim=prop.trim, iqr.rule=iqr.rule,points.trimmed=points.trimmed)
  return(output)
}

u.vec.na = u.vec
u.vec.na[20] = NA
trimmed.mean4(u.vec)
try(trimmed.mean4(u.vec.na))
trimmed.mean4(u.vec.na,na.rm = T)


###' The error message we just saw said that the input to the function quantile include NA, which triggers an error when na.rm=F for the function quantile.
###' Since we know what triggers this error (when na.rm=F for our function and v contains NAs), we may choose to report an error message that is for our own function.
###'


trimmed.mean5 = function(v,prop.trim=NULL,na.rm=FALSE) {
  if(na.rm){
    v = v[!is.na(v)]
  }else if(any(is.na(v))){
    stop("missing values and NaN's not allowed if 'na.rm' is FALSE")
  }
  if(is.null(prop.trim)){
    quartiles = quantile(v,probs = c(0.25, 0.75))## find Q1 and Q3
    iqr = quartiles[2]-quartiles[1] # define IQR
    upper.bound = quartiles[2]+1.5*iqr
    lower.bound = quartiles[1]-1.5*iqr
    p.outliers.lower = mean(v<lower.bound) # proportion of outliers with extremely small values
    p.outliers.upper = mean(v>upper.bound) # proportion of outliers with extremely large values
    iqr.rule=is.null(prop.trim)
    prop.trim = max(p.outliers.lower,p.outliers.upper)*2 # since we want to trim all outliers on both ends, the prop.trim should be 2*max()
  }
  print(paste0('prop.trim=',prop.trim))
  q1 = quantile(v, prob=prop.trim/2)
  q2 = quantile(v, prob=1-prop.trim/2)
  trimmed.mean = mean(v[q1 <= v & v <= q2])
  points.trimmed = v[q1>v|q2<v]
  output = list(trimmed.mean=trimmed.mean,prop.trim=prop.trim, iqr.rule=iqr.rule,points.trimmed=points.trimmed)
  return(output)
}

trimmed.mean5(u.vec)
try(trimmed.mean5(u.vec.na))
trimmed.mean5(u.vec.na,na.rm = T)

###' Now R says that there is an "Error in trimmed.mean5(u.vec.na)"


###'  You may try to further revise the function.
###'  For example, when there are NAs in the input vector, and when na.rm=F,
###'  what if instead of stop the function and report an error message, I want the output trimmed mean to be NAs, and report a Warning message.
###'  To make it happen, you will need function warning() in the place of stop(), and also do something else there.






###'
###' #### Practice
###' 1. What happens to myNrows if n>nrow(dt) or if your method is not recognizable. Using the insurance data, please try n=10000,method='head', and then n=10, method="I dont really care".
###' You may choose to return an informative error message, or return the output with default option, and a warning message (using R function warning()).
###' Edit your function myNrows so that it can do the following two.
###' (a) If n>nrow(dt), it returns an error message saying "n rows are requested, but there are only nrow(dt) rows in the dataset", where n and nrow(dt) are the actual values of input n and nrow(dt)
###' (b) If the method is not recognizable, it returns randomly selected n rows, but with a warning message "method option xx not recognized, randomly selected rows are returned"
###' 2. What happens to summarySubKey if col.key is not part of the column names of dt, or if there are no rows with value x.key in the column col.key.
###' Please try col.key='whatsup', x.key='male', and then col.key='region' and x.key='nh' with the insurance data.
###' Please edit your function summarySubKey to return relevant error messages "column col.key cannot be found", or "there are no rows with value x.key in the column col.key"
###' where x.key and col.key should be the actual input values that trigger the error.
###' Then test the function with the same inputs again.
myhead3 = function (dt, n = 6, method = 'random') {
  if (n > nrow(dt)){
    stop(paste0(n, " rows are requested, but there are only ", nrow(dt), " rows in the dataset"))
  }

  if (!(method %in% c('head', 'tail', 'random'))){
    messages = c("method option \'", method, "'\ not recognized, randomly selected rows are returned")
    warning(paste0(messages))
    return(dt[sample(1:nrow(dt), n, replace = FALSE),])
  }
  
  if (method == 'head') {
    id.sub = c(1:n)
  } else if (method == 'tail') {
    id.sub = c((nrow(dt) - n  + 1):nrow(dt))
  } else {
    id.sub = sample(1:nrow(dt), n, replace = FALSE)
  } 
  
  return(dt[id.sub,])
}
try(myhead3(insurance.df, 10000, 'head'))
try(myhead3(insurance.df, 10, 3))

summarySubKey = function (dt, col.key, x.key) {
  if (!(col.key %in% colnames(dt))) {
    messages = c("column \'", col.key, "'\ cannot be found")
    stop(paste0(messages))
  }
  
  if (!any(dt[,col.key] == x.key)) {
    messages = c("there are no rows with value ", x.key, 
                 " in the column \'", col.key, "\'")
    stop(paste0(messages))
  }
  
  summary(dt[dt[,col.key] == x.key,which(colnames(dt) != col.key)])
}
try(summarySubKey(insurance.df, col.key = 'whatsup', x.key = 'male'))
try(summarySubKey(insurance.df, col.key = 'region', x.key = 'nh'))
###'
###'
###' Essentially you can wrap any code as a function, read files, and write files.
###' For example, website \url{https://covidtracking.com/data/download} include covid data time series for each state.
###' Let us take a look at one state first

setwd("D:\\Dropbox\\courses\\stat_computing\\data\\")
dt.nh <- read.csv('https://covidtracking.com/data/download/new-hampshire-history.csv',header = T)
dim(dt.nh)
colnames(dt.nh)
dt.nh$date = as.Date(dt.nh$date,'%Y-%m-%d')
pdf(file='new-hampshire-daily-test-results.pdf',height=6, width=10)
plot(totalTestResultsIncrease~date,data=dt.nh,type = 'l',xaxt='n',main='Daily test results in New Hampshire')
axis(1,dt.nh$date,format(dt.nh$date,"%m/%d"))
dev.off()
write.csv(dt.nh,row.names = F,quote = F,file='new-hampshire-covid.csv')

###' Now we want to do this for all 50 states plus the other US territories whose data are available.
###' How can we write a function to do all of these?
###' The function does not need to return any output to the environment, but it does generate files that should have distinct names. Ideally named after the state/territory names.
###' Ideally, the input can be the state name, and then the function will automatically locate the file on the website (fortunately all files have interpretable names)
###' We will then create plots and local date files named after the input state.

plot.and.write.covid <- function(region){
  id.state = gsub(' ','-',region) # in the file names, the space is substituted by '-'
  dt <- read.csv(paste0('https://covidtracking.com/data/download/', id.state,'-history.csv'),header = T)
  dt$date = as.Date(dt$date,'%Y-%m-%d')
  pdf(file=paste0(id.state,'-daily-test-results.pdf'),height=6, width=10)
  plot(totalTestResultsIncrease~date,data=dt,type = 'l',xaxt='n',main=paste('Daily test results in',region))
  axis(1,dt$date,format(dt$date,"%m/%d"))
  dev.off()
  write.csv(dt,row.names = F,quote = F,file=paste0(id.state,'-covid.csv'))
  return(NULL) ### return nothing because it is not needed. YOu may also choose to return something such as summary(dt$totalTestResultsIncrease)
}

###' test the function

plot.and.write.covid('Maine')

###' Now let us do this for all the states and other territories.
###' Luckily, state.name is an R built-in vector of the state names. We will start with this, and add the other territories.
regions <- c(state.name, 'District of Columbia', 'American Samoa','Guam','Northern Mariana Islands','Puerto Rico','US Virgin Islands')
###' Then you can call this function in sapply to do the work.

#sapply(regions, plot.and.write.covid)

###' Note that there is actually a file for the data of all states on this website.
###' For educational purpose, we have assumed that it does not exist, and  we have to read, plot and write the dataset for 50+ times.
###'




###' Environment: what the function can see and do
###' Each function has its own environment
###' Names here override names in the global environment
###' Internal environment starts ith the named arguments
###' Assignments inside the function only change the internal environment
###' Names undefined in the function are looked for in the global environment
###' Environment examples
x = 7
y = 1000
adder = function(y) {
  x = x+y
  return(x)
}
adder(1)
x
y

###' It is generally a bad practice to use variables defined outside the function's environment without passing them as input arguments.
###' The only potential exception are for built-in constants like pi, letters, month.names, etc.
###'
###' Starting to write a function is easy. Write a perfect function is hard, if not impossible.
###' It is generally a good idea to start with a top-down function design.
###' Start with the big picture view of the overall task.
###' Break it into a few parts, and figure out how to fit the parts together.
###' Then try to break each part into smaller parts if necessary.
###'  Try-and-error is a normal and common strategy in coding and function design.
###'  Instead of considering a coding error as a math error, think of it as a miss in soccer/basketball shooting.
###'  It is normal even for the top player, and you just need to learn from these misses, and practice more.
###' With practice, this design strategy should become natural
###'
###'
###' source()
###'
###' If you are writing functions for potentially more than one application, you may want to write and save them on a separate R script.
###' If its name is myfunctions.R
###' You can add a line
###' source("path/to/myfunctions.R") in the R script for the actual analysis, then the functions are loaded in your current environment, just like you load an R package by library().
###'

