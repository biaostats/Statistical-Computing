#' ---
#' title: "Writing R functions"
#' author: "Biao Zhang"
#' date: "October 1, 2021"
#' ---

###' 
###' 1. (2 points) In a practice problem on control flow and iteration, you were introduced to SCAD.
###'  Smoothly Clipped Absolute Deviation (SCAD) Penalty is a popular penalty function used in high dimensional variable selection.
###' For fixed parameters lambda>0 and a>2, and input x>=0, it returns value lambda if x is no larger than lambda,
###'  (a*lambda-x)/(a-1) if x is between lambda and a*lambda, and 0 if x is larger than a*lambda.
###'  When x<0, it calculate the penalty for the absolute value of x.
###'  Please write a function scad1 that takes x,lambda and a as input, and output the value of the SCAD penalty, and test it for x=5, lambda=2, a=3.7
###'  How will you compute scad value for x=1:10, lambda=2, a=3.7?
scad1 = function(x, lambda, a) {
  output = 0
  
  if(abs(x) <= lambda){
    output = lambda
  }else if(abs(x) <= a*lambda && abs(x) > lambda){
    output = (a*lambda-x)/(a-1)
  }
  
  return(output)
}
scad1(x=5, lambda=2, a=3.7)
try(scad1(x=1:10, lambda=2, a=3.7))
###'  2. (2 points)Starting with your code in the previous question, please modify your code so that the inputs can be numeric vectors, and it vectorizes nicely. Let us call this function scad2
###'  Evaluate scad2 with x=1:10, lambda=2, a=3.7
scad2 = function(x, lambda, a) {
  output = ifelse(abs(x) <= lambda, lambda, ifelse(abs(x) >= a*lambda, 0, (a*lambda-x)/(a-1)))
  
  return(output)
}
scad2(x=1:10, lambda=2, a=3.7)
###'  3. (2 points) Starting with your code in the previous question, please modify your code so that it returns meaningful error messages
###'   if any of x, a, lambda is not numeric, or if the values of lambda or a is not in appropriate range. Let call it scad3
scad3 = function(x, lambda, a) {
  typeof.args = c(class(x), class(lambda), class(a))
  if(any(typeof.args != "numeric" & typeof.args != "integer") 
     || lambda <= 0 || a <= 2) {
    stop("Invalid inputs !")
  }
  
  output = ifelse(abs(x) <= lambda, lambda, ifelse(abs(x) >= a*lambda, 0, (a*lambda-x)/(a-1)))
  
  return(output)
}
try(scad3('Haha',3,10))
try(scad3(3,TRUE,7))
try(scad3(2,-1,8))
try(scad3(c('H', 2, 3), 5, 3))
scad3(5, 2, 3.7)
scad3(1:10,2,3.7)
scad3(1:5/7,.23,2.1)

###'  4.(3 points) Write an R function myUnique that takes a vector as input, and output a vector consisting of the unique values of the input vector.
###' R function unique() does exactly that. You are not allowed to use R function unique(), table() or any other functions using unique().
###' You need to implement it by yourself. Probably you need to write a loop for it.
###' After you are done, testing your function using the column children, and then the column region of the insurance data
insurance.df = read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv')
myUnique = function(v) {
  result = list()
  for(i in 1:length(v)) {
    index = as.character(v[i])
    if(length(result[[index]]) == 0) {
      result[[index]] = v[i] 
    }
  }
  output = unlist(result)
  names(output) = NULL #Remove all keys.
  return(output)
}
myUnique(insurance.df$children)
myUnique(insurance.df$region)
all(myUnique(insurance.df$children) == unique(insurance.df$children))
all(myUnique(insurance.df$region) == unique(insurance.df$region))
###' 5. (3 points) Write an R function that returns the most frequent value in a vector
Mfreq = function(v) {
  result = data.frame(Value = v[1], Count = 1)
  for(i in 2:length(v)) {
    if(!any(result$Value == v[i])) {
      result = rbind(result, list(Value = v[i], Count = 1))
    } else {
      result[result$Value == v[i], 'Count'] = result[result$Value == v[i], 'Count'] + 1
    }
  }
  return(result[result$Count == max(result$Count),'Value'])
}
Mfreq(insurance.df$children)
Mfreq(insurance.df$region)
###' It is the same as what we got from the table.
children.freq = table(insurance.df$children)
region.freq = table(insurance.df$region)
names(children.freq[children.freq == max(children.freq)])
names(region.freq[region.freq == max(region.freq)])
###' Comments: table() converts all values into characters.
###'
###' 6. (8 points) Missing values are common in real data analysis, there are various ways of dealing with them. See the wiki page of missing value and
###' an example blog post (https://towardsdatascience.com/6-different-ways-to-compensate-for-missing-values-data-imputation-with-examples-6022d9ca0779)
###' on different ways of handling it.
###' In most R datasets, missing values are coded as NA, but there are also cases where missing values are coded as 0, if 0 is not physically possible if not missing.
###' Please write an R function handleMissing that take dt, a data frame potentially with missing values, as input, and output a data frame with no missing values as the output.
###' The user will have the option to choose one of the following simple (and sometimes horrible) strategies for handling missing values.
###' (a)delete: delete the rows with any missing values in any column.
###' (b) random: replace the missing values in the column with a random samples of the non-missing values.
handleMissing = function(dt, method = "delete") {
  init.check = apply(is.na(dt), 2, which)
  na.cols = init.check[sapply(init.check,length) != 0]
  
  if (length(na.cols) > 0) {

    if (method == "delete") {
      rows.delete = unique(unlist(na.cols))
      return(dt[-rows.delete,])
    } else if (method == "random") {
        for(i in 1:length(na.cols)) {
        rows.na = na.cols[[i]]
        rows.random = sample(dt[-rows.na,names(na.cols[i])], length(rows.na),replace = TRUE)
        dt[rows.na,names(na.cols[i])] = rows.random
      }
    } else if (method == "mode"){
      
        for(i in 1:length(na.cols)) {
          if (typeof(dt[,names(na.cols[i])]) == "character") {
            rows.na = na.cols[[i]]
            cat.freq = table(dt[-rows.na,names(na.cols[i])])
            dt[rows.na,names(na.cols[i])] = names(cat.freq[cat.freq == max(cat.freq)])
          } else if (typeof(dt[,names(na.cols[i])]) %in% c("double", "integer")) {
            rows.na = na.cols[[i]]
            num.mean = mean(dt[-rows.na,names(na.cols[i])])
            if (typeof(na.cols[[i]]) == "integer") {
              num.mean = as.integer(num.mean)
              #Use as.integer() instead of round() to make sure the column
              #has the same data type.
            }
            dt[rows.na,names(na.cols[i])] = num.mean
          }
        }
      
    }
  }

  return(dt)
}
###'
###' Please test your function using the following modified hitters data, display the rows of the outputs in which the original data has missing values, and comment on whether your output makes sense.
hitters.df = read.csv("H:/MATH796/Week 4/ALL CSV FILES/Hitters.csv", header = T, quote = "\"")
sapply(hitters.df,class)
set.seed(0)
id.na.nl = sample(1:nrow(hitters.df),20,replace = F)
hitters.df$NewLeague[id.na.nl] = NA
id.na.errors = sample(1:nrow(hitters.df),20,replace = F)
hitters.df$Errors[id.na.errors] = NA
new.hitters.del = handleMissing(hitters.df)
new.hitters.rand = handleMissing(hitters.df, "random")
any(is.na(new.hitters.del))
any(is.na(new.hitters.rand))
init.check.glob = apply(is.na(hitters.df), 2, which)
na.cols.glob = init.check.glob[sapply(init.check.glob,length) != 0]
na.rows.glob = unique(unlist(na.cols.glob))
hitters.df[na.rows.glob,]
new.hitters.rand[na.rows.glob,]
###' Comments: all NA rows are randomly replaced by other observations in the same column.
dim(hitters.df)
dim(new.hitters.del)
###' Comments: all NA rows are deleted.
###'
###' (Optional) You may also add the following third strategy.
###' mode: replace the missing values in the column of categorical variables with the most frequent value in the column,
###' and replace the missing values in the column of numeric with the average of the non-missing values. You may want to round this average if the type of the column is integer.
###' (I personally find it to be slightly more tricky to implement than the other two due to the differential treatment of different data types, which is why it is optional.)
new.hitters.mode = handleMissing(hitters.df, "mode")
new.hitters.mode[na.rows.glob,]

