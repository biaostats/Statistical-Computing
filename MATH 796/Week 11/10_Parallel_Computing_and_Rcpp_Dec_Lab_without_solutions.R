#' ---
#' title: "Speeding up computation: parallel computing and Rcpp"
#' author: "Biao Zhang"
#' date: "December 14th, 2021"
#' ---
#'
#' ## Please do NOT share the questions with anyone who are not enrolled in this class. Please do NOT share your code with anyone when you are enrolled in this class. They will be considered as academic misconduct.
#'
#'
#'#### Note 1: I have not figured out how to knitr with Rcpp. Please put your R code (including C++ code in cppFunction()) and the R output into a document such as a word document. If you write a stand-alone .cpp file, please also submit it along side with your R code and output.
#'
#'#### Note 2: the points of these lab questions are low, not because they are easy, but because I do not want you to worry too much about your grades when experimenting a new programming language near the end of the semester.
#'
#'#### Note 3: you can use either cppFunction() or sourceCpp(), and you can decide whether or not using RcppArmadillo. I do think RcppArmadillo is helpful in Q2 because you need to invert a matrix. But it can still be completed  without it.
#'
#'1. (3 points) Write an Rcpp equivalent of which.max() for an input numeric vector. There is an Rcpp sugar function max().
#'
#'Please test with the input vector c(1:5,5:1,1:5). Does your function returns all the indecies that the maximum is reached?
#'
setwd('H:\\MATH796\\Week 11\\')
library(Rcpp)
test_1 = c(1:5,5:1,1:5)
sourceCpp('10_Lab_Rcpp.cpp')
which_max_cpp(test_1)
which.max(test_1)
#'
#'2. (3 points) Please write an Rcpp function of Newton optimization methods for the following class example with a=0.8, and then test it using the same initial value c(5,1) as in the class example.
#'
#' Tips:For matrix A and B with appropriate dimensions, arma::solve(A,B) is the RcppArmadillo equivalent of the R function solve(A,B).
#' When A is invertable, it computes solve(A)%*%B. But different in R, A and B in RcppArmadillo must be arma::mat format.
#' Since these matrices are small, you can define an matrix first, and then fill each position with appropriate values.
#'
# f.quad = function(x,a) x[1]^2+2*a*x[1]*x[2]+x[2]^2
# f.grad.quad = function(x,a) c(2*x[1]+2*a*x[2],2*a*x[1]+2*x[2])
# f.hess.quad = function(x,a) matrix(c(2,2*a,2*a,2),2,2) # In this case, the Hessian is not a function of $x$, but a constant.
# 
# update.newton <- function(xk,f.grad,f.hess){
#   x.next = xk-t(solve(f.hess(xk))%*%f.grad(xk))
#   return(x.next)
# }

sourceCpp('10_Lab_Rcpp.cpp')
eps = 1e-12
iter.max = 1000
iter=0
err.x=1
x.init = matrix(c(5,1),ncol = 1)
x.all = x.init
while(err.x>eps&iter<iter.max){
  x.new = update_newton_cpp(x.init,.8)
  err.x = sqrt(sum((x.new-x.init)^2))
  x.all = cbind(x.all,x.new)
  iter=iter+1
  x.init=x.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= (',paste0(x.new,collapse = ','),') at iteration ',iter,'.'))

#'
#' 3. (4 points) Acceptance-Rejection Sampling.
#' The following is our class practice problem for AR sampling. Please convert the solution to an Rcpp function.
#' Its only input is n, and let us fix c=2 as before. Please check whether your output makes sense using qqplot.
#' Tips: rexp(),rchisq,dexp(),dchisq() are all Rcpp sugar functions. But their outputs are NumericVectors.
#' e.g, NumericVector x=rexp(1,1) generates 1 sample from Exp(rate=1), and return a NumericVector with length 1. To access its first element or convert it to double, you can use x[0].
#'
#'
#' Now let us consider an example of simulating from Exponential distribution Exp(1) using this method.
#' The Exponential random variable only takes positive value, it is more efficient to choose Y such that it also only take possible value.
#' Let us say we use  $\chi^2_1$ distribution as Y.
#'
# n = 1000
# c = 2
# k = 0 #counter for accepted
# j = 0 # counter for sample generated.
# x.exp = numeric(n)
# set.seed(100)
# while (k < n) {
#   j=j+1
#   u = runif(1)
#   x = rchisq(1,df=1) #random variate from g
#   if (dexp(x)/(dchisq(x,df=1)*c) > u) {
#     #we accept x
#     k = k + 1
#     x.exp[k] = x
#   }
# }
# j
sourceCpp('10_Lab_Rcpp.cpp')
set.seed(100)
n = 1000
x.exp = ar_sampling_exp_cpp(n)
plot(qexp((rank(x.exp)-0.5)/n,rate = 1),x.exp)
abline(a=0,b=1)
