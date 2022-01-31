#' ---
#' title: "Monto Carlo Integration and Variance Reduction"
#' author: "Biao Zhang"
#' date: "August 01, 2021"
#' ---
#'
#' ## Please do NOT share this with anyone outside this class. It will be considered as academic misconduct.
#'


#'
#'  1. (6 points) Write a function to compute a Monte Carlo estimate of $F(x)$, the cdf of $Beta(3, 3)$ cdf, and use the function to estimate $F(x)$ for $x = 0.1, 0.2,\ldots , 0.9$.
#'  Compare the estimates with the values returned by the pbeta function in R.
#'
beta.MC = function(x) {
  m = 1000
  t = rbeta(m, shape1 = 3, shape2 = 3)
  theta.hat.ind = mean((t <= x))
  return(theta.hat.ind)
}
set.seed(20211025)
sapply(c(1:9)/10, beta.MC)
pbeta(c(1:9)/10, shape1 = 3, shape2 = 3)
#'
#'
#'  2. (6 points) Find two importance functions $f_1$ and $f_2$ that are supported on $(1,\infty)$ and are "close" to
#'  $$g(x)=\frac{x^2}{\sqrt{2\pi}}e^{-x^2/2}$$ for $x>1$. (Hint: the distribution you sample from could be a simple transformation of a common distribution that you have seen in an intro stat class)
#'  Then for $m=1000$, implement and compare the two MC estimators based on importance sampling.
#'
#'
g = function(x) {ifelse(x > 1, x^2*exp(-x^2/2)/sqrt(2*pi), 0)}
f1 = function(x) {ifelse(x >= 0, x^3/2*exp(-x^2/2), 0)} # sqrt(rgamma(shape = 2,rate = .5)).
f2 = function(x) {ifelse(x >= 0, 1/2*exp(-x/2), 0)} # rexp(rate = .5).

curve(f1, from = 1, to = 10,  lty = 2, lwd = 2, col = 'red', 
      ylab = 'function values', main = 'Density Plot')
curve(g, from = 1, to = 10, add = T, col = 'black')
curve(f2, from = 1, to = 10, add = T, lty = 3, lwd = 3, col = 'blue')
legend("topright", legend = c("g(x)","sqrt(Gamma)", "Exp"), lty = c(1:3), lwd = c(1:3),
       col = c('black','red','blue'), cex = .65)

rgf1 <- function(x) g(x)/f1(x)
rgf2 <- function(x) g(x)/f2(x)

curve(rgf2,from=1,to=10,add = F,lty=3,lwd=3,col='blue',
      main = 'Ratio of Density Plot')
curve(rgf1,from=1,to=10,add = T,lty=2,lwd=2,col='red')
legend("topright", legend = c("g(x)/sqrt(Gamma)", "g(x)/Exp"), lty = c(1,2), lwd = c(1,2),
       col = c('red','blue'), cex = .65)

m = 1000
theta.hat = numeric(2)
names(theta.hat) = c('sqrt(Gamma)', 'Exp')
se.theta = theta.hat
set.seed(20211025)
x1 = sqrt(rgamma(m, shape = 2, rate = .5))
fg1 = g(x1)/f1(x1)
theta.hat[1] = mean(fg1)
se.theta[1] = sd(fg1)/sqrt(m)

x2 = rexp(m, rate = .5)
fg2 = g(x2)/f2(x2)
theta.hat[2] = mean(fg2)
se.theta[2] = sd(fg2)/sqrt(m)
theta.hat
se.theta
integrate(g, lower = 1, upper = Inf) # true value.
#'
#' 3. (8 points) Stratified sampling can be combined with importance sampling, and it is called stratified importance sampling. Suppose we want to estimate $\theta=\int_{a}^bg(x)dx$.
#' Stratified sampling says that we can devide $[a,b]$ into $k$ intervals with break points $a=a_0<a_1<\ldots<a_k=b$, and estimate $\theta_j=\int_{a_{j=1}}^{a_j}g(x)dx$ for $j=1,\ldots,k$ separately, and then sum them.
#'
#' There is nothing to stop us from esimating $\theta_j$ using importance sampling with an importance function $f_j(x)$.
#' In the class example on choice of importance function, our best result is obtained with importance function $f_3(x) = e^{-x}/(1-e^{-1}), 0<x<1$.
#' Now divide the interval $[0,1]$ into five subintervals with equal size. Estimate $\int_0^1 \frac{e^{-x}}{1+x^2} dx$ based on stratified importance sampling,
#' and compare the standard error with the standard error of the estimate based on importance sampling with $f_3$ as the importance function.
#' Here you can use $\frac{e^{-x}}{e^{-(j-1)/k}(1-e^{-0.2})}$ as the importance function on subinterval $\frac{j-1}{5}<x<\frac{j}{k}$.
#'
m = 100
nrep = 20
#Stratified Importance
g.sub <- function(x,a) ifelse(x>=a&x<=(a+.2),exp(-x)/(1+x^2),0)
f3.sub <- function(x,a) ifelse(x>=a&x<=(a+.2), exp(-x)/(exp(-a)*(1 - exp(-.2))),0)

estImpStrata <- function(k){
  imp.sub <- function(a) {
    u1 = runif(m/k)
    x1 = -log(exp(-a) - exp(-a)*(1 - exp(-.2))*u1)
    fg1 = g.sub(x1,a)/f3.sub(x1,a)
    return(mean(fg1))
  }
  est.Imp.Strata.vec = sapply(c(0:(k-1))/k, imp.sub)
  return(sum(est.Imp.Strata.vec))
}

set.seed(2021)
estImpStrata.v = replicate(nrep, estImpStrata(5))
#Importance from the class
g <- function(x) ifelse(x>=0&x<=1,exp(-x)/(1+x^2),0)
f3 <- function(x) ifelse(x>=0&x<=1,exp(-x)/(1-exp(-1)),0)
set.seed(2021)
u3 <- runif(m)
x3 <- - log(1 - u3 * (1 - exp(-1)))
fg3 <- g(x3) / f3(x3)

res = c(mean(estImpStrata.v), mean(fg3))
std.res = c(sd(estImpStrata.v), sd(fg3)/sqrt(m))
names(res) = c('Strata Imp', 'Imp')
names(std.res) = c('Strata Imp', 'Imp')
res #Estimates Comparison
std.res #Standard Errors Comparison
