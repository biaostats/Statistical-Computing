#' ---
#' title: "Monto Carlo Integration and Variance Reduction"
#' author: "Qi Zhang"
#' date: "August 01, 2021"
#' ---
#' 
#' 
#'  ## Monte Carlo Integration 
#' 
#' Consider estimating an integral.
#' 
#' Simple Monte Carlo Estimator
#' 
#' Consider the problem of estimating $\theta = \int_0^1 g(x)dx$. If $X_1,\ldots, X_m$ is a random Uniform(0,1) sample, then
#' 
#' $$\hat{\theta} =\overline{g_m(X)}= \frac{1}{m}\sum_{i=1}^m g(X_i)$$ converges to $E[g(X)] = \theta$ with probability 1, by the Strong Law of Large Numbers. 
#' The simple Monte Carlo estimator of $\int_0^1 g(x)dx$ is $\overline{g_m(X)}$.
#' 
#' Example:
#' Compute a Monte Carlo estimate of $\theta = \int_0^1 e^{-x}dx$ and compare the estimate with the exact value.
#' 
#' If your calculus is not rusty, you should be able to find the exact value $\theta = \int_0^1 e^{-x}dx=1-e^{-1}\approx 0.6321$.
#' 

m <- 100
set.seed(07102021)
x <- runif(m)
theta.hat <- mean(exp(-x))
print(theta.hat)
print(1 - exp(-1))

#' 
#' What if we want to estimate $\theta = \int_a^b g(x)dx$? We can certainly make use of a random $Uniform(a,b)$ sample. But is its simple MC estimator still $\overline{g_m(X)}$?
#' 
#' Recall that $\overline{g_m(X)}$ converges to $E[g(X)]$. What is $E[g(X)]$ for $Uniform(a,b)$? We know the pdf of $Uniform(a,b)$ is $\frac{1}{b-a}$ in the interval $[a,b]$, and 0 otherwise. 
#' 
#'$$  E[g(X)]= \int_a^b \frac{1}{b-a} g(x)dx = \frac{1}{b-a}\int_a^b g(x)dx = \frac{\theta}{b-a}$$
#' 
#' So what should be the simple MC estimator of $\theta = \int_a^b g(x)dx$? Please find out, and apply it to the following practice example.
#' 
#' 
#' #### Practice 
#' 
#' Compute a Monte Carlo estimate of $\theta = \int_2^4 e^{-x}dx$ and compare the estimate with the exact value.
#' 
#' The exact value $\theta = \int_2^4 e^{-x}dx=e^{-2}-e^{-4}\approx 0.1170$.
#' 
m.0 <- 100
set.seed(07102021)
x.0 <- runif(m.0, min = 2, max = 4)
theta.hat.0 <- 2*mean(exp(-x.0))
print(theta.hat.0)
print(exp(-2) - exp(-4))
#' 
#' 
#' 
#' 
#' 
#' In the above example, we have shown that $E[g(X)]= \frac{\theta}{b-a}$.
#' Since we want to sample from $Uniform(a,b)$, we write $\theta = E[(b-a)g(X)] = \int_2^4(4-2)e^{-x}\cdot \frac{1}{4-2}dx$.
#' The function to be integrated is the product of $(4-2)e^{-x}$ and the pdf of the distribution that we sample from. The simple MC estimator is $\frac{1}{m}\sum_{i=1}^m (4-2)e^{-X_i}$.
#' 
#' This factorization can be applied to define MC estimators that take samples from other distributions as well.
#' For example, in the context of the first example, we know that $e^{-x}$ is the pdf of $Exponential(1)$ with support $[0,\infty)$.
#' We can re-write the integral of interests as $\theta = \int_0^1 e^{-x}dx = \int_0^\infty I(x\leq 1) e^{-x} dx$.
#' Here $I(statement)$ is an indicator function that takes value 1 if "statement" is true, and 0 otherwise. In this example, $I(x\leq 1)$ is a function of $x$ that equals to 1 if and only if $x\leq 1$. 
#' If $X_1,\ldots,X_m$ is a a random $Exponential(1)$ sample, another MC estimator of $\theta$ is $\frac{1}{m}\sum_{i=1}^m I(X_i\leq 1)$.  

m <- 100
set.seed(07102021)
x <- rexp(m,rate=1)
theta.hat.ind <- mean((x<=1))
print(theta.hat.ind)
print(1 - exp(-1))

#' 
#' This factorization is especially helpful when the integration is over an unbounded interval.
#' 
#' Example: compute a MC estimate of $\theta = \int_0^{\infty}xe^{-x}dx$. Since $e^{-x}$ is the pdf of $Exponential(1)$, this is just its population mean. 
#' If your calculus is not rusty, you can show that it is 1.
#' 
m <- 100
set.seed(07102021)
x <- rexp(m,rate=1)
theta.hat.mean <- mean(x)
print(theta.hat.mean)

#' 
#' #### Practice:
#' Compute a MC estimate of $\theta = \int_{-\infty}^{2}\frac{1}{\sqrt{2\pi}}e^{-x^2/2}dx$. As you may have recognized, $\frac{1}{\sqrt{2\pi}}e^{-x^2/2}$ is the pdf of $N(0,1)$
#' Please compute its MC estimate and compare with the deterministic calculation in R.
#' 
m.1 <- 100
set.seed(07102021)
x.1 <- rnorm(m.1)
theta.hat.mean.1 <- mean((x.1 <= 2))
print(theta.hat.mean.1)
print(pnorm(2))
#' 

#' 
#' 
#' #### Why do we care about MC integration? 
#' 
#' Some mathematical statistics:
#' 
#' A parameter of interests is of often the expectation of some function of the data.
#' Suppose we have random sample from a distribution $f_X(x)$, we are often interested in parameters that can be written as $\theta=E(h(X))$.
#' e.g., If $\theta$ is the population mean, $h(x) = x$. If $\theta$ is the population variance, $h(x) = (x-\mu)^2$ where $\mu=E(X)$.
#' If $X$ is a continuous random variable, $E(h(X))$ is an integral, and if discrete, a sum.
#' This is one reason why we are interested in estimating integrals.
#' 
#' Another reason: for any non-negative function $h(X)$, if its integral/sum over continuous/discrete domain D is finite, we can make it a probability density/mass function for some random variable.
#'  This integral/sum is called a normalizing constant. We are often interested in estimating this normalizing constant for further probability calculation. 
#' 
#'   
#' As you have seen in MC Inferenece, whenever we estimate something deterministic (e.g., an integral, a parameter) with something random (estimate from data), the output is subject to bias and variability
#'  The estimators are usually evaluated based on bias and standard error, and we want both of bias and variability to be small.
#'  Many statistical ideas are developed with these goals. 
#'  The procedures concerned in this module are generally unbiased (bias=0), and we will learn statistical ideas aiming at reducing the variability.
#'  
#'  To evaluate whether these ideas achieve their claimed goal, we need to derive the variance of simple MC estimator as the baseline.
#'  
#'  The simple MC estimator of $\theta = \int_a^b g(x)dx$ is $(b-a)\overline{g_m(X)} = \frac{b-a}{m}\sum_{i=1}^m g(X_i)$. Since $X_1,\ldots, X_m$ are iid samples with $Uniform(a,b)$ distribution  
#'  Therefore,
#'  
#'  $$Var\left((b-a)\overline{g_m(X)}\right)= Var\left(\frac{b-a}{m}\sum_{i=1}^m g(X_i)\right)= \frac{(b-a)^2}{m^2}Var\left(\sum_{i=1}^m g(X_i)\right)= \frac{(b-a)^2}{m}Var\left(g(X)\right)= \frac{1}{m}Var\left((b-a)g(X)\right)$$
#'  
#'  Here, the variance is over the distribution $Uniform(a,b)$.
#'  
#'  This can be used to calculate the number of simulations needed in order to achieve small enough standard error. If we want to standard error to be smaller than a number SE, there must be $Var\left((b-a)g(X)\right)/m\leq SE^2$. Thus, the minimal $m$ needed is $m\geq Var\left((b-a)g(X)\right)/SE^2$.
#'  
#'  Note that in practice, the true variances of these estimators are generally unknown, and need to be estimated using random samples. 
#'  Let $Y_i=(b-a)g(X_i)$, then an estimate of $Var\left((b-a)\overline{g_m(X)}\right)$ is $\frac{1}{m}\sum_{i=1}^m(Y_i-\bar{Y})^2$.
#'  

#'  
#'  If $\hat{\theta}_1$, and  $\hat{\theta}_2$ are two estimators for the same parameter $\theta$, then $\hat{\theta}_1$ is more efficient than $\hat{\theta}_2$ (in a statistical sense) if $Var(\hat{\theta}_1)/Var(\hat{\theta}_2)<1$.
#'  One can further quantify how much $\hat{\theta}_2$ is better than $\hat{\theta}_1$ by the percent reduction in variance achieved by using $\hat{\theta}_2$ instead of $\hat{\theta}_1$. It is 
#'  
#'  $$100\left(\frac{Var(\hat{\theta}_1)-Var(\hat{\theta}_2)}{Var(\hat{\theta}_1)}\right)$$
#'  
#'  After the practice, we will learn some more interesting MC integration methods that have smaller variance than the simple MC estimator, and evaluate them based on there variances.  
#'    
#'    

#' 
#' #### Practice
#'  
#'  1. Compute a Monte Carlo estimate of the standard normal cdf, by generating from the Uniform(0,x) distribution. Note that since we know $\Phi(0)=1/2$, 
#'  and for $x>0$, $\Phi(x)=P(X\leq x)=P(X\leq 0) + P(0<X\leq x)$ where $X$ is standard normal. So we just need to estimate $P(0<X\leq x)$ and then add $1/2$.
#'  Compare your estimates with the normal cdf function pnorm. Compute an estimate of the variance of your Monte Carlo estimate of $\Phi(2)$, and a 95% confidence interval for it.
#'  
#'  
pnorm.mc = function(x, n){
  if (x > 0) {
    t = runif(n, min = 0, max = x)
    phi.hat = x*mean(exp(-t^2/2))/sqrt(2*pi)
    se.hat = var(x*exp(-t^2/2))/(2*pi*n)
    result = c((.5 + phi.hat),se.hat)
    names(result) = c('phi.hat', 'se.hat')
    return(result)
  }
  
  t = rnorm(n)
  phi.hat = mean((t <= x))
  return(phi.hat)
}
set.seed(20211019)
print(pnorm.mc(1, 100))
print(pnorm(1))
CI = pnorm.mc(1, 100)
names(CI) = c('UCL95', 'LCL95')
CI[1] + 1.96*sqrt(CI[2])
CI[1] - 1.96*sqrt(CI[2])

print(pnorm.mc(-1, 5000)) # set larger n, o.w it is not accurate.
print(pnorm(-1))
#'  
#' #### Important sampling     
#'  
#'  Importance sampling is a generalization of the factorization technique you have seen in the previous examples.
#'  Essentially, it view the integration problem as an expected value problem. 
#'  Consider estimating $\theta = \int g(x) dx$. Suppose $X$ is a random variable with density function $f(x)$, such that $f(x) > 0$ on the set $\{x:g(x) \neq 0\}. 
#'  Define random variable $Y=g(X)/f(X)$. Then
#'  
#'  $$\theta = \int g(x) dx =\int \frac{g(x)}{f(x)}f(x) dx=\int yf(y) dy=E[Y]$$
#'  
#'  Estimate $E[Y]$ by simple Monte Carlo integration. That is, compute the average $\frac{1}{m}\sum_{i=1}^m Y_i=\frac{1}{m}\sum_{i=1}^m \frac{g(X_i)}{f(X_i)}$, where $X_1,\ldots,X_m$ are generated from the distribution with density $f(x)$.
#'  The density $f(x)$ is called the importance function.
#'  
#'  In an importance sampling method, the variance of the estimator based on $Y = g(X)/f(X)$ is $Var(Y)/m$, so the variance of Y should be small.
#'  Different choice of $f(x)$ may lead to different variances. The variance of $Y$ is small if $Y$ is nearly constant, 
#'  so the density $f(x)$ should be "close" to $g(x)$ in shape. Also, the variable with density $f(x)$ should be reasonably easy to simulate.
#'  

#'  
#'  
#'  Example: (Choice of the importance function).
#'  
#'  Compare MC estimates of $\int_0^1 \frac{e^{-x}}{1+x^2} dx$ with different choice of importance functions.
#'  The candidates for the importance functions are
#'  
#'  $$f_0(x) = 1, 0 < x < 1$$
#'  $$f_1(x) = e^{-x}, 0 <x <\infty$$,
#'  $$f_2(x) = (1+x^2)^{-1}/\pi, -\infty < x < \infty$$,
#'  $$f_3(x) = e^{-x}/(1-e^{-1}), 0<x<1$$
#'  $$f_4(x) = 4(1+x^2)^{-1}/\pi, 0<x<1$$.
#' The function to be integrated is 
#' $g(x) = \frac{e^{-x}}{1+x^2}$ for $0<x<1$, and 0 otherwise. While all five of the possible importance functions are positive on the set $0<x<1$ where $g(x) > 0$.
#'  $f_1$ and $f_2$ have larger ranges and many of the simulated values will contribute zeros to the sum, which is inefficient. 
#'  All of these distributions are easy to simulate; $f_2$ is standard Cauchy or $t(df = 1)$.
#'  The densities are plotted on (0,1) for comparison with g(x) as the following.
#'  

g <- function(x) ifelse(x>=0&x<=1,exp(-x)/(1+x^2),0)
f0 <- function(x) ifelse(x>=0&x<=1,1,0)
f1 <- function(x) ifelse(x>=0,exp(-x),0)
f2 <- function(x) 1/(pi*(1+x^2))
f3 <- function(x) ifelse(x>=0&x<=1,exp(-x)/(1-exp(-1)),0)
f4 <- function(x) ifelse(x>=0&x<=1,4/(pi*(1+x^2)),0)

curve(g,from=0,to=1,col='black',ylim=c(0,2),ylab = 'function values')
curve(f0,from=0,to=1,add = T,lty=2,lwd=2,col='red')
curve(f1,from=0,to=1,add = T,lty=3,lwd=3,col='blue')
curve(f2,from=0,to=1,add = T,lty=4,lwd=4,col='green')
curve(f3,from=0,to=1,add = T,lty=5,lwd=5,col='brown')
curve(f4,from=0,to=1,add = T,lty=6,lwd=6,col='purple')
legend(x='topright',legend = c('g','f0','f1','f2','f3','f4'),col = c('black','red','blue','green','brown','purple'),lty=1:6,lwd = 1:6)

#' Even though $f_1$ is the closest to $g$ in values, the function that corresponds to the most nearly constant ratio $g(x)/f(x)$ appears to be $f_3$. 
#' From the graph, we might prefer f3 for the smallest variance.
#' 
m <- 100
theta.hat <-  numeric(5)
names(theta.hat) = c('f0','f1','f2','f3','f4')
se.theta = theta.hat
set.seed(1234)
x0 <- runif(m) #using f0
fg0 <- g(x0)
theta.hat[1] <- mean(fg0)
se.theta[1] <- sd(fg0)/sqrt(m)

x1 <- rexp(m, 1) #using f1
fg1 <- g(x1) / f1(x1)
theta.hat[2] <- mean(fg1)
se.theta[2] <- sd(fg1)/sqrt(m)

x2 <- rcauchy(m) #using f2
fg2 <- g(x2) / f2(x2)
theta.hat[3] <- mean(fg2)
se.theta[3] <- sd(fg2)/sqrt(m)

u3 <- runif(m) #f3, inverse transform method
x3 <- - log(1 - u3 * (1 - exp(-1)))
fg3 <- g(x3) / f3(x3)
theta.hat[4] <- mean(fg3)
se.theta[4] <- sd(fg3)/sqrt(m)

u4 <- runif(m) #f4, inverse transform method
x4 <- tan(pi * u4 / 4)
fg4 <- g(x4) / f4(x4)
theta.hat[5] <- mean(fg4)
se.theta[5] <- sd(fg4)/sqrt(m)

rbind(theta.hat, se.theta)

#'  
#'  The computational results are consistent with our intuition.
#'  In particular, the two distributions $f_1$ and $f_2$ lead to very large SE, partially because the their supports are much wider, and consequently a large proportion of samples contribute 0 to the estimate.
#'  Let us check the proportions of 0's.
#'  
c(mean(fg0==0),mean(fg1==0),mean(fg2==0),mean(fg3==0),mean(fg4==0))
#'  
#'  

#'  
#'  Some theoretical results: consider estimating  $\theta = \int_A g(x) dx $ with importance sampling by sampling from distribution $\phi(x)$.
#'  There is $\theta = \int_A g(x) dx = \int_A \frac{g(x)}{\phi(x)}\phi(x) dx $. The optimal importance sampling distribution that leads to the smallest variance is 
#'  
#'  $$ \phi(x) = \frac{|g(x)|}{\int_A |g(x)| dx} $$
#'  
#'  Unfortunately, this result cannot be applied in practice, because it requires estimating $\int_A |g(x)| dx$, which is not available until we solve our target problem of estimating $\int_A g(x) dx$.
#'  The practical implication of this result is that one should try to find a sampling distribution whose pdf is as close to the normalized $|g(x)|$ (on $A$) as possible.
#'  

#'  
#' #### Practice
#'  
#'  1. Compute a Monte Carlo estimate of $\int_0^1 sin(\pi x)dx$ using importance sampling with a Beta distribution as the importance function. You need to justify your choice of shape parameters (not necessarily optimal) using visual comparisons.
#'  Compare your estimates with the exact value. What should the exact value be?
#'
#' Note: It is better to use inverse transformation to do it. It should be more flexible.
g.1 = function(x) sin(pi*x)
curve(g.1, from = 0, to = 1, ylim = c(0, 2.5),col = 'black', ylab = 'function values')
lines(seq(0,1,.01), dbeta(seq(0,1,.01), shape1 = .6, shape2 = .6), type = 'l', lty = 2, lwd = 2, col = 'red')
lines(seq(0,1,.01), dbeta(seq(0,1,.01), shape1 = 1, shape2 = 1), lty = 3, lwd = 3, col = 'blue')
lines(seq(0,1,.01), dbeta(seq(0,1,.01), shape1 = 1.2, shape2 = 1.2), lty = 4, lwd = 4, col = 'green')
lines(seq(0,1,.01), dbeta(seq(0,1,.01), shape1 = 1.5, shape2 = 1.5), lty = 5, lwd = 5, col = 'yellow')
lines(seq(0,1,.01), dbeta(seq(0,1,.01), shape1 = 2, shape2 = 2), lty = 6, lwd = 6, col = 'purple')

theta.mc = function(shape, n) {
  x = rbeta(n, shape1 = shape, shape2 = shape)
  fg = g.1(x)/dbeta(x, shape1 = shape, shape2 = shape)
  result = c(mean(fg), sd(fg)/sqrt(n))
  names(result) = c('theta.hat', 'se.theta')
  
  return(result)
}

sapply(c(.6,1,1.2,1.5,2), theta.mc, n = 100)
2/pi
#'              
#' #### Stratified Sampling
#'  
#'  An alternative approach to variance reduction is to reduce the variability of the random samples via stratified sampling.
#'  Stratified sampling divides the interval into segments (strata), estimates the integral on each stratum separately, and add them together.
#'  The sum is an estimate of the original target integral over the whole interval, and it has smaller variance than the estimate from the standard approach (assured by theory).  
#'  
#'  Now let us reconsider the previous example of estimating $\int_0^1 \frac{e^{-x}}{1+x^2} dx$.
#'  Devide the interval into, say, four subintervals of equal sizes, and compute a Monte Carlo estimate of the integral on each subinterval using $1/4$ of the total number of replicates. 
#'  Then sum these four estimates to obtain the estimate of the original target. That is, instead of estimating $\int_0^1 \frac{e^{-x}}{1+x^2} dx$ using $M=10000$ random samples from $Unif(0,1)$,
#'  we estimate each of $\int_0^0.25 \frac{e^{-x}}{1+x^2} dx$, $\int_0.25^0.5 \frac{e^{-x}}{1+x^2} dx$, $\int_0.5^0.75 \frac{e^{-x}}{1+x^2} dx$ and $\int_0.75^1 \frac{e^{-x}}{1+x^2} dx$ using $M/4=2500$ random samples from the corresponding uniform distribution over its interval.
#'    

g <- function(x) ifelse(x>=0&x<=1,exp(-x)/(1+x^2),0)

m = 100
set.seed(2021)
(est.g.direct = mean(g(runif(m,0,1))))# direct estimate
est.strata.1 = 0.25*mean(g(runif(m/4,0,0.25))) # estimate over interval [0,0.25], the density of Unif(0,0.25) is 1/0.25, which is why the mean needs to be multiplied by 0.25
est.strata.2 = 0.25*mean(g(runif(m/4,0.25,0.5))) # estimate over interval [0.25,0.5]
est.strata.3 = 0.25*mean(g(runif(m/4,0.5,0.75))) # estimate over interval [0.5,0.75]
est.strata.4 = 0.25*mean(g(runif(m/4,0.75,1))) # estimate over interval [0.75,1]
(est.strata = est.strata.1 + est.strata.2+est.strata.3+est.strata.4)

#' Now let us repeat the above for 20 times to check whether the stratefied estimator has smaller variance than the direct estimator over different runs.

nrep = 20
set.seed(2021)
est.g.direct.vec = replicate(nrep, mean(g(runif(m,0,1))))

estStrata <- function(ii){
  est.strata.1 = 0.25*mean(g(runif(m/4,0,0.25))) # estimate over interval [0,0.25], the density of Unif(0,0.25) is 1/0.25, which is why the mean needs to be multiplied by 0.25
  est.strata.2 = 0.25*mean(g(runif(m/4,0.25,0.5))) # estimate over interval [0.25,0.5]
  est.strata.3 = 0.25*mean(g(runif(m/4,0.5,0.75))) # estimate over interval [0.5,0.75]
  est.strata.4 = 0.25*mean(g(runif(m/4,0.75,1))) # estimate over interval [0.75,1]
  output = est.strata.1 + est.strata.2+est.strata.3+est.strata.4
  return(output)  
}

est.g.strata.vec = sapply(1:nrep, estStrata)

c(mean(est.g.direct.vec),mean(est.g.strata.vec))
c(sd(est.g.direct.vec),sd(est.g.strata.vec))


#'  
#'  Where does the reduction in standard error come from? Recall that standard error measures the variability of an estimator, and the variability of the estimator comes from the variability of the data.
#'  Intuitively, if the variability of the input data is reduced, the variability of the output estimate will decrease. 
#'  Stratified sampling reduces the variability of the input data. 
#'  
#'  To see this point, consider this. When you take a random sample of size M from $Unif(0,1)$, how many samples do you expect to be located in the sub-inverval $[0,0.25]$? It is $M/4$ 
#'  In an actual random sample of M, what is the chance of observing exactly that many.(Hint: what is the distribution of the number of points that fall in $[0,0.25]$ out of M independent observations of $Unif(0,1)$?) 
#'  The chance is very low, and we usually observe something different. The following shows a histogram of the number of points that fall in $[0,0.25]$ out of 100 $Unif(0,1)$ samples.
#'  
m=100
hist(replicate(1000, sum(runif(m,0,1)<=0.25)))

#' Stratified sampling divides the interval, and determine the sample size taken from each sub-interval based on what is expected from M independent observations from the whole interval.
#' In some sense, it removes the variability across the sub-intervals, which leads to reduction in the consequent standard error of the estimator.
#' 
#' In light of this, let us consider the impact of number of strata. Previously we divide the interval into four sub-intervals. If we chop it finer, do you expect the variability to increase or decrease? 
#' Let us find out by re-do the above example with different numbers of strata.  

m=100
nrep = 20

estStrata <- function(k){ # this function is for the stratified sampling estimator of the integral with k strata 
  estGJ = function(j) (1/k)*mean(g(runif(m/k,(j-1)/k,j/k))) # For given j, this internal function estimate the integral over interval [(j-1)/k,j/k] 
  est.strata.vec = sapply(1:k, estGJ)
  return(sum(est.strata.vec))  
}

k.vec = c(1,2,4,10,20)

set.seed(2021)
est.g.mat = replicate(nrep, sapply(k.vec,estStrata))
rownames(est.g.mat) = k.vec
dim(est.g.mat)
rowMeans(est.g.mat)
apply(est.g.mat,1,sd)

#' 
#' How does the standard error change?
#' 

#'  
#' #### Practice:
#'  
#'  1. Write a function to compute a Monte Carlo estimate of the standard normal cdf $\Phi(x)$ for $x>0$, by stratified sampling over interval $[0,x]$ with 5 strata. 
#'  For $x=1,2$, show empirically that the estimate based on stratified sampling has smaller standard error than the one based on samples from $Unif(0,x)$.
#'    
m.prac=100
nrep.prac = 20

estStrata.prac <- function(x, k){ # this function is for the stratified sampling estimator of the integral with k strata 
  estGJ = function(j, x.in) (x.in/k)*mean(dnorm(runif(m.prac/k,x.in*(j-1)/k,x.in*j/k))) # For given j, this internal function estimate the integral over interval [(j-1)/k,j/k] 
  est.strata.vec = sapply(1:k, estGJ, x.in = x)
  return(sum(est.strata.vec))  
}

set.seed(2021)
est.pnorm.strata = replicate(nrep.prac, sapply(c(1,2), estStrata.prac, k = 5))
rownames(est.pnorm.strata) = c('x.strata = 1', 'x.strata = 2')
est.pnorm = replicate(nrep.prac, sapply(c(1,2), function(x){x*mean(dnorm(runif(m.prac,0,x)))}))
rownames(est.pnorm) = c('x = 1','x = 2')
 
rowMeans(est.pnorm.strata)
rowMeans(est.pnorm)

apply(est.pnorm.strata, 1, sd)
apply(est.pnorm, 1, sd)