#' ---
#' title: "Methods for generating random variables"
#' author: "Qi Zhang"
#' date: "August 01, 2021"
#' ---
#' 
#' 
#'
#' ## Under the hood: Methods for generating random variables
#' 
#' It is simple for us to say "generate random samples from a distribution". It is may not so easy for the computer to do.
#' While there have been a bunch of R functions for generating random samples from various distribution, there may be situations in practice where direct simulation from a certain distribution (say distribution B) is difficult. 
#' What can we do then?
#' The basic idea is to directly simulate samples from a distribution that is easy to do, say, distribution C, and then somehow process this random samples from C so that they become equivalent to random samples from B. 
#' 
#' In the following, we will introduce two class of such ideas: transformation and acceptance-rejection. 
#'   
#' #### Transformation methods
#' 
#' You may have learned this in an introductory statistics course. If you apply a transformation to a random variable, it will have different distribution.
#' Sometimes we can find a function such that once it is applied to the random samples from distribution C, the output are random samples from distribution B.
#' Simple examples of these transformations include the following 
#' 
#' * If $Z\sim N(0,1)$, then $V=Z^2\sim \chi^2_1$
#' * If $Z\sim N(0,1)$ and $V\sim \chi^2_m$, and they are independent, then $T=\frac{Z}{\sqrt{V/m}}$ has the Student t distribution with m degrees of freedom.
#' * If $U\sim Gamma(r,\lambda)$ and $V\sim Gamma(s,\lambda)$ and they are independent, then $X=\frac{U}{U+V}\sim Beta(r,s)$.
#' * If $Z_1,\ldots, Z_K$ are independent samples from $N(0,1)$, then $Z_1^2+\ldots+Z_K^2$ has $\chi^2_K$ distribution. 
#' * If $U\sim \chi^2_m$ and $V\sim \chi^2_n$ and they are independent, then $F=\frac{U/m}{V/n}\sim F_{m,n}$.
#' 
#' Now let us take a look at a computational example showing why this approach works.
#' Suppose we want to simulate from $\chi^2_1$, R function rchisq() can do it directly.
n=2000
set.seed(1)
x.rchisq = rchisq(n,df=1)
#' We can evaluate whether a data vector follow a certain distribution in many ways.
#' One such ways is QQ plot
x.rchisq.quantile <- qchisq(p=rank(x.rchisq)/(n+1),df=1) # Is this order() ? or rank() ?
plot(x.rchisq.quantile, x.rchisq)
abline(b=1,a=0)
#' We can also compare the histogram and the pdf
hist(x.rchisq,freq = F)
curve(dchisq(x,df=1),add=T)
#' Or the empirical cdf with the actual cdf
#' 
curve(ecdf(x.rchisq)(x))
curve(pchisq(x,df=1),add=T,lty=2)


#' Now let us try to simulate from the same distribution by transform samples from N(0,1)
set.seed(10000)
x.normal = rnorm(n)
x.normal.sq = x.normal^2
hist(x.normal)


#' How can we examine whether two data vectors are from the same distribution? There are statistical tests for it.
#' But we will use plot to examine it, two plots that you may have seen in an introductory stat class.
#' 
#' The first one is histogram, you can overlay the pdf of the target distribution to examine the fit.
#' 
#' Or you can use Quantile-Quantile plot (Q-Q plot), and a special case of it is called normal probability plot. 
#' Recall that Q-Q plot is a scatter plot of with the observed data in the y-axis, and their corresponding theoretical quantile values along the x-axis.
#' We use it to examine whether the observed data follows the theoretical distribution.
#'  If the observed data does not deviate from the 45 degree line (y=x) significantly, we will conclude that the theoretical distribution fit the observed data ok. 
#'  
#'  To generate the corresponding quantiles of the observed data, we need to first generate the positions, and then put then through qchisq()
#' rank() tells you what order the numbers are in, order() tells you how to get them in ascending order.
x.normal.sq.quantile <- qchisq(p=(rank(x.normal.sq) - .5)/n,df=1)
head(rank(x.normal.sq)/(n+1))
head(x.normal.sq.quantile)
head(x.normal.sq)
plot(x.normal.sq.quantile,x.normal.sq)
abline(b=1,a=0)

#' Now let us try another example of simulating $\chi^2_5$ by simulating from standard normal
#' 
n=2000
set.seed(10)
x.normal = matrix(rnorm(n*5),n,5)
x.sq.sum = rowSums(x.normal^2)

#' To check whether its distribution is what we expect it to be.

plot(qchisq(p=(rank(x.sq.sum)-0.5)/n,df=5),x.sq.sum)
abline(b=1,a=0)

#' 
#' #### Practice
#' 
#' 1. Simulate 2000 samples from $t_2$ by transforming independent normal and $\chi^2$ variables using the following fact.
#' If $Z\sim N(0,1)$ and $V\sim \chi^2_m$, and they are independent, then $T=\frac{Z}{\sqrt{V/m}}$ has the Student t distribution with m degrees of freedom.
#' Then please examine how well it fits the distribution.
#' 
#' 2. Simulate 2000 samples from Beta(2,3) distribution by transforming independent Gamma variables using the following fact.
#'  $U\sim Gamma(r,\lambda)$ and $V\sim Gamma(s\lambda)$ and they are independent, then $X=\frac{U}{U+V}\sim Beta(r,s)$.
#' Then please examine how well it fits the distribution.
set.seed(20211012)
n <- 2000
x <- rnorm(n)
v <- rchisq(n, 2)
t <- x/sqrt(v/2)
plot(qt(p = rank(t)/(n + 1), df = 2), t)
abline(b=1,a=0)

plot(dt(seq(-4,4, .01), df = 1), type = 'l')
lines(dnorm(seq(-4,4, .01)))

U <- rgamma(n, shape = 2)
V <- rgamma(n, shape = 3)
X <- U/(U + V)
plot(qbeta(p = (rank(X) - .5)/n, shape1 = 2, shape2 = 3), X)
abline(b=1,a=0)
#' 
#' #### Inverse transformation method
#' 
#' Inverse transformation method is the most important transformation. It is based on the following theorem
#' 
#' THEOREM (Probability Integral Transformation) If X is a continuous random variable with cdf $F_X(x)$, then $U = F_X(X) \sim Uniform(0, 1)$.
#' 
#' MATH 7/855 may cover this theorem in detail. For this class, you only need to memorize it as a fact, and understand its implication in simulating random samples.
#' 
#' Recall that the cumulative distribution function cdf $F_X(x)=P(X\leq x)$ is a monotone function whose range is $[0,1]$.
#' What this theorem implies is that if $x_1,...,x_n$ are a random sample of size $n$ from a distribution with cdf $F_X(x)$,
#'  then  $F_X(x_1),...,F_X(x_n)$ can be considered as a random sample of size $n$ from $Uniform(0,1)$.
#'  
#'  Since $F_X(x)$ is a monotone function, its inverse function exists!. Let us call it $F_X^{-1}(u)$. e.g., Let $X\sim Exponential(\lambda)$, its cdf is $F_X(x)=1-e^{-\lambda x}$, and its inverse function is $F_X^{-1}(u)=-\log(1-u)/\lambda$ 
#'  
#'  If we are able to generate $u_1,\ldots, u_n$, a random sample of size n from $Uniform(0,1)$, 
#'  then $F_X^{-1}(u_1),...,F_X^{-1}(u_n)$ can be considered as a random sample of size $n$ from a distribution with cdf $F_X(x)$
#'  So if we are able to simulate from $Uniform(0,1)$, we can simulate from any distribution, as long as we know its cdf $F_X(x)$ and its inverse function!
#'  
#'  The above arguments focus on the case continuous variables, but it also works for discrete variables, if you define the inverse function of cdf as the following
#'  $F_X^{-1}(u) = \text{the smallest x such that }F_X(x)=u$.
#'  
#'   To simulate a continuous random variable X, the inverse transformation method can be summarized as follows.
#'   
#'   1. Derive the inverse function $F_X^{-1}(u)$.
#'   2. Write a command or function to compute $F_X^{-1}(u)$
#'   3. For each random variate required (can be vectorized in R):
#'   (a) Generate a random sample u from Uniform(0,1).
#'   (b) Calculate and output $x = F_X^{-1}(u)$.
#'  
#'  Let us use this method to simulate from exponential distribution with lambda=2. The inverse function of cdf is 

inverse_cdf_exp <- function(u,lambda) -log(1-u)/lambda
#' We first simulate from Unif(0,1), and apply this function to get the random draws from the exponential distribution.
n=2000
set.seed(10000)
x.unif = runif(n)
x.exp = inverse_cdf_exp(x.unif,lambda=2)

plot(qexp((rank(x.exp)-0.5)/n,rate=2),x.exp)
abline(b=1,a=0)

#' For the common continuous/discrete distributions, the inverse function of cdf is q* function, that is the inverse function of pnorm() is qnorm()
#'  
#'  Let us use this method to simulate from N(2,3)
n=2000
set.seed(10000)
x.unif = runif(n)
x.norm = qnorm(x.unif,mean = 2,sd=3)

plot(qnorm((rank(x.norm)-0.5)/n,mean=2,sd=3),x.norm)
abline(b=1,a=0)



#'   To simulate a discrete random variable X, the inverse transformation method can be summarized as follows.
#'   
#'   Let $\ldots<x_{i-1}<x_i<x_{i+1}<\ldots$ be all the possible values of $X$ (all discontinuity point of $F_X(x)$). 
#'   The inverse transformation is $F_X^{-1}(u)=x_i$ if and only if $F_X(x_{i-1})<u\leq F_X(x_i)$. 
#'   
#'    For each random sample required (can be vectorized in R):
#'    
#'   (a) Generate a random sample u from Uniform(0,1).
#'   (b) Output $F_X^{-1}(u)$, that is the value $x_i$ such that $F_X(x_{i-1})<u\leq F_X(x_i)$.
#'   

#' Let us consider simulating from a discrete random variable X with probability mass function $P(X=0)=0.1$, $P(X=1)=0.2$, $P(X=2)=0.2$, $P(X=3)=0.2$, $P(X=4)=0.3$.
#' The cdf of X is 
cdf.x <- function(x) 0*(x<0)+0.1*(x>=0&x<1)+0.3*(x>=1&x<2)+0.5*(x>=2&x<3)+0.7*(x>=3&x<4)+1*(x>=4)
curve(cdf.x,-1,5)
#' The inverse cdf is 
inverse_cdf_x <- function(u) 0*(u>=0&u<=0.1)+1*(u>0.1&u<=0.3)+2*(u>0.3&u<=0.5)+3*(u>0.5&u<=0.7)+4*(u>0.7&u<=1)
curve(inverse_cdf_x,0,1)

n=2000
set.seed(10000)
x.unif = runif(n)
x = inverse_cdf_x(x.unif)

#' If there are not many values, compare it with the empirical probability.
#' If there are many values, use QQ plot.

plot(inverse_cdf_x((rank(x)-0.5)/n),x)
abline(b=1,a=0)

#' 
#' #### Practice:
#' 
#' 1. Simulate a random sample with size 2000 from gamma distribution Gamma(shape=3,rate=2) using inverse transformation method, and examine how well it fits the target distribution.
#' 
#' 2. Simulate a random sample with size 2000 from beta distribution Beta(2,3) using inverse transformation method, and examine how well it fits the target distribution.
#' 
n = 2000
set.seed(20211012)
x.unif = runif(n)
x.gamma = qgamma(x.unif, shape = 3, rate = 2)
plot(qgamma((rank(x.gamma) - .5)/n, shape = 3, rate = 2), x.gamma)
abline(b = 1, a = 0)

x.beta = qbeta(x.unif, shape1 = 2, shape2 = 3)
plot(qbeta((rank(x.beta) - .5)/n, shape1 = 2, shape2 = 3), x.beta)
abline(b = 1, a = 0)
#'
#' #### Generating from mixture distributions.
#' 
#' Mixture distribution are the distributions whose pdf/pmf and cdf can written as weighted averages of the corresponding pdf/pmf's  and cdf's of several simple distributions.
#' One classic example is normal mixture. The following display the pdf of the equally weighted mixture of five normal distributions (Bart Simpson distribution).
#' 
dbart = function(x,sd=0.4) 0.2*dnorm(x,-2,sd)+0.2*dnorm(x,-1,sd)+0.2*dnorm(x,0,sd)+0.2*dnorm(x,1,sd)+0.2*dnorm(x,2,sd)

plot(seq(-8,8,.01), dcauchy(seq(-8,8,.01), scale = 1.5), type = 'l', col = 'red')
lines(seq(-8,8,.01), dbart(seq(-8,8,.01)))
dlls = function(x) {.5*exp(-abs(x))}
curve(dlls, -5, 5)
lines(seq(-5,5,.01), dcauchy(seq(-5,5,.01)), col = 'red')
#' Simulating from a mixture distribution is a special type of transformation.
#' Suppose we want to simulate from a mixture distribution with pdf/pmf $f(x)=\pi_1 f_1(x)+\ldots+\pi_1 f_K(x)$, where $f_j(x)$ is the pdf/pmf of a random variable, and the weights $\pi_1,\ldots,\pi_K$ are positive and sum to 1. 
#' To simulate from this mixture distribution, we can do the following
#' 
#' 1. Generate an integer $k\in\{1,\ldots,K\}$, with $P(\text{select j}) = \pi_j$.
#' 2. If $k = j$, get a random sample with pdf/pmf $f_j(x)$, and output it.
#' 
#' Now let us simulate from a mixture of three gamma distributions Gamma(shape,scale) Gamma(1,1), Gamma(10,5), Gamma(2,30) with weights 0.1,0.4,0.5.
#' First define the parameters of each normal distribution component and the weights of these components.
n=2000
pi.all = c(0.1,0.4,0.5)
shape.all = c(1,10,1)
scale.all = c(1,5,3)
#' First simulate the group indeces. 
#' 
set.seed(100)
id.comp = sample(1:3,size=n,replace = T,prob = pi.all)
#' Then we simulate according to the group index.
x.mixgamma = sapply(id.comp, function(kk) rgamma(1,shape = shape.all[kk],scale = scale.all[kk]))
hist(x.mixgamma,breaks = 100)
#' You may use a for loop instead if you find it more convenient.
x.mixgamma.for = rep(NA,n)
for(kk in 1:5){
  n.kk = sum(id.comp==kk)
  x.mixgamma.for[id.comp==kk] = rgamma(n.kk,shape = shape.all[kk],scale = scale.all[kk])
}
hist(x.mixgamma.for,breaks = 100)


#' 
#' #### Practice
#' 
#' Now let us simulate from Bart Simpson distribution.
dbart = function(x,sd=0.4) 0.2*dnorm(x,-2,sd)+0.2*dnorm(x,-1,sd)+0.2*dnorm(x,0,sd)+0.2*dnorm(x,1,sd)+0.2*dnorm(x,2,sd)
n=2000
mu.all = c(-2:2)
id.comp = sample(1:5, size = n, replace = T, prob = rep(.2,5))
x.dbart.for = rep(NA, n)
for (jj in 1:5) {
  n.jj = sum(id.comp == jj)
  x.dbart.for[id.comp == jj] = rnorm(n.jj, mean = mu.all[jj], sd = .4)
}
hist(x.dbart.for, breaks = 100, prob = TRUE)
#' 
#' #### Acceptance-Rejection method
#' 
#' Transformation methods, including the inverse transformation sampling, requires the analytical form of the transformation function. It is not always easy to compute.
#' For inverse transformation methods, the inverse function of cdf is not always handy (e.g., what is the inverse function of the cdf of N(0,1)).
#' 
#' An alternative to transformation methods is acceptance-rejection method for sampling. One advantage of this method is that to apply it, you only need to evaluate pdf/pmf of this random variable instead of cdf or its inverse.
#' The basic idea of acceptane-rejection is that in order to simulate n samples from a random variable with pdf/pmf $f_X(t)$, you first simulate more than n samples (maybe $c\times n$ for some constant $c>1$)
#'  from another random variable Y with pdf/pmf $g_Y(t)$, and then "trim" these samples stochastically based on the relationship of the two pdf/pmfs, and what is left can be considered as n samples with distribution $f_X$. 
#' 
#' This method can be applied when the following assumption is true.
#' $$\frac{f_X(t)}{g_Y(t)}\leq c$$
#' for all values $t$ such that $f(t)>0$ and some constant $c$. 
#' What this assumption implies are that (1) The possible values of Y contain all possible values of X, and (2) Y has "heavier" tail than X.
#' 
#' The method can be summarized as the following.
#' 
#' 1. Find a random variable Y with density g satisfying $f_X(t)/g_Y(t)\leq c$, for all t such that $f_X(t)>0$. Provide a method to generate random Y.
#' 2. For each random sample from X required:
#' (a) Draw value y from Y with density $g_Y$.
#' (b) Generate a random u from the Uniform(0, 1) distribution.
#' (c) If $u <\frac{f_X(y)}{c\cdot g(y)}$, accept the value y as a random draw from X with density $f_X$. Otherwise, reject and discard y, and repeat Step 2.
#' 
#' It can be shown that for each sample generated form Y, the probability that it is accepted as a sample of X is 1/c.
#' So in order to simulate n samples from X, you expect to simulate c*n samples from Y, in average. 
#' For this reason, smaller c is more desirable, because it reduce the computational cost.
#' 
#' Now let us consider an example of simulating from the beta distribution Beta(2,2). Suppose we choose Uniform(0,1) as $g_Y$. Note that it has the same support as Beta(2,2)
#' 
#' Next, we need to figure out the value of c such that 
#' $\max_t\left(\frac{f_X(t)}{g_Y(t)}\right)\leq c$ where $g_Y(t) = 1$, and $f_X(t)=6t(1-t)$ for $0<t<1$.
#'  $\frac{f_X(t)}{g_Y(t)}=6t(1-t)$, and it can be shown using derivative method that it achieve the maximum when $t=0.5$, and the value is $1.5$, so $c=1.5$. 
#'  Actually any c larger than this value also works. Some there is no need to to find the exact maximum value of this ratio, and all we need is just an upper bound.
#'  In this case, it is obvious that and upper bound is 6, because t is between 0 and 1. So without any calculation, we can set $c=6$.  
#'  Later we will show that smaller value of c is preferred because it leads to lower reject rate, and thus higher computational efficiency.
#'  
#'  Next, let us apply the acceptance-rejection method. To check how many random samples from Y are generated, we also add a counter for it.
n = 1000
c = 1.5
k = 0 #counter for accepted
j = 0 # counter for sample generated.
x.beta = numeric(n)
set.seed(100)
while (k < n) {
  j=j+1
  u = runif(1)
  x = runif(1) #random variate from g
  if (6*x * (1-x)/c > u) {
    #we accept x
    k = k + 1
    x.beta[k] = x
  }
}
j

plot(qbeta((rank(x.beta)-0.5)/n,shape1 = 2,shape2 = 2),x.beta)
abline(a=0,b=1)

#' If we change c to 6, computational efficiency will decrease. 
n = 1000
c = 6
k = 0 #counter for accepted
j = 0 # counter for sample generated.
x.beta.6 = numeric(n)
set.seed(100)
while (k < n) {
  j=j+1
  u = runif(1)
  x = runif(1) #random variate from g
  if (6*x * (1-x)/c > u) {
    #we accept x
    k = k + 1
    x.beta.6[k] = x
  }
}
j

plot(qbeta((rank(x.beta.6)-0.5)/n,shape1 = 2,shape2 = 2),x.beta.6)
abline(a=0,b=1)

#' As you may have expected, in order to generate n random draws from X, one may need to draw from Y for about c*n times.
#' One can prove in theory that the output has exactly the same distribution (same pdf/pmf) as $X$. 
#' We will not show any theoretical results in this class. For those who are interested, you can check the references of this class (page 69 of Rizzo 2019, page 309 of Gentle 2009, and page 155 of Givens and Hoeting 2013)
#' 


#' 

#' 
#' #### Practice
#' 
#' Now let us consider an example of simulating from Exponential distribution Exp(1) using this method.
#' The Exponential random variable only takes positive value, it is more efficient to choose Y such that it also only take possible value. 
#' Let us say we use  $\chi^2_1$ distribution as Y.
#' 
n = 1000
c = 2
k = 0 #counter for accepted
j = 0 # counter for sample generated.
x.exp = numeric(n)
set.seed(100)
while (k < n) {
  j=j+1
  u = runif(1)
  x = rchisq(1, df = 1) #random variate from g
  if (sqrt(2*pi*x)*exp(-x/2)/c > u) {
    #we accept x
    k = k + 1
    x.exp[k] = x
  }
}
j

plot(qexp((rank(x.exp)-0.5)/n,rate = 1),x.exp)
abline(a=0,b=1)
#' Solution: we need to figure out the value of c such that 
#' $\max_t\left(\frac{f_X(t)}{g_Y(t)}\right)\leq c$ where $g_Y(t) = \frac{t^{-1/2}e^{-t/2}}{\sqrt{2\pi}}$, and $f_X(t)=e^{-t}$
#'  $\frac{f_X(t)}{g_Y(t)}=\sqrt{2\pi t} e^{-t/2}$, and it can be shown using derivative method that it achieve the maximum when $t=1$, and the value is $\sqrt{2\pi/e}$, so $c=\sqrt{2\pi/e}$. 
#'  Actually any c larger than this value also works. Some there is no need to to find the exact maximum value of this ratio, and all we need is just an upper bound. 
#'  For simplicity, let us use c=2 here.
#'  
