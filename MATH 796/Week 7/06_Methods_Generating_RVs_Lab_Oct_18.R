#' ---
#' title: "Methods for generating random variables"
#' author: "Biao Zhang"
#' date: "August 01, 2021"
#' ---
#' 
#' 
#'  
#'  1. (3 points) Write a function that will generate and return a random sample of size n from the two-parameter exponential distribution $Exp(\lambda, \eta)$ 
#'  for arbitrary $n$, $\lambda$, and $\eta$ with pdf $f(x)=\lambda e^{-\lambda(x-\eta)}$ for $x\geq \eta$.
#'  Generate 1000 sample from $Exp(\lambda=3, \eta=2)$ using transformation method, and evaluate the fit of the data to the target distribution with histogram,
#'   and overlay the density curve of the target distribution.
#'  
set.seed(20211015)
rtwo.pars.exp = function(n, lambda, eta) {
 y = rexp(n, rate = lambda)
 x = y + eta
 return(x)
}
x.exp = rtwo.pars.exp(1000, lambda = 3, eta = 2)
hist(x.exp, breaks = 20, xlab = 'X', prob = TRUE, main = "Histogram of Loc-Exponential dist")
lines(seq(2,4.5,.01), 3*exp(-3*(seq(2,4.5,.01) - 2)), col = 'red')
#'   
#'  2. (3 points) The $Pareto(a, b)$ distribution has cdf $F(x) = 1 - (\frac{b}{x})^a$ for $x\geq b$ where $b>0$, and pdf $f(x)=\frac{a b^a}{x^{a+1}}$. 
#'  Derive the probability inverse transformation and use the inverse transform method to simulate a random sample from the $Pareto(4, 2)$ distribution.
#' Graph the density histogram of the sample, and evaluate the fit of the data to the target distribution with Q-Q plot.
#' 
library(VGAM)
set.seed(20211015)
inverse_cdf_pareto = function(x, a, b) exp(log(b) - log(1 - x)/a) 
x.unif = runif(2000)
x.pareto = inverse_cdf_pareto(x.unif, a = 4, b = 2)
hist(x.pareto, breaks = 20, xlab = 'X', prob = TRUE, main = "Histogram of Pareto dist")
lines(seq(2,14,.01), dpareto(seq(2,14,.01), scale = 2, shape = 4), col = 'red')
plot(qpareto((rank(x.pareto) - .5)/2000, scale = 2, shape = 4), x.pareto)
abline(b = 1, a = 0)
#' 
#' 3. (5 points) Now let us simulate from a mixture of four poisson distributions with lambda=0.7,10,25,40 with weights 0.1,0.3,0.3,0.3.
#' 
w.all = c(.1,.3,.3,.3)
lambda.all = c(.7,10,25,40)

set.seed(20211015)
id.comp = sample(1:4,size = 2000,replace = TRUE,prob = w.all)
x.mixpoiss.for = rep(NA, 2000)
for (jj in 1:4) {
  n.jj = sum(id.comp == jj)
  x.mixpoiss.for[id.comp == jj] = rpois(n.jj, lambda = lambda.all[jj])
}
hist(x.mixpoiss.for, breaks = 100)
#'  
#'  4. (5 points) The standard Laplace distribution has density $f(x) = \frac{1}{2}e^{-|x|}$. Generate a random sample of size 1000 from this distribution using acceptance rejection method
#'   using Cauthy distribution ($g_Y(y)=\frac{1}{\pi(1+x^2)}$, it is also t distribution with df=1) as "Y".
#'   Please evaluate the fit of the data to the target distribution by plotting the histogram (relative frequency) of the generated data, and overlay the density curve of the target distribution.
#'
#' Through the mathematical calculations, c = pi/e, for convenience, I set c = 1.5.
c = 1.5
k = 0 # counter for accepted
j = 0 # counter for sample generated.
x.laplace = numeric(1000)
set.seed(20211015)
while (k < 1000) {
  j=j+1
  u = runif(1)
  x = rcauchy(1) #random variate from g
  if (.5*exp(-abs(x))*pi*(1+ x^2)/c > u) {
    #we accept x
    k = k + 1
    x.laplace[k] = x
  }
}
j   
hist(x.laplace, breaks = 50, xlab = 'X', ylim = c(0,.5), prob = TRUE, main = "Histogram of Laplace dist")
lines(seq(-6,6,.01), .5*exp(-abs(seq(-6,6,.01))), col = 'red')
#' 
#' 5. (6 points) R package AR performs Acceptance-Rejection sampling. If you provides the density function of the target distribution, 
#' and specify the name of the sampling distribution and many other options, it calculates "c" (usually optimally), runs AR sampling, and return the samples with the size you ask for.
#' (a) Install R package AR, and read the help document of function AR.Sim
#' (b) Type AR.Sim in the console to read the code of this function. Other than the visualization that it provides, you will find that it essentially follows the same steps as we have done in class.
#' (c) Redo Lab question 4 using AR.Sim, and evaluate the output in the same way as in Lab question 4. Please use c(-100,100) as the xlim argument.
#' (d) Redo the practice problem of simulating from Bart-Simpson distribution with the sampling distribution of your choice, and evaluate your output using histogram with density curve on top. 
#' Please provide your rationale of choosing your sampling distribution. 
#' (e) (Optional) xlim argument provide the range of which AR.Sim calculates the optimal value of c. This range should be large enough to cover the peak region that contain most of the probability mass (say, >99.9%).
#' But AR.Sim will return non-sense result if it is too large, say c(-1e6,1e6). Please read the code of AR.Sim, especially the part that it calculates, can you see why?
#' In this sense, AR package is not well-written, and I suggest you to write your own code as much as you can.
#' 
#'
library(AR)
#' (b)
AR.Sim
#' (c)
set.seed(20211015)
x.laplace.AR = AR.Sim(n = 1000, f_X = function(x){.5*exp(-abs(x))}, Y.dist = "cauchy",
                      xlim = c(-100,100), Y.dist.par = c(0,1))
hist(x.laplace.AR, breaks = 50, xlab = 'X', ylim = c(0,.5), prob = TRUE, main = "Histogram of Laplace dist")
lines(seq(-6,6,.01), .5*exp(-abs(seq(-6,6,.01))), col = 'red')
#' (d)
dbart = function(x,sd=0.4) 0.2*dnorm(x,-2,sd)+0.2*dnorm(x,-1,sd)+0.2*dnorm(x,0,sd)+0.2*dnorm(x,1,sd)+0.2*dnorm(x,2,sd)
set.seed(20211015)
x.bart = AR.Sim(n = 2000, f_X = dbart, Y.dist = "unif", Y.dist.par = c(-3,3), xlim = c(-10,10))
hist(x.bart, breaks = 100, xlab = 'X', ylim = c(0,.3), xlim = c(-3,3), prob = TRUE, main = "Histogram of Bart Simpson dist")
lines(seq(-3.2,3.2,.01), dbart(seq(-3.2,3.2,.01)), col = 'red')
#' The idea is that the chosen sampling distribution would better have the same shape as Bart-Simpson distribution's,
#' then the rejection rate would remain relatively low values. For Bart Simpson dist, the distribution in the 'distr' 
#' package sharing such characteristics is the uniform distribution. As expected, both rejection rate and the histogram
#' are pretty good.
plot(seq(-8,8,.01), dbart(seq(-8,8,.01)), type = 'l', col = 'red')
lines(seq(-8,8,.01), dunif(seq(-8,8,.01), -3,3))
#' (e)
#' In the function AR.Sim(), the parameter xlim is mainly used in two places,
#' 1st, when we curve the plot, it tells the range of the plot.
#' 2nd, when we find the constant c, max f_X and max f_Y, it is used as a 
#' searching interval for optimize() function. If the range is too large, it 
#' will increase the burden of computation on this part. 
#' I think the solution might be the following, since f_X and f_Y are densities, 
#' they approach to 0 quickly. We could shorten the searching range based on 
#' this characteristic, since we only care about the peak region.
#' I'll skip the detailed codes, as this part is optional.
