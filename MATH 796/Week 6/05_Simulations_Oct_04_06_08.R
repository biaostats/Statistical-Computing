#' ---
#' title: "Simulations"
#' author: "Qi Zhang"
#' date: "August 01, 2021"
#' ---
#' 
#' ## Simulating random variables in R
#' 
#' R provides built-in functions that generate random samples, evaluate the values of probability density function, cumulative distribution function and quantiles for common distributions.
#' 
#' For example, four functions are documented in the help topic Binomial
#' 
size = 5
prob=0.4
x=3
q=3
p=0.8
n=8
setwd('D:\\Dropbox\\courses\\stat_computing\\Lecture notes')
#' P(X=x) if X has Binomial(size,prob)
dbinom(x, size, prob, log = FALSE)
#' $P(X\leq x)$ if X has Binomial(size,prob)
pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
#' What happens if lower.tail=F in the above function?
pbinom(q, size, prob, lower.tail = F, log.p = FALSE)
#' The smallest x such that $P(X\leq x)\leq p$ if X has Binomial(size,prob)
qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)
#' Generate n simple random samples from Binomial(size,prob)
rbinom(n, size, prob)
#'Some input arguments x, q, p,size,prob can be vectors which allows vectorized operation if used properly.
#' Other distributions with these built-in functions includes but not limited to normal distribution (*norm), exponential, gamma, chi-squared and etc.
#' You can search "Simulating from XX distribution in R" to get to the relevant help pages. 
#' 
#' The following is an incomplete list of the distributions and their utility functions (Table 3.1 from Rizzo 2019).
knitr::include_graphics('table301.bmp')
#'
#' Given a vector of candidate outcomes, you can take random samples from it using sample(), either with or without replacements. 
n=10
set.seed(0)
(samp.letters.norep <- sample(x=letters,size=n,replace = F))
set.seed(0)
(samp.letters.wrep <- sample(x=letters,size=n,replace = T))
#' These candidates can have repetitions. 
set.seed(0)
(samp.letters.candrep <- sample(x=rep(letters,2),size=n,replace = F))
#' You may also provide the probabilities for the candidates, and sample() will draw from them accordingly. 
#' The probability does not need to sum to one. prob vector will be normalized  
set.seed(0)
(samp.letters.prob <- sample(x=letters,size=n,replace = T,prob = 1:26))

#' 
#' #### Pseudo-random number generation
#' 
#' Random numbers generated in R are not random. They are Pseudo-random numbers. 
#' After all, computers can only do exactly what is programmed (Computers do not play dice). 
#' 
#' The "Pseudo" component in the random number generator is traditionally considered as negative, especially in the early days when the algorithm was not good.
#' But now the program has been improved. There are also many positives in using a deterministic pseudo random number generator. 
#' Most importantly, it allows us to reproduce a simulation study exactly, 
#' which helps us to identify the potential issues if something has gone wrong in some repetitions but not the others,
#' and extend the simulation study to add more components without re-run what has been done.    
#' 
#' To see why it is not random, you have to know the R function set.seed()
#' It sets the seed for the random number generator. Repeat the same random sample procedure with the same random seed lead to exactly the same output.
#' The random seed is changed in an deterministic way after a random sample procedure is applied. 
set.seed(0)
runif(5)
set.seed(0)
runif(5)
set.seed(9)
runif(5)
runif(5)
#' For repetitive simulation studies, someone have argued that it is better to set the random seed at the beginning once and only once.
#' This is a good advise. 
#' 
#' Another advise is to allow large gaps between the random seeds used in different repetitions or simulation studies. 
#' Otherwise, your random samples may not be independent. Some people have the habit of setting the random seeds of their, say 10 repetitions of simulation studies as 1 to 10.
#' The following illustrates why this is particularly bad sometimes. The results of runif(10) are the same except for one with random seeds 0 and 1. So the two samples are not independent.
#' But there is no such relationship between random seeds 1 and 2. I do not know why. But it does not hurt to set the random seeds for different repetitions or studies far apart.

set.seed(0)
runif(10)
set.seed(1)
runif(10)
set.seed(2)
runif(10)

#' 
#' #### Practice: 
#' 
#' 1. Simulate a random sample of size 10 from N(mean=3,sd=10)
#' 2. Simulate a random sample of size 100 from exponential distribution with mean 3. Then calculate the mean of your random sample. 
#' Does it agree with the expectation based on theory?
#' 3. We can also simulate structured data according to a model, e.g., a linear model. 
#' Simulate x, a length 100 vector from the uniform distribution between -2 to 2, simulate eps, 100 random samples from N(0,1).
#' and then define y=3+10*x+eps. Finally, fit a linear model of y using x as the predictor. Report the coefficients, are they as you have expected or not?
rnorm(10, mean = 3, sd = 10)
r.exp = rexp(100, rate = 1/3)
mean(r.exp)
x = runif(100, -2, 2)
eps = rnorm(100)
y = 3 + 10*x + eps
y.lm = lm(y ~ x)
summary(y.lm)
#' 
#' ## Using simulations to study statistical models and procedures.
#' 
#' Simulation studies can be useful for assessing statistical methods for a number of reasons, including:
#' 
#' * to check analytical results which imply that a method should have a certain behavior, to support the analytical results' validity
#' * to examine the finite sample performance of a method for which only large sample (asymptotic) results exist
#' * to examine the implementation of a method to ensure it works in the scenarios for which it was designed for.
#' * to examine the performance of a method when some of its assumptions are violated
#' * to compare the performance of different statistical methods under different conditions
#' * to calculate sample size or power when designing a study under certain assumptions.
#' 
#' Depending on your goals, planning a simulation study can be simple or complicated. The key elements that should be considered usually include the following:
#' 
#' * What are the aims of the study? Which statistical properties are you interested in assessing? e.g. bias of parameter estimates, variability/efficiency, confidence interval coverage
#' * How will you simulate data. i.e. what data generating mechanisms will you use? How many simulations will you perform?
#' * Which statistical methods will you apply to each simulated dataset?
#' * How will you quantify the performance of the different methods - these measures will be tied to the aims
#' 
#' For more detailed discussions on this topic, I refer to the paper "Using simulation studies to evaluate statistical methods"
#'  by Morris, White and Crowther (https://doi.org/10.1002/sim.8086), and a relevant blog post by Bartlett (http://thestatsgeek.com/SimulationStudiesinR.html) 
#'
#'  In what follows, I provide examples of evaluating the properties of some statistical procedures that you have seen in introductory statistics classes.
#'  
#'  Some introductory statistics and mathematical statistics:
#'  
#'  * Type I error rate: the probability of false rejection of the null when the null is true, 
#'  i.e., if the null is true, and we repeat the experiment in exactly the same way for a large number of times, the proportion of false rejection is the Type I error rate.
#'  
#'  * Power: the probability of rejecting the null for a fixed value of parameter (such that the alternative is true).
#'  i.e., if we repeat the experiment in exactly the same way with a fixed value of the parameter (such that the alternative is true) 
#'  for a large number of times, the proportion of rejections is the power.
#'  
#'  * Coverage Probability. Interpretation of a 95% confidence interval: if the experiment is repeated for a large number of times using the same data generation mechanism, 
#' and for each dataset generated, the same procedure is applied to each dataset to calculate a 95% confidence interval for it, then we expect 95% of these intervals to cover the true value of the parameter.
#' 
#'  * Criteria for evaluating estimators: Whenever we estimate something deterministic (e.g., a parameter called $\theta$) with something random (estimate from data),e.g., an estimator called $\hat{\theta}$ the output is subject to bias and variability
#'  
#'     * Bias: $E(\hat{\theta})-\theta$
#'     
#'     * Variability: measured by standard error, $\sqrt{Var(\hat{\theta})}$. If you repeat the experiment for a large number of times, say $M$ times, and each time ,you simulate from the same data generation mechanism with the same sample size,
#'      and estimate $\theta$ using the same method and get $M$ different estimates from them, say, $\hat{\theta}_1,\ldots \hat{\theta}_M$, the standard error can be estimated with $\sqrt{\frac{1}{M-1}\sum_{j=1}^M(\hat{\theta}_j-\bar{\theta})^2}$ where $\bar{\theta}=\frac{1}{M}\sum_{j=1}^M\hat{\theta}_j$
#'       
#'    * Mean squared error (MSE) is a measure that combine both. MSE$=\frac{1}{M}\sum_{j=1}^M(\hat{\theta}_j-\theta)^2$ 
#' 
#'    * We are also interested in the shape of the sampling distribution of the estimator (the histogram of $\hat{\theta}_1,\ldots \hat{\theta}_M$), and it is desirable to have a symmetric histogram that looks like normal distribution.
#'
#'Note that the true value of the parameter $\theta$ is needed to calculate MSE and Bias. But standard error can be calculated without using this knowledge. 
#'    
#'    
#' #### Monte Carlo Estimation
#' 
#'  Suppose we observe a normal variable, and we just want to  estimate the signal-to-noise ratio $|\mu|/\sigma$.
#'  
#'  
#'  Suppose we propose to estimate $|\mu|/\sigma$, with the estimator $|\bar{x}|/s$, and want to examine the bias, standard error and the sampling distribution of this estimator.
#'  Recall that an estimator like $|\bar{x}|/s$ is a function of the random samples $X_1,\ldots,X_n$.Hence it is also a random variable.
#'  The "sampling distribution" of an estimator if the distribution (like a histogram) of the values of the estimates (the value you get by applying this estimator) on a large number of simple random samples with the same size $n$ generated from the same data generation mechanism.  
#'  When we know the data generation mechanism as do in a simulation study, we can do exactly that.
#'  Let us say that we simulate data from $N(\mu=1,\sigma=2)$ with sample size n = 20 for 200 times. 
#'  We report $|\bar{x}|/s$ from each replicate data. The histogram of these estimates is an approximation of the sampling distribution  
#' 
#'   We will implement the simulation study in three steps.
#'  
#'  * First, write the code for one replications.
#'  * Second, polish the code to facilitate  replications.
#'  * Third, summarize the results from simulation replicates.
#'  

#' Simulate a sample of 20 from N(1,2)
#' 
n = 20
alpha=0.05
mean.x=1
sd.x = 2
set.seed(0)
x = rnorm(20,mean = mean.x,sd=sd.x)
#' calculate the test statistics
snr = abs(mean(x))/sd(x)
snr

#' Next, we will repeat the above simulation study for 200 times. and only collect
#' We can run a for() loop


n = 20
mean.x=1
sd.x = 2

nrep=200
set.seed(0)
output.sim.for = rep(NA,nrep)
for(ii in 1:nrep){
  # set.seed(0) # what will happen if you fun set.seed(0) within each iteration of the for loop?
  x = rnorm(n,mean = mean.x,sd=sd.x)
  #' calculate the test statistics
  snr = abs(mean(x))/sd(x)
  output.sim.for[ii]=snr
  
}

#' As we have discussed, it is desirable to avoid explicit loops. Let us use sapply or lapply instead.
#' First, we need to write a function that complete one simulation replication.
simOneRep <- function(ii,n,mean.x,sd.x,alpha){
  x = rnorm(n,mean = mean.x,sd=sd.x)
  #' calculate the test statistics
  snr = abs(mean(x))/sd(x)
  return(snr) 
}
#' What does the input ii do? Nothing!
#' This is ok, but not "neat". Alternatively, we can use the random seed as the input
#' 
simOneRep.randseed <- function(rand.seed,n,mean.x,sd.x,alpha){
  set.seed(rand.seed)
  x = rnorm(n,mean = mean.x,sd=sd.x)
  #' calculate the test statistics
  snr = abs(mean(x))/sd(x)
  return(snr) 
}

#' Generate a sequence of random seeds. Recall that large gap between these seeds is desirable.
all.seeds = 10^5*(1:nrep)
#' Then run the nrep simulations using sapply.
output.sim.sapply = sapply(all.seeds,simOneRep.randseed,n=n,mean.x=mean.x,sd.x=sd.x,alpha=alpha)
head(output.sim.sapply)

#' Another way to run simulations, especially when it is simple, is to use replicate() function
#' Its output is like sapply() with simplify=T, or like lapply() with simplify=F
#' The downside of this approach is that it does make providing inputs (including random seeds) easy. 
output.sim.replicate <- replicate(nrep, expr = {
  x = rnorm(n,mean = mean.x,sd=sd.x)
  snr = abs(mean(x))/sd(x)
},simplify = T)

#' 
#' 
#' Let us summarize the estimates of snr with bias, standard error, and a histogram.
#' Recall that the true value of SNR for this simulation setting is $|\mu|/\sigma=1/2$.
mean(output.sim.sapply)-abs(mean.x)/sd.x
sd(output.sim.sapply)
mean((output.sim.sapply-abs(mean.x)/sd.x)^2)
hist(output.sim.sapply,breaks = 20,main='',xlab='snr')

#' What can you say about the sampling distribution of $|\bar{x}|/s$ as illustrated in the histogram? 

#' You can save the output and the relevant simulation parameters.
save(output.sim.sapply,all.seeds,n,mean.x,sd.x,file='example_sim.rdata')


#' 
#' #### Practice:
#' 
#' In this practice, we will compare two regression procedures, least squared regression that minimizes the sum of the squares of the training errors,
#'  and least absolute deviations regression that minimizes the sum of the absolute values of the training errors. 
#'  The former is the most widely used approach, and produce optimal results when all its assumptions are satisfied.
#'  The latter is said to be more robust when there are outliers, but may have some undesirable properties in some scenaiors. 
#'   
#'1. Simulate from a linear model in a way that is similar to Q3 of the previous practice, except that the intercept is 1, sploe is 2, the sample size is 50, and x is between -1 to 1.
#'
#'2. Install R package L1pack and fit least absolute deviations regression using function lad() in this package, and fit the least square regression using lm()
#'Display the summary() output of the two models, and compare the estimate and the standard errors, which one do you think perform better? Please briefly describe why.
#'
#'3. Repeat this simulation for nrep = 100 times, and only keep the four regression coefficients from the two models for each iteration.
#'
#'4. Summarize the simulation outputs by calculating the mean, standard error and mean squared error, and plot the histogram for each estimate in the same plot. 
#'You may find the following customized function handy. It calculates mse, the mean squared error for given input "truth", the true value of the parameter, and "est", a vector of independent estimates of this parameter.
mse <- function(est,truth) mean((est-truth)^2)
#'
#'5. Repeat the simulation study, but now change the data generation mechanism with 5 outliers. 
#'We generate these outliers by adding the following two lines to the data generation mechanism in Q1: They randomly pick 5 observations and multiply the error term by 10.
#'
#'Depending on your syntax, you may need to revise them when inserting.
#'id.outlier = sample(1:n,5)
#'eps[id.outlier] = eps[id.outlier]*10
#' 1.
x = runif(50, -1, 1)
eps = rnorm(50)
y = 1 + 2*x + eps
y.lm = lm(y ~ x)
#' 2.
library(fastmatrix)
library(L1pack)
y.lad = lad(y ~ x)
summary(y.lm)
summary(y.lad)
#' The first one looks better, lower Std.Error.
#' As metntioned in the question, all assumptions are satisfied, then
#' the first one is optimal. The second one is more fitted to the 
#' situations with a lot of outliers.
#' 3.
nrep = 100
simCompare.randseed = function(rand.seed, n, Umin, Umax, slope, intercept) {
  set.seed(rand.seed)
  x = runif(n, Umin, Umax)
  eps = rnorm(n)
  y = intercept + slope*x + eps
  y.lm = lm(y ~ x)
  y.lad = lad(y ~ x)
  output = c(y.lm$coefficients[1],y.lm$coefficients[2],
             y.lad$coefficients[1],y.lad$coefficients[2])
  return(output)
}
all.seeds = 10^5*(1:nrep)
output.simCompare =  sapply(all.seeds, simCompare.randseed, n = 50,
                            Umin = -1, Umax = 1, slope = 2, intercept = 1)
row.names(output.simCompare) = c("y.lm.intercept","y.lm.coeff",
                                 "y.lad.intercept","y.lad.coeff")
#' 4.
rowMeans(output.simCompare)
apply(output.simCompare, MARGIN = 1, sd)
apply(output.simCompare[c(1,3),], MARGIN = 1, mse, truth = 1)
apply(output.simCompare[c(2,4),], MARGIN = 1, mse, truth = 2)
par(mfrow=c(2,2), mar=c(4,4,2,0.5))
apply(output.simCompare, MARGIN = 1, hist, breaks = 20, main = '')
#' 5.
nrep = 100
simCompare.randseed.outlier = function(rand.seed, n, Umin, Umax, slope, intercept) {
  set.seed(rand.seed)
  x = runif(n, Umin, Umax)
  eps = rnorm(n)
  ###Adding Outliers###
  id.outlier = sample(1:n,5)
  eps[id.outlier] = eps[id.outlier]*10
  #####################
  y = intercept + slope*x + eps
  y.lm = lm(y ~ x)
  y.lad = lad(y ~ x)
  output = c(y.lm$coefficients[1],y.lm$coefficients[2],
             y.lad$coefficients[1],y.lad$coefficients[2])
  return(output)
}
all.seeds = 10^5*(1:nrep)
output.simCompare.outlier =  sapply(all.seeds, simCompare.randseed.outlier, n = 50,
                                    Umin = -1, Umax = 1, slope = 2, intercept = 1)
row.names(output.simCompare.outlier) = c("y.lm.intercept","y.lm.coeff",
                                        "y.lad.intercept","y.lad.coeff")
rowMeans(output.simCompare.outlier)
apply(output.simCompare.outlier, MARGIN = 1, sd)
apply(output.simCompare.outlier[c(1,3),], MARGIN = 1, mse, truth = 1)
apply(output.simCompare.outlier[c(2,4),], MARGIN = 1, mse, truth = 2)
par(mfrow=c(2,2), mar=c(4,4,2,0.5))
apply(output.simCompare.outlier, MARGIN = 1, hist, breaks = 20, main = '')

#' #### Monte Carlo Evaluation of Confidence Interval 
#' 
#' Some introductory statistics. Interpretation of a 95% confidence interval: suppose the experiment is repeated for a large number of times using the same data generation mechanism, 
#' and for each dataset generated, the same procedure is applied to each dataset to calculate a 95% confidence interval for it, then we have a large number of intervals at the end. 
#' We expect 95% of these intervals to cover the true value of the parameter.
#' 
#' We can use a simulation study the evaluate the empirical coverage probability (or empirical confidence level). We calculate a confidence interval for each simulation replicate, and check whether it covers the true value of the parameter (the value that we have used to simulate the data).
#' The proportion of the simulation replicates for which the CI does cover it is the empirical coverage probability. If it is much lower than the nominal confidence level, it is not a good thing.
#'  
#' If $X_1,\ldots, X_n$ is a random sample from a normal distribution $N(\mu,\sigma^2)$, $n\geq 2$, and $S^2$ is the sample variance, then $V=\frac{(n-1)S^2}{\sigma^2}$ has distribution $\chi^2_{n-1}$.
#' A one side $100(1-\alpha)\%$ confidence interval is given by $(0,(n-1)s^2/\chi^2_{n-1,\alpha})$ where $\chi^2_{n-1,\alpha}$ is a quantile of $\chi^2_{n-1}$ distribution with right tail probability $\alpha$.
#' If we repetitively sample from a normal distribution with variance $\sigma^2$ for a large number of time, we expect that the proportion of these intervals that contains $\sigma^2$ to be $1-\alpha$ 
#' 
#' Now let us evaluate the empirical coverage probability of this interval using a simulation study. 
#' Same as before, let us simulate data from $N(\mu=1,\sigma=2)$ with sample size n = 20 for 200 times. 

#' The calculation of the 95% upper confidence limit (UCL) for one random sample is shown below.

n <- 20
alpha <- .05
mean.x=1
sd.x=2
set.seed(13)
x <- rnorm(n, mean=mean.x, sd=sd.x)
UCL <- (n-1) * var(x) / qchisq(alpha, df=n-1)
UCL
UCL>=sd.x^2

#' Recall that the interval to be considered is $(0,UCL)$. Since $\sigma^2$ is non-negative, the interval covers it as long as $UCL>=\sigma^2$.
#' 
#' Now let us repeat the simulation for 200 times and evaluate the empirical coverage probability.
UCL.onerep <- function(rand.seed,n,mean.x,sd.x,alpha){
  set.seed(rand.seed)
  x <- rnorm(n, mean=mean.x, sd=sd.x)
  UCL <- (n-1) * var(x) / qchisq(alpha, df=n-1)
  return(UCL)
} 

#' Then run 200 times with given seeds.
all.seeds = 100*(1:nrep)
ucl.all = sapply(all.seeds,UCL.onerep,n=n,mean.x=mean.x,sd.x=sd.x,alpha=alpha)
summary(ucl.all)

#' Count the number or proportion of intervals that contain $\sigma^2=4$
sum(ucl.all > sd.x^2)
mean(ucl.all > sd.x^2)

#' The result is that 188 of 200 intervals satisfied (UCL > 4), so the empirical coverage probability is 94% in this experiment. The result will vary but should be close to the theoretical value, 95%. 
#' 
#' 
#' How can we estimate the standard error of our estimate of the empirical coverage probability?
#' 
#' Recall that the interval from each simulation replicate either covers the true $\sigma^2$ or not, which is a Bernoulli random variable itself, just like a coin flip.
#' If you flip a coin for 200 times, what is the standard error of your estimate of the head probability? It is $\sqrt{\hat{p}(1-\hat{p})/200}$. In our case, the estimated standard error is $\sqrt{0.94(1-0.94)/200}=0.0168$.
#' 

#' 
#' #### Practice
#'
#'  
#' 1. Conduct a simulation study to evaluate the empirical coverage probability of one-sample t interval for the population mean, when the the samples are generated from $N(\mu=4,\sigma=2)$.
#' Please use $n=30$, confidence level 95%, the number of simulation replicates nrep=1000, and  report the empirical coverage probability.
#' 
#' 2. Repeat the above simulation study with data generated from Poisson distribution with $\lambda=4$, while everything else are kept the same.
#' 
#' 3. What assumption for t interval is violated in the simulation scenario in Q2? 
#' Comparing with the results of the two simulation studies, what will you say about the robustness of t interval against the violation of this assumption?
#' 
CL.t.rep <- function(rand.seed,n,mean.x,sd.x,alpha){
  set.seed(rand.seed)
  x <- rnorm(n, mean=mean.x, sd=sd.x)
  UCL <- mean(x) + qt(1 - alpha/2, df=n-1)*sd(x)/sqrt(n)
  LCL <- mean(x) + qt(alpha/2, df=n-1)*sd(x)/sqrt(n)
  return(c(LCL,UCL))
}
nrep = 1000
all.seeds = 100*(1:nrep)
ucl.all = sapply(all.seeds,CL.t.rep,n=30,mean.x=4,sd.x=2,alpha=.05)
sum(ucl.all[1,] <= 4 & ucl.all[2,] >= 4)/nrep

CL.t.pois.rep <- function(rand.seed,n,mean.x,alpha){
  set.seed(rand.seed)
  x <- rpois(n,lambda = mean.x)
  UCL <- mean(x) + qt(1 - alpha/2, df=n-1)*sd(x)/sqrt(n)
  LCL <- mean(x) + qt(alpha/2, df=n-1)*sd(x)/sqrt(n)
  return(c(LCL,UCL))
}
ucl.all.pois = sapply(all.seeds,CL.t.pois.rep,n=30,mean.x=4,alpha=.05)
sum(ucl.all.pois[1,] <= 4 & ucl.all.pois[2,] >= 4)/nrep
#' I think it is robust against the violation of normal assumption.
#' 
#' #### Monte Carlo Hypothesis Tests
#' 
#' Simulations can be also used to test a hypothesis. There are two things to be evaluated. 
#' 
#' (1) Empirical Type I error rate. Recall that we reject the null if p-value is less than $\alpha$, the significance level. This is because want the probability of making Type I error to be no more than $\alpha$.
#'  We want to evaluate whether this is true empirically. Suppose we simulate for a large number of times from a data generation mechanism such that the null hypothesis is true, 
#'  compute the p-value in the same way from each simulation replicate, and reject the null when it is no larger than $\alpha$. 
#'  There may still be rejections even though the null is true, and this proportion of rejections is the empirical Type I error rate.
#'  If it is larger than $\alpha$, the nominal significance level, it is not a good thing. If it is much smaller, it is ok, even though it indicates that the test may be conservative.
#'   
#' Let us consider one-sample t test for $H_0: \mu=0$ or $H_0: \mu> 0$. Let us simulate data from $N(\mu=0,\sigma=2)$ with sample size n = 20 for 1000 times. 
#' To evaluate the empirical Type I error rate, the data must be simulated from a model with $\mu=0$ because this is the null.
#' We use t.test() function to calculate the p-value, even though it is straightforward to code the formula from your introductory stat class.

n = 20
alpha=0.05
mean.x=0
sd.x = 2
nrep=1000

pval.onerep <- function(rand.seed,n,mean.x,sd.x){
  set.seed(rand.seed)
  x = rnorm(n,mean = mean.x,sd=sd.x)
  pval.t = t.test(x,alternative = 'greater',mu=0)$p.value
  return(pval.t) 
}

all.seeds = 1000*(1:nrep)
pval.all = sapply(all.seeds,pval.onerep,n=n,mean.x=mean.x,sd.x=sd.x)

#' Count the number/proportion of false rejections. The rejections are false rejections because we simulate the data from a model where the null is true.
sum(pval.all<alpha)
mean(pval.all<alpha)

#' (2) We also want to evaluate the empirical power of a test. For a particular value of the alternative, the power is the the probability that the null is rejected (being rejected correctly because now the alternative is true).
#' To evaluate this in simulations, we can repeat exactly what we did before, except that now we simulate from a model where the alternative is true.
#' Using the context of the previous example, let us examine the power of one-sample t test when $\mu=1$

all.seeds = 1000*(1:nrep)
pval.all.mu1 = sapply(all.seeds,pval.onerep,n=n,mean.x=1,sd.x=sd.x)

#' Count the number/proportion of correct rejections. The rejections are correct rejections because we simulate the data from a model where the alternative is true.
sum(pval.all.mu1<alpha)
mean(pval.all.mu1<alpha)

#' So the empirical power is 0.7.
#' 
#' When evaluating power with simulations, we do not necessarily only evaluate for one particular value of the alternative.
#' Instead, we may evaluate the power for a sequence of different values of the alternative.
#' Intuitively, the more distant a particular value of the alternative is from the null, the larger the power should be, e.g., in the above normal example, the power when $\mu=3$ should be larger than the power when $\mu=2$ for the same sample size.
#' 
#' Using the context of the previous example, let examine the power of one-sample t test when $\mu=0,0.25,0.5,0.75,1,0.25,1.5,1.75,2$. Note that the calculation of the empirical Type I error and power are exactly the same.
mu.seq = 0.25*(0:8)
all.seeds = matrix(1000*(1:(nrep*length(mu.seq))),ncol=length(mu.seq))

pval.all.museq.for = matrix(NA,nrow=nrep,ncol=length(mu.seq))
for(jj in 1:length(mu.seq)){
  pval.all.museq.for[,jj] = sapply(all.seeds[,jj],pval.onerep,n=n,mean.x=mu.seq[jj],sd.x=sd.x)
}

#' Since we are only collecting the p-value, and the function we wrote is flexible enough, we can actually avoid the for loop and vectorize in two layers.  

pval.all.museq = sapply(1:length(mu.seq), function(jj) sapply(all.seeds[,jj],pval.onerep,n=n,mean.x=mu.seq[jj],sd.x=sd.x))
dim(pval.all.museq)

#' Count the proportion of rejections for each value of $mu$
(power.museq = colMeans(pval.all.museq<alpha))

#' The results of this study can be best summarized as a power curve showing how the power change as $\mu$ changes.
#' Usually this power curve comes with a horizental line at $\alpha$ showing that when the null is true $\mu=0$, the proportion of rejection (Type I error rate) is not exceeding it.
plot(mu.seq,power.museq, xlab='mu',ylab='power',main='Power vs mu',type = 'b',lty=1,pch=16,cex=2,xlim=range(mu.seq),ylim=c(0,1))
abline(h=alpha,lty=2)


#' 
#'  
#' 
#' #### Practice
#' 
#'  1. One major use of Monte Carlo tests if for planning sample size for real experiments. Real experiments can be expensive, and the experimenter prefer not to enroll too many subjects. 
#'  But they also hope to have a large enough sample size, so that power for certain value of alternative is large enough (a domain-specific cutoff, usually 70%, 80% or 90%).
#'   Let us consider one-sample t test for $H_0: \mu=0$ or $H_0: \mu> 0$ at significance level $\alpha=0.05$ again, and try to find the sample size such that power is at least 0.9 when $\mu=1$ using simulations by following the steps below.
#'   (a) Simulate from $N(\mu=1,\sigma=2)$ with varying sample size n = 5,10,15,20,25,30,35,40,45,50 for 1000 times, and calculate the p-values. 
#'   (b) Calculate the power at $\mu=1$ for each sample size.
#'   (c) Plot the power curve against the sample size n, and add a horizontal dash line at the value of the desirable power. 
#'   (d) What is the minimal sample size with the desirable power?
#'   
n.seq = c(5,10,15,20,25,30,35,40,45,50)
alpha=0.05
mean.x=1
sd.x = 2
nrep=1000
all.seeds.nseq = matrix(1000*(1:(nrep*length(n.seq))),ncol=length(n.seq))

pval.onerep.nseq <- function(rand.seed,n,mean.x,sd.x){
  set.seed(rand.seed)
  x = rnorm(n,mean = mean.x,sd=sd.x)
  pval.t = t.test(x,alternative = 'greater',mu=0)$p.value
  return(pval.t) 
}
pval.all.nseq = sapply(1:length(n.seq), function(jj) sapply(all.seeds.nseq[,jj],pval.onerep.nseq,n=n.seq[jj],mean.x=mean.x,sd.x=sd.x))
(power.nseq = colMeans(pval.all.nseq<alpha))
plot(n.seq,power.nseq, xlab='n',ylab='power',main='Power vs n',type = 'b',lty=1,pch=16,cex=2,xlim=range(n.seq),ylim=c(0,1))
abline(h=alpha,lty=2)
abline(h=.9,lty = 4,col = 'red')
#' Above the red dash line, they are n = 40, 45, 50.