#' ---
#' title: "Simulations Lab"
#' author: "Biao Zhang"
#' date: "August 01, 2021"
#' ---
#' 
#'
#' 1.(5 points) Estimate the MSE of the trimmed means for random samples of size 20 generated from a standard Cauchy distribution. (The target parameter $\theta$ is the center or median; the expected value does not exist for Cauthy.) 
#' Summarize the estimates of MSE in a table for the proportion of trimmed = 0.1,0.2,0.3,0.4,0.5.

trimmed.mean = function(v,prop.trim=0.1) {  
  q1 = quantile(v, prob=prop.trim/2)
  q2 = quantile(v, prob=1-prop.trim/2)
  output = mean(v[q1 <= v & v <= q2])
  return(output)
}
mse <- function(est,truth) mean((est-truth)^2)
est.mse.cauchy = function(p.trim, n) {
  x = rcauchy(n)
  est = trimmed.mean(x, p.trim)
  output = mse(est = est, truth = 0)
  names(output) = paste0("trimmed_",p.trim)
  return(output)
}
trimmed = c(0.1,0.2,0.3,0.4,0.5)
set.seed(20211007)
sapply(trimmed, est.mse.cauchy, n = 20)
#' 
#' 2. (5 points) Plot the empirical power curve for the class example of one sample t-test, changing the alternative hypothesis to $H_1: \mu\neq 0$, and keeping the significance level $\alpha=0.05$
n.Q2 = 20
alpha.Q2=0.05
nrep.Q2=1000
mu.seq = 0.25*(0:8)
sd.x = 2


pval.onerep <- function(rand.seed,n,mean.x,sd.x){
  set.seed(rand.seed)
  x = rnorm(n.Q2,mean = mean.x,sd=sd.x)
  pval.t = t.test(x,alternative = 'two.sided',mu=0)$p.value
  return(pval.t) 
}

all.seeds.Q2 = matrix(1000*(1:(nrep.Q2*length(mu.seq))),ncol=length(mu.seq))
pval.all.museq = sapply(1:length(mu.seq), function(jj) sapply(all.seeds.Q2[,jj],pval.onerep,n=n.Q2,mean.x=mu.seq[jj],sd.x=sd.x))
(power.museq = colMeans(pval.all.museq<alpha.Q2))
plot(mu.seq,power.museq, xlab='mu',ylab='power',main='Power vs mu',type = 'b',lty=1,pch=16,cex=2,xlim=range(mu.seq),ylim=c(0,1))
abline(h=alpha.Q2,lty=2)
#' 
#' 3. (5 points) Suppose that $X_1,\ldots,X_n$ are a random sample from a from a lognormal distribution with unknown parameters $\mu$ and $\sigma^2$ (https://en.wikipedia.org/wiki/Log-normal_distribution). 
#' Construct a 95% confidence interval for the parameter $\mu$. 
#' Use a Monte Carlo method to obtain an empirical estimate of the confidence level.
nrep.Q3 = 1000

CL.t.rep <- function(rand.seed,n,mean.x,sd.x,alpha){
  set.seed(rand.seed)
  ln.x <- rnorm(n, mean=mean.x, sd=sd.x)
  UCL <- mean(ln.x) + qt(1 - alpha/2, df=n-1)*sd(ln.x)/sqrt(n)
  LCL <- mean(ln.x) + qt(alpha/2, df=n-1)*sd(ln.x)/sqrt(n)
  return(exp(c(LCL,UCL)))
}

all.seeds.Q3 = 100*(1:nrep.Q3)
ucl.all = sapply(all.seeds.Q3,CL.t.rep,n=30,mean.x=4,sd.x=2,alpha=.05) # I set mu =4, delta = 2
sum(ucl.all[1,] <= exp(4) & ucl.all[2,] >= exp(4))/nrep.Q3
#' 
#' 4. (9 points) Use Monte Carlo simulation to investigate whether the empirical Type I error rate of the t-test is approximately equal to the nominal significance level $\alpha$, when the sampled population is non-normal.
#'  The t-test is robust to mild departures from normality. Discuss the simulation results for the cases where the sampled population is (i) $\chi^2_1$, (ii) $Uniform(0,2)$, and (iii) $Exponential(rate=1)$.
#'   In each case, test $H_0: \mu=\mu_0$ vs $H_1: \mu \neq \mu_0$, where $\mu_0$ is the mean of the three distributions, respectively.
n.Q4 = 20
alpha.Q4 = 0.05
nrep.Q4 = 1000
all.seeds.Q4 = 1234*(1:nrep.Q4)
#' i)
pval.chisq <- function(rand.seed,n){
  set.seed(rand.seed)
  x = rchisq(n, df = 1)
  #mu is set to be 1, since for chisq dist, mean is equivalent to df.
  pval.t = t.test(x,alternative = 'two.sided',mu = 1)$p.value
  return(pval.t) 
}
pval.all.chisq = sapply(all.seeds.Q4, pval.chisq, n = n.Q4)
mean(pval.all.chisq < alpha.Q4)
#' Comments: it is not good as expected. Because $\chi^2_1$ departs from normality.
plot(seq(0,.5,.01),dchisq(seq(0,.5,.01),df = 1), type = 'l', xlab = 'x', ylab = 'density', main = 'Chisq(1)')
#' ii)
pval.unif <- function(rand.seed,n){
  set.seed(rand.seed)
  x = runif(n, min = 0, max = 1)
  #mu is set to be 0.5, since for uniform dist, mean is equivalent to (max - min)/2.
  pval.t = t.test(x,alternative = 'two.sided',mu = .5)$p.value
  return(pval.t) 
}
pval.all.unif = sapply(all.seeds.Q4, pval.unif, n = n.Q4)
mean(pval.all.unif < alpha.Q4)
#' Comments: it is very close to 0.5. Because $Uniform(0,2)$ is a flat line, you could consider it as a normal distribution
#' with the mean 0.5 and a extremely large variance.
plot(seq(0,.5,.01),dunif(seq(0,.5,.01),min = 0, max = 1), type = 'l', xlab = 'x', ylab = 'density', main = 'Uniform(0,2)')
#' iii)
pval.exp <- function(rand.seed,n){
  set.seed(rand.seed)
  x = rexp(n)
  #mu is set to be 1, since for exponential dist, mean is equivalent to 1/rate.
  pval.t = t.test(x,alternative = 'two.sided',mu = 1)$p.value
  return(pval.t) 
}
pval.all.exp = sapply(all.seeds.Q4, pval.exp, n = n.Q4)
mean(pval.all.exp < alpha.Q4)
#' Comments: it is not good as expected. Because $Exponential(rate=1)$ is a highly skewed distribution.
plot(seq(0,3,.01),dexp(seq(0,3,.01)), type = 'l', xlab = 'x', ylab = 'density', main = 'Exp(1)')
