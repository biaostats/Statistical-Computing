#' ---
#' title: "Bootstrap and Jackknife Part 1"
#' author: "Qi Zhang"
#' date: "August 01, 2021"
#' ---
#'
#' ##
#'
#' #### Some facts from Mathematical Statistics
#'
#' For a random variable $X$, its cumulative distribution function (cdf) is defined as $F_X(x)=P(X\leq x)$.
#' It uniquely determines the random variable.
#'
#' A parameter is some property of the population, so it can be presented as a function of cdf. That is, estimating a parameter is estimating some aspect of the cdf.
#'
curve(pnorm,from=-8,8)
ppoislambda = function(x) ppois(x,lambda=1)
curve(ppoislambda,from=-1,8)
#'
#' Since the value of cdf is a probability, it is between 0 and 1, and it is monotone non-descreasing in $x$.
#'
#'
#' We estimate parameters/cdf using data, typically iid samples drawn from the distribution.
#'
#' Recall that for each parameter, we use a sample version to estimate it, e.g., estimate the population mean using sample mean, population variance using sample variance.
#' How can we estimate the cdf of the population P(X<=x) using the sample? What is the "sample version" of cdf P(X<=x)?
#' It is the proportion of the sample points that fall below x.
#' If viewing it as a function of the value x, it is called the empirical (cumulative) distribution function (ecdf).
#'
x.norm = rnorm(20)
ecdf.xnorm = ecdf(x.norm)
curve(ecdf.xnorm,from= -6,to=6)

x.pois = rpois(20,lambda=1)
ecdf.xpois = ecdf(x.pois)
curve(ecdf.xpois,from= -1,to=8)

#'
#'
#'  Standard error of an estimator: the standard deviation of the estimator according to its sampling distribution, for sample mean, it is ...
#'  Not every estimator has such closed form standard error. e.g., sample median is a natual estimator of the population median, what is its standard error?
#'  If we know the true distribution (cdf), we can repetitively sample from this distribution, calculate the median of each sample, and use them as a random sample from the sampling distribution, and estimate the standard error.
#' When we do not know the true cdf, how can we estimate the standard error? What is the next best thing if the true cdf is unknown?
#' It is ecdf. So we can mimic the above process of standard error estimation by sampling from ecdf instead.
#' This is exactly bootstrap.
#'
#'#### What is Bootstrap
#'
#' The phrase bootstrapping originates from the idiom "pulling oneself up by one's bootstraps." \url{https://en.wikipedia.org/wiki/Bootstrapping}.
#' Since it is physically impossible, someone claims that its original meaning is trying to do something completely absurd.
#' But now it usually refers to a self-starting process that is supposed to continue or grwo without external input, and has been used in various fields.
#' In the context of statistics, it means learning the sampling distribution using only the data that you already have, and without any parametric assumption.
#'
#'
#' To generate a bootstrap random sample by resampling from your data $x=(x_1,\ldots,x_n)$, first generate n random integers $\{i_1,\ldots,i_n\}$ uniformly distributed on $\{1,\ldots, n\}$,
#'  and select the bootstrap sample $x^{\star} = (x_{i_1},\ldots , x_{i_n})$. Suppose $\theta$ is the parameter of interest ($\theta$ could be a vector), and $\hat{\theta}$ is an estimator of $\theta$.
#'   Then the bootstrap estimate of the distribution of $\hat{\theta}$ is obtained as follows.
#'   1. For each bootstrap replicate, indexed $b = 1,\ldots,B$:
#'     (a) Generate sample $x^{\star (b)} = (x_{i_1},\ldots , x_{i_n})$ by sampling with replacement from the observed sample $x_1,\ldots,x_n$.
#'     (b) Compute the $b$th replicate $\hat{\theta}^{(b)}$ from the $b$th bootstrap sample. This is referred to as a bootstrap replicate of $\hat{\theta}$
#'  2. The bootstrap estimate of the sampling distribution of $\hat{\theta}$ is the empirical distribution of the replicates $\hat{\theta}^{(1)},\ldots,\hat{\theta}^{(B)}$
#'
#' Example of bootstrap samples
#'
#' Suppose that we have observed the sample $x = \{2, 2, 1, 1, 5, 4, 4, 3, 1, 2\}$. Note that there 10 observations.
#' To generate a bootstrap sample, we first generate 10 random integers uniformly from 1 to 10.
#' Suppose our 10 random integers are $3,1,8,5,6,9,10,2,5,1$, then this bootstrap sample is the observations in $x$ that indexed by these integers.
#' They are $\{1,2,3,5,4,1,2,2,5,2\}$.
#'
#' #### Practice
#'
#' 1. In the context of the above example, suppose your 10 random indices are $1,1,3,5,5,7,8,9,10,10$, please find the bootstrap sample by hand.
#'
#' 2. Taking one bootstrap sample in R.
x = c(2, 2, 1, 1, 5, 4, 4, 3, 1, 2)
#'  Recall that you can sample integers using R function Sample. Please first sample 10 integers from 1 to 10 with replacement, and call this vector idb,
#'   and then find the coresponding bootstrap sample. Please set the random seed to be your favorite number.

set.seed(2048)
idb = sample(10,10,replace = T)
idb
x[idb]

set.seed(2048)
xb = sample(x,10,replace = T)
xb


#'
#' 3. You can also sample from a data vector directly with replacement using R function sample()
#' Please generate the bootstrap sample in this way using the same random seed as you in last part.
#' Are your two bootstrap samples exactly the same?
#'



#'
#'
#'
#' #### Bootstrap estimation of standard error
#'
#' The bootstrap estimate of standard error of an estimator $\hat{\theta}$ using the bootstrap replicates $\hat{\theta}^{(1)},\ldots,\hat{\theta}^{(B)}$ is
#' \[
#'  \hat{se}(\hat{\theta}^\star) = \sqrt{\frac{1}{B-1}\sum_{b=1}^B(\hat{\theta}^{(b)}-\bar{\hat{\theta}^{\star}})^2}
#' \]
#' where $\bar{\hat{\theta}^{\star}}=\frac{1}{B}\sum_{b=1}^B\hat{\theta}^{(b)}$.
#' According to references by Efron and Tibshirani, the number of replicates does not need to be very large for standard error estimation. Using $B=50$ is large enough, and rarely is $B>200$ necessary.
#' This recommendation is only for standard error estimation, not for other tasks such as confidence intervals.
#'
#' Example: Air-conditioning data (Example 1.1 of Davison and Hinkley)
#' The following $n=12$ times between failures of air-conditioning equipment are recorded. We wish to estimate the mean failure time, or its reciprocal, the failure rate.
#'
ac.failure = c(3,5,7,18,43,85,91,98,100,130,230,487)
(ac.mean = mean(ac.failure))
#' An estimator of the failure rate is just its reciprocal.
(ac.rate=1/mean(ac.failure))
#' Next let us estimate its standard error using bootstrap
#'
#' To generate each bootstrap replicate, we need to following the following steps
#'

boot.samp.b = sample(x=ac.failure,size=length(ac.failure),replace = T)
boot.rep.b = mean(boot.samp.b)
boot.rep.b

1/boot.rep.b

#' Now let us wrap the above code into a function, and replicate for $B=100$ times
#'
boot.mean <- function(dt){
  boot.samp.b = sample(x=dt,size=length(dt),replace = T)
  boot.rep.b = mean(boot.samp.b)
  return(boot.rep.b)
}

B = 100
set.seed(2021)
ac.boot.mean = replicate(B,boot.mean(ac.failure))

hist(ac.boot.mean,breaks = 20)

hist(1/ac.boot.mean,breaks = 20)

(se.ac.boot.mean = sd(ac.boot.mean))
(se.ac.boot.failurerate = sd(1/ac.boot.mean))
#'
#' Practice for those who have taken MATH 755/855: A common parametric model for failure time is Gamma distribution.
#' If the data follow gamma distribution, we know the sampling distribution of the sample mean is another Gamma distribution.
#' Please use the data to estimate the parameters of the related Gamma distributions, and compare the SE based on the Gamma model with our bootstrap estimate.
#'
#'

#'
#' #### Bootstrap estimation of Bias
#'
#' Fact from Introductory or Mathematical Statistics.
#'
#' If $E(\hat{\theta})=\theta$, we call $\hat{\theta}$ an unbiased estimator of $\theta$. Note that this expectation is taken over the sampling distribution of $\hat{\theta}$.
#' The bias of an estimator $\hat{\theta}$ for $\theta$ is defined as
#' \[
#' bias(\hat{\theta}) = E(\hat{\theta})-\theta
#' \]
#' When we know the true distribution that we take samples from, we know the value of $\theta$.
#' We can repetitively take samples from this distribution for $M$ times, and calculate $\hat{\theta}$ from each sample.
#' Let us call them $\hat{\theta}_1,\ldots,\hat{\theta}_M$. Using these samples from the actual distribution that our original data is sampled from,
#' the bias of the estimator $\hat{\theta}$ can be estimated as the
#' \[
#' \frac{1}{M}\sum_{j=1}^M \hat{\theta}_j - \theta
#' \]
#' This is what we could have done, if we know the true distribution (and thus the true value of the parameter) of the data.
#'
#'  In reality, we only have the data that we have already sampled from an unknown distribution. $x_1,\ldots, x_n$, and we do not know the true value of $\theta$.
#'  What can we use to estimate the true $\theta$? It is $\hat{\theta}$.
#' What can we do to mimic the process of repetitive sampling from the true unknown distribution? It is bootstraping (sampling from the empirical distribution)!
#' So the bootstrap estimate of bias is
#'
#' \[
#' \frac{1}{B}\sum_{b=1}^B \hat{\theta}^{(b)} - \hat{\theta} = \bar{\hat{\theta}^{\star}} - \hat{\theta}
#' \]
#'
#' If the bias is large, we may want to use this bias estimate to "debias" our original estimator $\hat{\theta}$. The debiased estimator is
#' \[
#' \hat{\theta}-\hat{bias} = \hat{\theta}- (\bar{\hat{\theta}^{\star}}-\hat{\theta}) = 2 \hat{\theta}-\bar{\hat{\theta}^{\star}}
#' \]
#' Generally speaking, if $|bias|/se\leq 0.25$, there is no need to adjust for bias. Here se is the standard error.
#'
#'
#'
#' Example: Air conditioning data.
#' Using the bootstrap replicates that we have generated, we can also assess the bias of the estimators.
#'
(bias.boot.mean = mean(ac.boot.mean)-ac.mean)
(bias.boot.rate = mean(1/ac.boot.mean)-ac.rate)
#' Based on these results, do you see the need of adjusting the estimators for the bias?
bias.boot.mean/se.ac.boot.mean
bias.boot.rate/se.ac.boot.failurerate

ac.rate-bias.boot.rate

#'
#' Example: evaluate quantities that involves more than one variables.
#' We will use the R built-in dataset state.x77, and investigate standard error and bias for the estimated average of population density (Population divided by Area) across 50 states.

dt.state = data.frame(state.x77)
head(dt.state)
(ave.density = mean(dt.state$Population/dt.state$Area))

#'
#' For now, R function sample() cannot automatically sample from the rows of a data.frame if you use the data frame as the input argument x.
#' So you need to sample the row indeces to get the bootstrap sample of rows. To generate one bootstrap replicate, we can follow the following steps.
#'
#'

boot.mean.density <- function(dt){
  n = nrow(dt)
  id.b = sample(1:n,size=n,replace = T)
  boot.samp.b = dt[id.b,]
  boot.rep.b = mean(boot.samp.b$Population/boot.samp.b$Area)
  return(boot.rep.b)
}

B = 100
set.seed(2046)
state.boot.mean.density = replicate(B,boot.mean.density(dt.state))

hist(state.boot.mean.density,breaks = 20)

(se.state.boot.mean.density = sd(state.boot.mean.density))
(bias.state.boot.mean.density = mean(state.boot.mean.density)-ave.density)


#'
#' #### Practice:
#'  You will use the built-in R dataset USArrests, and investigate the correlation between the Assault rate and UrbanPop.
#'  In particular, please calculate the standard error and bias for the estimated Pearson's correlation between the two variables using bootstrap with B = 100.
#'

head(USArrests)



boot.cor <- function(dt){
  n = nrow(dt)
  id.b = sample(1:n,size=n,replace = T)
  boot.samp.b = dt[id.b,]
  boot.rep.b = cor(boot.samp.b$Assault,boot.samp.b$UrbanPop)
  return(boot.rep.b)
}

B = 100
set.seed(2046)
out.boot.cor = replicate(B,boot.cor(USArrests))

hist(out.boot.cor,breaks = 20)

(assultpop.cor = cor(USArrests$Assault,USArrests$UrbanPop))

(se.boot.cor = sd(out.boot.cor))
(bias.boot.cor = mean(out.boot.cor)-assultpop.cor)



#'
#' #### The Jackknife estimation of Bias and standard error, limitation of Jackknife
#'
#' The jackknife is another data splitting method proposed a few decades earlier than the bootstrap, mostly used in estimating bias and standard error.
#' It split the data in a "leave-one-out" fashion. Formally, suppose we observe data $x=(x_1,\ldots, x_n)$, the $i$th jackknife sample is defined as
#' \[
#' x_{(i)} = (x_1,\ldots,x_{i-1},x_{i+1},\ldots, x_n)
#' \]
#' Suppose we are interested in evaluating the bias and standard error of, $\hat{\theta}$, an estimator of $\theta$.
#' Applying this estimator to the  $i$th jackknife sample gives the $i$th jackknife replicate $\hat{\theta}_{(i)}$.
#'
#' The jackknife estimate of bias and standard error are
#' \[
#' \hat{bias}_{jack} = (n-1)(\bar{\hat{\theta}_{(\cdot)}}-\hat{\theta})
#' \]
#' and
#' \[
#' \hat{se}_{jack} = \sqrt{\frac{n-1}{n}\sum_{i=1}^n (\hat{\theta}_{(i)}-\bar{\hat{\theta}_{(\cdot)}})^2}
#' \]
#' where $\bar{\hat{\theta}}=\frac{1}{n}\sum_{i=1}^n \hat{\theta}_{(i)}$.
#'
#'
#' To understand the $n-1$ factor in the formula of bias, it makes the formula correctly estimate the bias in the plug-in estimate of the variance $\hat{\theta} = \frac{1}{n}\sum_{i=1}^n(x_i-\bar{x})^2$.
#' To understand the factor $\frac{n-1}{n}$ in the formula of standard error, it makes the formula correctly estimate the standard error for sample mean.
#' If you have taken 755/855, you should be able to derive the above two statements mathematically.
#'

#' Example: In the context of the previous example with dataset state.x77, let us use jackknife to estimate standard error and bias for the estimated average of population density (Population divided by Area) across 50 states.

dt.state = data.frame(state.x77)
(ave.density = mean(dt.state$Population/dt.state$Area))

#' To generate one jackknife replicate, say for the 20th jackknife replicate, we need to do the following.
jack.samp.i = dt.state[-20,]
(jack.rep.i = mean(jack.samp.i$Population/jack.samp.i$Area))

#' Now we wrap this as a function, and run through for $i=1,\ldots, n$

jack.mean.density <- function(i,dt){
  jack.samp.i = dt[-i,]
  jack.rep.i = mean(jack.samp.i$Population/jack.samp.i$Area)
  return(jack.rep.i)
}

n = nrow(dt.state)
state.jack.mean.density = sapply(1:n,jack.mean.density,dt=dt.state)

#' Some technical questions: why didn't we set the number of replicates B as we did for bootstrap?
#' If we set the random seed differently here, will the result change?
#' And why didn't we use function replicate() here?

hist(state.jack.mean.density,breaks = 20)

(se.state.jack.mean.density = ((n-1)/sqrt(n))*sd(state.jack.mean.density))
(bias.state.jack.mean.density = (n-1)*(mean(state.jack.mean.density)-ave.density))

#' What does the results tell you? How do they compare with the bootstrap outputs?
#'
#' When the jackknife fails.
#'
#' The jackknife can fail when the estimator $\hat{\theta}$ is not a "smooth" function of the data, or when it is "discrete".
#' Smoothness means that small changes in the data correspond to small changes in the value of the statistics.
#' One such example is the median, or order statistics in general. In this example, the bootstrap estimate and jackknife estimates of the standard error are dramatically different.
#' In this particular case, jackknife fails, but bootstrap is ok.
#'
#' Example: (jackknife fail), suppose we observe 10 random integers from 1 to 100, and we are interested in estimating the median, an find its standard error.

set.seed(1984)
n = 1000
x = sample(1:100,size=n,replace = T)
x
median(x)
#' Here comes the jackknife estimate of the standard error
#'
jack.median <- function(i,dt){
  jack.samp.i = dt[-i]
  jack.rep.i = median(jack.samp.i)
  return(jack.rep.i)
}

int.jack.median = sapply(1:n,jack.median,dt=x)

(se.int.jack.median = ((n-1)/sqrt(n))*sd(int.jack.median))

mean(int.jack.median)
median(x)
#' The following is the bootstrap estimate

boot.median <- function(dt){
  boot.samp.i = sample(dt,size=length(dt),replace = T)
  boot.rep.i = median(boot.samp.i)
  return(boot.rep.i)
}
B=100
int.boot.median = replicate(B,boot.median(dt=x))

(se.int.boot.median = sd(int.boot.median))

mean(int.boot.median)

#' They are very different, and clearly something is wrong here. How can we tell? Usually it is believed that bootstrap is more trust-worthy than jackknife.
#' In this case, however, since we know the true distribution that our data is generated from (Uniform over integers from 1 to 100), we can estimate the standard error of median by repetitively sample from this true distribution.
#'

M = 100
int.samp.median = replicate(M, expr = median(sample(1:100,size=n,replace = T)))
(se.samp.median = sd(int.samp.median))

#' We can see that the bootstrap estimate is closer to the oracle estimate, i.e., the estimate by repetitively sampling from the true distribution that the data is generated from.
#' In contrast, the jeckknife estimate is far away, so it fails in this case.
#'
#'


#'
#' #### Practice:
#'  You will use the built-in R dataset USArrests again, and investigate the correlation between the Assault rate and UrbanPop.
#'  In particular, please calculate the standard error and bias for the estimated Pearson's correlation between the two variables using jackknife method, and compare with the bootstrap estimates you have observed.
#'


jack.cor <- function(i,dt){
  jack.samp.i = dt[-i,]
  jack.rep.i = cor(jack.samp.i$Assault,jack.samp.i$UrbanPop)
  return(jack.rep.i)
}

n = nrow(USArrests)
out.jack.cor = sapply(1:n,jack.cor,dt=USArrests)

hist(out.jack.cor,breaks = 20)

(se.jack.cor = ((n-1)/sqrt(n))*sd(out.jack.cor))
(bias.jack.cor = (n-1)*(mean(out.jack.cor)-assultpop.cor))


#'
#' #### Cross-Validation, a relevant yet different type of sample splitting.
#'
#' This is a good place to introduce cross-validation, a relevant yet different type of sample splitting.
#'
#' Cross validation is a data partitioning method that can be used to assess the stability of parameter estimates, the accuracy of a classification algorithm,the adequacy of a fitted model, and in many other applications.
#' In building a predictive model, a researcher can partition the data into training and test sets. The model is estimated using the data in the training set only, and the prediction error is estimated by running the trained model on the test set.
#'
#' The most common way of estimating prediction error in this manner is "k-fold" cross validation, which partitions the $n$ data points into $k$ disjoint test sets. For each test set, it uses the rest of the data for training, and assess prediction error using the test set At the end, the k errors are combined (usually sum or weighted average) as the final estimate of the prediction error.
#'
#' An extreme version of this is "n-fold" cross validation in which each test set is just one single data point. That is, the model is trained on all but one data point each time. This is also called "leave-one-out" cross validation. As you may have realized, it is very similar to Jackknife.
#'
#' The data splitting for leave-one-out CV is deterministic, and is random for k-fold CV in general. And such random splitting is done only once. An alternative is random splitting.
#'  That is, you randomly sample from the original data (usually WITHOUT replacement) to form the training set, and use the rest as the test set. You then repeat this process for multiple times. Different from k-fold cross validation, your test sets could overlap. If identical cases in training data is not an issue for your model, you may also sample from the original data WITH replacement (bootstraping!), and then use the data not in your bootstrap sample for assessing the prediction error. In  the context of random forest and the alikes, this is called the "Out-of-bag" (OOB) prediction error.

#'
#' Consider the state.x77 data, and suppose we are trying decide which linear model predict Murder rate better, using Population and HS.Grad, or also include a square term of HS.Grad
#' In a regression course, you may have learned many ways of doing it. For example, you can run both model, and look at the likelihood ratio test.

dt.state = data.frame(state.x77)

m1 = lm(Murder~Population+HS.Grad,data = dt.state)
summary(m1)
m2 = lm(Murder~Population+HS.Grad+I(HS.Grad^2),data = dt.state)
summary(m2)
anova(m1,m2,test='LRT')

#' In the following, let us compare the two models using 5-fold cross validation.
k=5
(n=nrow(dt.state))
nk = n/k # so each set has 10
id.cv = rep(1:k,nk) # generate IDs for test set
table(id.cv)
id.cv = sample(id.cv) # by default, it shuffle the vector so it is randomized.
errs = c(0,0) # initialize the output
for(ii in 1:k){
  dt.train = dt.state[id.cv!=ii,]
  dt.test = dt.state[id.cv==ii,]
  m1ii = lm(Murder~Population+HS.Grad,data = dt.train)
  m2ii = lm(Murder~Population+HS.Grad+I(HS.Grad^2),data = dt.train)
  err.mod1.ii = dt.test$Murder-predict(m1ii,newdata=dt.test)
  err.mod2.ii = dt.test$Murder-predict(m2ii,newdata=dt.test)
  errs[1] = errs[1]+sum(err.mod1.ii^2)
  errs[2] = errs[2]+sum(err.mod2.ii^2)
}
errs = errs/n # normalize by the total sample size.


#' We can write everything in a function and run it.
#'

modCompCv <- function(k,dt){
  n=nrow(dt)
  # what if n/k is not an integer
  nk = floor(n/k) # first floor it
  nr = n-k*nk # check how many left
  id.cv = rep(1:k,nk) # generate initial IDs
  if(nr>0) id.cv = c(id.cv,1:nr) # if any samples left, assign one to each of the first nr stratas.
  id.cv = sample(id.cv) # by default, it shuffles the vector
  errs = c(0,0)
  for(ii in 1:k){
    dt.train = dt[id.cv!=ii,]
    dt.test = dt[id.cv==ii,]
    m1ii = lm(Murder~Population+HS.Grad,data = dt.train)
    m2ii = lm(Murder~Population+HS.Grad+I(HS.Grad^2),data = dt.train)
    # If n/k is not an integer, different test sets may have different sample sizes. So you do not want to average the squared errors within each test set. Instead, you add them, and normalize by the total data size after the loop.
    errs[1] = errs[1]+sum((dt.test$Murder-predict(m1ii,newdata=dt.test))^2)
    errs[2] = errs[2]+sum((dt.test$Murder-predict(m2ii,newdata=dt.test))^2)
  }
  errs = errs/n
  names(errs) = c('m1','m2')
  return(errs)
}
modCompCv(5,dt.state)

modCompCv(7,dt.state)

#'
#'
#' #### Practice:
#'You will use the built-in R dataset USArrests again. Suppose we are interested modeling Rape rate and see how it may relate to UrbanPop. In particular, we are interested whether we should take a log transformation of the response variable because its distribution is skewed. Let us decide based on 10 fold cross validation, and compare the linear models with the formulas Rape~UrbanPop and log(Rape)~UrbanPop, respectively. Based on the results, do you think a log() transformation is needed?
#'
#'Note that the responses of the two models are different, and the two errors to be compared should be on the sample scale. For one point in the tet set, if the true value of the Rape rate is 3.1, the output of the predict() function for the model with formula log(Rape)~UrbanPop is 1.2, what is its prediction error in the scale of the orignal response?

hist(USArrests$Rape)



modCompCv <- function(k,dt){
  #dt=USArrests
  #k=10
  n=nrow(dt)
  # what if n/k is not an integer
  nk = floor(n/k) # first floor it
  nr = n-k*nk # check how many left
  id.cv = rep(1:k,nk) # generate initial IDs
  if(nr>0) id.cv = c(id.cv,1:nr) # if any samples left, assign one to each of the first nr stratas.
  id.cv = sample(id.cv) # by default, it shuffles the vector
  errs = c(0,0)
  for(ii in 1:k){
    #ii=1
    dt.train = dt[id.cv!=ii,]
    dt.test = dt[id.cv==ii,]
    m1ii = lm(Rape~UrbanPop,data = dt.train)
    m2ii = lm(log(Rape)~UrbanPop,data = dt.train)
    # If n/k is not an integer, different test sets may have different sample sizes. So you do not want to average the squared errors within each test set. Instead, you add them, and normalize by the total data size after the loop.
    errs[1] = errs[1]+sum((dt.test$Rape-predict(m1ii,newdata=dt.test))^2)
    errs[2] = errs[2]+sum((dt.test$Rape-exp(predict(m2ii,newdata=dt.test)))^2)
  }
  errs = errs/n
  names(errs) = c('m1','m2')
  return(errs)
}

modCompCv(10,USArrests)

modCompCv(3,USArrests)

