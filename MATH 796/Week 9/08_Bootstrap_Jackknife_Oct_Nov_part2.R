#' ---
#' title: "Bootstrap and Jackknife Part 2"
#' author: "Qi Zhang"
#' date: "August 01, 2021"
#' ---
#'
#' ##
#'


#'
#'
#' #### Bootstrap Confidence Intervals
#'
#' Now we introduce four methods to construct confidence invterals using bootstrap. They are
#' (1) Standard normal bootstrap confidence interval.
#' (2) Percentile confidence interval
#' (3) Basic bootstrap confidence interval
#' (4) Bootstrap t confidence interval
#'
#'
#' #### Standard normal bootstrap confidence interval.
#'
#' If the estimator $\hat{\theta}$ can be casted a a sample mean, its asympotic distribution is normal, and the central limit theory says that
#' \[
#' Z = \frac{\hat{\theta}-E(\hat{\theta})}{se(\hat{\theta})} \approx N(0,1)
#' \]
#' So an approximate $100(1-\alpha)\%$ CI for $\theta$ is
#' \[
#' \hat{\theta}\pm z_{1-\alpha/2}se(\hat{\theta})
#' \]
#' where $se(\hat{\theta})$ is estimated using bootstrap, and $z_{1-\alpha/2} = \Phi^{-1}(1-\alpha/2)$.
#'
#' In order to use this method, we have assumed that $\hat{\theta}$ is unbiased, that is $E(\hat{\theta})-\theta= 0$, and that the conditions of CLT are satisfied.
#' So it generally does not work well for highly skewed data.
#'
#' #### Percentile bootstrap confidence interval
#'
#'  In general, we do not know whether the sampling distribution of $\hat{\theta}$ is normal.
#'  After bootstrap, we can use the empirical distribution of $\hat{\theta}^{(1)},\ldots,\hat{\theta}^{(B)}$ as an estimate of it.
#'  We want to construct a symmetric $100(1-\alpha)\%$ CI for $\theta$ $(L,U)$ such that $P(L>\theta)=P(U<\theta)=\alpha/2$
#'  where the probabilities are calculated according to the sampling distribution of $\hat{\theta}$.
#'  Using the empirical distribution from bootstrap as its estimate gives us the percentile bootstrap confidence interval $(\hat{\theta}^\star_{\alpha/2},\hat{\theta}^\star_{1-\alpha/2})$,
#'  where $\hat{\theta}^\star_{\alpha/2}$ and $\hat{\theta}^\star_{1-\alpha/2}$ are just the $\alpha/2$ and $1-\alpha/2$ percentile of  $\hat{\theta}^{(1)},\ldots,\hat{\theta}^{(B)}$.
#'
#'  It has some advantage over the standard normal interval, but can be biased sometime. adjustments to percentile methods have been proposed such as bias-corrected and accelerated (BCa) percentile interval.
#'  The pros of percentile CIs include
#'  (1) Intuitive, and simple to implement
#'  (2) Range preserving: e.g., if the parameter is known to be between -1 to 1 such as correlation, the upper and lower bounds are guaranteed to be within this range.
#'  (3) Transformation invariant: if $g$ is a monotone increasing function of $\theta$, then  $(g(\hat{\theta}^\star_{\alpha/2}),g(\hat{\theta}^\star_{1-\alpha/2}))$ is a $100(1-\alpha)\%$ CI for $g(\theta)$.
#'
#'  The cons of percentile CIs include but not limited to
#'  (1) Generally biased, and not nearly symmetric
#'  (2) Usually narrower than the others for small $n$, and the coverage probability is lower than the nominal value.
#'
#'
#'
#' #### Basic Bootstrap Confidence Interval
#'
#'  The basic bootstrap CI consider the distribution of approximate pivotal quantity.
#'
#'  A pivotal quantity is a function of the observed data (and the unknown parameter) such that its distribution does not depend on any unknown parameters.
#'
#'  In the case of basic bootstrap CI, instead of considering the sampling distribution of $\hat{\theta}$ (which depends on $\theta$),
#'   it consider the distribution of $\hat{\theta}-\theta$, with the hope that its distribution does not depends on $\theta$, or at least being affected less by it.
#'  Again,  we want to construct a symmetric $100(1-\alpha)\%$ CI for $\theta$ $(L,U)$ such that $P(L>\theta)=P(U<\theta)=\alpha/2$, where the probabilities are calculated according to the sampling distribution of $\hat{\theta}-\theta$.
#' But now we have   (something looks wrong here, double check)
#'\[
#' \alpha/2 = P(L\geq\theta) = P(L-\hat{\theta}\geq\theta-\hat{\theta}) = P(\hat{\theta}-\theta\geq\hat{\theta}-L)
#'\]
#' Using the empirical distribution of $\hat{\theta}^{(1)}-\hat{\theta},\ldots,\hat{\theta}^{(B)}-\hat{\theta}$ to approximate the sampling distribution of $\hat{\theta}-\theta$,
#'  now $\hat{\theta}-L$ is the $(1-\alpha/2)$th percentile of $\hat{\theta}^{(b)}-\hat{\theta}$, which is $\hat{\theta}^\star_{1-\alpha/2}-\hat{\theta}$. So
#'  \[
#'  L = 2\hat{\theta} - \hat{\theta}^\star_{1-\alpha/2}
#'  \]
#'Similarly, the upper bound of the basic bootstrap CI is
#'\[
#'U = 2\hat{\theta} - \hat{\theta}^\star_{\alpha/2}
#'\]
#'
#'Since both percentile and basic bootstrap CIs rely on the quantiles of empirical distribution of the bootstrap replicates, the required number of replicates B is usually much larger than what is needed for standard error or bias estimation.
#'For example, B =999, or 1000.
#'
#' The pros of basic bootstrap CI includes
#' (1) Easy to implement
#' (2) Usually accurate
#'
#' The cons of basic bootstrap CI includes
#' (1) Still biased, though better than percentile CI in this aspect
#' (2) not range preserving or transformation invariant.
#'
#'
#' Example:
#' We will use the R built-in dataset state.x77 again, and construct 95\% CIs for the estimated average of population density (Population divided by Area) across 50 states using the above three methods.

dt.state = data.frame(state.x77)
(ave.density = mean(dt.state$Population/dt.state$Area))

boot.mean.density <- function(dt){
  n = nrow(dt)
  id.b = sample(1:n,size=n,replace = T)
  boot.samp.b = dt[id.b,]
  boot.rep.b = mean(boot.samp.b$Population/boot.samp.b$Area)
  return(boot.rep.b)
}

B = 1000
set.seed(2046)
state.boot.mean.density = replicate(B,boot.mean.density(dt.state))

p.upper = 1-0.05/2
p.lower=0.05/2

#' Standard normal bootstrap CI is just a z interval with se estimated using bootstrap. If using this method, you could have just set $B=100$
(se.state.boot.mean.density = sd(state.boot.mean.density))
(lb.statemeandensity.normal = ave.density-qnorm(p.upper)*se.state.boot.mean.density)
(ub.statemeandensity.normal = ave.density+qnorm(p.upper)*se.state.boot.mean.density)


#' Percentile bootstrap CI just use the percentiles of the bootstrap replicates.
(lb.statemeandensity.percentile = quantile(state.boot.mean.density,probs = p.lower,names = F))
(ub.statemeandensity.percentile = quantile(state.boot.mean.density,probs = p.upper,names = F))

#' Basic bootstrap CI uses the percentiles of the distribution of the bootstrap relicates after adjusting for $\hat{\theta}$.
(lb.statemeandensity.basic = 2*ave.density-quantile(state.boot.mean.density,probs = p.upper,names = F))
(ub.statemeandensity.basic = 2*ave.density-quantile(state.boot.mean.density,probs = p.lower,names = F))

#'
#' #### Practice:
#'  You will use the built-in R dataset USArrests again, and calculate the 90% CI for the correlation between the Assault rate and UrbanPop using the three methods with $B=999$.
#'




#'
#'
#'
#' #### Bootstrap t confidence interval
#'
#'  Bootstrap t CI does not use a Student t distribution. Instead, it bootstraps a "t type" statistics (a studentized statistic).
#'  Usually the distribution of such studentized statistics is less affected by the unknown parameters. Its steps are as the following.
#'
#'  1. Compute the observed statistic $\hat{\theta}$.
#'  2. For each replicate, indexed $b = 1,\ldots,B$:
#'    (a) Sample with replacement from x to get the $b$th bootstrap sample $x^{(b)}$ as before.
#'    (b) Compute $\hat{\theta}^{(b)}$ from the $b$th bootstrap sample.
#'    (c) Compute or estimate $se(\hat{\theta}^{(b)})$, the standard error $\hat{\theta}^{(b)}$.
#'     (It can be done analytically for some statistics, or approximated using either nonparametric delta method or a second layer of bootstrap (double bootstrap, bootstrap from $x^{(b)}$)
#'    (d) Compute the $b$th replicate of the "t" statistic, $t^{(b)} = \frac{\hat{\theta}^{(b)}-\hat{\theta}}{se(\hat{\theta}^{(b)})}$
#'  3. The sample of replicates $t^{(1)},\ldots,t^{(B)}$ is the reference distribution for bootstrap t. Find the sample quantiles $t^\star_{\alpha/2}$ and $t^\star_{1-\alpha/2}$
#'  4. Compute $se(\hat{\theta})$ using $\hat{\theta}^{(b)}$'s
#'  5. Compute confidence limits
#'  \[
#'  (\hat{\theta} - t^\star_{1-\alpha/2}\hat{se}(\hat{\theta}),  \hat{\theta} - t^\star_{\alpha/2}\hat{se}(\hat{\theta}))
#'  \]
#'
#'  Pros of Bootstrap t CI includes
#'  (1) Usually perform well empirically, especially if $\theta$ is approximately a location parameter.
#'  (2) Second-order accurate: theoretically proven more accurate than the basic and percentile.
#'
#'  Cons of Bootstrap t CI includes
#'  (1) Estimating $se(\hat{\theta}^{(b)})$ may require double-bootstrap: bootstrap from each bootstrap sample.
#'  (2) May not work as well for correlation/association measures.
#'  (3) Not range preserving or transformation invariant.
#'

#' Example:
#' We will use the R built-in dataset state.x77 again, and construct 95\% Bootstrap t CI for the estimated average of population density (Population divided by Area) across 50 states.

dt.state = data.frame(state.x77)
(ave.density = mean(dt.state$Population/dt.state$Area))

#' We can use the same function as that we have used before. But with a twist.
#'

boot.mean.density <- function(dt){
  n = nrow(dt)
  id.b = sample(1:n,size=n,replace = T)
  boot.samp.b = dt[id.b,]
  boot.rep.b = mean(boot.samp.b$Population/boot.samp.b$Area)
  return(boot.rep.b)
}

#' There are two levels of bootstrap.
#' We first bootstrap from the original data, and calculate the bootstrap replicate as before.
n = nrow(dt.state)
id.b = sample(1:n,size=n,replace = T)
boot.samp.b = dt.state[id.b,]
boot.rep.b = mean(boot.samp.b$Population/boot.samp.b$Area)
#' Then we need to bootstrap from this bootstrap sample to estimate the standard error for the bootstrap replciate $\hat{\theta}^{(b)}$.
#' To do this, we just need to repeat what we did before, using the bootstrap sample as the "original data"
#' Since it is for standard error estimation, we do not need a large number of replicates, 100 is usually good enough.
#'
B2= 100

state.boot.mean.density.rep.b = replicate(B2,boot.mean.density(boot.samp.b))
(se.state.boot.mean.density.rep.b = sd(state.boot.mean.density.rep.b))

#' Wrap these code to a function that does double-bootstrap (or iterative bootstrap)
#' This function output both of the bootstrap replicate and its standard error.

boot.double.mean.density <- function(dt,B2){
  n = nrow(dt)
  id.b = sample(1:n,size=n,replace = T)
  boot.samp.b = dt[id.b,]
  boot.rep.b = mean(boot.samp.b$Population/boot.samp.b$Area)
  boot.double.rep.b = replicate(B2,boot.mean.density(boot.samp.b))
  se.rep.b = sd(boot.double.rep.b)
  output = c(boot.rep.b,se.rep.b)
  names(output) = c('est','se')
  return(output)
}

#' Now run this function B=1000 times. This number of replicates needs to be large because it is for estimating quantiles of the studentized statistics.
B = 1000
B2=100
set.seed(2046)
state.boot.double.mean.density = replicate(B,boot.double.mean.density(dt.state,B2))
dim(state.boot.double.mean.density)
state.boot.double.mean.density[,1:3]

#' Calculate the bootstrap student t statistics, and find the needed quantiles.

t.boot = (state.boot.double.mean.density['est',]-ave.density)/state.boot.double.mean.density['se',]
hist(t.boot)

p.upper = 1-0.05/2
p.lower=0.05/2

(t.upper = quantile(t.boot,probs = p.upper,names = F))
(t.lower = quantile(t.boot,probs = p.lower,names = F))
#' Calculate the standard error using the output bootstrap replicates
(se.state.boot.mean.density = sd(state.boot.double.mean.density['est',]))

#' Here comes the bootstrap t interval
(lb.statemeandensity.boot.t = ave.density-t.upper*se.state.boot.mean.density)
(ub.statemeandensity.boot.t = ave.density-t.lower*se.state.boot.mean.density)

#'
#' #### Practice:
#'  You will use the built-in R dataset USArrests again, and calculate the 90% bootstrap t CI for the correlation between the Assault rate and UrbanPop.
#'

#'
#' #### Jackknife-after-Bootstrap
#'
#' We have used bootstrap to estimate the standard error and bias of $\hat{\theta}$.
#' Just like $\hat{\theta}$, these estimates are also functions of the data, so that they are also random variables.
#' Sometimes we are interested in the standard error of these estimates as well (the standard error of the estimated standard error and the estimated bias).
#' There are different ways of doing it. One way is to repetitively bootstrap from the original data, which is computationally intensive.
#' Another idea is to try the jackknife after bootstrap. The benefit of this approach is that it does not require any additional sampling or complicated computation.
#'
#' Recall that the jackknife  replicates $\hat{\theta}_{(i)}$ are calculated without sample $i$. After bootstrap B times, we now have bootstrap replicates $\hat{\theta}^{(1)},\ldots,\hat{\theta}^{(B)}$.
#' Which of them are calculated without using sample $i$? $\hat{\theta}^{(b)}$ is calculated without sample $i$ if this sample is not in the $b$th bootstrap sample!
#'
#' Let $J(i)\subset \{1,\ldots, B\}$ be the set of bootstrap samples that does not contain sample $i$, and $B(i)=|J(i)|$ be its size,
#'  we can then use these bootstrap replicates to compute a jackknife replicate of the quantity that we are interested in.
#'
#'  Suppose we are interested in estimating the standard error of $\hat{se}(\hat{\theta}^\star)$. Recall that we estimate it using bootstrap
#'  \[
#'  \hat{se}(\hat{\theta}^\star) = \sqrt{\frac{1}{B-1}\sum_{b=1}^B(\hat{\theta}^{(b)}-\bar{\hat{\theta}^{\star}})^2}
#' \]
#' where $\bar{\hat{\theta}^{\star}}=\frac{1}{B}\sum_{b=1}^B\hat{\theta}^{(b)}$.
#'
#' A jackknife replicate of it is calculated using the bootstrap samples without sample $i$ as the following.
#' \[
#'  \hat{se}_{J(i)} = \sqrt{\frac{1}{B(i)}\sum_{b\in J(i)}(\hat{\theta}^{(b)}-\bar{\hat{\theta}^{\star}_{(J(i))}})^2}
#' \]
#' where $\bar{\hat{\theta}^{\star}_{(J(i))}}=\frac{1}{B(i)}\sum_{b\in J(i)}\hat{\theta}^{(b)}$.
#'
#' Then the bias and standard error of $\hat{se}(\hat{\theta}^\star)$ can be calculated using its jackknife replicates $ \hat{se}_{J(1)},\ldots, \hat{se}_{J(n)}$ using the usual formula in the section of regular jackknife.
#'
#'
#' Example: Let us reconsider the R built-in dataset state.x77. Previously, we have calculated the standard error and bias for the estimated average of population density (Population divided by Area) across 50 states using bootstrap.
#' Now we want to find the standard error of the standard error.
#' In order to find $J(i)$ for $i=1,\ldots,n$, we need to keep track of which bootstrap sample contain which sample.
#' We can edit the function that we have used to output this information. Now the output will be a list of two elements.

dt.state = data.frame(state.x77)
(ave.density = mean(dt.state$Population/dt.state$Area))

boot.mean.density.wid <- function(dt){
  n = nrow(dt)
  id.b = sample(1:n,size=n,replace = T)
  boot.samp.b = dt[id.b,]
  boot.rep.b = mean(boot.samp.b$Population/boot.samp.b$Area)
  return(list(est=boot.rep.b,id=id.b))
}

B = 100
set.seed(2046)
state.boot.mean.density.wid = replicate(B,boot.mean.density.wid(dt.state))
class(state.boot.mean.density.wid)
dim(state.boot.mean.density.wid)
state.boot.mean.density.wid[,2]
state.boot.mean.density.wid[2,2]

#'
#' Now let us store the estimates and the ids in two objects.
#' Each column of state.boot.id contains the sample ids in this bootstrap sample.
#'
state.boot.mean.density = unlist(state.boot.mean.density.wid[1,],use.names=F)

state.boot.id = do.call('cbind',state.boot.mean.density.wid[2,])
dim(state.boot.id)

(se.state.boot.mean.density = sd(state.boot.mean.density))

#'
#' Now let us construct the jackknife-after-bootstrap estimate of the standard error of se.state.boot.mean.density
#' First we need to calculate the jakknife replicates.
#' The following function finds the bootstrap replicates that does not use sample i, and calculate their se as the $i$th jackknife replicate of se.state.boot.mean.density

se.jackafterboot <- function(est,id.mtx,i){
  ix.with.i = (id.mtx==i) # define a logical matrix of whether each sample is sample i
  keep = !apply(ix.with.i,2,any) # for each column, it check whether sample i is in it using any. Its complement is what we want to keep.
  out = sd(est[keep]) # calculate sd of the bootstrap replicates without using sample i. This is the ith jackknife replicate of se.state.boot.mean.density.
  return(out)
}
n = nrow(dt.state)
se.state.jackafterboot.mean.density = sapply(1:n, se.jackafterboot,est=state.boot.mean.density,id.mtx=state.boot.id)

#' Then plug it in to the usual formula of jackknife se.
#'
(se.se.state.boot.mean.density = ((n-1)/sqrt(n))*sd(se.state.jackafterboot.mean.density))

#' Remark: Suppose you are using random forest for predicting a continuous response, and you get a point estimate for each subject. What about a confidence interval or prediction interval? (what is the difference? which one is wider?) Or at least having some standard error for the prediction? You can bootstrap. But since prediction by random forest is based on bootstrapping, adding an additional layer of bootstraping can be computational intensive. Jackknife-After-Bootstrap comes in handy here for providing a standard error, even though it has been shown to be biased, and often underestimate the uncertainty (still better than nothing). Some more recent developments of random forest (e.g., generalized random forest) provides more accurate and theoretically justified standard error and prediciton intervals.

#'
#' #### Practice:
#'  Now let us reconsider the built-in R dataset USArrests, and find the standard error for our bootstrap estimates of the standard error for the estimated Pearson's correlation between the two variables using bootstrap with B = 100.
#'

#' Now please repeat this using R package boot. As far as I know, it does not have any function to compute se based on jackknife-after-bootstrap directly.
#' So you still need part of the calculation in the above.
#' But it has boot.array function that helps you to keep track of which bootstrap sample contains which original sample.
#' Please read the help page of boot.array.
#' Each row of the output of boot.array represent on bootstrap sample, and present the frequency of each original sample in that bootstrap sample.
#' If the element (k,t) is 0, it means that the orignal sample t is not in the kth bootstrap sample.


#'
#'
#' #### Influence
#'
#' Another use of jackknife after bootstrap is to generate the empirical influence of each observation.
#'
#' Before we introduce this use, we need to introduce influence first.
#'
#' In the setting of linear regression, you mayh have seen influence measures of data points.
#'  Essentially, it measures how the quantity of interests (e.g., the fitted coefficient) would change if the data point is deleted.
#'  The general definition of influence function is as the following. You will see some math, and you just need to remember that they exist.
#'
#'  Let $Y\sim F$, and $t(F)$ is a statistical function. Recall that a parameter is a property about the population, so it can be seen as a function of the distribution function that the data is generated from.
#'  Similarly, a statistic is a function of the data, so it can viewed as a function of the empirical distribution. So $t(F)$ returns the value of the parameter if $F$ is the true distribution,
#'  and returns the corresponding statistic if $F$ is the empirical distribution.
#'
#'   Intuitively, the influence at a particular value $y$ means that how $t(F)$ would change if $F$ is perturbed at this position. Some sort of derivative may be helpful here.
#'   Such intuition can be formally described using **Gateaux differentiability**.
#'    The **influence function $L_t$**, the first derivative of $t(\cdot)$ at distribution $F$, is defined as the following
#' \[
#'   L_t(y;F) = \lim_{\epsilon \rightarrow 0} \frac{t\{(1-\epsilon) F + \epsilon H_y\}-t(F)}{\epsilon} = \left. \frac{\partial t\{(1-\epsilon)F + \epsilon H_y\}}{\partial \epsilon} \right|_{\epsilon = 0}
#'  \]
#' where $H_y\equiv H(u-y)$, and $H(u-y)$ is a unit step function jumping from 0 to 1 at $y$.
#'
#' So if the empirical distribution of the observed data is used as $F$, and we further perturb it at the value of the $i$th observation by deleting this point,
#' the above definition leads to the jackknife estimate of the influence
#'\[
#'(n-1)(\hat{\theta}-\hat{\theta}_{(i)}) \quad i=1,\ldots,n
#'\]
#'
#' R function "empinf" estimate the empirical influence values for the observations from the original data or boot object with several options for the argument "type".
#' The following are what I have used before.
#' jack: the usual jackknife
#' inf: infinitesimal jackknife based on numerical differentiation.
#' reg: regression estimation based on the linear approximation of the statistic function.
#' The options that I have no experience with include: "pos" (positive jackknife, which include an observation twice instead of deleting it)
#'
#' Remark: after reading the source code of boot package, I think (with $90\%$ confidence) that only the regression method work with boot object.
#' The other methods estimate the empirical influence directly from the original data.
#' So R function "empinf" does not perform jackknife after bootstrap.
#'
#' But R function "jack.after.bootstrap" may. This function produce a jackknife-after-bootstrap plot for diagonoisis of the bootstrap analysis.
#'  If you specify the argument "useJ" to be true, it will use jackknife after bootstrap to estimate the jackknife replicates of the estimate from the whole bootstrap, and estimate empirical influence values using these jackknife replicates.
#'  If "useJ" argument is false, it will call R function "empinf" to calculate the influence values.
#'
#'
#' Example: Let us interpret the jackknife-after-bootstrap plot in the context of the R built-in dataset state.x77.Let us estimate the average population density using R package boot, and draw the jackknife after bootstrap plot.

dt.state = data.frame(state.x77)

mean.density <- function(dt,i){
  boot.samp.b = dt[i,]
  est = mean(boot.samp.b$Population/boot.samp.b$Area)
  return(est)
}

B = 100
set.seed(2046)
library(boot)
state.meandensity.boot.obj = boot(dt.state,statistic=mean.density,R=B)

jack.after.boot(state.meandensity.boot.obj)
#' On the vertical axis of this jackknife-after-bootstrap plot are the percentiles of $\hat{\theta}^{(b)}-\bar{\hat{\theta}^{\star}_{(J(i))}}$ for $b\in J(i)$. On the horizontal axis are the jackknife influence values of observation $i$, for $i=1,\ldots,n$
#' The observation IDs are also displayed below the points. The horizontal dash lines are the quantiles of $\hat{\theta}^{(b)}-\bar{\hat{\theta}^{\star}}$ for $b=1,\ldots,B$.
#' For each percentiles, the points are connected with solid lines. For one observation, if there is not highly influential points, we would expect these solid lines overlap with the corresponding dash lines.
#' If some lines are pulled away from the dash lines, it means that they are highly influenced by the points. In this example, observation number 21 and 30 are influential.
#' If one of them is deleted from the data, the bootstrap distribution will become much more peaked. It turns out that observation 30 is New Jersey which is the most densely populated state.

dt.state[c(21,30),]
which.max(dt.state$Population/dt.state$Area)


#'
#' #### Practice:
#'  Now let us reconsider the built-in R dataset USArrests, and draw the jackknife after bootstrap plot for the estimated Pearson's correlation between the two variables using bootstrap with B = 100.
#'
