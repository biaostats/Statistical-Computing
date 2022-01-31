#' ---
#' title: "Bootstrap and Jackknife Part 2: bootstrap CIs and JAB"
#' author: "Biao Zhang"
#' date: "Nov 13th, 2021"
#' ---
#'
#' ## Please do NOT share the questions with anyone who are not enrolled in this class. Please do NOT share your code with anyone when you are enrolled in this class. They will be considered as academic misconduct.
#'

#'
#' You will use the health insurance data again.
#'
insurance.df = read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv')
#'
#' 1. (6 points) Please use this data to find the bootstrap based 95\% CIs (normal, percentile, and basic) for the average difference in BMI between subjects with children and with no children.
#' Please implement from scratch as in the class examples.
#' Based on the intervals that you have calculated, do you think there is a real difference in BMI between the two groups?
#'
meandiff.insur = mean(insurance.df[insurance.df$children == 0,'bmi']) - mean(insurance.df[insurance.df$children != 0,'bmi'])

boot.meandiff <- function(dt){
  n = nrow(dt)
  id.b = sample(1:n,size=n,replace = T)
  boot.samp.b = dt[id.b,]
  boot.rep.b = mean(boot.samp.b[boot.samp.b$children == 0,'bmi']) - mean(boot.samp.b[boot.samp.b$children != 0,'bmi'])
  return(boot.rep.b)
}

B = 1000
set.seed(2046)
out.boot.meandiff = replicate(B,boot.meandiff(insurance.df))

se.boot.meandiff = sd(out.boot.meandiff)

p.upper = 1-0.05/2
p.lower=0.05/2

#' Normal bootstrap CI
(lb.meandiff.normal = meandiff.insur-qnorm(p.upper)*se.boot.meandiff)
(ub.meandiff.normal = meandiff.insur+qnorm(p.upper)*se.boot.meandiff)

#' Percentile bootstrap CI
(lb.meandiff.percentile = quantile(out.boot.meandiff,probs = p.lower,names = F))
(ub.meandiff.percentile = quantile(out.boot.meandiff,probs = p.upper,names = F))

#' Basic bootstrap CI 
(lb.meandiff.basic = 2*meandiff.insur-quantile(out.boot.meandiff,probs = p.upper,names = F))
(ub.meandiff.basic = 2*meandiff.insur-quantile(out.boot.meandiff,probs = p.lower,names = F))

#' Following the calculation results given above, there's no difference between two 
#' groups in BMI.
#'
#' 2. (6 points) Now please construct the three types of 95\% bootstrap intervals for the correlation between BMI and the medical charges.
#' Please implement from scratch as in the class examples.
#' Based on your intervals, do you think there is a non-zero correlation between the two?
cor.insur = cor(insurance.df$bmi, insurance.df$charges)

boot.cor <- function(dt){
  n = nrow(dt)
  id.b = sample(1:n,size=n,replace = T)
  boot.samp.b = dt[id.b,]
  boot.rep.b = cor(boot.samp.b$bmi, boot.samp.b$charges)
  return(boot.rep.b)
}

B = 1000
set.seed(2046)
out.boot.cor = replicate(B,boot.cor(insurance.df))

se.boot.cor = sd(out.boot.cor)

p.upper = 1-0.05/2
p.lower=0.05/2

#' Normal bootstrap CI
(lb.cor.normal = cor.insur-qnorm(p.upper)*se.boot.cor)
(ub.cor.normal = cor.insur+qnorm(p.upper)*se.boot.cor)

#' Percentile bootstrap CI
(lb.cor.percentile = quantile(out.boot.cor,probs = p.lower,names = F))
(ub.cor.percentile = quantile(out.boot.cor,probs = p.upper,names = F))

#' Basic bootstrap CI 
(lb.cor.basic = 2*cor.insur-quantile(out.boot.cor,probs = p.upper,names = F))
(ub.cor.basic = 2*cor.insur-quantile(out.boot.cor,probs = p.lower,names = F))
#' According to the calculation results above, there are correlations between
#' BMI and medical charges.
#'
#' 3. (3 points) Please use R package "boot" to generate 95% CIs for the correlation between BMI and medical charges based on normal, basic and percentile methods. Please do not output stud or bca interval in this lab question.
#'
library(boot)

bmichargesCor <- function(dt,i){
  return(cor(dt$bmi[i],dt$charges[i]))
}

set.seed(2046)
bmichargescor.boot.obj <- boot(insurance.df,R=1000,statistic=bmichargesCor,stype = 'i')

boot.ci(bmichargescor.boot.obj,type = c('norm','basic','perc'),conf = 0.95)

#'
#' 4. (8 points) Please use this data to find the bootstrap t 95\% CI for (1) the average difference in BMI between subjects with children and with no children.
#' and (2) the correlation between BMI and the medical charges. You may use either the boot package or implement from scratch.
#' Please interpret your intervals, i.e., is there a real difference in BMI between subjects with and without children? And do you think there is a non-zero correlation between BMI and the medical charges?
#'
#' Remarks: (a) If you use boot package, please make sure that the functions provided to the argument "statistic" also returns the estimated variance of $\hat{\theta}^{(b)}$.Please read the help pages of of the relevant functions in boot package very carefully.
#' (b) Note that for the case of difference in bmi, you do not really need double-bootstrap to calculate it. For the correlation between bmi and charges, it may be better to estimate it with double bootstrap.
#'
##################
##Avg Difference##
##################
boot.mean.density <- function(dt){
  n = nrow(dt)
  id.b = sample(1:n,size=n,replace = T)
  boot.samp.b = dt[id.b,]
  boot.rep.b = mean(boot.samp.b[boot.samp.b$children == 0,'bmi']) - mean(boot.samp.b[boot.samp.b$children != 0,'bmi'])
  n1 = length(boot.samp.b[boot.samp.b$children == 0,'bmi'])
  n2 = length(boot.samp.b[boot.samp.b$children != 0,'bmi'])
  var1 = var(boot.samp.b[boot.samp.b$children == 0,'bmi'])
  var2 = var(boot.samp.b[boot.samp.b$children != 0,'bmi'])
  se.rep.b = sqrt(var1/n1 + var2/n2)
  output = c(boot.rep.b,se.rep.b)
  names(output) = c('est','se')
  return(output)
}

# Now run this function B=1000 times. This number of replicates needs to be large because it is for estimating quantiles of the studentized statistics.
B = 1000
set.seed(2046)
insur.boot.mean.density = replicate(B,boot.mean.density(insurance.df))


# Calculate the bootstrap student t statistics, and find the needed quantiles.
t.mean.boot = (insur.boot.mean.density['est',]-meandiff.insur)/insur.boot.mean.density['se',]

p.upper = 1-0.05/2
p.lower=0.05/2

t.mean.upper = quantile(t.mean.boot,probs = p.upper,names = F)
t.mean.lower = quantile(t.mean.boot,probs = p.lower,names = F)
# Calculate the standard error using the output bootstrap replicates
se.insur.boot.mean.density = sd(insur.boot.mean.density['est',])

# Here comes the bootstrap t interval
(lb.insurmeandensity.boot.t = meandiff.insur-t.mean.upper*se.insur.boot.mean.density)
(ub.insurmeandensity.boot.t = meandiff.insur-t.mean.lower*se.insur.boot.mean.density)
##################
##CorBMI&Charges##
##################
boot.double.cor.density <- function(dt,B2){
  n = nrow(dt)
  id.b = sample(1:n,size=n,replace = T)
  boot.samp.b = dt[id.b,]
  boot.rep.b = cor(boot.samp.b$bmi, boot.samp.b$charges)
  boot.double.rep.b = replicate(B2,boot.cor(boot.samp.b))
  se.rep.b = sd(boot.double.rep.b)
  output = c(boot.rep.b,se.rep.b)
  names(output) = c('est','se')
  return(output)
}

B = 1000
B2=100
set.seed(2046)
insur.boot.double.cor.density = replicate(B,boot.double.cor.density(insurance.df,B2))

t.cor.boot = (insur.boot.double.cor.density['est',]-cor.insur)/insur.boot.double.cor.density['se',]

p.upper = 1-0.05/2
p.lower=0.05/2

t.cor.upper = quantile(t.cor.boot,probs = p.upper,names = F)
t.cor.lower = quantile(t.cor.boot,probs = p.lower,names = F)

se.insur.boot.cor.density = sd(insur.boot.double.cor.density['est',])

(lb.insurcordensity.boot.t = cor.insur-t.cor.upper*se.insur.boot.cor.density)
(ub.insurcordensity.boot.t = cor.insur-t.cor.lower*se.insur.boot.cor.density)
#' The conclusions do not change. There's no difference between two groups in BMI.
#' There's correlation between BMI and the medical charges.
#'
#' 5. (4 points) Please use jackknife after bootstrap to provide the standard error for the bootstrap estimate of the standard error of estimate of the average difference in BMI between subjects with children and with no children.
#' You are allowed to use R package boot. Please still use R=1000
bmiMean <- function(dt,i){
  dt.emp = dt[i,]
  est = mean(dt.emp[dt.emp$children == 0,'bmi']) - mean(dt.emp[dt.emp$children != 0,'bmi'])
  return(est)
}

bmiMean.boot.obj = boot(insurance.df, R = 1000, statistic = bmiMean)
bmiMean.boot.array = boot.array(bmiMean.boot.obj)

se.jackafterboot = function(est, freq.mtx, i) {
  keep = (freq.mtx[,i] == 0)
  out = sd(est[keep])
  return(out)
}
n = nrow(insurance.df)
se.bmiMean.jackafterboot = sapply(1:n, se.jackafterboot, est = bmiMean.boot.obj$t, freq.mtx = bmiMean.boot.array)
(se.se.bmiMean.boot = (n-1)/sqrt(n)*sd(se.bmiMean.jackafterboot))