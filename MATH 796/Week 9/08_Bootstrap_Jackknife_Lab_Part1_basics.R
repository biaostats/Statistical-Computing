#' ---
#' title: "Bootstrap and Jackknife Part 1: Basics"
#' author: "Biao Zhang"
#' date: "Nov 6th, 2021"
#' ---
#' ## Please do NOT share this with anyone outside this class. It will be considered as academic misconduct.
#'
#'
#'
#' You will use the health insurance data that we have used before in a practice problem for linear model.
#'
insurance.df = read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv')
#'
#' 1. (6 points) Please use this data to estimate the average difference in BMI between subjects with children and with no children.
#' Then use bootstrap with B = 200 to estimate the standard error and bias for your estimate. Please implement bootstraping procedure from scratch.
#'
(meandiff.insur = mean(insurance.df[insurance.df$children == 0,'bmi']) - mean(insurance.df[insurance.df$children != 0,'bmi']))
boot.meandiff.density <- function(dt){
  n = nrow(dt)
  id.b = sample(1:n,size=n,replace = T)
  boot.samp.b = dt[id.b,]
  boot.rep.b = mean(boot.samp.b[boot.samp.b$children == 0,'bmi']) - mean(boot.samp.b[boot.samp.b$children != 0,'bmi'])
  return(boot.rep.b)
}
B = 200
set.seed(20211105)
boot.meandiff.density = replicate(B,boot.meandiff.density(insurance.df))

hist(boot.meandiff.density,breaks = 20, main = 'Histogram of Mean Differences')

(se.boot.meandiff.density = sd(boot.meandiff.density)) # standard error
(bias.boot.meandiff.density = mean(boot.meandiff.density)-meandiff.insur) # bias
bias.boot.meandiff.density/se.boot.meandiff.density
#'
#' 2. (6 points)Please use this data to estimate the average difference in BMI between subjects with children and with no children.
#' Then use jackknife to estimate the standard error and bias for your estimate. Please implement bootstraping procedure from scratch.
#'
# The point estimator of the average difference is identical to the question 1, 
# I do not repeat it here.
jack.meandiff.density <- function(i,dt){
  boot.samp.b = dt[-i,]
  boot.rep.b = mean(boot.samp.b[boot.samp.b$children == 0,'bmi']) - mean(boot.samp.b[boot.samp.b$children != 0,'bmi'])
  return(boot.rep.b)
}

n = nrow(insurance.df)
jack.meandiff.density = sapply(1:n, jack.meandiff.density, dt = insurance.df)

hist(jack.meandiff.density, breaks = 20, main = 'Histogram of Mean Differences')

(se.jack.meandiff.density = (n-1)/sqrt(n)*sd(jack.meandiff.density))
(bias.jack.meandiff.density = (n-1)*(mean(jack.meandiff.density)-meandiff.insur)) # It is almost 0.
bias.jack.meandiff.density/se.jack.meandiff.density


###' 3. (10 points) Previously in Practice problems on Sep 22, we have used `lm()` to model the log of `charges`  on the other variables. It fits pretty well.
###' We now want to compare slightly different models, we want to compare two models using cross-validation.
###'  (1) the model with only the linear terms of age, sex, bmi, and smoker
###'  (2) the model with the same covariance as in model 1, plus a square term of bmi.
###' Please do the following by implementing CV from scratch as in class examples.
###'
###' a. Run 3-fold cross-validation to compare the  two models
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
    m1ii = lm(log(charges) ~ age + sex + bmi + smoker,data = dt.train)
    m2ii = lm(log(charges) ~ age + sex + bmi + smoker + I(bmi^2),data = dt.train)
    # If n/k is not an integer, different test sets may have different sample sizes. So you do not want to average the squared errors within each test set. Instead, you add them, and normalize by the total data size after the loop.
    errs[1] = errs[1]+sum((dt.test$charges-exp(predict(m1ii,newdata=dt.test)))^2)
    errs[2] = errs[2]+sum((dt.test$charges-exp(predict(m2ii,newdata=dt.test)))^2)
  }
  errs = errs/n
  names(errs) = c('m1','m2')
  return(errs)
}
modCompCv(3,insurance.df)
###' b. Repeat a) for 20 times, and report the mean and the standard error of the difference between the prediction errors of the two models. Does your conclusion on which model is better vary across the 20 CV analysis?
set.seed(1942)
errs.v = replicate(20, modCompCv(3, insurance.df))
errs.diff = errs.v[1,] - errs.v[2,]
mean(errs.diff)
sd(errs.diff)
hist(errs.diff, breaks = 20, xlim = c(-9e+05, 2e+05), main = "Histogram of Prediction Error Differences")
###' According to the histogram, most analysis results show the difference of prediction errors
###' are negative, indicating the linear log-model is better. However, there's one analysis result
###' shows the nonlinear log-model is better. The result varies. Since most show the linear is better,
###' we prefer to use linear log-model in this case.
###' 
###' c. Run leave-one-out cross-validation to compare the two models. Do you expect to see different numerical values of the prediction errors if you repeat your leave-one-out cross-validation procedure?
###'
oneOutCv <- function(dt){
  n=nrow(dt)
  errs = c(0,0)
  for(ii in 1:n){
    dt.train = dt[-ii,]
    dt.test = dt[ii,]
    m1ii = lm(log(charges) ~ age + sex + bmi + smoker,data = dt.train)
    m2ii = lm(log(charges) ~ age + sex + bmi + smoker + I(bmi^2),data = dt.train)
    errs[1] = errs[1]+sum((dt.test$charges-exp(predict(m1ii,newdata=dt.test)))^2)
    errs[2] = errs[2]+sum((dt.test$charges-exp(predict(m2ii,newdata=dt.test)))^2)
  }
  errs = errs/n
  names(errs) = c('m1','m2')
  return(errs)
}
oneOutCv(insurance.df)
###' As discussed in the class, unlike k-fold CV (k > 1), the leave-one-out CV is deterministic.
###' we do not expect to see different numerical value of prediction errors. It's redundant to 
###' repeat such work multiple times here. 

