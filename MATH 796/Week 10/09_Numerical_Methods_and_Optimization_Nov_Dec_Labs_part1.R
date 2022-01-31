#' ---
#' title: "Numerical Methods and Optimization"
#' author: "Biao Zhang"
#' date: "November 22th, 2021"
#' ---
#'
#' ##
#'
#' ## Please do NOT share the questions with anyone who are not enrolled in this class. Please do NOT share your code with anyone when you are enrolled in this class. They will be considered as academic misconduct.
#'

#' 1.(4 points) Do you still remember the value of $\sum_{i=0}^\infty \frac{1}{i!}$?  Please evaluate it within $10^6$ iterations with precision $\epsilon = 10^{-12}$, and compare it with the theoretical value e.
#'
iter.max.factorial = 10^6
eps.factorial = 1e-12
sum.factorial.old = 0 
change.factorial = 1000 
iter.factorial = 0
while(change.factorial>eps.factorial&iter.factorial<iter.max.factorial){
  sum.factorial = sum.factorial.old + 1/factorial(iter.factorial) # update the sum
  change.factorial= abs(sum.factorial-sum.factorial.old) # calculate the stopping criterion
  iter.factorial=iter.factorial+1 # update the iteration
  sum.factorial.old = sum.factorial # update the baseline
}
#' The following code report the results.
if(iter.factorial==iter.max.factorial)  print(paste0('The sequence does not converge in ',iter.max.factorial,' iterations!'))
if(change.factorial<eps.factorial) print(paste0('The seuence converges at value ',sum.factorial,' at iteration ',iter.factorial,'.'))
exp(1)
#'
#' 2. (4 points) Let us reconsider the p-series $\sum_{n=1}^\infty \frac{1}{n^p}$. When $p=1$, does it converge?
#' It turns out that it does not, and as $k$ increases, $\sum_{n=1}^k \frac{1}{n}$ is getting closer to $ln(k)+\gamma$ where $\gamma\approx 0.577215664901532\ldots$ is the Euler-Mascheroni constant.
#' Please use what you have learned in this question to write a program to approximate $\gamma$ with precision $\epsilon=10^{-12}$, and within $10^10$ iterations.
iter.max = 10^10
eps = 1e-12
sum.harmonic.old = 0 
change.harmonic = 1000 
iter = 0
while(change.harmonic>eps&iter<iter.max){
  sum.harmonic = sum.harmonic.old + sum(1/(iter+c(1:10^6)))
  change.harmonic= abs(sum.harmonic-sum.harmonic.old) 
  iter=iter+10^6 
  sum.harmonic.old = sum.harmonic
}

#' The following code report the results.
sum.harmonic - log(iter)
#'
#'  3. (5 points) Consider a class of truncated normal distribution. That is, the shape of their pdfs look like those for normal, but their supported is limited to an interval $[a,b]$.
#' The pdf of a truncated normal distribution on interval $(a,b)$ is $\frac{\phi(x;\mu,\sigma)}{\Phi(b;\mu,\sigma)-\Phi(a;\mu,\sigma)}$
#' where $\phi(x;\mu,\sigma)$ and $\Phi(x;\mu,\sigma)$ are the pdf and cdf of the un-truncated normal $N(\mu,\sigma)$.
#' Now consider $X\sim Truncated-Normal (\mu=3,\sigma=1,a=0,b=\infty)$ please use R function integrate() to calculate the following
#' (a) $P(X\leq 2)$
#' (b) $E(X)$
#' (c) $Var(X)$
#'
#' Are they still the same as for the un-truncated version? Do you expect what you see here?
truncated.moments.df = function(x, mean, std, a, b, p) {
  ifelse(x >= 0,x^p*dnorm(x, mean = mean, sd = std)/(pnorm(b, mean = mean, sd = std) - pnorm(a, mean = mean, sd = std)),0)
}
integrate(truncated.moments.df, 0, 2, mean = 3, std = 1, a = 0, b = Inf, p = 0)
(EX = integrate(truncated.moments.df, 0, Inf, mean = 3, std = 1, a = 0, b = Inf, p = 1))
EX.2 = integrate(truncated.moments.df, 0, Inf, mean = 3, std = 1, a = 0, b = Inf, p = 2)
EX.2$value - (EX$value)^2
#' There are little differences on mean and variance in comparison with what 
#' we expect. Since now the normal is truncated at 0, as it is showed in the following
#' plot, it is not surprise.
plot(seq(-1,10,.01),truncated.moments.df(seq(-1,10,.01), mean = 3, std = 1, a = 0, b = Inf, p = 0),type = "l",xlab = "x", ylab = "Density", main = "The Truncated Normal")
abline(h = 0, col = "blue")
#'
#'  4. (5 points)Implement the Newton's method to find a root of $x^3-14x^2+68x-115=0$ starting at $x=9$.The convergence parameter $\epsilon=10^-{12}$.
#' Be sure to plot the function on an appropriate range. The only solution is $x=5$
f.cubic = function(x) x^3-14*x^2+68*x-115
f.cubic.gradient = function(x) 3*x^2-28*x+68
plot(seq(2,8,.01),f.cubic(seq(2,8,.01)),type = "l",xlab = "x",ylab = "y")
abline(h = 0, col = "red")
eps = 1e-9 #Attention! eps cannot be set too small. If we use 1e-12 as indicated
           #by the question. Later in question5, the denominator f(xk) - f(xk1) 
           #in secant method will generate NaN to stop the process. 
iter.max = 200
update.newton <- function(xk,f,f.gradient){
  x.next = xk-f(xk)/f.gradient(xk)
  return(x.next)
}

iter.Q4=0
err.x.Q4=1
x.init.Q4=10
while(err.x.Q4>eps&iter.Q4<iter.max){
  x.new.Q4 = update.newton(xk=x.init.Q4, f=f.cubic,f.gradient = f.cubic.gradient)
  err.x.Q4 = abs(x.new.Q4-x.init.Q4)
  iter.Q4=iter.Q4+1
  x.init.Q4=x.new.Q4
}

#' The following code report the results.
if(iter.Q4==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x.Q4<eps) print(paste0('The algorithm converges at x= ',x.new.Q4,' at iteration ',iter.Q4,'.'))
#'
#' 5.(5 points) Please re-do the previous question using secant method.
#' Note that it needs two initial points to calculate the initial slope. For the previous quesiton, let say $x_0^{(0)}=10$ and $x_0^{(1)}=9$.
#' Does it use more or less iterations than Newton's method?
update.secant <- function(xk,xk1,f){
  x.next = xk-f(xk)*(xk-xk1)/(f(xk)-f(xk1))
  return(x.next)
}

iter.Q5=0
err.x.Q5=1
x.init0.Q5=9
x.init1.Q5=10

while(err.x.Q5>eps&iter.Q5<iter.max){
  x.new.Q5 = update.secant(xk=x.init0.Q5,xk1=x.init1.Q5, f=f.cubic)
  err.x.Q5 = abs(x.new.Q5-x.init1.Q5)
  iter.Q5=iter.Q5+1
  x.init1.Q5=x.init0.Q5
  x.init0.Q5=x.new.Q5
}
#' The following code report the results.
if(iter.Q5==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x.Q5<eps) print(paste0('The algorithm converges at x= ',x.new.Q5,' at iteration ',iter.Q5,'.'))
#' It uses more iterations than Newton's Method, since the derivative is approximated.