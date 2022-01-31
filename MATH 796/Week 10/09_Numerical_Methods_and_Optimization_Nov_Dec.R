#' ---
#' title: "Numerical Methods and Optimization"
#' author: "Qi Zhang"
#' date: "August 01, 2021"
#' ---
#'
#' ##
#'
#' ## Computer representation of real numbers and its impact on statistical programming.
#'
#' A positive decimal number $x$ is represented by the ordered coefficients $d_j$'s in the series
#' \[
#' d_n 10^n + d_{n-1} 10^{n-1}+\ldots+ d_1 10^1 + d_0 + d_{-1} 10^{-1} + d_{-2} 10^{-2} +\ldots
#' \]
#'
#' and decimal point separating $d_0$ and $d_{-1}$, where $d_j$ are integers in $\{0, 1,2,3,4,5,6,7,8, 9\}$.
#' The same number can be represented in base 2 using the binary digits $\{0, 1\}$ by $a_k,a_{k-1},\ldots, a_1,a_0,a_{-1},a_{-2},\ldots$, where
#' \[
#' x = a_k 2^k + a_{k-1} 2^{k-1} +\ldots + a_1 2 + a_0 + a_{-1}2^{-1} + a_{-2} 2^{-2} +\ldots ,
#' \]
#' $a_j \in \{0, 1\}$. The point separating $a_0$ and $a_{-1}$ is called the radix point.
#' Similarly, $x$ can be represented in any integer base $b>1$ by expanding in powers of $b$.
#'
#' Since no computer has infinite storage or memory space, a real number is stored as a finite sequence of integers instead of an infinite sequence.
#' So a computer representation of a real number is approximate, not exact.
#' Similarly, mathematical ideas such as limit, supremum, infimum, etc. cannot be exactly reproduced in the computer, neither.
#' Another source of discrepancy comes from the conversion "from" and "to" decimal, because base 2 representation is more natural to computer, and base 10 (decimal) is more natural to human.
#'
#' The following is an example of such discrepancy in mathematical identities as you have seen before.
(0.3-0.1)
(0.3-0.1)==0.2
0.2-(0.3-0.1)

#' One fix of such issue is to use R function "all.equal", which examine the "near identity"

all.equal(0.2,0.3-0.1)

all.equal(0.2,0.3)
isTRUE(all.equal(0.2,0.3))

isTRUE(all.equal(0.2,0.3-0.1))


#' Another commonly seen issue is when evaluating the ratio of two very large or very small numbers. For example, if you want to calculate the likelihood ratio of two models for your observed data.
#' Recall that the likelihood for observed iid data is just the product of the probability of observing each. It may have a very large or very small value, especially when the sample size $n$ is large.
#'
#' Consider the following exmaple. We simulate 1000 observations from $Poisson(\lambda=2)$, and want to determine whether $\lambda=2$ or $\lambda=3$ is a better fit for the data based on the ratio of the likelihood of the observed data assuming each model.
#' If the ratio $>1$, $\lambda=2$ is better. Obviously we expect the ratio $>1$ because $Poisson(\lambda=2)$ is the model that the data is generated from.

set.seed(100)
x.pois <- rpois(1000,lambda = 2)
d2.x = dpois(x.pois,lambda = 2)
lklhd2 = prod(d2.x)
d3.x = dpois(x.pois,lambda = 3)
lklhd3 = prod(d3.x)

lklhd2/lklhd3

#' The reason why this ratio is NaN is because both of the numerator and denominator are 0.
#'
lklhd2
lklhd3
summary(d2.x)
summary(d3.x)
#' Of course they are not exactly 0. But each of them is the product of 1000 small numbers.
#' The products are so small that the computer treat them as 0s.
#'
#' One way to avoid this is to computer the ratio on the log scale. Recall that for $a,b>0$ $a/b=e^{log(a)-log(b)}$ and $log(a\cdot b)=log(a)+log(b)$.
#' The reason why this may work is that while $e^{-10000}$ is very close to 0, its log is not. While $e^{10000}$ is treated as infinity by the computer, its log is not.
#'
ld2.x = dpois(x.pois,lambda = 2,log = T)
ld3.x = dpois(x.pois,lambda = 3,log = T)
log.lklhd2 = sum(ld2.x)
log.lklhd3 = sum(ld3.x)

log.lklhd2
log.lklhd3

log.lklhd2-log.lklhd3
#' Since the difference in log-likelihood (log of likelihood ratio) is $>0$, we can already conclude which model is better without calculating the actual likelihood ratio.
#' But if we insist to have this number, it is the following.
exp(log.lklhd2-log.lklhd3)
#' You can see that if the log likelihood ratio is a very large positive or negative number (say,$>10000$ or $<-10000$).It is still possible that the computer return 0 or Inf.
#' For this reason, we rarely calculate likelihood or likelihood ratio when working on computer, and often work in the log scale. You may have seen it in regression class or mathematical statistics class, the test statistics for the likelihood ratio test is usually the difference in log-likelihood ratio, times 2. If the two models are nested, it has a chi square distribution.
#'
#'
#' Computer does not have infinite memory, any mathematical expressions that involves infinite terms cannot be evaluated directly and exactly on a computer.
#' For example, you should have seen the sum of the following infinite series
#' \[
#' \sum_{i=0}^\infty a^i = \frac{1}{1-a}
#' \]
#' for $0<a<1$. This is exact in math.
#' When computer is asked to evaluate $\sum_{i=0}^\infty a^i$, it does not know the above exact equality, and cannot evaluate this sum exactly.
#' Instead, it evaluate this sum by approximating it with $\sum_{i=0}^n a^i$ for some large number $n$. The problem is how does it know the $n$ is large enough?
#'
#' This brings in the concept of convergence. Suppose we are interested in approximating some unknown objective $x_0$ by a sequence of $x_1,x_2,\ldots$ with the hope that $\lim_{n\rightarrow \infty}x_n= x_0$.
#' Since we cannot run the sequence forever, we need a large enough $n$, such that $x_n$ is a good approximation of $x_0$ based on some measure $\Delta$.
#' That is, for a pre-specified very small positive number $\epsilon$, if $\Delta(x_0,x_{n})\leq \epsilon$, we can stop the sequence at step $n$,
#' and report $x_n$ as a good-enough approximation of $x_0$. In practice, however, we do not know the true value of $x_0$.
#' So we stop the sequence when the update in the sequence is small enough, that it, we stop the sequence at step $n$ if $\Delta(x_n,x_{n-1})\leq \epsilon$.
#' If this happens, we say that the sequence converges based on the convergence criterion (or stopping criterion) $\Delta$.
#' In practice, if the number of iterations $n$ exceeds a prespecified value before the convergence criterion is satisfied,
#' we also stop the sequence, and say that the sequence does not converge. It does not mean it will never converge with a high limit of maximal iterations.
#'
#'
#' Example: evaluate $\sum_{i=0}^\infty a^i$ with $a=0.5$. We already know its exact value is $1/(1-0.5)=2$.
#' Let the computer do the computation with $\epsilon=10^{-10}$ and allow at most 1000 iterations.
#' What kind of loop is best suited for the job? a while loop.

iter.max = 1000
eps = 1e-20
sum.geom.old = 0 # initial value of the summation
change.geom = 1000 # initial value of the update in the sum, what happens if it is initialized as 0?
iter = 0 # initialize the iteration
while(change.geom>eps&iter<iter.max){
  sum.geom = sum.geom.old + 0.5^iter # update the sum
  change.geom= abs(sum.geom-sum.geom.old) # calculate the stopping criterion
  iter=iter+1 # update the iteration
  sum.geom.old = sum.geom # update the baseline
}

#' The following code report the results.
if(iter==iter.max)  print(paste0('The sequence does not converge in ',iter.max,' iterations!'))
if(change.geom<eps) print(paste0('The seuence converges at value ',sum.geom,' at iteration ',iter,'.'))

#' The above code is how it is done in languages like C. As we have discussed before, R is not friendly to explicit looping.
#' It is more efficient to vectorize the operation with a prespecified vector length. In the case of evaluating infinite series,
#' we do not know the this length. However, we can still partially vectorize the operation by adding chunks of 10 or 100 terms instead of one in each iteration of the while loop.
#' You can choose how many terms to add. For the series that you expect to run longer, you may want to use large chunk size.
iter.max = 1000
eps = 1e-20
sum.geom.old = 0 # initial value of the summation
change.geom = 1000 # initial value of the update in the sum, what happens if it is initialized as 0?
iter = 0 # initialize the iteration
while(change.geom>eps&iter<iter.max){
  sum.geom = sum.geom.old + sum(0.5^(iter+(0:9))) # update the sum by adding 10 terms
  change.geom= abs(sum.geom-sum.geom.old) # calculate the stopping criterion
  iter=iter+10 # update the iteration. Since we add 10 terms, iter should progress by 10.
  sum.geom.old = sum.geom # update the baseline
}

#' The following code report the results.
if(iter==iter.max)  print(paste0('The sequence does not converge in ',iter.max,' iterations!'))
if(change.geom<eps) print(paste0('The seuence converges at value ',sum.geom,' at iteration ',iter,'.'))


#'
#'
#' #### Practice:
#'
#' A p-series is defined as $\sum_{n=0}^\infty \frac{1}{n^p}$, and converges if $p>1$. Leonhard Euler has showed that when $p=2$, it converges to $\pi^2/6$.
#' Please evaluate the p-series with $p=2$ within 10^10 iterations with precision $\epsilon = 10^{-12}$.
#' You may want to consider a large chunk size, do you know why?
#' Then please compare your output with the exact value $\pi^2/6$.
#'





#'
#' ## Numerical Differentiation and Integration
#'
#' Once upon a time in calculus: for a differentiable function $f(x)$, its derivative at point $x$ is defined as
#' \[
#' f'(x) = \lim_{h\rightarrow 0} \frac{f(x+h)-f(x)}{h}
#' \]
#'
knitr::include_graphics("D:\\Dropbox\\courses\\stat_computing\\Lecture notes\\derivative.jpg")
#'
#' A numeric approximation of it is just $\frac{f(x+h)-f(x)}{h}$ for some very small positive value of $h$.
#' Another popular approach to numerical evaluation, though, is the following central difference formula
#' \[
#' f'(x) \approx \frac{f(x+h)-f(x-h)}{2h}
#' \]
#' for some small positive value of h.
#'
knitr::include_graphics("D:\\Dropbox\\courses\\stat_computing\\Lecture notes\\derivative_central.jpg")
#'
#'
#'
#' Once upon a time in calculus: for an integratable function $f(x)$, its intergral over interval $[a,b]$ is defined as
#' \[
#' \int_a^b f(x)dx = \lim_{mesh\rightarrow 0} \sum_{i=1}^n f(c_i)\Delta_i
#' \]
#' where $a=x_0<x_1,\ldots<x_n=b$, $c_i\in [x_{i-1},x_{i}]$, $\Delta_i=x_i-x_{i-1}$, and $mesh=\max_i \Delta_i$.
#'
knitr::include_graphics("D:\\Dropbox\\courses\\stat_computing\\Lecture notes\\integration.gif")
#'
#' To approximate it numerically, one just need to find the grid $a=x_0<x_1,\ldots<x_n=b$ so that the maximal sub-interval size is small.
#' One naive solution is to place equal spaced nodes, and select $c_i$ such that $f(c_i)=\frac{1}{2}(f(x_{i-1})+f(x_i))$. This leads to the following
#' \[
#' \int_a^b f(x)dx \approx \frac{h}{2}f(a)+  h\sum_{i=1}^{n-1} f(x_i) + \frac{h}{2}f(b)
#' \]
#' where $h=(b-a)/n$.
#'
#' Of course the grid does not need to be equally spaced, and there is flexibility in selecting $c_i$'s, or how to approximate $f(x)$ locally within each sub-interval in general.
#' This is why there are other approximation methods.
#'
#' We will not go deep in this topic. Instead, you just need to know that R function "integrate" does it.
#' Please read the help page of R function "integrate" now.
#'
#' Example: Compute $\int_0^\infty \frac{dy}{(cosh y-\rho r)^{n-1}}$ where $-1<\rho<1$, $-1<r<1$ are constants, and $n\geq 2$ is an integer.
#' We apply adaptive quadrature implemented by the integrate function provided in R.
#' First write a function that returns the value of the integrand. This function should take as its first argument a vector containing the nodes, and return a vector of the same length.
#' Additional arguments can also be supplied. This function or the name of this function is the first argument to "integrate".
#' Let us just integrate it with $r=0.5$, $\rho=0.2$, $n=10$.

f.example <- function(y,n,r,rho){
  return((cosh(y)-rho*r)^(1-n))
}

integrate(f.example,lower=0,upper=Inf,n=10,r=0.5,rho=0.2)

#' To see how the result depends on $\rho$, fix $n = 10$ and $r =0.5$. Then integrate and plot the value of the integral for different values of $\rho$.

rho.vec = 0.01*(-99:99)

intf.vec = sapply(rho.vec, function(rho) integrate(f.example,lower=0,upper=Inf,n=10,r=0.5,rho=rho)$value)

plot(rho.vec,intf.vec,type='l',xlab=expression(rho),ylab = "Integral Value (n=10,rho=0.5)")

#'
#'
#' #### Practice:
#'
#' Integration is important in statistics because probabilities for continuous variables are integrals, expectations and other moments for continuous variables are also integrals.
#' Let $X\sim N(3,1)$, please use R function integrate, but not dnorm, pnorm or rnorm to calculate the following
#' (a) $P(X\leq 2)$
#' (b) $E(X)$
#' (c) $Var(X)$
#'


#'
#'
#' ## Root-finding methods
#'
#' Let f(x) be a continuous function $f: R^ \rightarrow R^1$. A root (or zero) of the equation $f(x) = c$ is a number $x$ such that $g(x)=f(x)-c= 0$.
#'  Thus, we can restrict attention to solving $f(x) = 0$.
#'  One can choose from numerical methods that require evaluation of the first derivative of $f(x)$, and algorithms that do not require the first derivative.
#'  Newtons's method or Newton-Raphson method are examples of the first type, while Bisection method or Fixed point Method are examples of the second type of method.
#'  In either case, one must bracket the root between two endpoints where $f$ has opposite signs.
#'
#'
#' #### Fixed-Point Method
#'
#' A general type of iteration for root finding problems is called a fixed-point method, or fixed-point iteration.
#' Suppose $x_n$ is the current iteration, a fixed-point method find a function $g$, and update the root by $x^{(n+1)}=g(x^{(n)})$.
#'
#' If $x_0$ is a true solution of $f(x)=0$, then $f(x_0)=0$. Consequently, there is
#' \[
#' x_0 = f(x_0)+x_0
#' \]
#' Using this fact, the fixed-point iteration is then
#' \[
#' x_0^{(n+1)} = f(x_0^{(n)})+x_0^{(n)}
#' \]
#' This approach can be further accelerated using Aitken's $\Delta^2-$extrapolation, and the iteration formula (Steffensen's Fixed-Point Method) becomes
#' \[
#' x_0^{(n+1)} = x_0^{(n)} - \frac{f^2(x_0^{(n)})}{f(x_0^{(n)}+f(x_0^{(n)}))-f(x_0^{(n)})}
#' \]
#'
#' Example: Starting at $x=1$, find the solution of $x-log(|x|+1)-1=0$ using Steffensen's fixed point method
#'
#' Let us first define the function to be evaluated, and plot it to make sure that there is a solution.
f.example = function(x) x-log(abs(x)+1)-1
curve(f.example, from=-1,to=3)
abline(h=0,lty=2)
#' It appears that there is a solution around $x=4$.
#' Now let us determine $\epsilon$, the tolerance parameter for convergence, and the maximal number of iterations.
eps = 1e-10
iter.max = 200

#' Sometimes, it may be convenient to define the update as a function.
update.stef <- function(xk,f){
  fxk = f(xk)
  x.next = xk-fxk^2/(f(xk+fxk)-fxk)
  return(x.next)
}

iter=0
err.x=1
x.init =1
while(err.x>eps&iter<iter.max){
  x.new = update.stef(xk=x.init, f=f.example)
  err.x = abs(x.new-x.init)
  iter=iter+1
  x.init=x.new
}

#' The following code report the results.
if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= ',x.new,' at iteration ',iter,'.'))


#'
#'
#' #### Practice
#'
#'Starting at $x=5$, find a solution of $x^3-10x-10=0$ using Steffensen's fixed point method.
#' Be sure to plot the function before implementing the algorithm.
#'

#'
#'  The equation has more than one solution, and you have found one when starting at $x=5$. How can you find the other solutions?
#' If you get error message, please try to figure out why it happens by printing and interpreting the intermediate results.
#'



#'
#'
#' #### Newton's Method, univariate and multivariate
#'
#' Newton's method can be viewed as a fixed-point method that takes advantage of the first-order Taylor series of the function about a point near the solution.
#' Let $x^{(n)}_0$ be the current estimate of the root of $f(x)=0$. Then the first-order Taylor series is
#' \[
#' f(x) \approx f(x_0^{(n)}) + (x-x_0^{(n)})f'(x_0^{(n)})
#' \]
#'The update is obtained by solving this Taylor series approximation.
#' \[
#' f(x_0^{(n+1)}) \approx f(x_0^{(n)}) + (x_0^{(n+1)}-x_0^{(n)})f'(x_0^{(n)})
#' \]
#' Since we are trying to solve $f(x)=0$ and we expect $x_0^{(n+1)}$ is a closer to the root than $x_0^{(n)}$, it is reasonable to assume $f(x_0^{(n+1)})=0$.
#' If additionally $f'(x_0^{(n)})\neq 0$, the iteration based Newton's Method is
#' \[
#' x_0^{(n+1)} = x_0^{(n)} -\frac{f(x_0^{(n)})}{f'(x_0^{(n)})}
#' \]
#'
#'
#' Let us reconsider the previous example, starting at $x=1$, find the solution of $x-log(|x|+1)-1=0$ using Newton's method
#' The derivative of $x-log(|x|+1)-1$ is $1-\frac{sign(x)}{|x|+1}$. Note that this function is not differentiable at $x=0$.
#'
#'
#' Let us first define the function to be evaluated and the derivative.
f.example = function(x) x-log(abs(x)+1)-1
f.gradient.example = function(x) 1-sign(x)/(abs(x)+1)
#' Now let us determine $\epsilon$, the tolerance parameter for convergence, and the maximal number of iterations.
eps = 1e-10
iter.max = 200

#' Now define the update as a function.
update.newton <- function(xk,f,f.gradient){
  x.next = xk-f(xk)/f.gradient(xk)
  return(x.next)
}

iter=0
err.x=1
x.init =1
while(err.x>eps&iter<iter.max){
  x.new = update.newton(xk=x.init, f=f.example,f.gradient = f.gradient.example)
  err.x = abs(x.new-x.init)
  iter=iter+1
  x.init=x.new
}

#' The following code report the results.
if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= ',x.new,' at iteration ',iter,'.'))


#'
#' #### Practice:
#'
#'  Again, let us reconsider the practice problem of find a solution of $x^3-10x-10=0$ starting at $x=5$.
#' Let us use Newton's method.
#'

#' Note that the Newton's method usually uses less iterations than fixed point method.
#' Now please find the other two roots using the same initial values for them as you did in the previous question.

#'
#'
#'
#' Fixed-point methods can be generalized to solving a system of equation with multiple variables. We will just present an example of Newton's Method.
#' Suppose $f=(f_1,\ldots,f_n)$ is a differentiable vector-value function of $x=(x_1,\ldots,x_m)$, and we want to solve the system $f=0$.
#' The Jacobian for the system is
#' \[
#' J_f = \left(
#' \begin{array}{cccc}
#' \frac{\partial f_1}{\partial x_1} & \frac{\partial f_1}{\partial x_2} & \cdots & \frac{\partial f_1}{\partial x_m}\\
#' \frac{\partial f_2}{\partial x_1} & \frac{\partial f_2}{\partial x_2} & \cdots & \frac{\partial f_2}{\partial x_m}\\
#'  & & \vdots & \\
#' \frac{\partial f_n}{\partial x_1} & \frac{\partial f_n}{\partial x_2} & \cdots & \frac{\partial f_n}{\partial x_m}\\
#' \end{array}
#' \right)
#' \]
#'
#' If $x_0^{(k)}\in R^m$ is the current estimate of the root of $f(x)=0$, the first order Taylor series about this point is
#' \[
#' f(x)\approx f(x_0^{(k)}) + J_f(x_0^{(k)})(x-x_0^{(k)})
#' \]
#' Under the assumption of $f(x_0^{(k+1)})=0$, $x_0^{(k+1)}$ is the solution of the linear system
#' \[
#' J_f(x_0^{(k)})(x_0^{(k+1)}-x_0^{(k)}) = - f(x_0^{(k)})
#' \]
#' Generally $m\neq n$. But if $m=n$ and matrix $J_f(x_0^{(k)})$ is invertable, the solution can be written as the following simple form
#' \[
#' x_0^{(k+1)}=x_0^{(k)}-(J_f(x_0^{(k)}))^{-1}f(x_0^{(k)})
#' \]
#'
#' In practice, inverting such a matrix in each iteration is not ideal, methods have been proposed to approximate it, with the hope to reduce computation.
#'
#'
#'

#' #### Root finding methods without evaluating gradients.
#'
#' The Secant method for root finding is similar to Newton's method in using the slope to determine successive points in the iteration.
#' The difference is that while Newton method uses the derivative $f'(x^{(k)})$ as the slope, secant method uses $\frac{f(x^{(k)})-f(x^{(k-1)})}{x^{(k)}-x^{(k-1)}}$ to approximate it. In this sense, it is a discrete approximation of Newton method.
#' The update for Secant method is
#' \[
#' x_0^{(n+1)} = x_0^{(n)} -\frac{f(x_0^{(n)})(x^{(n)}-x^{(n-1)})}{f(x^{(n)})-f(x^{(n-1)})}
#' \]
#' Now let us try secant method on the example that we have been working on.
#' Note that it needs two initial points to calculate the initial slope, while Newton method only need one (because Newton method evaluates the derivative).

f.example = function(x) x-log(abs(x)+1)-1
#' Now let us use the same $\epsilon$,and the maximal number of iterations.
eps = 1e-10
iter.max = 200

#' Now define the update as a function.

update.secant <- function(xk,xk1,f){
  x.next = xk-f(xk)*(xk-xk1)/(f(xk)-f(xk1))
  return(x.next)
}

iter=0
err.x=1
x.init0 =1
x.init1 =1.1

while(err.x>eps&iter<iter.max){
  x.new = update.secant(xk=x.init0,xk1=x.init1, f=f.example)
  err.x = abs(x.new-x.init1)
  iter=iter+1
  x.init1=x.init0
  x.init0=x.new
}

#' The following code report the results.
if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= ',x.new,' at iteration ',iter,'.'))

#' Does it use more or less iterations than Newton's method?
#'
#' The multivariate generalization of Secant method is called Broyden's method, which belongs to a general class of quasi-Newton methods with the hope of improving Newton's method.
#'


#'
#' Another derivative-free root finding method that is not a fixed-point method is bisection. Its idea is very simple.
#' Suppose we start with an interval $(a_0,b_0)$, if $f(a_0)$ and $f(b_0)$ have different sign, then there must be a root in this interval.
#' Let $c_0 = (a_0+b_0)/2$ be the middle point of this interval, we compute $f(c_0)$, if it has different sign with $f(a_0)$, there must be a root between $(a_0,c_0)$.
#' Otherwise, it must have different sign with $f(b_0)$, and there must be a root between $(c_0,b_0)$. Define this new interval as $(a_1,b_1)$, and repeat the above process.
#' The algorithm stops when the size of the interval is small enough, that is, the interval is literally just one point. After convergence, the middle point of this interval is reported as the final answer.
#' Now let us try this method on the previous example with initial interval $(1,3)$. Does Bisection use more or less iterations than Secant or Newton's method?
#'

f.example = function(x) x-log(abs(x)+1)-1
#' Now let us use the same $\epsilon$,and the maximal number of iterations.
eps = 1e-10
iter.max = 200

update.bisection <- function(interval,f){
  f.l = f(interval[1])
  f.u = f(interval[2])
  if(f.l*f.u<0){
    c0 = mean(interval)
    f.c = f(c0)
    if(f.l*f.c<0){
      interval.next = c(interval[1],c0)
    }else{
      interval.next = c(c0,interval[2])
    }
  }else{
    stop('There may not be a root in the initial interval!')
  }
  return(interval.next)
}

iter=0
err.x=1
interval.init =c(1,3)
while(err.x>eps&iter<iter.max){
  interval.new = update.bisection(interval=interval.init, f=f.example)
  err.x = abs(diff(interval.new))
  iter=iter+1
  interval.init=interval.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= ',mean(interval.new),' at iteration ',iter,'.'))

#' #### Practice:
#'
#'  Again, let us reconsider the practice problem of find a solution of $x^3-10x-10=0$ starting at $x=5$.
#' Let us use Secant method, and Bisection method with appropriate initial points/intervals based your plot of the function.
f.example = function(x) x^3-10*x-10
eps = 1e-10
iter.max = 200

#' Now define the update as a function.

update.secant <- function(xk,xk1,f){
  x.next = xk-f(xk)*(xk-xk1)/(f(xk)-f(xk1))
  return(x.next)
}

iter=0
err.x=1
x.init0 =1
x.init1 =1.1

while(err.x>eps&iter<iter.max){
  x.new = update.secant(xk=x.init0,xk1=x.init1, f=f.example)
  err.x = abs(x.new-x.init1)
  iter=iter+1
  x.init1=x.init0
  x.init0=x.new
}

#' The following code report the results.
if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= ',x.new,' at iteration ',iter,'.'))
plot(seq(-5,5,.01),f.example(seq(-5,5,.01)),type = "l")
abline(h = 0, col = "red")
abline(v = x.init0, col = "blue")

update.bisection <- function(interval,f){
  f.l = f(interval[1])
  f.u = f(interval[2])
  if(f.l*f.u<0){
    c0 = mean(interval)
    f.c = f(c0)
    if(f.l*f.c<0){
      interval.next = c(interval[1],c0)
    }else{
      interval.next = c(c0,interval[2])
    }
  }else{
    stop('There may not be a root in the initial interval!')
  }
  return(interval.next)
}

iter=0
err.x=1
interval.init =c(-2,0)
while(err.x>eps&iter<iter.max){
  interval.new = update.bisection(interval=interval.init, f=f.example)
  err.x = abs(diff(interval.new))
  iter=iter+1
  interval.init=interval.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= ',mean(interval.new),' at iteration ',iter,'.'))
abline(v = mean(interval.new), col = "purple")
#'
#'
#' ## Optimization
#'
#' Optimization is extremely important in statistics.
#' For example, maximum Likelihood Estimation (MLE) is the most essential method for model estimation, which maximizes the likelihood function.
#' Least square estimation is one of the most popular approtion for estimating regression models, which minimizes the sum of squares of the regression errors.
#' Most model fitting procedure involves minimizing some sort of loss function.
#'
#' Since maximizing a function $g(x)$ is equivalent to minimizing $-g(x)$, we focus on minimization without losing generality.
#'
#' Many R functions does general optimization, either constrained or unconstrained, including but not limited to "optim","optimize", "constrOptim", "nlm", "mle".
#' I usually only use "optim" because I have found it to be the most flexible one, and offers various algorithms for both constrained and unconstrained problems.
#' I usually use Nelder-Mead algorithm that does not evaluate the derivatives, or one of the Newton or quasi-Newton method.
#' There are also specialized programs for specific types of problems such as linear or quadratic problems, which we will not cover in this course.
#'
#' In this lecture, I will focus on the descent methods, for which Newton's method is a special case.
#' I will also give you a simple illustration of the heuristic of Nelder-Mead algorithm.
#'
#' Consider a general minimization problem for a scaler valued function $f$ defined on $D\subset R^m$.
#' We denote a solution as $x_\star$
#' \[
#' x_\star = arg min_{x\in D} f(x)
#' \]
#' We assume this function is twice differentiable in all variables.
#'
#' The local shape information of this function is provided by the vector of partial derivatives, which is called the gradient defined as below
#' \[
#' \nabla f(x) = (\frac{\partial f(x)}{\partial x_1},\frac{\partial f(x)}{\partial x_2},\ldots,\frac{\partial f(x)}{\partial x_m})
#' \]
#' For a scaler function $f$ that is twice differentiable, more information about the stationary point can be obtained from the second derivatives, which are organized into a matrix called the Hessian
#' \[
#' H_f(x) = \nabla (\nabla f(x)) = \left( \frac{\partial^2 f(x)}{\partial x_i \partial x_j} \right)_{i,j=1}^m = \frac{\partial^2 f(x)}{\partial x \partial x^T}
#' \]
#' Sometimes the Hessian matrix is denoted as $\nabla^2 f(x)$ in the literature. But more ofthem $\nabla^2 f(x)$ denotes the diagonal of the hessian matrix. That is $diag(\frac{\partial^2 f(x)}{\partial x_1^2},\frac{\partial^2 f(x)}{\partial x_2^2},\ldots,\frac{\partial^2 f(x)}{\partial x_m^2})$
#'
#'
#' Convex function: a scalar function $f(x)$ is convex if for any $x,y\in D$ and $t\in[0,1]$, there is $f(tx+(1-t)y)\leq tf(x)+(1-t)f(y)$. The following are some examples of univariate convex and nonconvex functions.
#' Concave function: a scalar function $f(x)$ is concave if $-f(x)$ is convex.

#'
knitr::include_graphics("D:\\Dropbox\\courses\\stat_computing\\Lecture notes\\convex.jpg")
#'
#'
#' Convexity and gradient/Hessian:
#' If $f(x)$ is a univariate scalar differentiable function, and if it is convex, then $f'(x)$ is monotone non-decreasing. Consequently, if it is twice differentiable, $f^"(x)\geq 0$.
#'
#' If $f(x)$ is a multivariate scalar differentiable function, and if it is convex, then $\nabla f(x)$ is monotone nondecreasing in each of its coordinates.
#' If it is twice differentiable, $H_f(x)$ is positive semidefinite (can you still recall what it means in linear algebra?).
#'
#' The reverse is also true for functions that are twice differentiable: that is, if the $H_f(x)$ is positive semidefinite, then $f(x)$ is convex.
#' Note that some convex functions are not (twice-differentiable), so their convexity cannot be determined using Hessian.
#'
#' Convex and concave functions are important in optimization, both in practice and education. For example, if a function is convex and if it has an interior stationary point, it is the unique solution of the minimization problem.
#' There are also special technics on optimization for convex (concave) functions that can run a full course. We will not get into it in this course.
#'
#' Solution of an optimization problem is usually an iterative process, moving from one point on the function to another. The basic things to determine are
#'
#' (1) direction or path $p$, in which to step next
#' (2) how far to step. (The step length is $\alpha\parallel p \parallel$, for the scalar $\alpha>0$)
#'
#' To summarize, the iteration is
#' \[
#' x^{(k+1)} = x^{(k)} + \alpha^{(k)} p^{(k)}
#' \]
#'
#' For differentiable objectives, its derivative at the optimum is 0. So generally speaking, you can apply any root finding methods to the derivative of the objective to find potential optimums. You may get stuck at a stationary point with zero derivative, but not an optimum. Nevertheless, the following optimization methods for differentiable objectives all use similar ideas that you have seen in root finding..
#'
#' #### Descent methods
#'
#' For a differentiable function to be minimized, from any given point, an obvious direction to move is the negative gradient.
#'
#' For example, for univariate function $f(x)$, and suppose we are at $x^{(k)}$ now. If $f'(x^{(k)})>0$, we want to move to the negative direction,
#' and let the next point $x^{(k+1)}$ to be smaller than $x^{(k)}$. If $f'(x^{(k)})>0$, we want to move to the negative direction,
#' and let the next point $x^{(k+1)}$ to be larger than than $x^{(k)}$. So there is always $p f'(x)<0$.
#'
#' The same rationale holds for multivariate function, and we want to move to the direction $p$ such that $p^T\nabla f(x)<0$.
#' A direction $p$ that satisfies this is called a descent direction at point $x$.
#' To avoid possible numerical problems for quantities too close to zero, we often require $p^T\nabla f(x)<-\epsilon$ for some $\epsilon>0$.
#'
#' There are different ways to find "good" direction. One a "good" direction is found, there are also ways to find a "long" step such that the objective function is still decreasing.
#' While these heuristic principles are useful, we must be careful in applying them.
#' The optimization methods that use gradient to find the direction and the step size are all gradient methods for optimization. For them, the direction $p$ can be often expressed as
#' \[
#' R p = -\nabla f(x)
#' \]
#' for some positive definite matrix $R$ and when $\nabla f(x)\neq 0$. Different choices of $R$ leads to different search directions.
#'
#' Steepest descent/gradient descent)
#'
#' An obvious choice of $R$ is the identity matrix $I_m$, which leads to the steepest descent direction
#' \[
#' p^{(k)} =  -\nabla f(x^{(k)})
#' \]
#' and the update
#' \[
#' x^{(k+1)} = x^{(k)} -\alpha^{(k)}\nabla f(x^{(k)})
#' \]
#' where $\alpha^{(k)}$ is determined from some method such as the line search or some other methods. The step size based on Barzilai-Borwein method is presented below.
#' \[
#' \alpha^{(k)} = \frac{|(x^{(k)}-x^{(k-1)})^T(\nabla f(x^{(k)})-\nabla f(x^{(k-1)}))|}{\parallel \nabla f(x^{(k)})-\nabla f(x^{(k-1)})\parallel^2}
#' \]
#'

#' Example: Consider minimizing the quadratic function $f(x;a) = x_1^2+2ax_1x_2+x_2^2$ for $-1<a<1$. Obviously the minimum is at $(0,0)$ regardless the value of $a$.
#' Let us first consider $a=0$, and use the gradient descent method to search for the minimum starting at point $(5,1)$ with step size $\alpha=0.4$.
#' Its gradient is $\nabla f(x) = (2x_1+2ax_2,2ax_1+2x_2)$.
f.quad = function(x,a) x[1]^2+2*a*x[1]*x[2]+x[2]^2
f.grad.quad = function(x,a) c(2*x[1]+2*a*x[2],2*a*x[1]+2*x[2])

#' Let us plot the contour of this function first.
#'
options(warn=-1)
x = seq(-5,5,length.out = 100)
y = seq(-5,5,length.out = 100)
f.mat = sapply(x, function(xx) sapply(y, function(yy) f.quad(c(xx,yy),a=0)))
dim(f.mat)
contour(x=x,y=y,z=f.mat,nlevels = 10)

update.gd.bb <- function(xk,xk1,f.grad){
  alpha = abs(sum((xk-xk1)*(f.grad(xk)-f.grad(xk1))))/sum((f.grad(xk)-f.grad(xk1))^2)
  x.next = xk-alpha*f.grad(xk)
  return(x.next)
}

eps = 1e-12
iter.max = 100
iter=0
err.x=1
x.init1 =c(5,2)
x.init =c(5,1)
x.all = x.init
while(err.x>eps&iter<iter.max){
  x.new = update.gd.bb(xk=x.init,xk1=x.init1, f.grad=function(x) f.grad.quad(x,a=0))
  err.x = sqrt(sum((x.new-x.init)^2))
  x.all = rbind(x.all,x.new)
  iter=iter+1
  x.init1=x.init
  x.init=x.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= (',paste0(x.new,collapse = ','),') at iteration ',iter,'.'))

arrows(x0=x.all[-nrow(x.all),1],y0=x.all[-nrow(x.all),2],x1=x.all[-1,1],y1=x.all[-1,2],col='red')


#' Now let us consider the case with $a=-0.8$
#'
options(warn=-1)
x = seq(-5,5,length.out = 100)
y = seq(-5,5,length.out = 100)
f.mat = sapply(x, function(xx) sapply(y, function(yy) f.quad(c(xx,yy),a=-0.8)))
dim(f.mat)
contour(x=x,y=y,z=f.mat)


update.gd.bb <- function(xk,xk1,f.grad){
  alpha = abs(sum((xk-xk1)*(f.grad(xk)-f.grad(xk1))))/sum((f.grad(xk)-f.grad(xk1))^2)
  x.next = xk-alpha*f.grad(xk)
  return(x.next)
}



eps = 1e-12
iter.max = 100
iter=0
err.x=1
x.init1 =c(5,2)
x.init =c(5,1)
x.all = x.init
while(err.x>eps&iter<iter.max){
  x.new = update.gd.bb(xk=x.init,xk1=x.init1, f.grad=function(x) f.grad.quad(x,a=-0.8))
  err.x = sqrt(sum((x.new-x.init)^2))
  x.all = rbind(x.all,x.new)
  iter=iter+1
  x.init1=x.init
  x.init=x.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= (',paste0(x.new,collapse = ','),') at iteration ',iter,'.'))

arrows(x0=x.all[-nrow(x.all),1],y0=x.all[-nrow(x.all),2],x1=x.all[-1,1],y1=x.all[-1,2],col='red')


#'
#' The steepest descent method is robust as long as the gradient is not zero. But the direction change often, and zigzag to the minimum. So it can be slow.
#' It works the best if the contours of the objective are roughly circular, and may zigzag more when the contours are more elliptical.
#' For objectives that are more eliptical, An $R$ that is not identity may be better.
#'


#'
#' #### Newton's method
#'
#' Newton's method choose $R= (H_f(x^{(k)}))^{-1}$ and $\alpha^{(k)}=1$, and the iteration is
#' \[
#' x^{(k+1)} = x^{(k)} +p^{(k)}
#' \]
#' where $p^{(k)}$ is the solution of the linear system
#' \[
#' H_f(x^{(k)})p^{(k)}=-\nabla f(x^{(k)}
#' \]
#' When the Hessian is invertable, there is $p^{(k)}=-(H_f(x^{(k)}))^{-1}\nabla f(x^{(k)}$.
#'
#' Note that this is exactly the same method to find the root of $\nabla f(x)=0$, because the Hessian matrix is essentially the Jacobian of the gradients.
#'
#' Newton's method, by scaling the path by the Hessian, is more likely to point the path towards a local minimum than the steepest descent.
#' Newton's method is the most effective when the objective is close to a quadratic within a region close to a local minimum. In other cases, it may be unreliable.
#'

#' Let us consider the previous quadratic example. For the quadratic function $f(x;a) = x_1^2+2ax_1x_2+x_2^2$ for $-1<a<1$.
#' Its gradient is $\nabla f(x) = (2x_1+2ax_2,2ax_1+2x_2)$, and the Hessian is $H_f(x) = \left(\begin{array}{cc}2&2a\\2a&2\end{array}\right)$.
#' Let us first consider $a=0$, and use the gradient descent method to search for the minimum starting at point $(5,1)$.
f.quad = function(x,a) x[1]^2+2*a*x[1]*x[2]+x[2]^2
f.grad.quad = function(x,a) c(2*x[1]+2*a*x[2],2*a*x[1]+2*x[2])
f.hess.quad = function(x,a) matrix(c(2,2*a,2*a,2),2,2) # In this case, the Hessian is not a function of $x$, but a constant.

#' Let us plot the contour of this function first.
#'
options(warn=-1)
x = seq(-5,5,length.out = 100)
y = seq(-5,5,length.out = 100)
f.mat = sapply(x, function(xx) sapply(y, function(yy) f.quad(c(xx,yy),a=0)))
dim(f.mat)
contour(x=x,y=y,z=f.mat,nlevels = 10)


update.newton <- function(xk,f.grad,f.hess){
  x.next = xk-t(solve(f.hess(xk))%*%f.grad(xk))
  return(x.next)
}

eps = 1e-12
iter.max = 1000
iter=0
err.x=1
x.init =c(5,1)
x.all = x.init
while(err.x>eps&iter<iter.max){
  x.new = update.newton(xk=x.init, f.grad=function(x) f.grad.quad(x,a=0),f.hess=function(x) f.hess.quad(x,a=0))
  err.x = sqrt(sum((x.new-x.init)^2))
  x.all = rbind(x.all,x.new)
  iter=iter+1
  x.init=x.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= (',paste0(x.new,collapse = ','),') at iteration ',iter,'.'))

arrows(x0=x.all[-nrow(x.all),1],y0=x.all[-nrow(x.all),2],x1=x.all[-1,1],y1=x.all[-1,2],col='red')

#' Now let us consider the case with $a=-0.8$
#'
options(warn=-1)
x = seq(-5,5,length.out = 100)
y = seq(-5,5,length.out = 100)
f.mat = sapply(x, function(xx) sapply(y, function(yy) f.quad(c(xx,yy),a=-0.8)))
dim(f.mat)
contour(x=x,y=y,z=f.mat)

update.newton <- function(xk,f.grad,f.hess){
  x.next = xk-t(solve(f.hess(xk))%*%f.grad(xk))
  return(x.next)
}

eps = 1e-12
iter.max = 1000
iter=0
err.x=1
x.init =c(5,1)
x.all = x.init
while(err.x>eps&iter<iter.max){
  x.new = update.newton(xk=x.init, f.grad=function(x) f.grad.quad(x,a=-0.8),f.hess=function(x) f.hess.quad(x,a=-0.8))
  err.x = sqrt(sum((x.new-x.init)^2))
  x.all = rbind(x.all,x.new)
  iter=iter+1
  x.init=x.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= (',paste0(x.new,collapse = ','),') at iteration ',iter,'.'))

arrows(x0=x.all[-nrow(x.all),1],y0=x.all[-nrow(x.all),2],x1=x.all[-1,1],y1=x.all[-1,2],col='red')

#' In the case of $a=-0.8$, the contour is more elliptical, and the path of gradient descant zigzag to the center.
#' In contrast, Newton's method adjusts the direction and step size based on the hessian, and reach the center much quicker and more directly.
#'
#' #### Practice:
#'
#' Please implement the Steepest descent method and the Newton's method to find the minimum of the univariate function $f(x) = x(x-1)(x-2)(x-4)$, with starting point $x=4$
#'
f.quad = function(x) x*(x-1)*(x-2)*(x-4)
f.grad.quad = function(x) (2*x-1)*(x-2)*(x-4) + x*(x-1)*(2*x-6)
f.hess.quad = function(x) 2*(x-2)*(x-4) + (3*x-2)*(2*x-6) + x*(3*x-7)

update.gd.bb <- function(xk,xk1,f.grad){
  alpha = abs(sum((xk-xk1)*(f.grad(xk)-f.grad(xk1))))/sum((f.grad(xk)-f.grad(xk1))^2)
  x.next = xk-alpha*f.grad(xk)
  return(x.next)
}

eps = 1e-12
iter.max = 100
iter=0
err.x=1
x.init1 = 3.8
x.init = 4
x.all = x.init
while(err.x>eps&iter<iter.max){
  x.new = update.gd.bb(xk=x.init,xk1=x.init1, f.grad=function(x) f.grad.quad(x))
  err.x = sqrt(sum((x.new-x.init)^2))
  x.all = rbind(x.all,x.new)
  iter=iter+1
  x.init1=x.init
  x.init=x.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= (',paste0(x.new,collapse = ','),') at iteration ',iter,'.'))

update.newton <- function(xk,f.grad,f.hess){
  x.next = xk-f.grad(xk)/f.hess(xk)
  return(x.next)
}

eps = 1e-12
iter.max = 100
iter=0
err.x=1
x.init =4
x.all = x.init
while(err.x>eps&iter<iter.max){
  x.new = update.newton(xk=x.init, f.grad=f.grad.quad,f.hess=f.hess.quad)
  err.x = sqrt(sum((x.new-x.init)^2))
  x.all = rbind(x.all,x.new)
  iter=iter+1
  x.init=x.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= (',paste0(x.new,collapse = ','),') at iteration ',iter,'.'))
#' Newton's method needs more iterations than the descent method (49 vs. 8).
#' Newton's method
#' "The algorithm converges at x= (3.3263454633581) at iteration 49."
#' Descent method
#' "The algorithm converges at x= (3.32634546335783) at iteration 8."

#'
#' Now please repeat the Newton method using various initial points at -0.5,0,0.5,1,1.5,2,2.5,3, what do you find? Any guess why?
#'
#'
eps = 1e-12
iter.max = 100
iter=0
err.x=1
x.init = 2.5
x.all = x.init
while(err.x>eps&iter<iter.max){
  x.new = update.newton(xk=x.init, f.grad=function(x) f.grad.quad(x),f.hess=function(x) f.hess.quad(x))
  err.x = sqrt(sum((x.new-x.init)^2))
  x.all = rbind(x.all,x.new)
  iter=iter+1
  x.init=x.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at x= (',paste0(x.new,collapse = ','),') at iteration ',iter,'.'))
#' init = -0.5
# "The algorithm converges at x= (0.392747981126941) at iteration 10."
#' init = 0
# "The algorithm converges at x= (0.392747981126937) at iteration 9."
#' init = 0.5
# "The algorithm converges at x= (0.392747981126934) at iteration 8."
#' init = 1
# "The algorithm converges at x= (1.53090655551526) at iteration 14."
#' init = 1.5
# "The algorithm converges at x= (1.53090655551525) at iteration 11."
#' init = 2
# "The algorithm converges at x= (1.53090655551525) at iteration 13."
#' init = 2.5
# "The algorithm converges at x= (1.53090655551525) at iteration 13."
#' init = 3
# "The algorithm converges at x= (3.32634546335759) at iteration 51."

#'
#'
#' Facts from Mathematical Statistics: let $x=(x_1,\ldots,x_n)$ be iid samples from a distribution with pdf or pmf $f(x;\theta)$ where $\theta$ are unknown parameters.
#' The likelihood function is a function of $\theta$ that is defined as
#' \[
#' L(\theta;x) = \prod_{i=1}^n f(x_i;\theta)
#' \]
#' Given the observed data $x$, this function can be used to find the value of $\theta$ that fits the observed data "best".
#' Since this function is essentially the probability or the probability density of observing $x$, what we have observed, we prefer the value of $\theta$ that maximizes this chance.
#' The $\hat{\theta}$ that maximizes $L(\theta;x)$ is the maximum likelihood estimator of $\theta$.
#' This approach is called  Maximum Likelihood Estimation (MLE), and it is one of the most important approach for estimating parameters of a parametric model.
#' In most cases, it is much easier to work with the log likelihood
#' \[
#' \ell(\theta;x) = \ln(L(\theta;x)) = \ln(\prod_{i=1}^n f(x_i;\theta)) = \sum_{i=1}^n \ln(f(x_i;\theta))
#' \]
#' Since $ln(y)$ is a monotone increasing function, maximizing $\ell(\theta;x)$ leads to identical solution as maximizing $L(\theta;x)$.
#' Maximizing $\ell(\theta;x)$ is identical to minimizing $-\ell(\theta;x)$.
#'
#' Example: In your intro stat or mathematical statistics class, you may have learned about the gamma distribution
#' with pdf $f_X(x;\theta) = \frac{\beta^\alpha x^{\alpha-1}}{\Gamma(\alpha)}e^{-\beta x}$, where $\theta=(\alpha,\beta)$ are the shape parameter and the rate parameter.
#' Suppose we observe $x_1,\ldots,x_n$ from this distribution.
#' In this example, we will implement the gradient descent method and Newton method for MLE of its parameters.
#'
#' First, let us write down the likelihood
#' \[
#' L(\theta;x) = \prod_{i=1}^n f_X(x_i;\theta) = \prod_{i=1}^n \frac{\beta^\alpha x_i^{\alpha-1}}{\Gamma(\alpha)}e^{-\beta x_i}
#' \]
#' The log likelihood is
#' \[
#' \ell(\theta;x) = \ln(L(\theta;x)) = \sum_{i=1}^n \ln(f(x_i;\theta)) = \sum_{i=1}^n\left(\alpha\ln(\beta)+(\alpha-1)\ln(x_i)-\ln(\Gamma(\alpha))-\beta x_i\right) = n \alpha\ln(\beta) +(\alpha-1)\sum_{i=1}^n\ln(x_i) -n\ln(\Gamma(\alpha))-\beta\sum_{i=1}^n x_i
#' \]
#' Our objective to be minimized is $-\ell(\theta;x) = -n \alpha\ln(\beta) -(\alpha-1)\sum_{i=1}^n\ln(x_i) +n\ln(\Gamma(\alpha))+\beta\sum_{i=1}^n x_i$.
#' For gamma distribution, the parameters $\alpha,\beta>0$. So it is a constrained optimization problem, that is, only search values within a certain range.
#' We will not cover constrained optimization in this course, a workaround of this issue is to transform the parameters with no range constraints via a monotone transformation.
#' In this case, it is the log transformation. Re-define $\theta = (\delta,\gamma)=(\ln(\alpha),\ln(\beta))$. Then we can optimize without constraints.
#' The objective to be minimized becomes
#' \[
#' -\ell(\theta;x) = -n e^\delta\gamma -(e^\delta-1)\sum_{i=1}^n\ln(x_i) +n\ln(\Gamma(e^\delta))+e^\delta\sum_{i=1}^n x_i
#' \]
#'
#' To derive the gradient and Hessian, we will need to use the special functions in mathematics related to the gamma function $\Gamma(x)$.
#' In particular,$\psi(x,k) = \frac{d^{k+1}}{dx^{k+1}}\ln(\Gamma(x))$. This is also readily implemented in R.
#' The gradient of $-\ell(\theta;x)$ is
#' \[
#' -\nabla\ell(\theta;x) = \left(-\frac{\partial \ell(\theta;x)}{\partial \delta}, -\frac{\partial \ell(\theta;x)}{\partial \gamma}\right)
#' = \left(-ne^\delta\gamma-e^\delta\sum_{i=1}^n\ln(x_i)+n e^\delta \psi(e^\delta,0), -n e^\delta+e^\gamma\sum_{i=1}^n x_i\right)
#' \]
#' and the Hessian matrix is
#' \[
#' H(\theta) = \left(\begin{array}{cc}
#' ne^\delta(1-\gamma-\frac{1}{n}\sum_{i=1}^n\ln(x_i)+\psi(e^\delta,0)+e^\delta\psi(e^\delta,1)) &  -ne^\delta\\
#' -ne^\delta & e^\gamma \sum_{i=1}^nx_i\\
#' \end{array}\right)
#' \]
#' Now let us simulate 50 observations from Gamma(2,4), and apply gradient descent and Newton method to find MLE
#'
set.seed(2048)
x = rgamma(50,shape=2,rate=4)
#' Now let us implement the two methods to minimizing the negative log likelihood. Note that R provides implementations of the special functions of mathematics such as the $\Gamma$ functions.
#' In particular, R function gamma(x) returns $\Gamma(x)$, and psigamma(x,deriv=k) returns the $\frac{d^{k+1}}{d\alpha^{k+1}}\ln(\Gamma(x))$.

f.gamma = function(theta,x) -length(x)*exp(theta[1])*theta[2]-(exp(theta[1])-1)*sum(log(x))+length(x)*lgamma(exp(theta[1]))+exp(theta[2])*sum(x)
f.grad.gamma = function(theta,x) c(-length(x)*exp(theta[1])*theta[2]-exp(theta[1])*sum(log(x))+length(x)*exp(theta[1])*psigamma(exp(theta[1]),deriv = 0),-length(x)*exp(theta[1])+exp(theta[2])*sum(x))
f.hess.gamma = function(theta,x) matrix(c(length(x)*exp(theta[1])*(exp(theta[1])*psigamma(exp(theta[1]),deriv=1)+psigamma(exp(theta[1]),deriv=0)-mean(log(x))-theta[2]),-length(x)*exp(theta[1]),-length(x)*exp(theta[1]),exp(theta[2])*sum(x)),2,2)


update.gd.bb <- function(xk,xk1,f.grad){
  alpha = abs(sum((xk-xk1)*(f.grad(xk)-f.grad(xk1))))/sum((f.grad(xk)-f.grad(xk1))^2)
  x.next = xk-alpha*f.grad(xk)
  #  print(c(alpha,f.grad(xk),x.next))
  return(x.next)
}


eps = 1e-12
iter.max = 100
iter=0
err.x=1
theta.init1 =c(1,1)
theta.init =c(1,2)
theta.all = theta.init
while(err.x>eps&iter<iter.max){
  theta.new = update.gd.bb(xk=theta.init,xk1=theta.init1, f.grad=function(theta) f.grad.gamma(theta,x))
  err.x = sqrt(sum((theta.new-theta.init)^2))
  theta.all = rbind(theta.all,theta.new)
  iter=iter+1
  theta.init1=theta.init
  theta.init=theta.new
}

#' Report the estimates at the original scale.
if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at (alpha,beta)= (',paste0(exp(theta.new),collapse = ','),') at iteration ',iter,'.'))



update.newton <- function(xk,f.grad,f.hess){
  x.next = xk-t(solve(f.hess(xk))%*%f.grad(xk))
  return(x.next)
}

eps = 1e-12
iter.max = 100
iter=0
err.x=1
theta.init =c(1,2)
theta.all = theta.init
while(err.x>eps&iter<iter.max){
  theta.new = update.newton(xk=theta.init, f.grad=function(theta) f.grad.gamma(theta,x),f.hess=function(theta) f.hess.gamma(theta,x))
  err.x = sqrt(sum((theta.new-theta.init)^2))
  theta.all = rbind(theta.all,theta.new)
  iter=iter+1
  theta.init=theta.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at (alpha,beta)= (',paste0(exp(theta.new),collapse = ','),') at iteration ',iter,'.'))


#'
#'
#' #### Practice
#'
#' 1. Least square estimation (LSE) is the most commonly used method in regression analysis. You should have learned the formula for the slope and intercept in your regression class.
#' Suppose we observe $(x_1,y_1),\ldots,(x_n,y_n)$, LSE find the slope $\beta_1$ and intercept $\beta_0$ by minimizing the square loss $\sum_{i=1}^n(y_i-\beta_0-\beta_1 x_i)^2$
#' Denote this squared loss by $L(\beta_1,\beta_0;x,y)$. Let $x_i=i$ for $i=1,\ldots, 10$, and simulate $y_i$ from $N(1+2x_i,\sigma=3)$.
#' Please derive the gradient and Hessian of this function, and implement the gradient descent and Newton's method for LSE of the linear regression.
#' Then compare the output of your implementations with the output of R function lm, are they numerically identical?






#' Improving Newton's method
#'
#' (1) One way of increasing the reliability of Newton's method is to use a damped version of the update. That is, let $p^{(k)}$ be the root of the linear system
#' \[
#' (H_f(x^{(k)})+D^{(k)})p^{(k)}= -\nabla f(x^{(k)})
#' \]
#' where $D^{(k)}$ is a diagonal matrix with nonnegative elements.
#' (2) Another way of increasing reliability is to restric the movments to the "trusted regions" where the second-order Taylor expansion provides a good approximation.
#' (3) To improve the computational efficiency by avoid the cost in computing and inverting the Hessian matrix in each iteration, a whole class of Quasi-Newton methods have been proposed.
#'  These methods deploy ideas such as discrete approximation of the Hessian (secant condition), and iteratively updating $R^{(k)}$ directly without computing or inverting Hessian.
#'

#'
#'
#' Nelder-Mead Simplex, a gradient-free optimization method
#'
#'Nondifferentiable functions can also be optimized using methods that use no gradients but only the value of the function.
#'Most of these methods are somewhat heuristic, and One such example is Nelder-Mead Simplex.
#'This is the method that I use the most often, even for differentiable functions.
#'However, we will not get into details of this method.
#'Instead, I will just briefly describe how it works with the following picture.
#'The goal is to remind you that gradient-free optimization methods exist.
#'
#' Suppose $f(x)$ is a scalar function with $m$ variables. Nelder-Mead start with evaluating the function at $m+1$ points extreme points.
#' In the following picture, we consider the case with $m=2$, so Nelder-Mead start with three points, $x_b$, $x_s$ and $x_w$ and suppose the point $x_w$ provides the worst value and $x_b$ the best.
#' That is $f(x_w)\geq f(x_s)\geq f(x_b)$. Also let $x_c$ be the average of the $m$ points other than $x_w$.
#'
#' In one iteration of Nelder-Mead, it first reflect $x_w$ to the other side of $x_c$ based a a reflection coefficient, and then determine the next set of $m+1$ points based on the following graph.

knitr::include_graphics("D:\\Dropbox\\courses\\stat_computing\\Lecture notes\\neldermead.png")

#'
#' #### R function optim
#'
#' optim is one of the most widely used general purpose optimizer in R that offers a wide range of algorithm choices. I usually use the gradient-free algorithm "Nelder-Mead", or "L-BFGS-B" which is a Quasi-Newton approach.
#'
#' Let us re-consider the example of MLE for Gamma distribution, and use optim
#'

set.seed(2048)
x = rgamma(50,shape=2,rate=4)
f.gamma = function(theta) -length(x)*exp(theta[1])*theta[2]-(exp(theta[1])-1)*sum(log(x))+length(x)*lgamma(exp(theta[1]))+exp(theta[2])*sum(x)

out.nm = optim(par=c(1,1),fn=f.gamma,method = "L-BFGS-B")
exp(out.nm$par)

out.lbfgsb = optim(par=c(1,1),fn=f.gamma,method = "Nelder-Mead")
exp(out.lbfgsb$par)


#' optim() also allows box constraints. So it can optimize the log likelihood of Gamma distribution directly without the log transformations of the parameters.
#' We just need to add lower bound for the parameters. Similarly, one can also add an upper bound for each parameter.

set.seed(2048)
x = rgamma(50,shape=2,rate=4)
f.gamma.orig = function(theta) -length(x)*theta[1]*log(theta[2])-(theta[1]-1)*sum(log(x))+length(x)*lgamma(theta[1])+theta[2]*sum(x)

out.lbfgsb.orig = optim(par=c(1,1),fn=f.gamma.orig,lower=c(0,0),method = "L-BFGS-B")
out.lbfgsb.orig$par

out.nm.orig = optim(par=c(1,1),fn=f.gamma.orig,lower=c(0,0),method = 'Nelder-Mead')
out.lbfgsb.orig$par

#' There are also optimization functions for specific tasks such as for specific types of nonlinear optimizations, or constrained optimizations.
#' #' It also comes to my attention that many argues that R function optim is out of date, and one should use some newer packages for general optimization (e.g., \url{https://www.r-bloggers.com/2016/11/why-optim-is-out-of-date/}).
#' While they are making good points, I have not tested these new packages in practice.
#'
#'
#'
#' #### Practice
#'
#' 1. Redo the practice problem of Least square estimation (LSE) using R function optim

