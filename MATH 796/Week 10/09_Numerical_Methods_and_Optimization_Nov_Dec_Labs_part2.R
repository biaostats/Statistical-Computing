#' ---
#' title: "Numerical Methods and Optimization"
#' author: "Biao Zhang"
#' date: "December 4th, 2021"
#' ---
#'
#' ##
#'
#' ## Please do NOT share the questions with anyone who are not enrolled in this class. Please do NOT share your code with anyone when you are enrolled in this class. They will be considered as academic misconduct.
#'


#'
#'  1. (8 points) The negative log likelihood of $n$ iid samples from $N(\mu,\sigma^2)$ is
#' \[
#' -\ell(\theta;x) = \frac{1}{2\sigma^2}\sum_{i=1}^n(x_i-\mu)^2+\frac{n}{2}\ln(\sigma^2)
#' \]
#'  Please simulate 50 observations from $N(\mu=2,\sigma^2=1)$. Then derive, and implement gradient descent and Newton's method for MLE.
#'  You may or may not need transformation of the parameters.
#'
#'
#' Since $\sigma^2>0$, let $\gamma = \ln(\sigma^2)$, and $\theta=(\mu,\gamma)$. Then
#' \[
#' -\ell(\theta;x) = \frac{e^{-\gamma}}{2}\sum_{i=1}^n(x_i-\mu)^2+\frac{n}{2}\gamma
#' \]

#'  The gradient of $-\ell(\theta;x)$ is
#' \[
#' -\nabla\ell(\theta;x) = \left(\frac{n e^{-\gamma}}{2}(\mu-\bar{x}),\frac{n}{2}-\frac{e^{-\gamma}}{2}\sum_{i=1}^n(x_i-\mu)^2\right)
#' \]
#' and the Hessian matrix is
#' $$
#' H(\theta) = \left(\begin{array}{cc}
#' \frac{n e^{-\gamma}}{2} &  -\frac{n e^{-\gamma}}{2}(\mu-\bar{x})\\
#' -\frac{n e^{-\gamma}}{2}(\mu-\bar{x}) & \frac{e^{-\gamma}}{2}\sum_{i=1}^n(x_i-\mu)^2\\
#' \end{array}\right)
#' $$
#' Now let us simulate 50 observations from N(2,1), and apply gradient descent and Newton method to find MLE
#'
set.seed(2048)
x = rnorm(50,mean=2,sd=1)
f.norm = function(theta,x) .5*exp(-theta[2])*sum((x-theta[1])^2)+.5*length(x)*theta[2]
f.grad.norm = function(theta,x) c(.5*length(x)*exp(-theta[2])*(theta[1]-mean(x)), .5*length(x)-.5*exp(-theta[2])*sum((x-theta[1])^2))
f.hess.norm = function(theta,x) matrix(c(.5*length(x)*exp(-theta[2]),-.5*length(x)*exp(-theta[2])*(theta[1]-mean(x)),-.5*length(x)*exp(-theta[2])*(theta[1]-mean(x)),.5*exp(-theta[2])*sum((x-theta[1])^2)),2,2)

update.gd.bb <- function(xk,xk1,f.grad){
  alpha = abs(sum((xk-xk1)*(f.grad(xk)-f.grad(xk1))))/sum((f.grad(xk)-f.grad(xk1))^2)
  x.next = xk-alpha*f.grad(xk)
  return(x.next)
}

eps = 1e-12
iter.max = 100
iter=0
err.x=1
theta.init1 =c(1,0.5)
theta.init =c(1,0.6)
theta.all = theta.init
while(err.x>eps&iter<iter.max){
  theta.new = update.gd.bb(xk=theta.init,xk1=theta.init1, f.grad=function(theta) f.grad.norm(theta,x))
  err.x = sqrt(sum((theta.new-theta.init)^2))
  theta.all = rbind(theta.all,theta.new)
  iter=iter+1
  theta.init1=theta.init
  theta.init=theta.new
}

#' Report the estimates at the original scale.
if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at (mu,sigma.sq)= (',paste0(c(theta.new[1],exp(theta.new[2])),collapse = ','),') at iteration ',iter,'.'))

update.newton <- function(xk,f.grad,f.hess){
  x.next = xk-t(solve(f.hess(xk))%*%f.grad(xk))
  return(x.next)
}

eps = 1e-12
iter.max = 100
iter=0
err.x=1
theta.init =c(1,.6)
theta.all = theta.init
while(err.x>eps&iter<iter.max){
  theta.new = update.newton(xk=theta.init, f.grad=function(theta) f.grad.norm(theta,x),f.hess=function(theta) f.hess.norm(theta,x))
  err.x = sqrt(sum((theta.new-theta.init)^2))
  theta.all = rbind(theta.all,theta.new)
  iter=iter+1
  theta.init=theta.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at (mu,sigma.sq)= (',paste0(c(theta.new[1],exp(theta.new[2])),collapse = ','),') at iteration ',iter,'.'))

#'
#'
#' 2.  (8 points)LSE is not limited to linear regression, but also nonlinear regression or curve fitting in general. Let us consider a nonlinear regression case.
#' Suppose you observe $(x_1,y_1),\ldots, (x_n,y_n)$, and believe that the pattern fits $\hat{y}=\beta_0+ x_i^{\beta_1}$.
#' The parameters can be estimated by minimizing the sum of the squared errors $g(\beta)=\sum_{i=1}^n(\beta_0+x_i^{\beta_1}-y_i)^2$ where $\beta = (\beta_0,\beta_1)$.
#' You will derive and implement the gradient descent and Newton's method for it.
#' Please simulate the data as the following. Fo $i=1,\ldots, 50$, let $x_i=i/10$, $\epsilon_i\sim Unif(-1,1)$ and $y_i = 4+x_i^3+\epsilon_i$.
#'

#' The gradient of $g(\beta)$ is
#' \[
#' \nabla g(\beta) = \left(2\sum_{i=1}^n(\beta_0+x_i^{\beta_1}-y_i),2\sum_{i=1}^n \log(x_i)x_i^{\beta_1}(\beta_0+x_i^{\beta_1}-y_i)\right)
#' \]
#' and the Hessian matrix is
#' $$
#' H(\beta) = \left(\begin{array}{cc}
#' 2n & 2\sum_{i=1}^n\log(x_i)x^{\beta_1} \\
#' 2\sum_{i=1}^n\log(x_i)x^{\beta_1}  & 2\sum_{i=1}^n\log(x_i)^2 x_i^{\beta_1}(\beta_0+2x_i^{\beta_1}-y_i)\\
#' \end{array}\right)
#' $$
set.seed(1201)
x = (1:50)/10
y = 4+x^3+runif(length(x),min=-1,max=1)

f.lse = function(beta,x,y) sum((beta[1]+x^beta[2]-y)^2)

f.grad.lse = function(beta,x,y) c(2*sum(beta[1]+x^beta[2]-y),2*sum(log(x)*x^beta[2]*(beta[1]+x^beta[2]-y)))

f.hess.lse = function(beta,x,y) matrix(c(2*length(x),2*sum(log(x)*x^beta[2]),2*sum(log(x)*x^beta[2]),2*sum((log(x))^2*x^beta[2]*(beta[1]+2*x^beta[2]-y))),2,2)

update.gd.bb <- function(xk,xk1,f.grad){
  alpha = abs(sum((xk-xk1)*(f.grad(xk)-f.grad(xk1))))/sum((f.grad(xk)-f.grad(xk1))^2)
  x.next = xk-alpha*f.grad(xk)
  return(x.next)
}

eps = 1e-12
iter.max = 300
iter=0
err.x=1
beta.init1 =c(0,0)
beta.init =c(1,1)
beta.all = beta.init
while(err.x>eps&iter<iter.max){
  beta.new = update.gd.bb(xk=beta.init,xk1=beta.init1, f.grad=function(beta) f.grad.lse(beta,x,y))
  err.x = sqrt(sum((beta.new-beta.init)^2))
  beta.all = rbind(beta.all,beta.new)
  iter=iter+1
  beta.init1=beta.init
  beta.init=beta.new
}

#' Report the estimates at the original scale.
if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at (beta.0,beta.1)= (',paste0(beta.new,collapse = ','),') at iteration ',iter,'.'))

update.newton <- function(xk,f.grad,f.hess){
  x.next = xk-t(solve(f.hess(xk))%*%f.grad(xk))
  return(x.next)
}

eps = 1e-12
iter.max = 100
iter=0
err.x=1
beta.init =c(0,0)
beta.all = beta.init
while(err.x>eps&iter<iter.max){
  theta.new = update.newton(xk=beta.init, f.grad=function(beta) f.grad.lse(beta,x,y),f.hess=function(beta) f.hess.lse(beta,x,y))
  err.x = sqrt(sum((beta.new-beta.init)^2))
  beta.all = rbind(beta.all,beta.new)
  iter=iter+1
  beta.init=beta.new
}

if(iter==iter.max)  print(paste0('The algorithm does not converge in ',iter.max,' iterations!'))
if(err.x<eps) print(paste0('The algorithm converges at (beta.0,beta.1)= (',paste0(beta.new,collapse = ','),') at iteration ',iter,'.'))
#' Weird results, but this is what we got. LSE may not be recommended in this case.
#' 
#' 3. (6 points) Redo the previous lab questions using optim. You may want to explore different algorithms specified by input argument "method". I usually use "Nelder-Mead" or "BFGS".
#'
out.lse.lbfgsb = optim(par=c(0,0),fn=function(beta) f.lse(beta,x,y),method =  "L-BFGS-B")
out.lse.nm = optim(par=c(0,0),fn=function(beta) f.lse(beta,x,y),method =  "Nelder-Mead")
out.lse.lbfgsb$par
out.lse.nm$par
#' In comparison with the previous results by LSE, the results by optim() are more reliable.