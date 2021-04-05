library(nloptr)
library(modelsummary)
#NUMBER 4

#setting the seed
set.seed(100)

#make a matrix
X <- matrix(data=rnorm(n=(10*100000)), nrow = 100000, ncol = 10)
X[,1]=1

#make eps vector
eps <- c(rnorm(n=100000, mean=0, sd=0.5))

#make beta vector
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

#make the Y vector Y=X*beta+eps
Y <- (X%*%beta)+eps

#NUMBER 5

# Our objective function
objfun <- function(beta,y,X) {
  return (crossprod((y-X%*%beta)^2))
}
# Gradient of our objective function
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
# Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
# Optimize
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=Y,X=X)
print(result)


#NUMBER 6
# set up a stepsize
alpha <- 0.0000003
# set up a number of iteration
iter <- 500
# define the gradient 
gradient <- function(beta,y,X) {
  return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}
# randomly initialize a value to x
set.seed(100)
x <- floor(runif(1)*10)
# create a vector to contain all xs for all steps
x.All <- vector("numeric",iter)
# gradient descent method to find the minimum
for(i in 1:iter){
  x <- x - alpha*gradient(x)
  x.All[i] <- x
  print(x)
}
# print result and plot all xs for every iteration
print(paste("The minimum of f(x) is ", x, sep = ""))

#NUMBER 7

#Nedler-Mead
# Our objective function
objfun <- function(x) {
  return( lm(Y~X-1) )
}
# initial values
xstart <- 5
# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)
# Find the optimum!
res <- nloptr( x0=xstart,eval_f=objfun,opts=options)
print(res)

#NUMBER 8
library(nloptr)
# Our objective function
objfun  <- function(theta,y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}


gradient <- function(theta,Y,X) {
  grad <- as.vector(rep(0,length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%beta)/(sig^2) 
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y-X%*%beta)/(sig^3)
  return ( grad ) }

#NUMBER 9
est <- lm(Y~X-1)
modelsummary(est, output = "latex")

