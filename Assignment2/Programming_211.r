#11. Computing with a nonconjugate single-parameter model: suppose y1, . . . , y5 are independent samples from a Cauchy distribution with unknown center θ and known scale 1: 
#p(yi|θ) ∝ 1/(1 + (yi − θ)2). 
#Assume, for simplicity, that the prior distribution for θ is uniform on [0, 100]. Given the observations (y1, . . . , y5) = (43, 44, 45, 46.5, 47.5):

#(a) Compute the unnormalized posterior density function, p(θ)p(y|θ), on a grid of points θ = 0, 1/m, 2/m, . . . , 100, for some large integer m. Using the grid approximation, compute and plot the normalized posterior density function, p(θ|y), as a function of θ.


## Sampling Distribution
dist <- function (y,th) {
dist0 <- NULL
for (i in 1:length(th))
dist0 <- c(dist0,prod(dcauchy(y,th[i])))
dist0}


## DATA
y<- c(43,44,45,46.5,47.5)

## Parameter grid
step <- .01
theta <- seq(0,100,step)

## Unnormalized Density
dist.unnorm <- dist(y,theta)

## Normalized Denisty
dist.norm <- dist.unnorm/(step*sum(dist.unnorm))

## Plot
plot(theta, dist.norm , ylim=c(0,1.5*max(dist.norm)), type="l", xlab="theta", ylab="normalized posterior", xaxs="i", yaxs="i" , col="red")




##(b) Sample 1000 draws of θ from the posterior density and plot a histogram of the draws.
## Sampling
theta_sample <- sample(theta, 1000, step*dist.norm,replace=TRUE)

## Histogram
hist(theta_sample, xlab="theta", breaks=seq(0,100,0.5), xaxs="i", yaxs="i" ,ylim=c(0,250), col="red")

hist(theta_sample, xlab="theta", breaks=seq(40,50,0.5), xaxs="i", yaxs="i" ,ylim=c(0,250), col="red")



## (c) Use the 1000 samples of θ to obtain 1000 samples from the predictive distribution of a future observation, y6, and plot a histogram of the predictive draws
y6 <- rcauchy(length(theta_sample),theta_sample,1)

## Histogram
hist(y6,xlab="Future Observation", nclass=100, xaxs="i", yaxs="i", xlim=c(-200,200), ylim=c(0,500), col="red")


