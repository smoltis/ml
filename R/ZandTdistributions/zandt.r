
# we have 10K students in freshman class
population <- seq(1,10000) # x <- 1:10000

# a random 5 students (permutation)
sample <- sample(population,5,replace = FALSE)

# bootstrap
#rand5 <- sample(population,5,replace = TRUE)

#find average GPA of those 5 students
#but declare that average GPA of the entire class is ...

# Why to sample?
# 1. mean of sample to work fast on larger populations
# however distribution has to be sampled as well, approximation, estimate of best
# as n - number of samples becomes less and less the greater is the risk of error
# and we often know nothing about mean, variance and standard deviation of our population
# 2. the larger the sample the more it is representative of the entire population
# 3. the smller sample the higher change that we either miss that variation or over-emphasize is
# Distribution of a sampled population is called t-distribution (Student's distribution)

# When to use t-distribution?
# - When our sample size id n<=30 
# - and/or we don't know the variance/standard deviation of the entire population
# we use t-distribution instead of z-distribution (standard normal distribution)

# Note:
#1. Benifit: Can use smaller samples
#2. Price to pay: margin-of-error trade-off
#3. Takes into account sample size using (n-1) degrees of freedom (adjustment to a sample size "wiggle room", number of independent observations)
#4. The shape of the distribution will be different for different degrees of freedom
#5. t-distr is lower in the middle and fatter on the sides in comaprison with z-distr
#6. win n>30 and n->inf t-distribution and z-distribution converge

#Decision tree for t-distribution
# Is Standard deviation of population known? Yes - use Z, No - use T
# Is n>=30 ? Yes - use either Z or T, No - use T

#t-distr 5 degrees of freedom
t <- seq(-5,5,0.01)
n <- 5 
tdistr <- dt(t,n)

#t-distr 46 degrees of freedom
plot(t,tdistr,type="l",col="blue")
lines(t,dt(t,46),col="red")

#normal distribution
hx <- dnorm(t)
lines(t,hx,col="green")

#area to the left of nth percentile of 46 degrees of freedom of t-distr
qt(0.05,46)
