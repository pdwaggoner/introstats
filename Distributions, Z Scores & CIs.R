## Exploring Z Scores and Plotting Confidence Intervals

# Create a vector of random numbers to be used as sample data
vector <- c(3,13, 15,20,13,8,17,11,9,6,9,6,11,10,14,11,7,10,10,16,16,12,11,14,16,13,8,8,10,11,7,11,5,9,12,6,11,8,8,18,10,13,7,8,11,9,10,6,12,9,11,12,6,4,13,
            11,6,7,12,11,14,8,7,8,7,19,13,8,14,12,15,15,12,16,15,28,10,15,16,9,19,19,19,14,8)

# Now, simulate some data, based on our "real" observations in vector
set.seed(2673)

d <- seq(1, 20, length = 20) # create an vector, d, from 1-20
m <- numeric(length(d)) # create an empty vector, m, where means will be stored

for(i in 1:length(d)) { # loop over all onservations and store for the ith observation of m
  m[i] <- mean(sample(vector, size=4, replace=T)) 
  }

# Now calculate and store some key values
mu <- mean(vector) # store the mean of vector
sigma <- sd(vector) # store the standard deviation of vector
se <- sigma/sqrt(4) # calculate and store the standard error as an object - sigma/sqrt(n)
upper = mu + 1.96*se # calculate the upper bound of the CI (xbar +- z*se)
lower = mu - 1.96*se # and now the lower bound (using, "-" in the equation in the comment in line 18 above)

# Write a function, zs, to caluclate the z score based on our stored objects above in lines 9-19
zs <- function(m, mu, sigma) {
  (mean(m)-mu)/(sigma/sqrt(4)) 
  }

# Use your function to calculate the z score for each sampled value above
z <- numeric(length(d))
for(i in 1:length(d)) {
  z[i] <- zs(m[i], mu, sigma)
  }

# Calculate the upper CI for each observation
for(i in 1:length(d)) {
  upper[i] <- m[i] + 1.96*se
  }

# Calculate the lower CI for each observation
for(i in 1:length(d)) {
  lower[i] <- m[i] - 1.96*se
  }

# Now, plot each to see the distribution of values with CIs, based on the Z scores we just calculated using our sample data
plot(d,upper,pch=19,ylim=c(0,25))
points(d,lower,pch=19)
with(sapply(d,function(i) lines(c(i,i),c(lower[i],upper[i]))))
abline(a=mu, b=0, col="red") # repeat the steps above to see a different distribution; one obs should be outside of the mean, given the 95% CIs we created and applied (19/20)

# Update the parameters and constraints in the loops and objects above to see adjustments in the distribution and the CIs to get a better sense of what is going on.
