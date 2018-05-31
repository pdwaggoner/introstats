## This code was largely adapted from part of nicer's MCMC code
# Set our parameters
m <- 0 # mean
s <- 1 # standard deviation

# Now sample from this distribution (mean = 0; sd = 2)
set.seed(235)
samples <- rnorm(10000, m, s) # the `rnorm` function simply generates a random sample from a normal distribution

# Let's check the mean from our sample
mean(samples) # it is very close to the true mean we previously set (0)

# Let's now check the summary statistics for our sample
summary(replicate(1000, mean(rnorm(10000, m, s))))

# Now we can compute the cumulative mean (for element k, the sum of elements 1, 2,..., k, divided by k) by writing a simple function
cummean <- function(x)
  cumsum(x) / seq_along(x)

# Now we can plot the convergence to the true mean
plot(cummean(samples), type="l", xlab="Sample", ylab="Cumulative mean",
     main = "Convergence to True Mean (0)",
     panel.first=abline(h=0, col="red"), las=1) # set the hline at 0 for comparison

# Here are another 50 random samples on the same plot to see the same pattern (convergence to the true mean of 0) 
set.seed(5523)
plot(cummean(samples), type="l", xlab="Sample", ylab="Cumulative mean",
     main = "Convergence to True Mean (0)\nOver 50 Samples",
     ylim = c(-2,2), # set wider limit for y axis to center the samples in the plot
     panel.first=abline(h=0, col="red"), las=1, log="x") # set the hline at 0 for comparison
for (i in seq_len(50))
  lines(cummean(rnorm(10000, m, s)),
        col=rgb(runif(1), runif(1), runif(1), .5))

# Change the parameter values and repeat the steps above to see how the samples change accordingly (e.g., changing m to 2 will shift all samples to converge to 2 as this is the "true mean", increasing sd values will shift the sample mean given wider variance, and so on.)
