set.seed(8235) # or anything else you want to make it; do this for any simulations, etc.

# Simulating Coin Toss
k <- c(0, 1)
p <- c(.5, .5)
x <- sample(k, size=1, prob=p, replace=T)

# Simulate sequences of coin tosses; first create some objects to store stuff
n <- seq(1, 200, length = 200)
heads <- numeric(length(n))
cumulative <- numeric(length(n))
for(i in 1:length(n)) {
  heads[i] <- sum(sample(k, size=1, prob=p, replace=T) == 1)
  cumulative[i] <- sum(heads[1:i])/i 
}

# Inspect
sim <- cbind(n, heads, cumulative)
plot(n, cumulative, type="l")

# Same as previous step, only automating simulations (with `Mosaic` Package)
install.packages("mosaic")
library(mosaic)

rflip(200) # try repeatedly to see it change

coins <- do(1000)*rflip(6) # flip 6 coins 1000 times simultaneously and save as an object
tally(~heads, data = coins) # Now look at the distribution of "heads" for each coin

# From coin to dice
die <- 1:6
p <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
x <- sample(die, size=1, prob=p, replace=T)
dice <- do(1000)*sample(die, size=1, prob=p, replace=T)
tally(~dice, data = dice) # change "result" to "dice"

# Intro to distributions; Binomial distribution (Base R)
dbinom(3, 6, .5) # density function = 0.65625 (substituting 0,1,2,3 and running four separate times, then adding up)

pbinom(3, 6, .5) # cumulative distribution = 0.65625 (do it once instead, using the CDF)

# Different combinations ((n!/r!*(n-r)!) -- compare with permutations below)
prod(6:1) # 720
prod(31:29) # 26970
prod(31:1)/prod(28:1) # 26970

# Permutations (n!/(n-r)!)
prod(31:1)/(prod(28:1)*prod(3:1)) # 4495; only use one combination once, rather than reorder and call it a permutation

# Sample problem: How many different five-card poker hands are possible from a standard 52-card deck? Then, calculate the probability of being dealt a royal flush. 
prod(52:1)/(prod(47:1)*prod(5:1)) # 2,598,960 combinations - drop the "*prod(5:1)" to see the permutations, if order mattered - 
                                  # you will see there are many more if we altered each combination to cover all permutations

4/2598960 # = 1.539077e-06, or 0.000001539077, or .0002 % chance of being dealt a royal flush
1/649740 # because there is a 1 in 649,740 chance of being dealt a single royal flush
