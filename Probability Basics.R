set.seed(1000000) # or anything else you want to make it

# Simulating Coin Toss
k <- c(0, 1)
p <- c(.5, .5)
x <- sample(k, size=1, prob=p, replace=T)


# Question 2
n <- seq(1, 200, length = 200)

heads <- numeric(length(n))
cum <- numeric(length(n))

for(i in 1:length(n)) {
  heads[i] <- sum(sample(k, size=1, prob=p, replace=T) == 1)
  cum[i] <- sum(heads[1:i])/i 
}


# Question 3
sim <- cbind(n, heads, cum)

plot(n, cum, type="l")


# Question 4 (with Mosaic Package)
install.packages("mosaic")
library(mosaic)

rflip(200) # try over and over, and you will see it changes


# Question 5
coins <- do(1000)*rflip(6) # flip 6 coins 1000 times simultaneously and save as an object
tally(~heads, data = coins) # Now look at the distribution of "heads" for each coin


# Question 6
die <- 1:6
p <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
x <- sample(die, size=1, prob=p, replace=T)

dice <- do(1000)*sample(die, size=1, prob=p, replace=T)

tally(~dice, data = dice) # change "result" to "dice"


# Question 7 (binomial distribution in Base R)
dbinom(3, 6, .5) # density function = 0.65625 (substituting 0,1,2,3 and running four separate times, then adding up)

pbinom(3, 6, .5) # cumulative distribution = 0.65625 (do it once instead, using the CDF)


# Question 8
prod(6:1) # 720

prod(31:29) # 26970

prod(31:1)/prod(28:1) # 26970


# Question 9
dbinom(2, 22, .30) # 0.01658881 -> 2 same birthday, of 22 players, at a 30 % chance


# Question 10
prod(31:1)/(prod(28:1)*prod(3:1)) # 4495
                                  # only use one combination once, rather than reorder and call it a permutation


# Question 11 
prod(52:1)/(prod(47:1)*prod(5:1)) # 2,598,960 combinations - drop the "*prod(5:1)" to see the permutations, if order mattered - 
                                  # you will see there are many more if we altered each combination to cover all permutations

4/2598960 # = 1.539077e-06, or 0.000001539077, or .0002 % chance of being dealt a royal flush

1/649740 # because there is a 1 in 649,740 chance of being dealt a single royal flush



