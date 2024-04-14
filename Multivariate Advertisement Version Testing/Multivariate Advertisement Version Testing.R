# Credit : superdatascience.com


# ----- Data
dataset <- read.csv("Ads_CTR_Optimisation.csv")

# ----- 1/ Upper Confidence Bound
# Variables
N <- nrow(dataset)
d <- length(dataset)
numbers_of_selections <- integer(d)
sums_of_rewards <- integer(d)
ads_selected <- integer()
total_reward <- 0

# Model
for (n in 1:N){
  ad <- 0
  max_upper_bound <- 0
  for (i in 1:d){
    if (numbers_of_selections[i] > 0){
      average_reward <- sums_of_rewards[i] / numbers_of_selections[i]
      delta_i <- sqrt(3/2 * log(n) / numbers_of_selections[i])
      upper_bound <- average_reward + delta_i
    } else {
      upper_bound <- 1e400
    }
    if (upper_bound > max_upper_bound){
      max_upper_bound <- upper_bound
      ad <- i
    }
  }
  ads_selected <- append(ads_selected, ad)
  numbers_of_selections[ad] <- numbers_of_selections[ad] + 1
  reward <- dataset[n, ad]
  sums_of_rewards[ad] <- sums_of_rewards[ad] + reward
  total_reward <- total_reward + reward
}

# Variable Analysis
ads_selected
numbers_of_selections
sums_of_rewards

k <- (sums_of_rewards / numbers_of_selections) * 100
round(k, 1)

# Visualisation
hist(ads_selected, 
     col = "blue",
     main = "Histogram of Ads Selections",
     xlab = "Ads",
     ylab = "Number of Times Each Ad Was Selected")


# ----- 2/ Thompson Sampling
# Variables
N <- nrow(dataset)
d <- length(dataset)
ads_selected <- integer()
total_reward <- 0
# ----- number of times that the ad got reward 1 & 0
numbers_of_rewards_1 <- integer(d)
numbers_of_rewards_0 <- integer(d)

# Model
for (n in 1:N){
  ad <- 0
  max_random <- 0
  for (i in 1:d){
    random_beta <- rbeta(n = 1, shape1 = numbers_of_rewards_1[i] + 1,
                         shape2 = numbers_of_rewards_0[i] + 1)
    if (random_beta > max_random){
      max_random = random_beta
      ad = i
    }
  }
  ads_selected <- append(ads_selected, ad)
  reward <- dataset[n, ad]
  if (reward == 1){
    numbers_of_rewards_1[ad] <- numbers_of_rewards_1[ad] + 1
  } else {
    numbers_of_rewards_0[ad] <- numbers_of_rewards_0[ad] + 1
  }
  total_reward <- total_reward + reward
}

# Variable Analysis
ads_selected
numbers_of_rewards_1

# Visualisation
hist(ads_selected, 
     col = "blue",
     main = "Histogram of Ads Selections",
     xlab = "Ads",
     ylab = "Number of Times Each Ad Was Selected")