
# the following function calculates the basic statistics mean, standard deviation
# and a 95% confidence interval for normal random numbers

output_basic_stats <- function(number_obs) {

  # generate a vector of random numbers
  random_normal <- rnorm(number_obs)

  # calculate mean and std-deviation with basic functions
  mean_ <- mean(random_normal)
  std_dev <- sd(random_normal)

  # function to calculate a 95%-ci using mean and std-dev
  calc_conf_int <- function(random_normal) {
    c(
      mean_ - 1.96 * std_dev / sqrt(number_obs),
      mean_ + 1.96 * std_dev / sqrt(number_obs)
    )
  }

  # calculate the confidence interval
  conf_int <- calc_conf_int(random_normal)

  # give back as a list
  list(
    mean_ = mean_,
    std_dev = std_dev,
    conf_int = conf_int
  )
}

# set seed to make it reproduceable
set.seed(1328) # 1328 is the founding year of the Augustiner Brauerei!

# set the numbers
y <- 100
z <- y^2

# do the calculations
y_stats <- output_basic_stats(y)
z_stats <- output_basic_stats(z)

# show results
y_stats
z_stats
