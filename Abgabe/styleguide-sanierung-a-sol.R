### Exercise 6 GRM


# load needed packages
library(tidyverse)


######## next section does load and clean up the data ########

# load foodstamp data set by using read.table function
foodstamp_data <- read.table("foodstamp.dat", header = FALSE)

# rename columns by using colnames function
colnames(foodstamp_data) <- c("y", "TEN", "SUP", "INC")

# show head and summary of foodstamp data set by using head and summary function
head(foodstamp_data)
summary(foodstamp_data)

# clean up observations with "no income" by using filter function and
# redefine the data set
foodstamp_data <- foodstamp_data %>% filter(INC > 0)

######## next section fits a logit model and computes diagonal elements ########

# fit a generalized linear model with log-link (logit-model) using glm function
# and link = logit
fit_logit_model <- glm(
  formula = y ~ TEN + SUP + INC,
  family = binomial(link = "logit"), data = foodstamp_data
)

# to interpret the model use summary
summary(fit_logit_model)

# function that outputs the diagonal elements of the generalized hat matrix
# of a generalized linear model to later identify high-leverage points
output_diag_elements <- function(glmobject) {
  w <- glmobject$weights # weights of the IWLS-steep
  x <- model.matrix(glmobject) # extract design-matrix X
  wsqrt_x <- sqrt(w) * x
  finv <- summary(glmobject)$cov.scaled # Inverse Fisher-matrix with scaled cov

  calcualte_diag_elements <- diag(wsqrt_x %*% finv %*% t(wsqrt_x))

  return(calcualte_diag_elements)
}

# give out the diagonal elements of the fitted logit-model
diag_elements_logit <- output_diag_elements(fit_logit_model)

######## next section plots the diagonal elements and identifies high-leverage points ########

# set number of observations in data using length function
n <- length(foodstamp_data$y)

# plot of diagonal elements against indicies using plot function to get
# first overview
par(mfrow = c(1, 1))
plot(1:n, diag_elements_logit,
  type = "b", xlab = "Index",
  ylab = expression(h[ii])
)

# identify high-leverage points:
# if H >= 2 * (p/n) where p is the number of coefficients in
# the model and n is the number of observations we have a high-leverage point

# set number of coefficients by using length function
p <- length(fit_logit_model$coef)

# draw a line in the plot to identify high-leverage points optically
# using abline function
abline(h = 2 * p / n, lty = 3)

# identify high-leverage points using which function
show_high_leverage_points <- which(diag_elements_logit > 2 * p / n)

# observation 33 and 108 from the data set seem to be high-leverage points
show_high_leverage_points

######## next section compares my function with an implemented R function  ########

# get diagonal elements using R function hatvalues
diag_elements_r_function <- hatvalues(fit_logit_model)

# we get the same diagonal elements using R's hatvalue function
all.equal(diag_elements_logit, diag_elements_r_function)
