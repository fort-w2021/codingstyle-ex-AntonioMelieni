output_basic_stats <- function(number_obs) {
  
  random_normal <- rnorm(number_obs)
  random_normal_sq <- random_normal^2
  
  normal_vector <- cbind(random_normal, random_normal_sq)
  
  mean_ <- lapply(normal_vector, mean)
  std_dev <- lapply(normal_vector, sd)
  conf_int <- lapply(normal_vector, confint(level = 0.975))
  
  list(mean_ = mean_, 
       std_dev = std_dev, 
       conf_int = conf_int
       )
}
















random_normal <-rnorm(100)
y<-rnorm(100)
z<-rnorm(100)
v=z^2  


m <- mean(random_normal ); s <- sd(random_normal ); n <- length(random_normal )
c(m - 1.96*s/sqrt(n), m + 1.96*s/sqrt(n))

m <- mean(y)
s <- sd(y)
n <- length(y)
c(m - 1.96*s/sqrt(n), m + 1.96*s/sqrt(n))

n <- length(z)
m <-  mean(z)
s <-sd(z)
c(m-1.96*s/sqrt(n),m+1.96*s/sqrt(n))

n <- length(v)
m <- mean(v)
s <- sd(v)
half_width <- 1.96*s/sqrt(n)
c(m - half_width, m + half_width)