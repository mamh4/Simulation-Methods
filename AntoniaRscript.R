source("rscript.r")

#install.packages("MASS")
library(MASS)

sample_mean <- mean(data$CLM_FREQ) #calculating the sample mean
sample_mean
sample_variance <- var(data$CLM_FREQ) #calculating the sample variance
sample_variance

#################################QUESTION 1###########################


#after observing the data$FREQ_claim we understood that the number of claims ranges from 0 to 7
#observed_data is a list with key values referring to the number of claims
observed_data <- list("0claim"=0, "1claim" = 0, "2claim" = 0, "3claim" = 0, "4claim" = 0,
                      "5claim" = 0, "6claim" = 0, "7claim" = 0)

# Iterate through the elements of data$CLM_FREQ
for (i in data$CLM_FREQ) {
  if (i ==0) {
    observed_data[["0claim"]] <- observed_data[["0claim"]] + 1
  }else if (i==1){
    observed_data[["1claim"]] <- observed_data[["1claim"]] + 1
  } else if (i == 2) {
    observed_data[["2claim"]] <- observed_data[["2claim"]] + 1
  } else if (i == 3) {
    observed_data[["3claim"]] <- observed_data[["3claim"]] + 1
  } else if (i == 4) {
    observed_data[["4claim"]] <- observed_data[["4claim"]] + 1
  } else if (i == 5) {
    observed_data[["5claim"]] <- observed_data[["5claim"]] + 1
  } else if (i == 6) {
    observed_data[["6claim"]] <- observed_data[["6claim"]] + 1
  } else if (i == 7) {
    observed_data[["7claim"]] <- observed_data[["7claim"]] + 1
  }
}



for (i in 1:length(observed_data)) {
  observed_data[[i]] <- observed_data[[i]] / length(data$CLM_FREQ)
}


observed_data



##Negative Binomial#############################

pdf_negative_bin <- function(x, r, p) {
  choose(x + r - 1, x) * (1 - p)^x * p^r
} #pdf of Negative Binomial

##According to the method of moments we assume that the sample mean equals E(X)
#and the sample variance equals VAR(X)

#E(X)=(1-p)*r/p
#VAR(X)=r*(1-p)/p^2


# Define a class called "MyClass"
setClass("MLE_neg_bin",
         representation(sample_mean = "numeric", sample_variance = "numeric", r = "numeric", p = "numeric"))

# Create a constructor function for the class
MLE_neg_bin <- function(sample_mean, sample_variance) {
  obj <- new("MLE_neg_bin")
  obj@sample_mean <- sample_mean
  obj@sample_variance <- sample_variance
  obj@r <- NA_real_
  obj@p <- NA_real_
  return(obj)
}

# Define a method for the class that calculates r and p values
calculate <- function(obj) {
  
  obj@p <- obj@sample_mean / obj@sample_variance
  obj@r <- obj@sample_mean ^2 / (obj@sample_variance - obj@sample_mean)
  
  return(obj)
}


my_obj <- MLE_neg_bin(sample_mean, sample_variance)
my_obj <- calculate(my_obj)
print(paste("the MLE estimator of r, in Negative Binomial is",my_obj@r))
print(paste("the MLE estimator of p, in Negative Binomial is", my_obj@p))

my_obj@r
my_obj@p


empirical_data <- list()

for (i in 1:length(observed_data)) {
  empirical_data[i] <- pdf_negative_bin(i, my_obj@r, my_obj@p)
}


empirical_data

chisq.test(unlist(empirical_data),unlist(observed_data))

chisq.test(unlist(observed_data),unlist(observed_data))

#p-value = 0.2289
#since the p-value (0.2289) is greater than the typical significance level of 0.05
#we cannot reject the null hypothesis.
#there is no significant evidence to suggest that the observed data 
#significantly deviates from the expected values based on the empirical data

#################Binomial############################

pdf_binomial <- function(x, n, p) {
  choose(n, x) * (1 - p)^(n-x) * p^x
}

mle_bin<- function(data){
  sum(data)/length(data)
}

mle_bin(data$CLM_FREQ)

###############################NEGATIVE BINOMIAL################

###########calculates the number of of trials required 
#to achieve r successes in a negative binomial distribution


negative_binomial_antithetic_estimator <- function (sample_size, r, p) {
  sum <- 0 
  for (i in 1:sample_size) {
    random_number <-  runif(1,0,1)
    sum <- sum + simulated_negative_binomial(random_number, r, p) + simulated_negative_binomial(1-random_number, r, p)  
  }
  sum <- sum /(2* sample_size )
}

negative_binomial_antithetic_estimator(1000,my_obj@r,my_obj@p)


simulate_nb_with_u_input <- function(random_numbers, r, p) {
  result <- c()
  for(i in 1:length(random_numbers)){
    counter <- 0 
    lower_bound <- 0 
    upper_bound <- choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    while (! ((upper_bound > random_numbers[i] ) & (lower_bound <= random_numbers[i]) )) {
      counter <- counter + 1 
      lower_bound <- upper_bound 
      upper_bound <- upper_bound +  choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    }
    result[i] <- counter
  }
  return(result)
}

simulate_nb_with_u_input(1000,obj@r,obj@p)

antithetic_variates_nb <- function(size, r, p){
  if(size %% 2 == 0){
    u1_vector <- runif(size/2,0,1)
    u2_vector <- 1-u1_vector
  } else {
    u1_vector <- runif(size/2 +1,0,1)
    u2_vector <- 1-u1_vector
  }
  
  f_u1 <- simulate_nb_with_u_input(random_numbers=u1_vector,r = r, p = p)
  f_u2 <- simulate_nb_with_u_input(random_numbers=u2_vector,r = r, p = p)
  
  return(append(f_u1,f_u2))
}

###########################################################################
###############################QUESTION 2###############################
###############simulate from exponential#######################

exponential_simulation <- function(lambda, size){
  simulated_exponential <- rep(0, size) # a vector of zeros with length size
  #that will store the simulated exponential values
  for (i in 1:size){
    initial_random_number <- runif(1,0,1)
    single_simulated_exponential <- - log(1 - initial_random_number )/lambda
    #given by inversion method
    simulated_exponential[i] <- single_simulated_exponential
  }
  return (simulated_exponential)
}

exponential_simulation(sample_mean,1000)



######################################QUESTION 2####################################

n=1000
set.seed(1)
u=runif(n)
u

set.seed(5)
poisson_simulation <- function(lambda, size){
  simulated_poisson <- rep(0, size) #The variable simulated_poisson is initialized 
#as a vector of zeros with length size
#This vector will store the simulated Poisson values
  for (i in 1:size){
    sum_of_exponentials <- 0.0 
    counter <- 0 
    while (! (sum_of_exponentials > lambda )) {
      initial_random_number <- runif(1,0,1) #length 1 lower bound 0 upper bound 1 
      simulated_exponential <- -log(1 - initial_random_number ) 
      #given by inversion method for parameter = 1 
      sum_of_exponentials <- sum_of_exponentials + simulated_exponential
      counter <- counter + 1 
    }
    simulated_poisson[i] <- counter -1
  }
  return (simulated_poisson)
}

##############################ACCEPTANCE/REJECTION METHOD########################

X = runif(4500, 0, 1)
U = runif(4500, 0, 1)

pdf_negative_bin <- function(x, r, p) {
  choose(x + r - 1, x) * (1 - p)^x * p^r
}

count = 1
accept = c()

while (count <= 4500 & length(accept) < 1000) {
  test_u = U[count]
  test_x = pdf_negative_bin(X[count], r, p) / (3.125 * dunif(X[count], 0, 1))
  
  if (test_u <= test_x) {
    accept = c(accept, X[count])
  }
  
  count = count + 1
}

hist(accept)



#######################################ANTITHETIC VARIATES############################

negative_binomial_antithetic_estimator <- function (sample_size, r, p) {
  sum <- 0 
  for (i in 1:sample_size) {
    random_number <-  runif(1,0,1)
    sum <- sum + negative_binomial_inversion_method(random_number, r, p) + negative_binomial_inversion_method(1-random_number, r, p)  
  }
  sum <- sum /(2* sample_size )
}

simulate_nb_with_u_input <- function(random_numbers, r, p) {
  result <- c()
  for(i in 1:length(random_numbers)){
    counter <- 0 
    lower_bound <- 0 
    upper_bound <- choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    while (! ((upper_bound > random_numbers[i] ) & (lower_bound <= random_numbers[i]) )) {
      counter <- counter + 1 
      lower_bound <- upper_bound 
      upper_bound <- upper_bound +  choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    }
    result[i] <- counter
  }
  return(result)
}

simulate_nb_with_u_input(1000,obj@r,obj@p)


negative_binomial_antithetic_estimator <- function (sample_size, r, p) {
  sum <- 0 
  for (i in 1:sample_size) {
    random_number <-  runif(1,0,1)
    sum <- sum + negative_binomial_inversion_method(random_number, r, p) + negative_binomial_inversion_method(1-random_number, r, p)  
  }
  sum <- sum /(2* sample_size )
}

simulate_nb_with_u_input <- function(random_numbers, r, p) {
  result <- c()
  for(i in 1:length(random_numbers)){
    counter <- 0 
    lower_bound <- 0 
    upper_bound <- choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    while (! ((upper_bound > random_numbers[i] ) & (lower_bound <= random_numbers[i]) )) {
      counter <- counter + 1 
      lower_bound <- upper_bound 
      upper_bound <- upper_bound +  choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    }
    result[i] <- counter
  }
  return(result)
}

############################################QUESTION 4################################


#Risk premiums calculated using the data

total_loss <- 0
# Assuming 'matrix' is your two-dimensional matrix
for (i in 1:nrow(data)) {
  for (j in 2:8) { 
    # Iterating from the 2nd to the 8th column, the columns including claims height 
    #and claims severity
    total_loss <- total_loss + data[i, j]
  }
}
riskpremium <- total_loss/ nrow(data) #the estimation is basically the total amount paid
#for claims divided by the number of claims
riskpremium #the premium using our data should be 766.067



#Risk premium calculated based on our estimated frequency and severity models



