source("rscript.r")

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

###############################SIMULATE FROM NEGATIVE BINOMIAL################


simulate_negative_binomial <- function(r, p, size) {
  simulated_negative_binomial <- integer(size)  # Create an integer vector of "size" length
  
  for (i in 1:size) {
    counter <- 0
    lower_bound <- 0
    upper_bound <- choose(counter + r - 1, counter) * p^r * (1 - p)^counter #P(X=0) where
    #X~Negative Binomial 
    
    random_number <- runif(1, 0, 1) #generates a single  rn~U(0,1)
    
    while (!(upper_bound > random_number && lower_bound <= random_number)) {
      counter <- counter + 1
      lower_bound <- upper_bound
      upper_bound <- upper_bound + choose(counter + r - 1, counter) * p^r * (1 - p)^counter
    }
    
    simulated_negative_binomial[i] <- counter
  }
  
  return(simulated_negative_binomial)
}





######################################QUESTION 2#########################################