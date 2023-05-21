source("rscript.r")
#############################Negative Binomial##############################


library(MASS)

sample_mean <- mean(data$CLM_FREQ) #calculating the sample mean
sample_mean
sample_variance <- var(data$CLM_FREQ) #calculating the sample variance
sample_variance


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

pdf_negative_bin <- function(x, r, p) {
  choose(x + r - 1, x) * (1 - p)^x * p^r
} 

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
