############################################################################################################################
####################################################### Read libraries #####################################################
############################################################################################################################
library(readr) # Check encoding
library(magrittr) # Pipe operator
library(dplyr) # data wrangling
library(purrr) # efficient one liner pattern-matching
library(ggplot2) # additional graphics



############################################################################################################################
#################################################### Read and store data ###################################################
############################################################################################################################

data_orig <-read.csv(file = "DATA_SET_8.csv",sep = ";",encoding = "ASCII") %>%
  as.data.frame() # specify encodeing to use regex

#Always keep original data as is.
data <- data_orig
rm(data_orig) #memory management
############################################################################################################################
#################################################### Function Definitions ##################################################
############################################################################################################################
# Check whether number of claims matches the number of row entries.

# In the end store data here
############################################## Prototype ##################################################################
# models <- list( claim_freq_models = list (
#                                             poisson = list(
#                                                             pdf = function(lambda,x){
#                                                               return(lambda^x * exp(-lambda) / factorial((x)))}
#                                                             random_poisson = "to be defined",
#                                                             hist,
#                                                             lines),
#                                             
#                                             negative_binomial = list(
#                                                             pdf = function(r, p, k){return(choose((k+r-1),k) * p^r * (1-p)^k)}
#                                             )),
#                 claim_severity_models = list (
#                                             exponential = list(
#                                                           pdf = function(lambda,x){
#                                                             return(lambda *exp(-x*lambda))}),
#                                             gamma = list(
#                                                           pdf = function(alpha, lamba, x ){
#                                                             #no idea if the following works 
#                                                             if (x < 0) 
#                                                               {return(0.0) } 
#                                                             else 
#                                                               #needs gamma function 
#                                                               {return(lambda * exp(- lambda * x )*(lambda*x)^(alpha -1 )/1 )}
#                                                           }
#                                             ),
#                                             ))

##########################################################################################################################
##perhaps remove
pdf_poisson <- function(lambda,x){
  return((lambda^x) * exp(-lambda) / factorial(x))
}

pdf_exponential <- function(lambda,x){
  return(lambda *exp(-lambda))
}
##unitel here
#Our Own RNG? function?

simulate_poisson <- function(lambda, size){
  simulated_poisson <- rep(0, size)
  for (i in 1:size){
    sum_of_exponentials <- 0.0 
    counter <- 0 
    while (! (sum_of_exponentials > lambda )) {
      initial_random_number <- runif(1,0,1) 
      #length 1 lower bound 0 upper bound 1 
      simulated_exponential <- - log(1 - initial_random_number ) 
      #given by inversion method for parameter = 1 
      sum_of_exponentials <- sum_of_exponentials + simulated_exponential
      counter <- counter + 1 
    }
    simulated_poisson[i] <- counter -1
  }
  return (simulated_poisson)
}

simulate_exponential <- function(lambda, size){
  simulated_exponential <- rep(0, size)
  for (i in 1:size){
    initial_random_number <- runif(1,0,1)
    single_simulated_exponential <- - log(1 - initial_random_number )/lambda
    #given by inversion method
    simulated_exponential[i] <- single_simulated_exponential
  }
  return (simulated_exponential)
}

simulate_erlang <- function(size,shape,scale_lambda){
  simulation_vector <- c()
  
  for(j in 1:size){
    simulated_gamma <- 0
      for(i in 1:shape){
        simulated_gamma <- simulated_gamma + simulate_exponential(scale_lambda,1)
      }
    simulation_vector[j] <- simulated_gamma
  }
  return(simulation_vector)
}

simulate_nb <- function(size, r, p) {
  random_numbers <- runif(size,0,1)
  result <- c()
  for(i in 1:size){
  counter <- 0 
  lower_bound <- 0 
  upper_bound <- choose(counter + r - 1 , counter )* p^r * (1-p)^counter
  while (! ((upper_bound > random_numbers[i] ) & (lower_bound <= random_numbers[i]) )) {
    counter <- counter + 1 
    lower_bound <- upper_bound 
    upper_bound <- upper_bound +  choose(counter + r - 1 , counter )* p^r * (1-p)^counter
    #cat("lower bound ", lower_bound, "   upper bound ", upper_bound, " random number ", random_number, "   counter ", counter)
    #print(" ")
  }
  result[i] <- counter
  }
  return(result)
}


############################################################################################################################
################################################# Data Quality Check #######################################################
############################################################################################################################
#Read data types
for(col in colnames(data)){
  print(typeof(data[,col]))
}
#[1] "integer"
#[2] "integer"
#[3] "character"  => should be numeric
#[4] "character"  => should be numeric
#[5] "character"  => should be numeric
#[6] "character"  => should be numeric
#[7] "character"  => should be numeric
#[8] "character"  => should be numeric
#[9] "character"  => should be numeric
#[10] "character" => should be numeric
#[11] "character" => should be numeric
#[12] "double"
#[13] "character"
#[14] "character"
#[15] "double"

##Careful conversion from string to numeric
#Count data amount in the columns to be converted
missing_data_count <- c() # To be used for validation after conversion
for(i in 3:9){
  counter <- 0
  for(j in 1:nrow(data)){
    if(is.na(data[j,i])){
      counter= counter + 1
    }
    missing_data_count[i-2] <- counter
  }
}
#Result: No missing data

#Use regex to remove potential white spaces.
for(i in 3:9){
    data[,i] <- gsub("\\s+", "", data[,i])
}

#Count data amount in the columns to be converted
zero_data_count <- c() # To be used for validation after conversion
for(i in 3:9){
  counter = 0
  for(j in 1:nrow(data)){
    if(data[j,i]=="-"){
      data[j,i]<-"0" #to make-sure conversion went correctly without NA coercion
      counter= counter + 1
    }
  }
  zero_data_count[i-2] <- counter
}

#Convert to numeric
for(i in 3:9){
    data[i] <- as.double(data[[i]])
}


zero_data_count_after_conversion <- c() # To be used for validation after conversion
for(i in 3:9){
  counter_2 = 0
  for(j in 1:nrow(data)){
    if(data[j,i]==0){
      counter_2= counter_2 + 1
    }
    zero_data_count_after_conversion[i-2] <- counter_2
  }
  counter_2 <- 0
}
# zero data count matches
# check correct conversion of -ve values
data[3][data[3]<0] #still exist
#check greater than 3 digit values
data[4][data[4]>999]


# Analyse negative values
negative_value_indices <- rep(FALSE,nrow(data))
for(i in 3:9){
  for(j in 1:nrow(data)){
    if(data[j,i]<0){
      negative_value_indices[j] <- TRUE
    }
  }
}
dplyr::filter(data,negative_value_indices)
#Result: We decided on excluding two policies with -ve CLM_AMT_1 values
data <- dplyr::filter(data,!negative_value_indices)



############################################################################################################################
################################################### Data Exploration #######################################################
############################################################################################################################


# Get summary of the data
summary(data)

features_cat <- c(  'CAR_USE',
                    'CAR_TYPE',
                    'GENDER',
                    'AREA')

# convert to factors for plotting
for (v in features_cat) {
  data[,v] <- as.factor(data[,v])
}


features_int <- 'Age'


claims_amounts <- c('CLM_AMT_1',
                    'CLM_AMT_2',
                    'CLM_AMT_3',
                    'CLM_AMT_4',
                    'CLM_AMT_5',
                    'CLM_AMT_6',
                    'CLM_AMT_7')



for (f in features_cat) {
  plot(data[,f], main=f, xlab='', las=2,col=data$CLM_FREQ)
  grid()
}
# Results: (%-wise)
# Rural has higher impact on the number of claims
# Being Female has higher impact on the number of claims
# Panel truck has higher impact on the number of claims
# Commercial car use has higher impact on the number of claims


default_par <- par(no.readonly = TRUE) #save default drawing parameter

for (f in claims_amounts) {
  plot(data[,f], main=f, xlab='' ,las=2,col=data$CLM_FREQ)
  grid()
}
#CLM_AMT_6 and CLM_AMT_7 rare occurrence


plot(data$CLM_FREQ, data$AGE, main='Age', xlab='',las = 2)
grid()


############################################################################################################################
########################################################### Q1 #############################################################
############################################################################################################################


#################################################### Q1 Check Poisson ######################################################
#Here we assume that the number of claims follows a poisson distribution. We apply the maximum likelihood method to obtain
#an estimator for the poisson parameter lambda.


#Maximum likelihood Estimator for poisson case to estimate lambda
lambda_hat_poisson <- 1/length(data$CLM_FREQ) * sum(data$CLM_FREQ)

#Here we plot a histogram of our claim frequency data against the theoretical pdf of parameter lambda hat.
hist(data$CLM_FREQ, breaks = 30, freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
#overlay density
x <- seq(0,10,length.out = 11)
y <- dpois(x,lambda=lambda_hat_poisson) #Theoretical Distribution
lines(x,y)


#Result: Graphically poisson seems to be a good model, to quantify how good of a fit it is we test it agains a theoretical pdf.
# To make the pdfs comparable we will discetise the theoretical pdf to make them "comparable".
empirical_pdf <- rep(0,9)
for(i in 1:8){
  empirical_pdf[i] <- table(data$CLM_FREQ)[i] / sum(data$CLM_FREQ)
}
theoretical_pdf <- c(0,9)
for(i in 1:8){
  theoretical_pdf[i] <- dpois(i-1,lambda =lambda_hat_poisson)
}
theoretical_pdf[9] <- 1-sum(theoretical_pdf[1:8])

#We now have two comparable vectors. We test them using Chi-squared test.
chisq.test(empirical_pdf,theoretical_pdf)
# Warning message because the frequency in some bins is less than 5 which considered to be minimum





#Mixed Poisson Approach N follows  LAMBDA which it self follows Poi(lambda_hat)?


############################################# Q1 Check Negative binomial ###################################################

#MoM Estimator
p_hat <- 1 / var(data$CLM_FREQ) * mean(data$CLM_FREQ)
r_hat <- mean(data$CLM_FREQ)*p_hat /(1-p_hat)

#Here we plot a histogram of our claim frequency data against the theoretical pdf of parameter p_hat and r_hat.
hist(data$CLM_FREQ, breaks = 30, freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
#overlay density
x <- seq(0,10,length.out = 11)
y <- dnbinom(x,size = r_hat,prob = p_hat) #Theoretical Distribution
lines(x,y)


#To make the pdf comparable, we discretise and apply the chi.sq test
theoretical_pdf_nb <- c(0,9)
for(i in 1:8){
  theoretical_pdf[i] <- dnbinom(x=i-1 , size = r_hat, prob = p_hat)
}
theoretical_pdf[9] <- 1-sum(theoretical_pdf[1:8])

chisq.test(empirical_pdf,theoretical_pdf)
# Warning message because the frequency in some bins is less than 5 which considered to be minimum



#******************************************************* Q1 Result *********************************************************

hist(data$CLM_FREQ, breaks = 30, freq = FALSE, main = "Empirical Histogram with Theoretical PDFs")
x <- seq(0,10,length.out = 11)
y <- dpois(x, lambda = lambda_hat_poisson) #Theoretical Distribution
y_2 <- dnbinom(x,size = r_hat,prob = p_hat) #Theoretical Distribution
lines(x,y, col = "Red")
lines(x,y_2, col = "Blue")
legend("topright", legend = c("Poisson Distribution", "Negative Binomial Distribution"),
       col = c("red", "blue"), lty = 1)

#Claim Frequency:
#We decided to stick with poisson for claim frequency. Both p-values against the theoretical distribution are exactly
#the same. However, poisson has interesting properties.... etc..






############################################################################################################################
########################################################### Q2 #############################################################
############################################################################################################################



############################################# Q2 Check Exponential #########################################################

claim_size_vector <- vector()
for  (i in 3:9 ){
  for (j in 1:nrow(data)) 
    if ( data[j,i] > 0 ){
      claim_size_vector <- append(claim_size_vector, data[j,i])
    }
}


hist(claim_size_vector,breaks = 20,freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
x <- seq(0,3500,length.out = length(claim_size_vector))

#Maximum Likelihood Estimator
lambda_hat_exp <- 1 / (1/length((claim_size_vector)) * sum(claim_size_vector))
y <- dexp(x, lambda = lambda_hat_exp) #Theoretical Distribution
lines(x,y)


## Test whether exponential is a good model.

ks.test(claim_size_vector, rate = lambda_hat_exp) #kolomogorov smirnov test


theoretical_quantiles <- qexp(ppoints(length(claim_size_vector)),rate = lambda_hat_exp )

qqplot(theoretical_quantiles, claim_size_vector,
       xlab = "Theoretical Quantiles", ylab = "Observed Quantiles",
       main = "QQ Plot for Exponential Distribution")
abline(0, 1, col = "red", lty = 2)  # Add reference line



################################################ Q2 Check Gamma ############################################################



hist(claim_size_vector,breaks = 20,freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
x <- seq(0,3500,length.out = length(claim_size_vector))

#Maximum Likelihood Estimator
gamma_estimated_theta <- var(claim_size_vector) / mean(claim_size_vector)
gamma_estimated_k <- mean(claim_size_vector)^2/ var(claim_size_vector)

y_2 <- dgamma(x,shape = gamma_estimated_k, scale = gamma_estimated_theta ) #Theoretical Distribution
lines(x,y_2)



#Additionally we include 11 plot against the theoretical distribution
theoretical_quantiles <- qgamma(ppoints(100), shape = gamma_estimated_k, scale = gamma_estimated_theta )

qqplot(theoretical_quantiles, claim_size_vector,
       xlab = "Theoretical Quantiles", ylab = "Observed Quantiles",
       main = "QQ Plot for Gamma Distribution")
abline(0, 1, col = "red", lty = 2)  # Add reference line

ks.test(claim_size_vector, theoretical_quantiles)


qqplot(claim_size_vector,rgamma(length(claim_size_vector),
                                            shape = gamma_estimated_k,scale = gamma_estimated_theta),
                                            main = "QQ Plot for Gamma Distribution")









#******************************************************* Q2 Result *********************************************************
#Claim Severity:
#With regard to claim frequency we decided to select the gamma distribution. 

hist(claim_size_vector,breaks = 20,freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
x <- seq(0,3500,length.out = 812)

y <- dexp(x,lambda = lambda_hat_exp)
y_2 <- dgamma(x,shape = gamma_estimated_k, scale = gamma_estimated_theta ) #Theoretical Distribution
lines(x,y, col = "Red")
lines(x,y_2, col = "Blue")
legend("topright", legend = c("Exonential Distribution", "Gamma Distribution"),
       col = c("red", "blue"), lty = 1)




############################################################################################################################
########################################################### Q3 #############################################################
############################################################################################################################

set.seed(2023)
############################################## Q3 Monte Carlo Negative Binomial ###########################################
#To assess the reliability of our monte carlo estimators in relation to the data we have, we will simulate 1000 negative 
#binomial random variables using the same parameter p and r we obtained from the ***MoM*** estimator. We will do the following
#1) Plot histograms of the mean and variance values of the simulation to compare to the data.
#2) Plot a histogram of the average of those 1000 simulations (converting to the true value) and compare them to the data.
#3) Make a sample t-test to test whether the mean value could serve as an expectation of the mean value of the simluations



#Approach 1
nb_simulations_list <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  nb_simulations_list[[i]] <- vector(mode = "list", length = 1000)
}
for(i in 1:1000){
  nb_simulations_list[[i]] <- rnbinom(1000,size = r_hat, p = p_hat)##Switch with our own simulation function
}

mean_vector_nb <- c()
var_vector_nb <- c()

for(i in 1:1000){
  mean_vector_nb[i] <- mean(nb_simulations_list[[i]])
  var_vector_nb[i] <- var(nb_simulations_list[[i]])
}


hist(mean_vector_nb, main = "Expectation of 1000 Negative Binomial Simulations")
abline(v = mean(data$CLM_FREQ),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)

hist(var_vector_nb, main = "Variance of 1000 Negative Binomial Simulations")
abline(v = mean(var(data$CLM_FREQ)),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)



#Approach 2
#We can also plot the claim frequency against the average of the 10,000 simulations
generate_random_comparable_nb <- function(r,p){
  
  my_nb_simulation <- rnbinom(1000,size = r, p = p)
  
  
  random_vector <- c()
  
  for (i in 1:8) {
    result <- table(my_nb_simulation)[as.character(i-1)] 
    if (is.na(result)) {
      random_vector[i] <- 0
    } else {
      random_vector[i] <- result
    }
    
  }
  random_vector[9] <- 1000- sum(random_vector[1:8])
  return(random_vector)    
}




nb_simulations_list_comparable <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  nb_simulations_list_comparable[[i]] <- vector(mode = "list", length = 9)
}


for(i in 1:1000){
  nb_simulations_list_comparable[[i]] <- generate_random_comparable_nb(r=r_hat,p = p_hat)
}

# We can plot the average of the 1000 simulations against the data
avg_of_simulations_nb <- c()
for(j in 1:9){
  for(i in 1:1000){
    avg_of_simulations_nb[j] <- mean(sapply(nb_simulations_list_comparable, "[[", j))
  }
}

barplot(table(data$CLM_FREQ), col = gray(0,0.5), beside = T)
barplot(avg_of_simulations_nb, col = gray(1,0.8),beside = T,add = T)
legend("topright", legend = c("Original Data", "Average of 1000 Simulations"), 
       fill = c(gray(0, 0.5), gray(1, 0.8)))


#Approach 3
t.test(x = mean_vector_nb, mu = mean(data$CLM_FREQ))





########################################### Q3 Variance reduction Negative Binomial #######################################

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



nb_simulations_list_anthithetic <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  nb_simulations_list_anthithetic[[i]] <- vector(mode = "list", length = 1000)
}
for(i in 1:1000){
  nb_simulations_list_anthithetic[[i]] <- antithetic_variates_nb(size = 1000,r = r_hat, p = p_hat)
}

mean_vector_nb_antithetic <- c()
var_vector_nb_antithetic <- c()



for(i in 1:1000){
  mean_vector_nb_antithetic[i] <- mean(nb_simulations_list_anthithetic[[i]])
  var_vector_nb_antithetic[i] <- var(nb_simulations_list_anthithetic[[i]])
}


hist(mean_vector_nb_antithetic, main = "Expectation of 1000 Negative Binomial Simulations - Antithetic Method")
abline(v = mean(data$CLM_FREQ),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)

hist(var_vector_nb_antithetic, main = "Variance of 1000 Negative Binomial Simulations - Antithetic Method")
abline(v = mean(var(data$CLM_FREQ)),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)








#******************************************************** Results ********************************************************#

par(mfrow = c(2, 2),cex.main = 0.8)
hist(mean_vector_nb, main = "Expectation NB w/o variance reduction")
abline(v = mean(data$CLM_FREQ),col="Red")


hist(var_vector_nb, breaks = 40,main = "Variance NB w/o variance reduction")
abline(v = mean(var(data$CLM_FREQ)),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)


hist(mean_vector_nb_antithetic, main = "Expectation NB Antithetic Method")
abline(v = mean(data$CLM_FREQ),col="Red")


hist(var_vector_nb_antithetic,breaks = 40, main = "Variance NB Antithetic Method")
abline(v = mean(var(data$CLM_FREQ)),col="Red")
par(no.readonly = T)
length(claim_size_vector)


##################################################### Q3 Monte Carlo Gamma ################################################

#Here we will simulate random gamma distributions with the Method of moments scale and shape parameters and test our data against
#each of them and take the mean p-value.

gamma_simulations_list <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  gamma_simulations_list[[i]] <- vector(mode = "list", length = length(claim_size_vector))
}
for(i in 1:1000){
  gamma_simulations_list[[i]] <- rgamma(length(claim_size_vector),shape = gamma_estimated_k, 
                                          scale = gamma_estimated_theta)##Switch with our own simulation function
}



mean_vector_gamma <- c()
var_vector_gamma <- c()

for(i in 1:1000){
  mean_vector_gamma[i] <- mean(gamma_simulations_list[[i]])
  var_vector_gamma[i] <- var(gamma_simulations_list[[i]])
}


hist(mean_vector_gamma, main = "Expectation of 1000 Gamma Simulations")
abline(v = mean(claim_size_vector),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)

hist(var_vector_gamma, main = "Variance of 1000 Gamma Simulations")
abline(v = mean(var(claim_size_vector)),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)


t.test(x = mean_vector_gamma, mu = mean(claim_size_vector))
t.test(x = var_vector_gamma, mu = mean(var(claim_size_vector)))





############################################### Q3 variance reduction Gamma ###############################################





simulate_gamma_with_u_input <- function(random_numbers, shape, scale) {
  return(qgamma(random_numbers, shape = shape , scale = scale))
}


antithetic_variates_gamma <- function(size, shape, scale){
  if(size %% 2 == 0){
    u1_vector <- runif(size/2,0,1)
    u2_vector <- 1-u1_vector
  } else {
    u1_vector <- runif(size/2 +1,0,1)
    u2_vector <- 1-u1_vector
  }
  
  f_u1 <- simulate_gamma_with_u_input(random_numbers=u1_vector,shape = shape, scale = scale)
  f_u2 <- simulate_gamma_with_u_input(random_numbers=u2_vector,shape = shape, scale = scale)
  
  return(append(f_u1,f_u2))
}




gamma_simulations_list_anthithetic <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  gamma_simulations_list_anthithetic[[i]] <- vector(mode = "list", length = length(claim_size_vector))
}
for(i in 1:1000){
  gamma_simulations_list_anthithetic[[i]] <- antithetic_variates_gamma(size = length(claim_size_vector),
                                                                       shape = gamma_estimated_k,
                                                                        scale = gamma_estimated_theta)
}

mean_vector_gamma_antithetic <- c()
var_vector_gamma_antithetic <- c()


for(i in 1:1000){
  mean_vector_gamma_antithetic[i] <- mean(gamma_simulations_list_anthithetic[[i]])
  var_vector_gamma_antithetic[i] <- var(gamma_simulations_list_anthithetic[[i]])
}


hist(mean_vector_gamma_antithetic, main = "Expectation of 1000 Gamma Simulations - Antithetic Method")
abline(v = mean(data$CLM_FREQ),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)

hist(var_vector_gamma_antithetic, main = "Variance of 1000 Gamma Simulations - Antithetic Method")
abline(v = mean(var(data$CLM_FREQ)),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)



# Control Variate method:
# We introduce Pareto with parameter 500 and 8
x <- sort(rgamma(length(claim_size_vector),shape = gamma_estimated_k, scale = gamma_estimated_theta))
y <- sort(rPareto(length(claim_size_vector), 500, 8, truncation = NULL))
z <- x - cov(x,y)*1/var(y)*(y - 8*500/(8-1)) #Works


#Importance sampling, higher variance :o
#I = Gamma
#F = F_tilda = lambda*e^(-lambda*x)
x <- rexp(1000,lambda_hat_exp)
ICE <- dgamma(x,shape = gamma_estimated_k, scale = gamma_estimated_theta) / dexp(x,rate = lambda_hat_exp) * x




#*******************************************************Results*********************************************************#


hist(mean_vector_gamma, main = "Expectation Gamma w/o variance reduction ")
abline(v = mean(claim_size_vector),col="Red")


hist(var_vector_gamma, main = "Variance Gamma w/o variance reduction",breaks = 40)
abline(v = mean(var(claim_size_vector)),col="Red")
legend("topright", legend = "Data", col = "red", lty = 1)


hist(mean_vector_gamma_antithetic, main = "Expectation Gamma Simulations - Antithetic Method")
abline(v = mean(claim_size_vector),col="Red")


hist(var_vector_gamma_antithetic, main = "Variance Gamma - Antithetic Method", breaks = 40)
abline(v = var(claim_size_vector),col="Red")










