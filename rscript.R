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
  return(lambda^x * exp(-lambda) / factorial((x)))
}

pdf_exponential <- function(lambda,x){
  return(lambda *exp(-x*lambda))
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

simulate_gamma <- function(shape,scale,size){}


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
lambda_hat <- 1/length(data$CLM_FREQ) * sum(data$CLM_FREQ)

#Here we plot a histogram of our claim frequency data against the theoretical pdf of parameter lambda hat.
hist(data$CLM_FREQ, breaks = 30, freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
#overlay density
x <- seq(0,10,length.out = 1000)
y <- pdf_poisson(lambda_hat,x) #Theoretical Distribution
lines(x,y)


#Result: Graphically poisson seems to be a good model, to quantify how good of a fit it is we test it agains a theoretical pdf.
# To make the pdfs comparable we will discetise the theoretical pdf to make them "comparable".
empirical_pdf <- rep(0,9)
for(i in 1:8){
  empirical_pdf[i] <- table(data$CLM_FREQ)[i] / sum(data$CLM_FREQ)
}
theoretical_pdf <- c(0,9)
for(i in 1:8){
  theoretical_pdf[i] <- poisson_pdf(i-1,lambda_hat)
}
theoretical_pdf[9] <- 1-sum(theoretical_pdf[1:8])

#We now have two comparable vectors. We test them using Chi-squared test.
chisq.test(empirical_pdf,theoretical_pdf)






#Mixed Poisson Approach N follows  LAMBDA which it self follows Poi(lambda_hat)?


############################################# Q1 Check Negative binomial ###################################################

#MME Estimator
r_hat <- mean(data$CLM_FREQ)^2 / (var(data$CLM_FREQ) - mean(data$CLM_FREQ)) ###Rubbish
p_hat <- 1- (mean(data$CLM_FREQ ) / var(data$CLM_FREQ )) ##Rubbish

#Here we plot a histogram of our claim frequency data against the theoretical pdf of parameter p_hat and r_hat.
hist(data$CLM_FREQ, breaks = 30, freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
#overlay density
x <- seq(0,10,length.out = 11)
y <- dnbinom(x,size = r_hat,prob = p_hat) #Theoretical Distribution
lines(x,y)



#To make the pdf comparable, we discretise and apply the chi.sq test
theoretical_pdf <- c(0,9)
for(i in 1:8){
  theoretical_pdf[i] <- dnbinom(x=i-1 , size = r_hat, prob = p_hat)
}
theoretical_pdf[9] <- 1-sum(theoretical_pdf[1:8])






############################################################################################################################
########################################################### Q2 #############################################################
############################################################################################################################



############################################# Q2 Check Exponential #########################################################
claim_size_vector <- apply(data[,3:9],1 ,sum)

claim_size_vector_ecxl_zero <- claim_size_vector[claim_size_vector>0]

hist(claim_size_vector_ecxl_zero,breaks = 20,freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
x <- seq(0,3500,length.out = 1000)

#Maximum Likelihood Estimator
lambda_hat <- 1 / (1/length((claim_size_vector_ecxl_zero)) * sum(claim_size_vector_ecxl_zero))
y <- pdf_exponential(lambda_hat,x) #Theoretical Distribution
lines(x,y)


## Test whether exponential is a good model.

simulate_exponential(lambda_hat,length(claim_size_vector_ecxl_zero))

ks.test(claim_size_vector_ecxl_zero, "pexp", lambda_hat) #kolomogorov smirnov test

qqplot(claim_size_vector_ecxl_zero,simulate_exponential(lambda_hat,length(claim_size_vector_ecxl_zero)))



################################################ Q2 Check Gamma ############################################################



hist(claim_size_vector_ecxl_zero,breaks = 20,freq = FALSE, main = "Empirical Histogram with Theoretical PDF")
x <- seq(0,3500,length.out = 812)

#Maximum Likelihood Estimator
gamma_estimated_theta <- var(claim_size_vector_ecxl_zero) / mean(claim_size_vector_ecxl_zero)
gamma_estimated_k <- mean(claim_size_vector_ecxl_zero)^2/ var(claim_size_vector_ecxl_zero)

y_2 <- dgamma(x,shape = gamma_estimated_k, scale = gamma_estimated_theta ) #Theoretical Distribution
lines(x,y_2)


qqplot(claim_size_vector_ecxl_zero,rgamma(1000,shape = gamma_estimated_k,gamma_estimated_theta))


############################################################################################################################
########################################################### Q3 #############################################################
############################################################################################################################


################################################# Q3 Monte Carlo Poisson ###################################################

#Here we will simulate a Poisson random variables using the same parameter lambda we obtained from the MLE estimator.
#The simulation algorithm is in the function simulate poisson. To assess the reliability of our Monte Carlo estimator to
#Our data we follow two approaches motivated by the concept of multiple testing. That is, due to randomness a simple one
#time chi squared test could be significant whereas it should not or vice versa. To mitigate this issue we will generate
#1000 randomly generated poisson random variables with the parameter we estimated and test against each of them obtaining
#1000 pv-values instead of a single. We could then take its average value as a more robust statistical test.

#In that we follow two approaches 
#1) We apply the goodness of fit on the 1000 randomly generated poisson.
#2) We apply the goodness of fit on 1000 randomly generated poisson PDFs. Which should overall be less volatile since
# it would not be susceptible to changes in magnitude in comparison to the first approach.

#Approach 1
poisson_simulations_list <- vector(mode = "list", length = 1000)
for(i in 1:1000) {
  poisson_simulations_list[[i]] <- vector(mode = "list", length = 1000)
}
for(i in 1:1000){
  poisson_simulations_list[[i]] <- simulate_poisson(lambda_hat,1000)
}

pval_vector <- vector()
for(i in 1:1000){
  pval_vector[i] <- chisq.test(poisson_simulations_list[[i]],data$CLM_FREQ)$p.val
}
mean(pval_vector)

#Approach 2
#Essentially we will create a similar table containing comparable pdfs based on the poisson monte carlo estimators
simulated_pdfs <- vector(mode = "list", length = 1000)
for(i in seq_along(simulated_pdfs)) {
  simulated_pdfs[[i]] <- vector(mode = "list", length = 9)
}

generate_random_comparable_poisson <- function(lambda){
  
  my_poisson_simulation <- simulate_poisson(lambda,1000)
  
  
  random_vector <- c()
  
  for (i in 1:8) {
    prob <- table(my_poisson_simulation)[as.character(i-1)] / 1000
    if (is.na(prob)) {
      random_vector[i] <- 0
    } else {
      random_vector[i] <- prob
    }
    
  }
  random_vector[9] <- 1- sum(random_vector[1:8])
  return(random_vector)    
}




for(i in 1:1000){
  simulated_pdfs[[i]] <- generate_random_comparable_poisson(lambda_hat)##issue here
}
#In deed much more stable p-values in comparison to using randomly generated poisson
pval_vector <- c()
for(i in 1:1000){
  pval_vector[i] <- chisq.test(simulated_pdfs[[i]],empirical_pdf)$p.val
}

mean(pval_vector)




################################################# Q3 Variance Reduction Poisson ###################################################

# We now consider variance reduction techniques
# Does it make sense, Expected value of poisson is the variance!! Do we really want to reduce the variance??











################################################# Q3 Monte Carlo Gamma ############################################################












