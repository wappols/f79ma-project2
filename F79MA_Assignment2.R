#
#     R-code to generate data for F79MA Assignment 2.  
#     This code MUST be placed at the start of your own R-script. 
#     You must edit the variable last_four_digits to fit 
#     your own student ID number
#
RNGkind(sample.kind = "Rejection")
last_four_digits = 3289 ## change this number

set.seed(last_four_digits)

dataset <- read.csv(file = "count_data.csv", header = TRUE)
Full_Data <- dataset$x
My_Data <- sample(Full_Data,size=1000,replace=FALSE)


#####################################################################
# Please insert your R code after this line
#####################################################################

# Maximum likelihood estimate p_hat for My_Data
k = 5
x_bar = mean(My_Data)
Quantity1 = k / x_bar

# Deriving the posterior density for p and finding its mean
n = 1000
s = sum(My_Data - k)
alpha_post = n * k
beta_post = s + (1/2)
posterior_samples = rbeta(n, shape1 = k * n, shape2 = beta_post)
Quantity2 = mean(posterior_samples)

# Generating samples for prior
eps = 0.001
p_values = seq(0.001, 0.999, length = n)
prior_samples = rbeta(n, shape1 = eps, shape2 = 0.5)

# Plotting the prior and posterior distributions
hist(prior_samples, breaks = 1000, xlim = c(0, 0.02), probability = TRUE, col = 'red')
hist(posterior_samples, probability = TRUE, col = 'blue')