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
n = 100
s = sum(My_Data - 5)
posterior = rbeta(n, shape1 = k * n, s + (1/2))
Quantity2 = mean(posterior)