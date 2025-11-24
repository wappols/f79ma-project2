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

library(ggplot2)

# Maximum likelihood estimate p_hat for My_Data
k = 5
x_bar = mean(My_Data)
Quantity1 = k / x_bar

# Deriving the posterior density for p and finding its mean
n = 1000
alpha_post = n * k
beta_post = sum(My_Data - k) + 0.5
p_vals = seq(0.001, 0.999, length.out = n)
posterior_density = dbeta(p_vals, shape1 = alpha_post, shape2 = beta_post)
Quantity2 = alpha_post / (alpha_post + beta_post)

# Defining the Jeffrey's prior
jeffrey_prior = 1 / (p_vals * sqrt(1 - p_vals))

# Putting samples into a data frame for ggplot
prior_posterior_df = rbind(
  data.frame(
    p = p_vals,
    density = jeffrey_prior,
    type = "Jeffrey's Prior"
  ),
  data.frame(
    p = p_vals,
    density = posterior_density,
    type = "Posterior"
  )
)

ggplot(data = prior_posterior_df, aes(x = p, y = density, colour = type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~type, scales = "free_y") +
  labs(
    title = "Jeffrey's Prior vs. Posterior",
    x = "p",
    y = "Density"
  ) +
  scale_colour_manual(values = c(
    "Jeffrey's Prior" = "mediumpurple4",
    "Posterior" = "chartreuse4"
  )) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Using the pmf of the predictive distribution
# to find the value of pi(Z = 5|x)

z = 5

# Using lbeta() to prevent underflowing to 0
Quantity3 = exp(lchoose(z - 1, k - 1) +
                lbeta(alpha_post + k, beta_post + z - k) -
                lbeta(alpha_post, beta_post))

# Simulating 10000 realizations from the posterior
# predictive distribution

realisations = 10000
p_samples = rbeta(realisations, alpha_post, beta_post)

# Note: The extra " + r" term is due to the parameterisation of NegBin
# (rnbinom() counts the no. of failures before k successes)
posterior_predictive_samples = rnbinom(realisations, size = k, prob = p_samples) + k


# Creating a dataframe of the predictive 
# distribution and Full_Data

pred_obs_df = rbind(
  data.frame(
    value = posterior_predictive_samples,
    source = "Predictive"
  ),
  data.frame(
    value = Full_Data,
    source = "Full_Data"
  )
)

# Plotting both histograms together

ggplot(pred_obs_df, aes(x = value, fill = source)) +
  geom_histogram(
    binwidth = 2,
    color = "black",
    alpha = 0.5,
    position = "identity"
  ) +
  facet_wrap(~source, scales = "free") +
  scale_fill_manual(
    values = c(
    "Predictive" = "chartreuse2",
    "Full_Data" = "orchid2")
  ) +
  labs(
    title = "Histograms of Posterior Predictive Distribution and Full_Data",
    x = "Count",
    y = "Frequency",
    fill = "Distribution"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))